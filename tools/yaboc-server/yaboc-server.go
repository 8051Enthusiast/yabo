package main

import (
	"crypto/sha256"
	"errors"
	"io"
	"net/http"
	"os"
	"os/exec"
	"strconv"
	"time"

	lru "github.com/hashicorp/golang-lru/v2"
)

type config struct {
	// The timeout for the compile command
	CompileTimeout time.Duration
	// The port the server listens on
	Port int
	// The cache size
	CacheSize int
}

func get_config() config {
	conf := config{
		CompileTimeout: 10 * time.Second,
		Port:           8000,
		CacheSize:      128,
	}
	if compileTimeout := os.Getenv("YABOCSERV_COMPILE_TIMEOUT"); compileTimeout != "" {
		compileTimeout, err := time.ParseDuration(compileTimeout)
		if err != nil {
			panic(err)
		}
		conf.CompileTimeout = compileTimeout
	}
	if port := os.Getenv("YABOCSERV_PORT"); port != "" {
		port, err := strconv.Atoi(port)
		if err != nil {
			panic(err)
		}
		conf.Port = port
	}
	if cacheSize := os.Getenv("YABOCSERV_CACHE_SIZE"); cacheSize != "" {
		cacheSize, err := strconv.Atoi(cacheSize)
		if err != nil {
			panic(err)
		}
		conf.CacheSize = cacheSize
	}
	return conf
}

func evict(_ requestHash, file string) {
	os.Remove(file)
}

func compile(code []byte, cmd []string, tail bool) ([]byte, error) {
	infile, err := os.CreateTemp("", "yabo_*.yb")
	if err != nil {
		return nil, err
	}
	defer os.Remove(infile.Name())
	_, err = infile.Write(code)
	infile.Close()
	if err != nil {
		return nil, err
	}

	outfile, err := os.CreateTemp("", "yabo_*.so")
	if err != nil {
		return nil, err
	}
	defer outfile.Close()
	defer os.Remove(outfile.Name())

	if tail {
		cmd = append(cmd, "--target-features=+tail-call")
	}
	cmd = append(cmd, infile.Name(), outfile.Name())

	compileCmd := exec.Command(cmd[0], cmd[1:]...)
	_, err = compileCmd.Output()
	if err != nil {
		return nil, err
	}

	out, err := os.ReadFile(outfile.Name())
	if err != nil {
		return nil, err
	}
	return out, nil
}

func set_cors(w http.ResponseWriter) {
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "OPTIONS, POST, PUT")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Cross-Origin-Embedder-Policy", "require-corp")
	w.Header().Set("Cross-Origin-Opener-Policy", "same-origin")
}

type requestHash struct {
	sourceHash [32]byte
	tail       bool
}

type server struct {
	cache  *lru.Cache[requestHash, string]
	config config
}

func hash(source []byte, tail bool) requestHash {
	hash := sha256.Sum256(source)
	return requestHash{
		sourceHash: hash,
		tail:       tail,
	}
}

func (serv *server) addCacheEntry(hash requestHash, compiled []byte) {
	file, err := os.CreateTemp("", "yabo_*.so")
	if err != nil {
		return
	}
	defer file.Close()
	_, err = file.Write(compiled)
	if err != nil {
		return
	}
	serv.cache.Add(hash, file.Name())
}

func (serv *server) getCacheEntry(hash requestHash) []byte {
	file, ok := serv.cache.Get(hash)
	if !ok {
		return nil
	}
	compiled, err := os.ReadFile(file)
	if err != nil {
		serv.cache.Remove(hash)
		return nil
	}
	return compiled
}

func (serv *server) compileHandler(w http.ResponseWriter, r *http.Request, cmd []string) {
	set_cors(w)
	if r.Method == http.MethodOptions {
		return
	}
	if r.Method != http.MethodPost && r.Method != http.MethodPut {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}
	code, err := io.ReadAll(r.Body)
	if err != nil {
		http.Error(w, "Error reading request body", http.StatusBadRequest)
		return
	}
	tail := r.URL.Query().Get("tail") == "1"
	hash := hash(code, tail)
	if compiled := serv.getCacheEntry(hash); compiled != nil {
		w.Write(compiled)
		return
	}
	res := make(chan []byte, 1)
	errChan := make(chan error, 1)
	go func() {
		out, err := compile(code, cmd, tail)
		if err != nil {
			errChan <- err
		} else {
			res <- out
		}
	}()
	select {
	case out := <-res:
		serv.addCacheEntry(hash, out)
		w.Write(out)
	case err := <-errChan:
		compileError := &exec.ExitError{}
		if errors.As(err, &compileError) {
			w.WriteHeader(http.StatusUnprocessableEntity)
			w.Write(compileError.Stderr)
		} else {
			http.Error(w, "Error compiling", http.StatusInternalServerError)
		}
	case <-time.After(serv.config.CompileTimeout):
		http.Error(w, "Compile timeout", http.StatusRequestTimeout)
	}
}

func serveStatic(w http.ResponseWriter, r *http.Request, root string) {
	set_cors(w)
	if r.Method == http.MethodOptions {
		return
	}
	if r.Method != http.MethodGet {
		http.Error(w, "Method is not allowed", http.StatusMethodNotAllowed)
		return
	}
	rootFs := os.DirFS(root)
	http.ServeFileFS(w, r, rootFs, r.URL.Path)
}

func main() {
	if len(os.Args) < 2 {
		panic("No command given")
	}
	compileCommand := os.Args[1:]
	conf := get_config()
	cache, err := lru.NewWithEvict(conf.CacheSize, evict)
	if err != nil {
		panic("Could not create cache: " + err.Error())
	}
	server := &server{
		config: conf,
		cache:  cache,
	}
	http.HandleFunc("/compile", func(w http.ResponseWriter, r *http.Request) {
		server.compileHandler(w, r, compileCommand)
	})
	root := "."
	if env_root := os.Getenv("YABOCSERV_ROOT"); env_root != "" {
		root = env_root
	}
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		serveStatic(w, r, root)
	})
	http.ListenAndServe(":"+strconv.Itoa(conf.Port), nil)
}
