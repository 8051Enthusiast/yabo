package main

import (
	"errors"
	"io"
	"net/http"
	"os"
	"os/exec"
	"strconv"
	"time"
)

type config struct {
	// The timeout for the compile command
	CompileTimeout time.Duration
	// The port the server listens on
	Port int
}

func get_config() config {
	conf := config{
		CompileTimeout: 10 * time.Second,
		Port:           8000,
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
	return conf
}

func compile(code []byte, cmd []string) ([]byte, error) {
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

func compileHandler(w http.ResponseWriter, r *http.Request, conf config, cmd []string) {
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
	res := make(chan []byte, 1)
	errChan := make(chan error, 1)
	go func() {
		out, err := compile(code, cmd)
		if err != nil {
			errChan <- err
		} else {
			res <- out
		}
	}()
	select {
	case out := <-res:
		w.Write(out)
	case err := <-errChan:
		compileError := &exec.ExitError{}
		if errors.As(err, &compileError) {
			w.WriteHeader(http.StatusUnprocessableEntity)
			w.Write(compileError.Stderr)
		} else {
			http.Error(w, "Error compiling", http.StatusInternalServerError)
		}
	case <-time.After(conf.CompileTimeout):
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
	http.HandleFunc("/compile", func(w http.ResponseWriter, r *http.Request) {
		compileHandler(w, r, conf, compileCommand)
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
