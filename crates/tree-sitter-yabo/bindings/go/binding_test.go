package tree_sitter_yabo_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_yabo "github.com/8051enthusiast/yabo/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_yabo.Language())
	if language == nil {
		t.Errorf("Error loading yabo grammar")
	}
}
