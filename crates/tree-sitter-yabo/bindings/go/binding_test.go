package tree_sitter_yabo_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-yabo"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_yabo.Language())
	if language == nil {
		t.Errorf("Error loading Yabo grammar")
	}
}
