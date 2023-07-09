(setq treesit-language-source-alist
      '((nix "https://github.com/nix-community/tree-sitter-nix")))

(treesit-install-language-grammar 'nix)
