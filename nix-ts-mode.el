;;; nix-ts-mode.el --- Major mode for Nix expressions, powered by tree-sitter -*- lexical-binding: t -*-

;; Maintainer: Remi Gelinas <mail@remigelin.as>
;; Homepage: https://github.com/remi-gelinas/nix-ts-mode
;; Version: 0.1.1
;; Keywords: nix
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:


;; A major mode for editing Nix expressions, powered by the new built-in tree-sitter support in Emacs 29.1.

;;; Code:
;; (unless (version< emacs-version "29.1")
;;   (error "`nix-ts-mode` requires at least Emacs 29 for tree-sitter support"))

(require 'treesit)

(unless (treesit-available-p)
  (error "`nix-ts-mode` requires Emacs to be built with tree-sitter support"))

(declare-function treesit-parser-create "treesit.c")

;; Other

(defcustom nix-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `nix-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'nix)

(defvar nix-ts--treesit-builtins
  ;; nix eval --impure --expr 'with builtins; filter (x: !(elem x [ "abort" "derivation" "import" "throw" ]) && isFunction builtins.${x}) (attrNames builtins)'
  '("add" "addErrorContext" "all" "any" "appendContext" "attrNames" "attrValues" "baseNameOf" "bitAnd" "bitOr" "bitXor" "break" "catAttrs" "ceil" "compareVersions" "concatLists" "concatMap" "concatStringsSep" "deepSeq" "derivationStrict" "dirOf" "div" "elem" "elemAt" "fetchGit" "fetchMercurial" "fetchTarball" "fetchTree" "fetchurl" "filter" "filterSource" "findFile" "floor" "foldl'" "fromJSON" "fromTOML" "functionArgs" "genList" "genericClosure" "getAttr" "getContext" "getEnv" "getFlake" "groupBy" "hasAttr" "hasContext" "hashFile" "hashString" "head" "intersectAttrs" "isAttrs" "isBool" "isFloat" "isFunction" "isInt" "isList" "isNull" "isPath" "isString" "length" "lessThan" "listToAttrs" "map" "mapAttrs" "match" "mul" "parseDrvName" "partition" "path" "pathExists" "placeholder" "readDir" "readFile" "removeAttrs" "replaceStrings" "scopedImport" "seq" "sort" "split" "splitVersion" "storePath" "stringLength" "sub" "substring" "tail" "toFile" "toJSON" "toPath" "toString" "toXML" "trace" "traceVerbose" "tryEval" "typeOf" "unsafeDiscardOutputDependency" "unsafeDiscardStringContext" "unsafeGetAttrPos" "zipAttrsWith"))

(defvar nix-ts--treesit-constants
  ;; nix eval --impure --expr 'with builtins; filter (x: !(isFunction builtins.${x} || isBool builtins.${x})) (attrNames builtins)'
  '("builtins" "currentSystem" "currentTime" "langVersion" "nixPath" "nixVersion" "null" "storeDir"))

;; Settings
(defvar nix-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'nix
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :language 'nix
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'nix
   :feature 'delimiter
   '(["," "." ";"] @font-lock-delimiter-face)

   :language 'nix
   :feature 'keyword
   `((let_expression
      (["let" "in"] @font-lock-keyword-face))
     (if_expression
      ["if" "then" "else"] @font-lock-keyword-face)
     (rec_attrset_expression
      ("rec" @font-lock-keyword-face))
     (with_expression
      ("with" @font-lock-keyword-face))
     (assert_expression
      ("assert" @font-lock-keyword-face))
     ((identifier) @font-lock-keyword-face
      (:match
       ,(rx-to-string
	 `(seq bol (or "throw" "abort")
	       eol))
       @font-lock-keyword-face))

     ;; "or" is technically an operator, but we fontify it as a keyword
     ((identifier) @font-lock-keyword-face
      (:match
       ,(rx-to-string
	 `(seq bol "or" eol))
       @font-lock-keyword-face)))

   :language 'nix
   :feature 'string
   :override t
   `((string_fragment) @font-lock-string-face
     (string_expression
      ("\"" @font-lock-string-face))
     (indented_string_expression
      ("''" @font-lock-string-face))
     (interpolation
      (["${" "}"] @font-lock-misc-punctuation-face)))

   :language 'nix
   :feature 'operator
   `((binary_expression operator: _ @font-lock-operator-face)
     (unary_expression operator: _ @font-lock-operator-face))

   :language 'nix
   :feature 'number
   `([(integer_expression) (float_expression)] @font-lock-constant-face)

   :language 'nix
   :feature 'path
   `((path_expression
      (path_fragment) @font-lock-string-face))

   :language 'nix
   :feature 'builtin
   `((variable_expression name: (identifier) @font-lock-builtin-face
			  (:match
			   ,(rx-to-string
			     `(seq bol (or ,@nix-ts--treesit-builtins)
				   eol))
			   @font-lock-builtin-face)))
   :language 'nix
   :feature 'constant
   `((variable_expression name: (identifier) @font-lock-constant-face
			  (:match
			   ,(rx-to-string
			     `(seq bol (or ,@nix-ts--treesit-constants "true" "false")
				   eol))
			   @font-lock-constant-face)))
   :language 'nix
   :feature 'attribute
   `((attrpath
      (identifier) @font-lock-variable-name-face))

   :language 'nix
   :feature 'ellipses
   `((ellipses) @font-lock-misc-punctuation-face)

   :language 'nix
   :feature 'function
   `((function_expression
      ":" @font-lock-misc-punctuation-face)))

  "Tree-sitter font-lock settings for `nix-ts-mode'.")

;; Indentation
(defvar nix-ts-mode-indent-rules
  (let ((offset nix-ts-mode-indent-offset))
    `((nix
       ((node-is "}") parent-bol 0)
       ((node-is ")") parent-bol 0)
       ((node-is "]") parent-bol 0)
       ((parent-is "parenthesized_expression") parent-bol ,offset)))))

;; Keymap
(defvar nix-ts-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `nix-ts-mode'.")

;; Syntax map
(defvar nix-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    table)
  "Syntax table for `nix-ts-mode'.")

;;;###autoload
(define-derived-mode nix-ts-mode prog-mode "Nix"
  "Major mode for editing Nix expressions, powered by treesitter.

\\{nix-ts-mode-map}"
  :group 'nix
  :syntax-table nix-ts-mode--syntax-table
  
  (when (treesit-ready-p 'nix)
    (treesit-parser-create 'nix)

    ;; Font locking
    (setq-local treesit-font-lock-settings nix-ts-mode--font-lock-settings)
    
    (setq-local treesit-font-lock-feature-list
                '((comment builtin)
                  (keyword string path)
                  (number constant attribute)
                  (bracket delimiter operator ellipses function)))
    
    (setq-local treesit-font-lock-level 4)

    ;; Indentation
    (setq-local treesit-simple-indent-rules nix-ts-mode-indent-rules)
    
    (treesit-major-mode-setup)))

(provide 'nix-ts-mode)
;;; nix-ts-mode.el ends here
