;;; nix-ts-mode.el --- Major mode for editing Nix expressions, powered by tree-sitter. -*- lexical-binding: t -*-

;; Maintainer: Remi Gelinas <mail@remigelin.as>
;; Homepage: https://github.com/remi-gelinas/nix-ts-mode
;; Version: 0.1.0-alpha
;; Keywords: nix
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:


;; A major mode for editing Nix expressions, powered by the new built-in tree-sitter support in Emacs 29.1.

;;; Code:

(require 'treesit)

(declare-function treesit-parser-create "treesit.c")

;; Custom faces

(defgroup nix-ts-font-faces nil
  "Nix font faces."
  :group 'nix-ts-mode
  :group 'faces)

(defface nix-ts-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Used to highlight Nix keywords."
  :group 'nix-ts-font-faces)

(defface nix-ts-keyword-exception-face
  '((t :inherit nix-ts-keyword-face))
  "Used to highlight Nix exception keywords."
  :group 'nix-ts-font-faces)

(defface nix-ts-string-face
  '((t :inherit font-lock-string-face))
  "Used to highlight Nix strings."
  :group 'nix-ts-font-faces)

(defface nix-ts-string-interpolation-face
  '((t :inherit font-lock-warning-face))
  "Used to highlight interpolation in Nix strings."
  :group 'nix-ts-font-faces)

(defface nix-ts-path-face
  '((t :inherit nix-ts-string-face))
  "Used to highlight Nix paths."
  :group 'nix-ts-font-faces)

(defface nix-ts-constant-face
  '((t :inherit font-lock-constant-face))
  "Used to highlight Nix constants and literals."
  :group 'nix-ts-font-faces)

(defface nix-ts-operator-face
  '((t :inherit font-lock-operator-face))
  "Used to highlight Nix operators."
  :group 'nix-ts-font-faces)

(defface nix-ts-delimiter-face
  '((t :inherit nix-ts-operator-face))
  "Used to highlight Nix delimiters."
  :group 'nix-ts-font-faces)

(defface nix-ts-comment-face
  '((t :inherit font-lock-comment-face))
  "Used to highlight Nix comments."
  :group 'nix-ts-font-faces)

(defface nix-ts-bracket-face
  '((t :inherit font-lock-bracket-face))
  "Used to highlight Nix brackets."
  :group 'nix-ts-font-faces)

(defface nix-ts-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Used to highlight Nix builtins."
  :group 'nix-ts-font-faces)

;; Other

(defcustom nix-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `nix-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'nix)

(defvar nix--treesit-builtins
					; nix eval --impure --expr 'with builtins; filter (x: !(elem x [ "abort" "derivation" "import" "throw" ]) && isFunction builtins.${x}) (attrNames builtins)'
  '("add" "addErrorContext" "all" "any" "appendContext" "attrNames" "attrValues" "baseNameOf" "bitAnd" "bitOr" "bitXor" "break" "catAttrs" "ceil" "compareVersions" "concatLists" "concatMap" "concatStringsSep" "deepSeq" "derivationStrict" "dirOf" "div" "elem" "elemAt" "fetchGit" "fetchMercurial" "fetchTarball" "fetchTree" "fetchurl" "filter" "filterSource" "findFile" "floor" "foldl'" "fromJSON" "fromTOML" "functionArgs" "genList" "genericClosure" "getAttr" "getContext" "getEnv" "getFlake" "groupBy" "hasAttr" "hasContext" "hashFile" "hashString" "head" "intersectAttrs" "isAttrs" "isBool" "isFloat" "isFunction" "isInt" "isList" "isNull" "isPath" "isString" "length" "lessThan" "listToAttrs" "map" "mapAttrs" "match" "mul" "parseDrvName" "partition" "path" "pathExists" "placeholder" "readDir" "readFile" "removeAttrs" "replaceStrings" "scopedImport" "seq" "sort" "split" "splitVersion" "storePath" "stringLength" "sub" "substring" "tail" "toFile" "toJSON" "toPath" "toString" "toXML" "trace" "traceVerbose" "tryEval" "typeOf" "unsafeDiscardOutputDependency" "unsafeDiscardStringContext" "unsafeGetAttrPos" "zipAttrsWith"))

(defvar nix--treesit-constants
					; nix eval --impure --expr 'with builtins; filter (x: !(isFunction builtins.${x} || isBool builtins.${x})) (attrNames builtins)'
  '("builtins" "currentSystem" "currentTime" "langVersion" "nixPath" "nixVersion" "null" "storeDir"))

;; Settings
(defvar nix-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'nix
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @nix-ts-bracket-face)

   :language 'nix
   :feature 'comment
   '((comment) @nix-ts-comment-face)

   :language 'nix
   :feature 'delimiter
   '(["," "." ";"] @nix-ts-delimiter-face)

   :language 'nix
   :feature 'keyword
   `((let_expression
      (["let" "in"] @nix-ts-keyword-face))
     (if_expression
      ["if" "then" "else"] @nix-ts-keyword-face)
     (rec_attrset_expression
      ("rec" @nix-keyword-face))
     (with_expression
      ("with" @nix-keyword-face))
     (assert_expression
      ("assert" @nix-ts-keyword-exception-face))
     ((identifier) @nix-ts-keyword-exception-face
      (:match
       ,(rx-to-string
	 `(seq bol (or "throw" "abort")
	       eol))
       @nix-ts-keyword-exception-face)))

   :language 'nix
   :feature 'string
   :override t
   `((string_fragment) @nix-ts-string-face
     (string_expression ("\"" @nix-ts-string-face))
     (indented_string_expression ("''" @nix-ts-string-face))
     (interpolation
      ("${" @nix-ts-string-interpolation-face)
      ("}" @nix-ts-string-interpolation-face)))

   :language 'nix
   :feature 'operator
   `((binary_expression operator: _ @nix-ts-operator-face)
     (unary_expression operator: _ @nix-ts-operator-face))

   :language 'nix
   :feature 'number
   `([(integer_expression) (float_expression)] @nix-ts-constant-face)

   :language 'nix
   :feature 'path
   `((path_expression (path_fragment) @nix-ts-constant-face))

   :language 'nix
   :feature 'builtin
   `((variable_expression name: (identifier) @nix-ts-builtin-face
			  (:match
			   ,(rx-to-string
			     `(seq bol (or ,@nix--treesit-builtins)
				   eol))
			   @nix-ts-builtin-face)))
   :language 'nix
   :feature 'constant
   `((variable_expression name: (identifier) @nix-ts-constant-face
			  (:match
			   ,(rx-to-string
			     `(seq bol (or ,@nix--treesit-constants)
				   eol))
			   @nix-ts-constant-face))))

  "Tree-sitter font-lock settings for `nix-ts-mode'.")

;;;###autoload
(define-derived-mode nix-ts-mode prog-mode "Nix"
  "Major mode for editing Nix expressions, powered by treesitter."
  :group 'nix
  
  (when (treesit-ready-p 'nix)
    (treesit-parser-create 'nix)

    ;; Font locking
    (setq-local treesit-font-lock-settings nix-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment builtin)
                  (keyword string path)
                  (number constant)
                  (bracket delimiter operator)))
    (setq-local treesit-font-lock-level 4)
    
    (treesit-major-mode-setup)))

(provide 'nix-ts-mode)
;;; nix-ts-mode.el ends here
