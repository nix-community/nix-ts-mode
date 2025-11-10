;;; nix-ts-mode.el --- Major mode for Nix expressions, powered by tree-sitter -*- lexical-binding: t -*-

;; Maintainer: Remi Gelinas <mail@remigelin.as>
;; Homepage: https://github.com/nix-community/nix-ts-mode
;; Version: 0.1.5
;; Keywords: nix languages tree-sitter
;; Package-Requires: ((emacs "29.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A major mode for editing Nix expressions, powered by the built-in
;; tree-sitter support in Emacs 29+.
;;
;; Features:
;; - Complete syntax highlighting for all Nix language constructs
;; - Semantic highlighting distinctions (variables vs properties, etc.)
;; - Indentation support
;;
;; Requires Emacs 30.1+ for the latest font-lock faces including
;; font-lock-punctuation-face and other tree-sitter enhancements, however,
;; it will still provide good coverage on 29.1+.

;;; Code:

(require 'treesit)

(unless (treesit-available-p)
  (error "`nix-ts-mode` requires Emacs to be built with tree-sitter support"))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")

;; Other

(defgroup nix-ts nil
  "Major mode for editing Nix expressions."
  :prefix "nix-ts-"
  :group 'languages)

(defcustom nix-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `nix-ts-mode'."
  :type 'integer
  :safe 'integerp)

(defvar nix-ts--treesit-builtins
  ;; nix eval --impure --expr 'with builtins; filter (x: !(elem x [ "abort" "derivation" "import" "throw" ]) && isFunction builtins.${x}) (attrNames builtins)'
  ;; Also includes primops (__* functions)
  '("add" "addErrorContext" "all" "any" "appendContext" "attrNames" "attrValues" "baseNameOf"
    "bitAnd" "bitOr" "bitXor" "break" "catAttrs" "ceil" "compareVersions" "concatLists" "concatMap"
    "concatStringsSep" "deepSeq" "derivation" "derivationStrict" "dirOf" "div" "elem" "elemAt"
    "fetchGit" "fetchMercurial" "fetchTarball" "fetchTree" "fetchurl" "filter" "filterSource"
    "findFile" "floor" "foldl'" "fromJSON" "fromTOML" "functionArgs" "genList" "genericClosure"
    "getAttr" "getContext" "getEnv" "getFlake" "groupBy" "hasAttr" "hasContext" "hashFile"
    "hashString" "head" "intersectAttrs" "isAttrs" "isBool" "isFloat" "isFunction" "isInt" "isList"
    "isNull" "isPath" "isString" "length" "lessThan" "listToAttrs" "map" "mapAttrs" "match" "mul"
    "parseDrvName" "partition" "path" "pathExists" "placeholder" "readDir" "readFile" "removeAttrs"
    "replaceStrings" "scopedImport" "seq" "sort" "split" "splitVersion" "storePath" "stringLength"
    "sub" "substring" "tail" "toFile" "toJSON" "toPath" "toString" "toXML" "trace" "traceVerbose"
    "tryEval" "typeOf" "unsafeDiscardOutputDependency" "unsafeDiscardStringContext"
    "unsafeGetAttrPos" "zipAttrsWith"
    ;; primops (__<tab> in nix repl)
    "__add" "__addErrorContext" "__all" "__any" "__appendContext" "__attrNames" "__attrValues"
    "__baseNameOf" "__bitAnd" "__bitOr" "__bitXor" "__catAttrs" "__ceil" "__compareVersions"
    "__concatLists" "__concatMap" "__concatStringsSep" "__currentSystem" "__currentTime" "__deepSeq"
    "__div" "__elem" "__elemAt" "__fetchurl" "__filter" "__filterSource" "__findFile" "__floor"
    "__foldl'" "__fromJSON" "__functionArgs" "__genList" "__genericClosure" "__getAttr"
    "__getContext" "__getEnv" "__getFlake" "__groupBy" "__hasAttr" "__hasContext" "__hashFile"
    "__hashString" "__head" "__intersectAttrs" "__isAttrs" "__isBool" "__isFloat" "__isFunction"
    "__isInt" "__isList" "__isPath" "__isString" "__langVersion" "__length" "__lessThan"
    "__listToAttrs" "__mapAttrs" "__match" "__mul" "__nixPath" "__nixVersion" "__parseDrvName"
    "__partition" "__path" "__pathExists" "__placeholder" "__readDir" "__readFile" "__removeAttrs"
    "__replaceStrings" "__seq" "__sort" "__split" "__splitVersion" "__storeDir" "__storePath"
    "__stringLength" "__sub" "__substring" "__tail" "__toFile" "__toJSON" "__toPath" "__toString"
    "__toXML" "__trace" "__traceVerbose" "__tryEval" "__typeOf" "__unsafeDiscardOutputDependency"
    "__unsafeDiscardStringContext" "__unsafeGetAttrPos" "__zipAttrsWith"))

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
   :override t
   `((let_expression
      (["let" "in"] @font-lock-keyword-face))
     ;; Deprecated let attrset expression (let { body = ...; })
     (let_attrset_expression
      ("let" @font-lock-keyword-face))
     (if_expression
      ["if" "then" "else"] @font-lock-keyword-face)
     (rec_attrset_expression
      ("rec" @font-lock-keyword-face))
     (with_expression
      ("with" @font-lock-keyword-face))
     (inherit
      ("inherit" @font-lock-keyword-face))
     (inherit_from
      ("inherit" @font-lock-keyword-face))
     (assert_expression
      ("assert" @font-lock-keyword-face))
     ;; Special keywords: throw, abort, import
     ((variable_expression name: (identifier) @font-lock-keyword-face)
      (:match
       ,(rx-to-string
         `(seq bol (or "throw" "abort" "import")
           eol))
       @font-lock-keyword-face))

     ;; "or" is technically an operator, but we fontify it as a keyword
     (select_expression
      ("or" @font-lock-keyword-face)))

   :language 'nix
   :feature 'string
   :override t
   `((string_fragment) @font-lock-string-face
     (string_expression
      ("\"" @font-lock-string-face))
     (indented_string_expression
      ("''" @font-lock-string-face))
     ;; Escape sequences in strings
     (escape_sequence) @font-lock-escape-face
     ;; Dollar escape in indented strings (''$)
     (dollar_escape) @font-lock-escape-face
     (interpolation
      (["${" "}"] @font-lock-punctuation-face)))

   :language 'nix
   :feature 'operator
   `((binary_expression operator: _ @font-lock-operator-face)
     (unary_expression operator: _ @font-lock-operator-face)
     ;; has_attr_expression (x ? y to test if attribute exists)
     (has_attr_expression operator: _ @font-lock-operator-face)
     ;; @ and ? operators
     ["@" "?" "="] @font-lock-operator-face)

   :language 'nix
   :feature 'number
   `([(integer_expression) (float_expression)] @font-lock-number-face)

   :language 'nix
   :feature 'path
   `(;; Regular paths
     (path_expression) @font-lock-constant-face
     ;; Home paths (~/)
     (hpath_expression) @font-lock-constant-face
     ;; Store paths (<nixpkgs>)
     (spath_expression) @font-lock-constant-face)

   :language 'nix
   :feature 'uri
   `((uri_expression) @font-lock-string-face)

   :language 'nix
   :feature 'parameter
   `(;; Function parameters in formals ({ x, y, ... })
     (formal name: (identifier) @font-lock-variable-name-face)
     ;; Universal function parameter (x: body) - simple case without destructuring
     (function_expression
      universal: (identifier) @font-lock-variable-name-face)
     ;; @ pattern operator
     (function_expression
      "@" @font-lock-operator-face))

   ;; At-pattern binding needs override to take precedence over universal parameter
   :language 'nix
   :feature 'parameter-atpattern
   :override t
   `(;; @ pattern identifier - forward order: args@{ x, y }
     ;; Use type-face for visual distinction - it's the "base object" containing all params
     (function_expression
      universal: (identifier) @font-lock-type-face
      "@")
     ;; @ pattern identifier - reverse order: { x, y }@opts
     (function_expression
      "@"
      universal: (identifier) @font-lock-type-face))

   :language 'nix
   :feature 'function-call
   `(;; Function calls: highlight function name in apply_expression
     (apply_expression
      function: (variable_expression
                 name: (identifier) @font-lock-function-call-face))
     ;; Function calls with select_expression (builtins.map, lib.mkOption, etc.)
     (apply_expression
      function: (select_expression
                 attrpath: (attrpath
                            (identifier) @font-lock-function-call-face :anchor))))

   :language 'nix
   :feature 'property
   `(;; Base variable in select expressions - use type-face for visual distinction
     ;; (Nix has no type system, so this face is available and visually distinct)
     (select_expression
      expression: (variable_expression
                   name: (identifier) @font-lock-type-face))
     ;; Property access in select expressions - the MEMBERS (obj.property)
     (select_expression
      attrpath: (attrpath
                 (identifier) @font-lock-property-use-face))
     ;; Binding attributes in attrsets (all identifiers in the path)
     (binding_set
      (binding
       attrpath: (attrpath
                  (identifier) @font-lock-property-name-face)))
     ;; Source in inherit_from - use type-face for visual distinction (inherit (SOURCE) attrs)
     (inherit_from
      (variable_expression
       name: (identifier) @font-lock-type-face))
     ;; Inherited attributes FROM a source (inherit (pkgs) hello) - these are properties
     (inherit_from
      attrs: (inherited_attrs
              (identifier) @font-lock-property-use-face)))

   :language 'nix
   :feature 'builtin
   :override t
   `(;; Builtin functions (standalone)
     (variable_expression name: (identifier) @font-lock-builtin-face
                          (:match
                           ,(rx-to-string
                             `(seq bol (or ,@nix-ts--treesit-builtins)
                               eol))
                           @font-lock-builtin-face))
     ;; Builtin functions with builtins prefix (builtins.map, etc.)
     (select_expression
      expression: (variable_expression
                   name: (identifier) @_builtins
                   (:equal @_builtins "builtins"))
      attrpath: (attrpath
                 attr: (identifier) @font-lock-builtin-face)))

   :language 'nix
   :feature 'constant
   :override t
   `(;; Language constants (boolean)
     (variable_expression name: (identifier) @font-lock-constant-face
                          (:match
                           ,(rx-to-string
                             `(seq bol (or ,@nix-ts--treesit-constants "true" "false")
                               eol))
                           @font-lock-constant-face)))

   :language 'nix
   :feature 'definition
   :override t
   `(;; Function definitions (name = x: y;)
     (binding
      attrpath: (attrpath
                 (identifier) @font-lock-function-name-face :anchor)
      expression: (function_expression)))

   :language 'nix
   :feature 'variable
   `(;; General variable references
     (variable_expression
      name: (identifier) @font-lock-variable-use-face)
     ;; Plain inherit (inherit x y z) - these create NEW variable bindings
     (inherit
      attrs: (inherited_attrs
              (identifier) @font-lock-variable-name-face)))

   :language 'nix
   :feature 'paren-base
   :override t
   `(;; Parenthesized base in select - highlight parens with type-face for visual grouping
     ;; This overrides bracket-face to make the base stand out like simple variable bases
     (select_expression
      expression: (parenthesized_expression
                   ["(" ")"] @font-lock-type-face)))

   :language 'nix
   :feature 'ellipses
   `((ellipses) @font-lock-punctuation-face)

   :language 'nix
   :feature 'punctuation
   `((function_expression
      ":" @font-lock-punctuation-face))

   :language 'nix
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `nix-ts-mode'.")

;; Indentation
(defun nix-ts-indent-multiline-string (n parent bol &rest rest)
  "Return the indent prefix for the current multi-line string line.
For the first line, this is the previous line offset+nix-indent-offset,
and for subsequent lines it's the previous line's indentation."
  ;; If the current line is the first relevant one in the multiline
  ;; string, indent it to the default level (2 spaces past the
  ;; previous line's offset):
  (if (and (equal (treesit-node-child (treesit-node-parent parent) 1)
                  parent)
           (<= (count-lines (treesit-node-start parent) (point)) 1))
      (+ (apply (alist-get 'parent-bol treesit-simple-indent-presets)
                n parent bol rest)
         nix-ts-mode-indent-offset)
    ;; If the current line is already indented to some level, leave it alone:
    (if (/= bol
            (save-excursion
              (beginning-of-line)
              (point)))
        bol
      ;; otherwise, indent to the level of the previous line by default.
      (save-excursion
        (forward-line -1)
        (if (looking-at "\s+")
            (match-end 0)
          ;; in case neither line has discernable indentation, just
          ;; indent to bol
          bol)))))

(defvar nix-ts-mode-indent-rules
  `((nix
     ((parent-is "source_code") column-0 0)
     ((node-is "]") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is "then") parent-bol 0)
     ((node-is "else") parent-bol 0)
     ((node-is ";") parent-bol 0)
     ((node-is "^in$") parent-bol 0)
     ((node-is "binding_set") parent-bol nix-ts-mode-indent-offset)
     ((match "interpolation" "indented_string_expression" nil nil nil) nix-ts-indent-multiline-string 0)
     ((parent-is "indented_string_expression") parent-bol 0)
     ((parent-is "string_fragment") nix-ts-indent-multiline-string 0)
     ((parent-is "binding_set") parent-bol 0)
     ((parent-is "binding") parent-bol nix-ts-mode-indent-offset)
     ((parent-is "let_expression") parent-bol nix-ts-mode-indent-offset)
     ((parent-is "attrset_expression") parent-bol nix-ts-mode-indent-offset)
     ((node-is "inherited_attrs") parent-bol nix-ts-mode-indent-offset)
     ((parent-is "inherited_attrs") parent-bol 0)
     ((parent-is "list_expression") parent-bol nix-ts-mode-indent-offset)
     ((parent-is "apply_expression") parent-bol nix-ts-mode-indent-offset)
     ((parent-is "parenthesized_expression") parent-bol nix-ts-mode-indent-offset)
     ((parent-is "formals") parent-bol nix-ts-mode-indent-offset)))
  "Tree-sitter indent rules for `nix-ts-mode'.")

;; Keymap
(defvar nix-ts-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `nix-ts-mode'.")

;; Syntax map
(defvar nix-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    table)
  "Syntax table for `nix-ts-mode'.")

(defun nix-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ("binding"
     (treesit-node-text
      (treesit-node-child-by-field-name node "attrpath") t))))

;;;###autoload
(define-derived-mode nix-ts-mode prog-mode "Nix"
  "Major mode for editing Nix expressions, powered by treesitter.

\\{nix-ts-mode-map}"
  :syntax-table nix-ts-mode--syntax-table

  (when (treesit-ready-p 'nix)
    (treesit-parser-create 'nix)

    ;; Font locking
    (setq-local treesit-font-lock-settings nix-ts-mode--font-lock-settings)

    (setq-local treesit-font-lock-feature-list
                '((comment builtin constant)
                  (string path uri)
                  (number operator definition function-call keyword)
                  (parameter property variable bracket delimiter ellipses punctuation paren-base parameter-atpattern error)))

    ;; Comments
    (setq-local comment-start "# ")
    (setq-local comment-start-skip "#+\\s-*")

    ;; Indentation
    (setq-local treesit-simple-indent-rules nix-ts-mode-indent-rules)

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `((nil "\\`binding\\'" nil nil)))

    ;; Navigation.
    (setq-local treesit-defun-type-regexp (rx (or "binding")))
    (setq-local treesit-defun-name-function #'nix-ts-mode--defun-name)

    (treesit-major-mode-setup))

  (when (functionp 'derived-mode-add-parents)
    (derived-mode-add-parents 'nix-ts-mode '(nix-mode))))

(provide 'nix-ts-mode)
;;; nix-ts-mode.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
