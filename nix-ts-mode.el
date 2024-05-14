;;; nix-ts-mode.el --- Major mode for Nix expressions, powered by tree-sitter -*- lexical-binding: t -*-

;; Maintainer: Remi Gelinas <mail@remigelin.as>
;; Homepage: https://github.com/nix-community/nix-ts-mode
;; Version: 0.1.4
;; Keywords: nix languages
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

;; A major mode for editing Nix expressions, powered by the new
;; built-in tree-sitter support in Emacs 29.1.

;;; Code:
;; (unless (version< emacs-version "29.1")
;;   (error "`nix-ts-mode` requires at least Emacs 29 for tree-sitter support"))

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
     (inherit
      ("inherit" @font-lock-keyword-face))
     (inherit_from
      ("inherit" @font-lock-keyword-face))
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
      ":" @font-lock-misc-punctuation-face))

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
     ((node-is "binding_set") parent-bol nix-ts-mode-indent-offset)
     ((match "interpolation" "indented_string_expression" nil nil nil) nix-ts-indent-multiline-string 0)
     ((parent-is "indented_string_expression") parent-bol 0)
     ((parent-is "string_fragment") nix-ts-indent-multiline-string 0)
     ((parent-is "formals") parent-bol 0)
     ((parent-is "binding_set") parent-bol 0)
     ((parent-is "binding") parent-bol nix-ts-mode-indent-offset)
     ((parent-is "let_expression") parent-bol nix-ts-mode-indent-offset)
     ((parent-is "attrset_expression") parent-bol nix-ts-mode-indent-offset)
     ((parent-is "list_expression") parent-bol nix-ts-mode-indent-offset)
     ((parent-is "apply_expression") parent-bol nix-ts-mode-indent-offset)
     ((parent-is "parenthesized_expression") parent-bol nix-ts-mode-indent-offset)))
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
                '((comment builtin)
                  (keyword string path)
                  (number constant attribute)
                  (bracket delimiter error operator ellipses function)))

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

    (treesit-major-mode-setup)))

(provide 'nix-ts-mode)
;;; nix-ts-mode.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
