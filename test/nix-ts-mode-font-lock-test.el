;;; nix-ts-mode-font-lock-tests.el --- Font lock tests for `nix-ts-mode`. -*- lexical-binding: t -*-

;; Maintainer: Remi Gelinas <mail@remigelin.as>
;; Homepage: https://github.com/remi-gelinas/nix-ts-mode
;; Version: 0.1.2
;; Keywords: nix
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:


;; Tests for tree-sitter powered font-locking in `nix-ts-mode`.

;;; Code:
(require 'ert)
(require 'nix-ts-mode)

(defmacro with-nix-buffer (&rest body)
  "Run `BODY` in the context of a new buffer set to `nix-ts-mode`."
  `(with-temp-buffer
     (delay-mode-hooks (nix-ts-mode))
     ,@body))

(defun check-faces (content pairs)
  ""
  (with-nix-buffer
   (insert content)
   (save-excursion
     (setq-local treesit-font-lock-level 4)
     (treesit-font-lock-recompute-features)
     (treesit-font-lock-fontify-region (point-min) (point-max) t))
   (dolist (pair pairs)
     (goto-char (point-min))
     (cl-destructuring-bind (string face) pair
       (let ((case-fold-search nil))
	 (search-forward string))
       (let* ((beg (match-beginning 0))
              (end (match-end 0))
              (prop-ranges (object-intervals (buffer-substring beg end)))
              (face-ranges (cl-loop for range in prop-ranges
                                    for face = (plist-get (elt range 2) 'face)
                                    when face
                                    collect (list (elt range 0) (elt range 1) face))))
         (should (equal `(,string ,face-ranges)
                        `(,string ((0 ,(- end beg) ,face))))))))))

;; Features

(ert-deftest nix-ts-bracket ()
  (check-faces
   "{
  test = [
    (doFunc \"test\")
  ];
}"
   '(("(" font-lock-bracket-face)
     (")" font-lock-bracket-face)
     ("[" font-lock-bracket-face)
     ("]" font-lock-bracket-face)
     ("{" font-lock-bracket-face)
     ("}" font-lock-bracket-face))))

(ert-deftest nix-ts-comment ()
  (check-faces "# This is a Nix comment, alright"
	       '(("Nix comment" font-lock-comment-face))))

(ert-deftest nix-ts-delimiter ()
  (check-faces
   "{ attribute.attribute = {param, ...}: {}; }"
   '(("." font-lock-delimiter-face)
     ("," font-lock-delimiter-face)
     (";" font-lock-delimiter-face))))

(ert-deftest nix-ts-keyword ()
  (check-faces "
let
  pkgs = {
    test = \"\";
  };
  a = x.y or z;
in rec {
  inherit pkgs;
  test = with pkgs; test;
}"
	       '(("let" font-lock-keyword-face)
		 ("inherit" font-lock-keyword-face)
		 ("in" font-lock-keyword-face)
		 ("with" font-lock-keyword-face)
		 ("rec" font-lock-keyword-face)
		 ("or" font-lock-keyword-face))))


(ert-deftest nix-ts-error ()
  (check-faces "
let }
  "
	       '(("let" font-lock-warning-face)
                 ("}" font-lock-warning-face))))

(ert-deftest nix-ts-escape-sequence ()
  (check-faces "\"test\\nstring\\t\\\"escaped\\\"\""
               '(("\\n" font-lock-escape-face)
                 ("\\t" font-lock-escape-face)
                 ("\\\"" font-lock-escape-face))))

(ert-deftest nix-ts-uri ()
  ;; Note: Unquoted URIs are deprecated in modern Nix, so we test with quoted strings
  ;; which still get uri_expression node type in older parsers, or string_expression in newer ones
  (check-faces "{
  url = \"https://example.com/file.tar.gz\";
}"
               '(("https://example.com/file.tar.gz" font-lock-string-face))))

(ert-deftest nix-ts-paths ()
  (check-faces "{
  path1 = ./relative/path;
  path2 = /absolute/path;
  path3 = ~/home/path;
  path4 = <nixpkgs>;
}"
               '(("./relative/path" font-lock-constant-face)
                 ("/absolute/path" font-lock-constant-face)
                 ("~/home/path" font-lock-constant-face)
                 ("<nixpkgs>" font-lock-constant-face))))

(ert-deftest nix-ts-function-parameters ()
  (check-faces "{ x, y ? 5, z, ... }: x + y"
               '(("x" font-lock-variable-name-face)
                 ("y" font-lock-variable-name-face)
                 ("z" font-lock-variable-name-face))))

(ert-deftest nix-ts-universal-parameter ()
  (check-faces "x: x + 1"
               '(("x" font-lock-variable-name-face))))

(ert-deftest nix-ts-at-pattern ()
  (check-faces "{
  forward = args@{ x, y }: args.x;
  reverse = { foo, bar }@opts: opts.baz;
}"
               '(;; At-pattern binding gets type-face (the "base object" containing params)
                 ("args" font-lock-type-face)
                 ("opts" font-lock-type-face)
                 ;; @ operator
                 ("@" font-lock-operator-face)
                 ;; Destructured parameters get variable-name-face
                 ("x" font-lock-variable-name-face)
                 ("y" font-lock-variable-name-face)
                 ("foo" font-lock-variable-name-face)
                 ("bar" font-lock-variable-name-face))))

(ert-deftest nix-ts-function-call ()
  (check-faces "{
  result = map (x: x + 1) list;
  result2 = builtins.filter (x: x > 0) list;
  result3 = customFunc arg;
}"
               '(;; map is a builtin, so it gets builtin face, not function-call
                 ("map" font-lock-builtin-face)
                 ;; filter with builtins prefix gets builtin face
                 ("filter" font-lock-builtin-face)
                 ;; customFunc is not a builtin, so it gets function-call face
                 ("customFunc" font-lock-function-call-face))))

(ert-deftest nix-ts-function-definition ()
  (check-faces "{
  myFunction = x: y: x + y;
  anotherFunc = { a, b }: a + b;
}"
               '(("myFunction" font-lock-function-name-face)
                 ("anotherFunc" font-lock-function-name-face))))

(ert-deftest nix-ts-property-access ()
  (check-faces "{
  value = pkgs.hello;
  nested = lib.mkOption { };
  justAccess = config.services.nginx.enable;
}"
               '(;; Base variables in select get type-face for visual distinction
                 ("pkgs" font-lock-type-face)
                 ("lib" font-lock-type-face)
                 ("config" font-lock-type-face)
                 ;; Property members get property-use-face
                 ("hello" font-lock-property-use-face)
                 ;; mkOption gets function-call-face because it's being called
                 ("mkOption" font-lock-function-call-face)
                 ;; These are just property access, not function calls
                 ("enable" font-lock-property-use-face))))

(ert-deftest nix-ts-builtin-functions ()
  (check-faces "{
  x = map toString [ 1 2 3 ];
  y = builtins.head list;
  z = __add 1 2;
}"
               '(("map" font-lock-builtin-face)
                 ("toString" font-lock-builtin-face)
                 ("head" font-lock-builtin-face)
                 ("__add" font-lock-builtin-face))))

(ert-deftest nix-ts-constants ()
  (check-faces "{
  x = null;
  y = true;
  z = false;
  sys = builtins.currentSystem;
}"
               '(("null" font-lock-constant-face)
                 ("true" font-lock-constant-face)
                 ("false" font-lock-constant-face)
                 ("builtins" font-lock-constant-face)
                 ("currentSystem" font-lock-builtin-face))))

(ert-deftest nix-ts-import-keyword ()
  (check-faces "import ./file.nix"
               '(("import" font-lock-keyword-face))))

(ert-deftest nix-ts-operators ()
  (check-faces "args@{ x ? 5, y }: x + y"
               '(("@" font-lock-operator-face)
                 ("?" font-lock-operator-face)
                 ("+" font-lock-operator-face))))

(ert-deftest nix-ts-numbers ()
  (check-faces "{
  int = 42;
  float = 3.14;
  negInt = -10;
  negFloat = -2.5;
}"
               '(("42" font-lock-number-face)
                 ("3.14" font-lock-number-face)
                 ("10" font-lock-number-face)
                 ("2.5" font-lock-number-face))))

(ert-deftest nix-ts-string-interpolation ()
  (check-faces "\"hello ${name} world\""
               '(("${" font-lock-punctuation-face)
                 ("}" font-lock-punctuation-face))))

(ert-deftest nix-ts-inherited-attrs ()
  (check-faces "{
  inherit (pkgs) hello world;
  inherit x y z;
}"
               '(("inherit" font-lock-keyword-face)
                 ;; inherit (SOURCE) - source gets type-face for visual distinction
                 ("pkgs" font-lock-type-face)
                 ;; inherit (pkgs) hello - hello is a PROPERTY being accessed
                 ("hello" font-lock-property-use-face)
                 ("world" font-lock-property-use-face)
                 ;; inherit x y z - these are VARIABLE BINDINGS being created
                 ("x" font-lock-variable-name-face)
                 ("y" font-lock-variable-name-face))))

(ert-deftest nix-ts-binding-attrs ()
  (check-faces "{
  name = \"value\";
  nested.attr = 42;
}"
               '(("name" font-lock-property-name-face)
                 ("nested" font-lock-property-name-face))))

(ert-deftest nix-ts-has-attr-expression ()
  (check-faces "{
  hasX = x ? y;
  hasAttr = obj ? attr;
}"
               '(("?" font-lock-operator-face))))

(ert-deftest nix-ts-dollar-escape ()
  ;; In indented strings, ''$ escapes the interpolation
  ;; The tree structure is: dollar_escape contains just the '' part
  (with-nix-buffer
   (insert "'' test ''${notInterpolated} ''")
   (save-excursion
     (setq-local treesit-font-lock-level 4)
     (treesit-font-lock-recompute-features)
     (treesit-font-lock-fontify-region (point-min) (point-max) t))
   ;; Find the middle '' which is the dollar_escape
   (goto-char (point-min))
   (search-forward "test ")
   (let* ((beg (point))
          (end (+ beg 2))
          (face (get-text-property beg 'face)))
     (should (equal face 'font-lock-escape-face)))))

(ert-deftest nix-ts-let-attrset ()
  (check-faces "let { x = 1; body = x; }"
               '(("let" font-lock-keyword-face))))

(provide 'nix-ts-mode-font-lock-tests)
;;; nix-ts-mode-font-lock-tests.el ends here
