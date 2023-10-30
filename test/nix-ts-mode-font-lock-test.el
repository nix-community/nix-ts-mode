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
in rec {
  test = with pkgs; test;
}"
	       '(("let" font-lock-keyword-face)
		 ("in" font-lock-keyword-face)
		 ("with" font-lock-keyword-face)
		 ("rec" font-lock-keyword-face))))

(provide 'nix-ts-mode-font-lock-tests)
;;; nix-ts-mode-font-lock-tests.el ends here
