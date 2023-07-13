;;; nix-ts-mode-font-lock-tests.el --- Font lock tests for `nix-ts-mode`. -*- lexical-binding: t -*-

;; Maintainer: Remi Gelinas <mail@remigelin.as>
;; Homepage: https://github.com/remi-gelinas/nix-ts-mode
;; Version: 0.1.0
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
  (let ((test-buffer-name "*nix-ts-mode-test-buffer*"))
    `(save-current-buffer
       (set-buffer (get-buffer-create ,test-buffer-name))
       (nix-ts-mode)
       ,@body
       (kill-buffer ,test-buffer-name))))

(defun check-faces (content pairs)
  ""
  (with-nix-buffer
   (let ((pos (point)))
     (insert content)
     (save-excursion
       (treesit-font-lock-fontify-region pos (point))))
   (dolist (pair pairs)
     (goto-char (point-min))
     (cl-destructuring-bind (string face) pair
       (let ((case-fold-search nil))
	 (search-forward string))
       (should (eql (text-property-not-all (match-beginning 0) (match-end 0) 'face face) nil))))))

;; Features

(ert-deftest nix-bracket ()
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

(ert-deftest nix-comment ()
  (check-faces "# This is a Nix comment, alright"
	       '(("Nix comment" font-lock-comment-face))))

(ert-deftest nix-delimiter ()
  (check-faces
   "{ attribute.attribute = {param, ...}: {}; }"
   '(("." font-lock-delimiter-face)
     ("," font-lock-delimiter-face)
     (";" font-lock-delimiter-face))))

(ert-deftest nix-keyword ()
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
