;;  -*- lexical-binding: t -*-

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

(defun check-syntax-face-match-range (beg end face)
  ""
  (while (< beg end)
    (setq beg (1+ beg))))

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
       (check-syntax-face-match-range (match-beginning 0) (match-end 0) face)))))

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
