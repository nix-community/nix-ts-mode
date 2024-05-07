;;; nix-ts-mode-indent-test.el --- Indentation tests for nix-ts-mode. -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Fuchs

;; Author: Andreas Fuchs <asf@glyn.local>
;; Keywords: nix
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:


;; Tests for tree-sitter powered indentation in `nix-ts-mode`.

;;; Code:
(require 'ert)
(require 'ert-x)
(require 'nix-ts-mode)

(defmacro with-nix-buffer-contents (&rest body)
  "Run `BODY` in the context of a new buffer set to `nix-ts-mode`."
  `(with-temp-buffer
     (delay-mode-hooks (nix-ts-mode))
     ,@body))

(defun check-indentation (contents)
  "Reindent CONTENTS according to nix-ts-mode's rules and check that it matches.

CONTENT is a correctly-indented nix expression; all its lines' leading
whitespace is stripped, then re-indented and checked that the
output is identical to the given expression."
  (let ((dedented (replace-regexp-in-string "^\\s+" "" contents)))
    (insert dedented)
    (indent-region (point-min) (point-max))
    (should (equal (buffer-substring (point-min) (point-max))
                   contents))))

;;; Features

(ert-deftest nix-multiline-string ()
  (ert-test-erts-file (ert-resource-file "indent-multiline-string.erts")
                      (lambda ()
                        (nix-ts-mode)
                        (indent-region (point-min) (point-max)))))
