(defun reload-nix-buffers ()
  (interactive)
  (mapc
   (lambda (buffer)
     (with-current-buffer buffer
       (unless buffer-read-only
         (when (derived-mode-p 'nix-ts-mode)
           (unload-feature 'nix-ts-mode t)
           (load-file "nix-ts-mode.el")
           (nix-ts-mode)
           (treesit-inspect-mode)))))
   (buffer-list)))
