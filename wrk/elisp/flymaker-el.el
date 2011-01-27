
(require 'cl)
(require 'erlang-flymake)

(defun flymaker--get-dir-offset (thingie depth)
  "Pass in '../' and a 3 and get '../../../' back."
  (let ((result nil))
    (dotimes (n depth result)
      (setq result (concat thingie result)))))

(defun flymaker--get-relevant-dirs (paths regexp)
  "Takes a list of paths and returns subdirs in each path that
matches regexp."
  (loop for p in paths
        nconc (directory-files p t regexp)))

(defun flymaker--traverse-directory-tree (directory match-regexp depth)
  "Recursively looks through a directory `depth' levels down and
returns a list of all found dirs."
  (flymaker--traverse-directory-tree-helper
   (concat directory (flymaker--get-dir-offset "../" depth)) match-regexp depth 0))

(defun flymaker--traverse-directory-tree-helper (directory regexp depth current-depth)
  (when (> depth current-depth)
    (loop for dir in (directory-files (file-name-as-directory directory) t)
          when (and (file-directory-p dir)
                    (not (equal "." (file-name-nondirectory dir)))
                    (not (equal ".." (file-name-nondirectory dir))))
          collect dir into directories
          finally return
          (nconc directories
                 (loop for dir in directories
                       nconc (flymaker--traverse-directory-tree-helper
                              dir regexp depth (1+ current-depth)))))))

(defun* flymaker--get-dir-of-buffer (&optional (buffer (current-buffer)))
  "Return the dir the buffers visiting file is in."
  (file-name-directory (file-truename (buffer-file-name buffer))))

(defun flymaker--get-paths (name n)
  (let* ((base-dir (flymaker--get-dir-of-buffer))
         (all-dirs (flymaker--traverse-directory-tree base-dir name n)))
    (flymaker--get-relevant-dirs all-dirs (concat name "$"))))


(defun flymaker--find-code-paths ()
  (mapcar (lambda (dir) (concat dir "/")) (flymaker--get-paths "ebin" 2)))

(defun flymaker--find-includes ()
  (mapcar (lambda (dir) (concat dir "/")) (flymaker--get-paths "include" 2)))

(setq erlang-flymake-get-code-path-dirs-function 'flymaker--find-code-paths)
(setq erlang-flymake-get-include-dirs-function 'flymaker--find-includes)


(provide 'flymaker-el)
