(require 'cl)
(require 'compile)

(defun* get-closest-gemfile-root (&optional (file "Gemfile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/")))
    (loop
     for d = default-directory then (expand-file-name ".." d)
     if (file-exists-p (expand-file-name file d))
     return d
     if (equal d root)
     return nil)))

(defun rspec-compile-file ()
  (interactive)
  (compile (format "cd %s;bundle exec rspec %s"
                   (get-closest-gemfile-root)
                   (file-relative-name (buffer-file-name) (get-closest-gemfile-root))
                   ) t))

(defun rspec-compile-on-line ()
  (interactive)
  (compile (format "cd %s;bundle exec rspec %s -l %s"
                   (get-closest-gemfile-root)
                   (file-relative-name (buffer-file-name) (get-closest-gemfile-root))
                   (line-number-at-pos)
                   ) t))

(defun zeus-rspec-compile-file ()
  (interactive)
  (compile (format "cd %s;zeus test %s"
                   (get-closest-gemfile-root)
                   (file-relative-name (buffer-file-name) (get-closest-gemfile-root))
                   ) t))

;;; Code:
(defun rspec-simple-shell-command (command file-separator working-dir)
  "Executes 'command' and returns the list of printed files in
   the form '((short/file/name . full/path/to/file) ...). The
   'file-separator' character is used to split the file names
   printed by the shell command and is usually set to \\n or \\0"
  (let ((command-output (shell-command-to-string
                         (format "cd %s; %s"
                                 (shell-quote-argument working-dir) command))))
    (let ((files (delete "" (split-string command-output file-separator))))
      (mapcar (lambda (file)
                (cons file (expand-file-name file working-dir)))
              files))))

(defun find-related-file ()
  "find related file"
  (interactive)
  (let* (
         (current-file-name (buffer-file-name))
         (app-root (get-closest-gemfile-root))
         (file-list (rspec-simple-shell-command (concat "search_related " current-file-name) "\n" app-root))
         )
    (rspec-simple-ido-find-file file-list)
    ))

(defun rspec-simple-ido-find-file (file-list)
  "Actually find file to open, using ido."
  (unwind-protect
      (if (= 1 (length file-list))
          (progn
            (let (file (car (car file-list)))
              (find-file (car (car file-list)))
              ))
        (progn
          (let ((file (ido-completing-read "Related file "
                                           (mapcar 'car file-list))))
            (cond
             (file (find-file (cdr (assoc file file-list))))
             ((eq ido-exit 'fallback) (ido-find-file))))
          ))))


(add-hook 'enh-ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c l") 'rspec-compile-on-line)
            (local-set-key (kbd "C-c k") 'rspec-compile-file)
            (local-set-key (kbd "C-c s") 'zeus-rspec-compile-file)
            ))

(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c l") 'rspec-compile-on-line)
            (local-set-key (kbd "C-c k") 'rspec-compile-file)
            (local-set-key (kbd "C-c s") 'zeus-rspec-compile-file)
            ))

(provide 'rspec-simple)
