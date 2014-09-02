;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'compile)
(require  's )

(defface rspec-button-face
  '((((class color)) (:foreground "DeepSkyBlue"))
    (t (:reverse-video t)))
  "Face to use for highlighting links in rspec files."
  :group 'faces
  :group 'button)

(define-button-type 'rspec-ref-button
  'help-echo "Push to create an empty reference definition"
  'face 'rspec-button-face)

(defvar rspec-outline-mode-hook nil)

(defvar *rspec-outline-mode-map*
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "q" 'kill-this-buffer)
    map))

(define-derived-mode rspec-outline-mode fundamental-mode "rspec-outline mode"
  "A mode for viewing rspec outline"
  (interactive)
  (use-local-map *rspec-outline-mode-map*)
  (run-hooks 'rspec-outline-mode-hook))


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

(defvar rspec-simple-source-dir        nil "Private variable.")

(defun rspec-compile-file ()
  (interactive)
  (compile (format "cd %s;bundle exec rspec --format d %s"
                   (get-closest-gemfile-root)
                   (file-relative-name (buffer-file-name) (get-closest-gemfile-root))
                   ) t))

(defun rspec-compile-on-line ()
  (interactive)
  (progn
    (window-configuration-to-register 9)
    (compile (format "cd %s;bundle exec rspec %s:%s"
                     (get-closest-gemfile-root)
                     (file-relative-name (buffer-file-name) (get-closest-gemfile-root))
                     (line-number-at-pos)
                     ) t)))

(defun zeus-rspec-compile-file ()
  (interactive)
  (compile (format "cd %s;zeus test %s"
                   (get-closest-gemfile-root)
                   (file-relative-name (buffer-file-name) (get-closest-gemfile-root))
                   ) t))

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

(defun rspec-file-outline (rspec-parse-command rspec-file-name)
  "gather outline of specified rspec file"
  (let
      ((command-output (shell-command-to-string
                        (format "%s %s"
                                rspec-parse-command rspec-file-name))))
    (s-split "\n" command-output)))

(defun rspec-display-file-outline ()
  "make rpec outline"
  (interactive)
  (let (
        (rspec-outline-buffer (get-buffer-create (generate-new-buffer-name "*rspec-outline*")))
        (old-rspec-buffer (current-buffer))
        )
    (setq outline-list (rspec-file-outline (rspec-parse-command-path) (buffer-file-name)))
    (switch-to-buffer-other-window rspec-outline-buffer)
    (dolist (line  outline-list)
      (let ((line-list (s-split "::" line)))

        (insert-text-button
         (concat (first line-list) "\n") :type 'rspec-ref-button
         'follow-link t 'action (lambda (button)
                                  (progn
                                    (goto-line (string-to-number (second line-list)) old-rspec-buffer))))))
    (rspec-outline-mode)
    ))

;; return rspec-parse-file
(defun rspec-parse-command-path ()
  (concat (rspec-simple-source-dir) "bin/rspec_parser"))

(defun rspec-find-related-file ()
  "find related file"
  (interactive)
  (let* (
         (current-file-name (buffer-file-name))
         (app-root (get-closest-gemfile-root))
         (file-list (rspec-simple-shell-command
                     (concat
                      (concat (rspec-simple-source-dir) "bin/search_related ")
                      current-file-name) "\n" app-root)
                    )
         )
    (rspec-simple-ido-find-file file-list)
    ))

(defun rspec-simple-source-dir ()
  (or rspec-simple-source-dir
      (setq rspec-simple-source-dir (file-name-directory (find-lisp-object-file-name
                                                          'rspec-simple-source-dir (symbol-function 'rspec-simple-source-dir))))))

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



(provide 'rspec-simple)
