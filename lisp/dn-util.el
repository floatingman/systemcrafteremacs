;;;; -*- lexical-binding: t; -*-

(require 'recentf)

;;; ============================================================================
;;; Variables
;;; ============================================================================

(defvar mf/ignored-directories
  `(,user-emacs-directory
    "eln-cache"))

(defvar mf/ignored-suffixes
  '(".7z" ".bz2" ".db" ".dll" ".dmg" ".elc" ".exe" ".fasl" ".gz" ".iso" ".jar"
    ".o" ".pyc" ".rar" ".so" ".sql" ".sqlite" ".tar" ".tgz" ".xz" ".zip"))

;;; ============================================================================
;;; Macros
;;; ============================================================================

(defmacro fn (&rest body)
  `(lambda () ,@body))

(defmacro fn! (&rest body)
  `(lambda () (interactive) ,@body))

(defmacro quiet! (&rest forms)
  `(cond
    (noninteractive
     (let ((old-fn (symbol-function 'write-region)))
       (cl-letf ((standard-output (lambda (&rest _)))
                 ((symbol-function 'load-file)
                  (lambda (file) (load file nil t)))
                 ((symbol-function 'message) (lambda (&rest _)))
                 ((symbol-function 'write-region)
                  (lambda (start end filename &optional append visit lockname
                                 mustbenew)
                    (unless visit (setq visit 'no-message))
                    (funcall old-fn start end filename append visit lockname
                             mustbenew))))
         ,@forms)))
    ((or debug-on-error debug-on-quit)
     ,@forms)
    ((let ((inhibit-message t)
           (save-silently t))
       (prog1 ,@forms (message ""))))))

;;; ============================================================================
;;; Functions
;;; ============================================================================

(defun mf/show-startup-time ()
  (message "Emacs startup time: %.2fs (%d GCs)"
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(defun mf/etc-file (file-name)
  (expand-file-name (format "etc/%s" file-name) user-emacs-directory))

(defun mf/cache-dir-p (path)
  (string-prefix-p (getenv "XDG_CACHE_HOME") (expand-file-name path)))

(defun mf/smarter-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun mf/yank-primary-selection ()
  (interactive)
  (let ((primary (or (gui-get-primary-selection) (gui-get-selection))))
    (when primary
      (push-mark (point))
      (insert-for-yank primary))))

(defun mf/delete-file (filename)
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (delete-file filename)))

(defun mf/rename-file ()
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir))
                            (yes-or-no-p (format "Create directory '%s'?"
                                                 dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (message "File '%s' successfully renamed to '%s'" name
                        (file-name-nondirectory new-name))))))))

(provide 'dn-util)
