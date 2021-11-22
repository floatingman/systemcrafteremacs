;;;; -*- lexical-binding: t; -*-

;; Install setup.el. We use this to concisely perform repetitive tasks, such as
;; installing and loading packages.
(straight-use-package '(setup :type git :host nil :repo "https://git.sr.ht/~pkal/setup"))
(require 'setup)

(defmacro define-setup-macro (name signature &rest body)
  "Shorthand for 'setup-define'. NAME is the name of the local macro. SIGNATURE
is used as the argument list for FN. If BODY starts with a string, use this as
the value for :documentation. Any following keywords are passed as OPTS to
'setup-define'."
  (declare (debug defun))
  (let (opts)
    (when (stringp (car body))
      (setq opts (nconc (list :documentation (pop body)) opts)))
    (while (keywordp (car body))
      (let ((prop (pop body))
            (val `',(pop body)))
        (setq opts (nconc (list prop val) opts))))
    `(setup-define ,name
       (cl-function (lambda ,signature ,@body))
       ,@opts)))

;;; ============================================================================
;;; Custom setup.el local macros
;;; ============================================================================

(define-setup-macro :pkg (recipe)
  "Install RECIPE with 'straight-use-package'. This macro can be used as HEAD,
and will replace itself with the first RECIPE's package."
  :repeatable t
  :shorthand (lambda (x)
               (let ((recipe (cadr x)))
                 (if (consp recipe)
                     (car recipe)
                   recipe)))
  `(straight-use-package ',recipe))

(define-setup-macro :hide-mode (&optional mode)
  "Hide the mode-line lighter of the current mode. Alternatively, MODE can be
specified manually, and override the current mode."
  :after-loaded t
  (let ((mode (or mode (setup-get 'mode))))
    `(progn
       (setq minor-mode-alist
             (remq (assq ',(intern (format "%s-mode" mode)) minor-mode-alist)
                   minor-mode-alist))
       (setq minor-mode-alist
             (remq (assq ',mode minor-mode-alist)
                   minor-mode-alist)))))

(define-setup-macro :load-after (features &rest body)
  "Load the current feature after FEATURES."
  :indent 1
  (let ((body `(progn
                 (require ',(setup-get 'feature))
                 ,@body)))
    (dolist (feature (nreverse (ensure-list features)))
      (setq body `(with-eval-after-load ',feature ,body)))
    body))

(define-setup-macro :with-after (features &rest body)
  "Evaluate BODY after FEATURES are loaded."
  :indent 1
  (let ((body `(progn ,@body)))
    (dolist (feature (nreverse (ensure-list features)))
      (setq body `(with-eval-after-load ',feature ,body)))
    body))

(define-setup-macro :with-state (state &rest body)
  "Change the evil STATE that BODY will bind to. If STATE is a list, apply BODY
to all elements of STATE. This is intended to be used with ':bind'."
  :indent 1
  :debug '(sexp setup)
  (let (bodies)
    (dolist (state (ensure-list state))
      (push (let ((setup-opts (cons `(state . ,state) setup-opts)))
              (setup-expand body))
            bodies))
    (macroexp-progn (nreverse bodies))))

(define-setup-macro :bind (key command)
  "Bind KEY to COMMAND in current map, and optionally for current evil states."
  :after-loaded t
  :debug '(form sexp)
  :repeatable t
  (let ((state (cdr (assq 'state setup-opts)))
        (map (setup-get 'map))
        (key (setup-ensure-kbd key))
        (command (setup-ensure-function command)))
    (if state
        `(with-eval-after-load 'evil
           (evil-define-key* ',state ,map ,key ,command))
      `(define-key ,map ,key ,command))))

(define-setup-macro :global-bind (key command)
  "Globally bind KEY to COMMAND, and optionally for the current evil states."
  :debug '(form sexp)
  :repeatable t
  (let ((state (cdr (assq 'state setup-opts)))
        (key (setup-ensure-kbd key))
        (command (setup-ensure-function command)))
    (if state
        `(with-eval-after-load 'evil
           (evil-define-key* ',state global-map ,key ,command))
      `(global-set-key ,key ,command))))

(define-setup-macro :disable ()
  "Unconditionally abort the evaluation of the current body."
  (setup-quit))

(define-setup-macro :delay (seconds)
  "Require the current FEATURE after SECONDS of idle time."
  :indent 1
  `(run-with-idle-timer ,seconds nil #'require ',(setup-get 'feature) nil t))

(define-setup-macro :with-idle-delay (seconds &rest body)
  "Evaluate BODY after SECONDS of idle time."
  :indent 1
  `(run-with-idle-timer ,seconds nil (lambda () ,@body)))

(define-setup-macro :advise (symbol where arglist &rest body)
  "Add a piece of advice on a function. See 'advice-add' for more details."
  :after-loaded t
  :debug '(sexp sexp function-form)
  :indent 3
  (let ((name (gensym "setup-advice-")))
    `(progn
       (defun ,name ,arglist ,@body)
       (advice-add ',symbol ,where #',name))))

(provide 'dn-setup)
