;;; init.el --- Emacs Initialisation -*- lexical-binding: t -*-

;;;; = Package Manager Setup
(require 'package)

;; Emacs 27.x has gnu elpa as the default
;; Emacs 28.x adds the nongnu elpa to the list by default, so only
;; need to add nongnu when this isn't Emacs 28+
(when (version< emacs-version "28")
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 99)   ; prefer GNU packages
                          ("nongnu" . 80)   ; use non-gnu packages if
                                            ; not found in GNU elpa
                          ("stable" . 70)   ; prefer "released" versions
                                            ; from melpa
                          ("melpa"  . 0)))  ; if all else fails, get it
                                            ; from melpa

;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initizlization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)

;; Ask package.el to not add (package-initialize) to .emacs. (25)
(setq package--init-file-ensured t)

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        ;; (require 'package)
        (package-initialize)
        ;; Install 'setup' if not installed yet.
        (unless (package-installed-p 'setup)
          (package-refresh-contents)
          (package-install 'setup))
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse (apply #'nconc
                           ;; Only keep package.el provided loadpaths.
                           (mapcar #'(lambda (path)
                                       (if (string-prefix-p package-user-dir-real path)
                                           (list path)
                                         nil))
                                   load-path))))))

;; Lazy load everything
(customize-set-variable 'package-quickstart t)

;;;; = Setup macros (extended)

(eval-when-compile
  (require 'setup)

  ;;;; Setup macro template
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

  (defmacro fc (&rest body)
    `(lambda () ,@body))

  ;;;; Hide minor mode from the modeline
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

  ;;;; Load feature after feature(s)
  (define-setup-macro :load-after (features &rest body)
                      "Load the current feature after FEATURES."
                      :indent 1
                      (let ((body `(progn
                                     (require ',(setup-get 'feature))
                                     ,@body)))
                        (dolist (feature (nreverse (ensure-list features)))
                          (setq body `(with-eval-after-load ',feature ,body)))
                        body))

  ;;;; Evaluate after feature loaded
  (define-setup-macro :with-after (features &rest body)
                      "Evaluate BODY after FEATURES are loaded."
                      :indent 1
                      (let ((body `(progn ,@body)))
                        (dolist (feature (nreverse (ensure-list features)))
                          (setq body `(with-eval-after-load ',feature ,body)))
                        body))

  ;;;; Change modal state that body binds to
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

  ;;;; Bind command in feature map
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

  ;;;; Bind command in global map
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

  ;;;; Disable feature
  (define-setup-macro :disable ()
                      "Unconditionally abort the evaluation of the current body."
                      (setup-quit))

  ;;;; Delay loading feature by seconds
  (define-setup-macro :delay (seconds)
                      "Require the current FEATURE after SECONDS of idle time."
                      :indent 1
                      `(run-with-idle-timer ,seconds nil #'require ',(setup-get 'feature) nil t))

  ;;;; Delay evaluating after seconds
  (define-setup-macro :with-idle-delay (seconds &rest body)
                      "Evaluate BODY after SECONDS of idle time."
                      :indent 1
                      `(run-with-idle-timer ,seconds nil (lambda () ,@body)))

  ;;;; Add advice on function
  (define-setup-macro :advise (symbol where arglist &rest body)
                      "Add a piece of advice on a function. See 'advice-add' for more details."
                      :after-loaded t
                      :debug '(sexp sexp function-form)
                      :indent 3
                      (let ((name (gensym "setup-advice-")))
                        `(progn
                           (defun ,name ,arglist ,@body)
                           (advice-add ',symbol ,where #',name)))))
