;;; early-init.el --- Emacs Init before GUI

;; Emacs (27) can now be configured using an early init file.
;; The file is called "early-init.el", in 'user-emacs-directory'.  It is
;; loaded very early in the startup process: before graphical elements
;; such as the tool bar are initialized, and before the package manager
;; is initialized.  The primary purpose is to allow customizing how the
;; package system is initialized given that initialization now happens
;; before loading the regular init file (see below).

;; It is recommended against putting any customizations in this file that
;; don't need to be set up before initializing installed add-on packages,
;; because the early init file is read too early into the startup
;; process, and some important parts of the Emacs session, such as
;; 'window-system' and other GUI features, are not yet set up, which could
;; make some customization fail to work.

;; ============================================================================
;;; Speed improvments
;; ============================================================================

;;; - Report on startup time
(defconst +before-load-init-time (current-time))
(defun +load-init-time ()
  "Loading time of user init files including time for `after-init-hook'."
  (let ((time1 (float-time
                (time-subtract after-init-time +before-load-init-time)))
        (time2 (float-time
                (time-subtract (current-time) +before-load-init-time))))
    (message (concat "Man Made Emacs : "
                     "Loading init files: %.0f [msec], "
                     "of which %.f [msec] for `after-init-hook'."
		     " Garbage objects collected - %d")
             (* 1000 time1) (* 1000 (- time2 time1)) gcs-done)))
(add-hook 'after-init-hook #'+load-init-time t)

;; Overwrite the standard Emacs GNU message
(eval-after-load "startup" '(fset 'display-startup-echo-area-message (lambda ()(+load-init-time))))

;; Defer file checks
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda () (setq file-name-handler-alist last-file-name-handler-alist)))

;; Defer garbage collection
(setq gc-cons-threshold most-positive-fixnum)

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; When idle for 15sec run the Garbage Collection no matter what.
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda ()
                         (let ((inhibit-message t))
                           (message "Man Made Emacs : Garbage Collector has run for %.06fsec"
                                    (k-time (garbage-collect)))))))

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; Packages are compiled ahead-of-time.
(setq native-comp-deferred-compilation t)
(setq native-comp-speed 2)
(setq native-comp-async-report-warnings-errors nil)
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local obsolete))
(setq debug-on-error nil)

;; Inform if native-comp is not available
(when (not (and (fboundp 'native-comp-available-p)
                (native-comp-available-p)))
  (message "Man Made Emacs : Native compilation is *not* available"))

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq package-enable-at-startup t)

;;Never use a site file
(setq site-run-file nil)

;; ============================================================================
;;; GUI settings to improve startup-time
;; ============================================================================

;; Resizing the Emacs frame can be an expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq-default frame-title-format nil)
(setq-default frame-inhibit-implied-resize t)

;; Disable start-up screen
(setq-default inhibit-splash-screen t)
(setq-default inhibit-startup-screen t)
(setq-default inhibit-startup-message t)
(setq-default initial-scratch-message nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
;; Disable tool, menu, and scrollbars. these are just clutter (the scrollbar
;; also impacts performance). I am intentionally not calling `menu-bar-mode',
;; `tool-bar-mode', and `scroll-bar-mode' (see doom for inspiration) because
;; they do extra and unnecessary work that can be more concisely and efficiently
;; expressed with these six lines:
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Fundamental mode at startup
(setq initial-major-mode 'fundamental-mode)

;; Inhibit font caching
(setq inhibit-compacting-font-caches t)

;; Set the encoding system
(set-language-environment "UTF-8")

;; echo buffer
;; Don't display any message
;; https://emacs.stackexchange.com/a/437/11934
(defun display-startup-echo-area-message ()
  (message ""))

;; And bury the scratch buffer, don't kill it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))
