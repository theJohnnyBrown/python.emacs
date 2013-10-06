;; vendored code outside of package archives
(load "~/.emacs.d/nxhtml/autostart.el")

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))
(require 'cl)

(defvar my-packages
  '(clojure-mode coffee-mode expand-region pbcopy
		 magit markdown-mode paredit python
		 rainbow-mode tangotango-theme popup fuzzy pos-tip smartrep))


(when (>= emacs-major-version 24)
(require 'package)
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t))
(add-to-list 'package-archives 
    '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))
 
(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(load-theme 'tangotango t)
(setq inhibit-splash-screen t)

(require 'pbcopy)
(turn-on-pbcopy)

;; copied from zeroein.el

(eval-when-compile (require 'ein-notebooklist))
(require 'ein)

;; (defvar zeroein:root-dir "~/.emacs.d/elpa/ein-20130710.2114/")
;; (defun zeroein:path (p &rest ps)
;;   (if ps
;;       (apply #'zeroein:path
;;            (concat (file-name-as-directory p) (car ps)) (cdr ps))
;;     (concat zeroein:root-dir p)))
;; (setq ein:use-auto-complete-superpack t)

(require 'auto-complete-config nil t)
;; (declare-function global-auto-complete-mode "auto-complete.el")
(when (featurep 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode t))

;; MuMaMo
(custom-set-faces
   '(mumamo-background-chunk-major
     ((((class color) (min-colors 88) (background dark)) nil)))
   ;; '(mumamo-background-chunk-submode1
   ;;   ((((class color) (min-colors 88) (background dark)) nil)))
   )

;;; Workaround

;; Suppress this warning when using mumamo:
;; Warning: `font-lock-syntactic-keywords' is an obsolete variable (as of 24.1);
;;     use `syntax-propertize-function' instead.
(when (and (equal emacs-major-version 24)
           (equal emacs-minor-version 1))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-syntactic-keywords)))
;; See: http://stackoverflow.com/a/5470584/727827
(setq ein:notebook-modes '(ein:notebook-mumamo-mode ein:notebook-plain-mode))
