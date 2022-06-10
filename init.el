;; vendored code outside of package archives
(setq byte-compile-warnings '(not obsolete))
;; (load "~/.emacs.d/nxhtml/autostart.el")

;; built-in packages
(require 'uniquify)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))
(require 'cl)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (concat (getenv "HOME") "/bin"))


(defvar my-packages
  '(expand-region pbcopy ein xclip verb
    magit markdown-mode paredit python cider csharp-mode go-mode csv-mode
    rainbow-mode tangotango-theme dracula-theme popup fuzzy pos-tip smartrep multiple-cursors
    ;; clojure stuff:
    ;; http://fgiasson.com/blog/index.php/2014/05/22/my-optimal-gnu-emacs-settings-for-developing-clojure-so-far/
    clojure-mode auto-complete ac-cider paredit popup
    rainbow-delimiters inf-clojure use-package vterm))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  ;; (add-to-list 'package-archives
  ;; 	       '("marmalade" . "https://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
	       (cons "melpa"  "https://melpa.org/packages/") t)
  (package-initialize))

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

(use-package vterm
    :ensure t)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package html-to-hiccup
  :ensure t
  ;; (define-key clojure-mode-map (kbd "H-h") 'html-to-hiccup-convert-region)
  )

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)


;; =============== clojure config ======================
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)

;; General Auto-Complete
(require 'auto-complete-config)
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.5)
(ac-config-default)

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

(add-hook 'clojure-mode-hook 'paredit-mode)

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-string-face ((t (:foreground "#ad7fa8" :slant normal))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#eeeeec"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#fce94f"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#8ae234"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#fcaf3e"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#ad7fa8"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#729fcf"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#e9b96e"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#ef2929")))))

(setq nrepl-popup-stacktraces nil) ;; provisional
;; =====================================================


;; (load-theme 'tangotango t)
(load-theme 'dracula t)
(setq inhibit-splash-screen t)

(when (eq window-system 'x)
  (xclip-mode 1)
  (setq x-select-enable-clipboard t)
  (setq x-select-enable-primary t))

(when (eq window-system 'ns)
  (require 'pbcopy)
  (turn-on-pbcopy))

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
(setq ein:notebook-modes
      '(ein:notebook-mumamo-mode ein:notebook-plain-mode))

;;; end zeroein.el

;; personal idiosyncracies
(setq cider-test-show-report-on-success t)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)
(set-default 'truncate-lines t)
(setq-default fill-column 80)
(set-default 'require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-set-key "\C-xe" 'mc/edit-lines)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
;; Scroll down with the cursor,move down the buffer one
;; line at a time, instead of in larger amounts.
(setq scroll-step 1)

;; backup files (doesn't work)
(setq backup-directory-alist
      `(("." . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
         ,(expand-file-name "\\2" temporary-file-directory) t)))

(setq uniquify-buffer-name-style 'post-forward)
(windmove-default-keybindings)
(global-set-key (kbd "C-x C-r") 'rename-buffer)

(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Surround word or region with html tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tag-word-or-region (&optional start-reg end-reg tag)
    "Surround current word or region with a given tag."
    (interactive "r\nsEnter tag (without <>): ")
    (let (pos1 pos2 bds start-tag end-tag)
        (setq start-tag (concat "<" tag ">"))
        (setq end-tag (concat "</" (car (split-string tag " ")) ">"))
            (progn
                (goto-char end-reg)
                (insert end-tag)
                (goto-char start-reg)
                (insert start-tag))))

(global-set-key "\C-xt" 'tag-word-or-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUG STATEMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c d")
			   (lambda () (interactive)
			     (insert "import ipdb;ipdb.set_trace()")))))

(add-hook 'js-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c d") (lambda () (interactive)
					   (insert "debugger;")))))

(electric-pair-mode +1)

(set-face-attribute 'default nil :family "Monaco" :height 100 :weight 'normal)
;; (set-face-attribute 'default nil :family "Monaco" :height 120 :weight 'normal)
;;

(when (string-equal system-type "darwin")
  (set-frame-parameter nil 'fullscreen 'fullboth)
  (setq mac-command-modifier 'meta))

(when (string-equal system-type "gnu/linux")
  (toggle-frame-fullscreen))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq cider-repl-display-in-current-window t)
(setq inf-clojure-repl-use-same-window t)

;; auto reload .dir-locals.el
(add-hook 'emacs-lisp-mode-hook
          (defun enable-autoreload-for-dir-locals ()
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook (make-variable-buffer-local 'after-save-hook)
                        'my-reload-dir-locals-for-all-buffer-in-this-directory))))

(defadvice split-window-horizontally (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'split-window-horizontally)

(defadvice split-window-vertically (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'split-window-vertically)

(add-to-list 'auto-mode-alist '("\\.bazel\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.tf\\'" . hcl-mode))
