;; vendored code outside of package archives
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("603a831e0f2e466480cdc633ba37a0b1ae3c3e9a4e90183833bc4def3421a961" default))
 '(global-linum-mode t)
 '(package-selected-packages
   '(solidity-mode hcl-mode verb vterm-toggle vterm use-package typescript-mode cider rjsx-mode csharp-mode yaml-mode xclip tangotango-theme smartrep scala-mode rainbow-mode rainbow-delimiters python pos-tip pbcopy paredit multiple-cursors markdown-mode fuzzy expand-region company))
 '(safe-local-variable-values
   '((cljr-suppress-no-project-warning . t)
     (cider-ns-refresh-after-fn . "system/override")
     (cider-ns-save-files-on-refresh . t)
     (cider-known-endpoints
      ("localhost" "7888"))
     (cider-clojure-cli-global-options "-A:shadow")
     (cider-shadow-watched-builds "app" "cards")
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (cider-cljs-lein-repl . "(do (dev) (go) (cljs-repl))")
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend")))
 '(show-paren-mode t)
 '(typescript-indent-level 2))

(setq byte-compile-warnings '(not obsolete))
;; (load "~/.emacs.d/nxhtml/autostart.el")
(defvar native-comp-deferred-compilation-deny-list nil)
(setq package-enable-at-startup nil)

;; built-in packages
(require 'uniquify)
(require 'cl)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (concat (getenv "HOME") "/bin"))


(defvar my-packages
  '(expand-region pbcopy xclip verb typescript-mode company
    markdown-mode paredit python cider csharp-mode go-mode csv-mode
    rainbow-mode popup fuzzy pos-tip smartrep multiple-cursors rust-mode
    solidity-mode yaml-mode dracula-theme hc-zenburn-theme spacemacs-theme
    ;; clojure stuff:
    ;; http://fgiasson.com/blog/index.php/2014/05/22/my-optimal-gnu-emacs-settings-for-developing-clojure-so-far/
    clojure-mode paredit popup
    rainbow-delimiters inf-clojure use-package vterm))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(dolist (p my-packages)
    (straight-use-package p))

;; (straight-use-package 'spacemacs-theme)

(straight-use-package 'solidity-mode)

(straight-use-package 'vterm
    :ensure t)

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)


;; =============== clojure config ======================
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)


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


(load-theme 'spacemacs-dark t)

(setq inhibit-splash-screen t)

(when (eq window-system 'x)
  (xclip-mode 1)
  (setq x-select-enable-clipboard t)
  (setq x-select-enable-primary t))

(when (eq window-system 'ns)
  (require 'pbcopy)
  (turn-on-pbcopy))

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

;; lockfiles annoy the compiler watch sometimes
(setq create-lockfiles nil)


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

(defun font-size ()
  (interactive)
  (set-face-attribute 'default nil :family "Monaco" :height 100 :weight 'normal))

(defun font-size-present ()
  (interactive)
  (set-face-attribute 'default nil :family "Monaco" :height 125 :weight 'normal))

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

(setq cider-clojure-cli-global-options "-A:dev")

(add-to-list 'load-path "~/.emacs.d/locals/")

(defun load-if-exists (filename)
  "Load the elisp file FILENAME if it exists on `load-path'."
  (let ((filepath (locate-library filename)))
    (when filepath
      (load filepath))))


(load-if-exists "locals-looplegendz.el")
;; (load-if-exists "locals-sunchat.el")

(defun spit-append (filename data)
  "Appends DATA to FILENAME."
  (with-temp-buffer
    (insert data)
    (write-region (point-min) (point-max) filename t)))

(defun spit (filename data)
  "Writes DATA to FILENAME."
  (with-temp-buffer
    (insert data)
    (write-region (point-min) (point-max) filename)))

(defun append-utils-shell-config (shell-config-file)
  (spit-append shell-config-file
               (string-join '("# inserted by init.el"
                              "\nsource \"$HOME/.emacs.d/vterm.sh\""
                              "alias gs='git status' "
                              "alias gd='git diff'"
                              "alias ga='git add'"
                              "rgg() { rg --color=always \"$1\" | less -S }"
                              "# end init.el inserts")
                            "\n")))

;; (append-utils-shell-config "~/.zshrc")

(add-hook 'vterm-mode-hook #'goto-address-mode)
(setq browse-url-browser-function 'browse-url-default-browser)
