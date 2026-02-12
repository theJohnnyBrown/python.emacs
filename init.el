(require 'uniquify)
;; vendored code outside of package archives
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-linum-mode t)
 ;; '(package-selected-packages
 ;;   '(solidity-mode hcl-mode verb vterm-toggle vterm use-package typescript-mode cider rjsx-mode csharp-mode yaml-mode xclip tangotango-theme smartrep scala-mode rainbow-mode rainbow-delimiters python pos-tip pbcopy paredit multiple-cursors markdown-mode fuzzy expand-region company))
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

;; (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;; (add-to-list 'exec-path "/usr/local/bin")
;; (add-to-list 'exec-path (concat (getenv "HOME") "/bin"))


(defvar my-packages
  '(pbcopy xclip typescript-mode vterm zenburn-theme cyberpunk-theme
    markdown-mode paredit python cider go-mode csv-mode
    multiple-cursors rust-mode
    solidity-mode yaml-mode
    ;; clojure stuff:
    ;; http://fgiasson.com/blog/index.php/2014/05/22/my-optimal-gnu-emacs-settings-for-developing-clojure-so-far/
    clojure-mode popup rainbow-delimiters inf-clojure))

;; download and install straight-use-package
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

;; install packages
(dolist (p my-packages)
    (straight-use-package p))

;; configure certain packages
(use-package vterm
  :config
  (add-hook 'vterm-mode-hook #'goto-address-mode)
  (setq browse-url-browser-function 'browse-url-default-browser)
  (define-key vterm-mode-map (kbd "C-c o") #'find-file-at-point)
  (define-key vterm-copy-mode-map (kbd "C-c o") #'find-file-at-point))

(use-package vterm
  :straight (vterm :type git :host github :repo "akermu/emacs-libvterm")
  :config
  (add-hook 'vterm-mode-hook #'goto-address-mode)
  (setq browse-url-browser-function 'browse-url-default-browser)
  (define-key vterm-mode-map (kbd "C-c o") #'find-file-at-point)
  (define-key vterm-copy-mode-map (kbd "C-c o") #'find-file-at-point))

(use-package cyberpunk-theme
  :config
  (load-theme 'cyberpunk t))

;; ================ copy/paste from/to emacs
(when (eq window-system 'x)
  (xclip-mode 1)
  (setq x-select-enable-clipboard t)
  (setq x-select-enable-primary t))

(when (eq window-system 'ns)
  (require 'pbcopy)
  (turn-on-pbcopy))

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



;; =============== clojure config ======================
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)


;; lisp theming
;; --------------------------------------------------------
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
;; -----------------------------------------------------------------

;; ================== general config =========================
(electric-pair-mode +1)
(setq inhibit-splash-screen t)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)
(set-default 'truncate-lines t)
(setq-default fill-column 80)
(set-default 'require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq uniquify-buffer-name-style 'post-forward)
(setq ring-bell-function 'ignore)
(setq create-lockfiles nil)
(tool-bar-mode 0)
(setq make-backup-files nil)
(setq auto-save-default nil)

(defun font-size ()
  (interactive)
  (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 125 :weight 'normal))


(defun font-size-smaller ()
  (interactive)
  (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 90 :weight 'normal))

(defun font-size-larger ()
  (interactive)
  (set-face-attribute 'default nil :family "DejaVu Sans Mono"  :height 150 :weight 'normal))

;; (font-size)
 (font-size-smaller)
;; (font-size-larger)

;; (when (string-equal system-type "darwin")
;;   (set-frame-parameter nil 'fullscreen 'fullboth)
;;   (setq mac-command-modifier 'meta))

(when (string-equal system-type "gnu/linux")
  (toggle-frame-fullscreen))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq cider-repl-display-in-current-window t)
(setq inf-clojure-repl-use-same-window t)

(defadvice split-window-horizontally (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'split-window-horizontally)

(defadvice split-window-vertically (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'split-window-vertically)

(add-to-list 'auto-mode-alist '("\\.bazel\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.tf\\'" . hcl-mode))

(setq cider-clojure-cli-global-options "-A:dev")

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(setq auto-revert-interval 1)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
;; Scroll down with the cursor,move down the buffer one
;; line at a time, instead of in larger amounts.
(setq scroll-step 1)
;; ======================= end general config ========================

;; =============== vterm shell setup and aliases ===================
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
                              "# end init.el inserts")
                            "\n")))

;; uncomment this and eval it once when setting up
;; (append-utils-shell-config "~/.zshrc")


;; ================= local config files (good place for secret env vars) ================
(add-to-list 'load-path "~/.emacs.d/locals/")
(defun load-if-exists (filename)
  "Load the elisp file FILENAME if it exists on `load-path'."
  (let ((filepath (locate-library filename)))
    (when filepath
      (load filepath))))
(load-if-exists "locals-jb.el")



;; ============ shortcuts =================
(global-set-key "\C-xe" 'mc/edit-lines)
(global-set-key (kbd "C-x C-r") 'rename-buffer)
(windmove-default-keybindings)
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
