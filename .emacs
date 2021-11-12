;; Who am I
(setq user-full-name "Gurunandan Bhat")
(setq user-mail-address "gbhat@pobox.com")

;; The shiny new package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu"   . "http://elpa.gnu.org/packages/"))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default))
 '(package-selected-packages
   '(exec-path-from-shell toml-mode lsp-ui flycheck lsp-mode ## rustic web-mode solarized-theme paredit markdown-mode magit js2-mode go-mode fill-column-indicator auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((((class color) (min-colors 89)) (:background unspecified :foreground "#dc322f"))))
 '(cperl-hash-face ((((class color) (min-colors 89)) (:background unspecified :foreground "#dc322f"))))
 '(cperl-nonoverridable-face ((t (:foreground "#2aa198" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "darkslategray"))))
 '(font-lock-keyword-face ((((class color) (min-colors 89)) (:foreground "#6c71c4" :weight unspecified))))
 '(font-lock-string-face ((t (:foreground "#859900")))))

(defvar gbhat/packages '( auto-complete
			  company
                          fill-column-indicator
			  flycheck
			  go-mode
			  gnu-elpa-keyring-update
                          js2-mode
			  lsp-mode
                          magit
                          markdown-mode
                          org
                          paredit
			  powerline
			  smart-mode-line
			  rust-mode
			  rustic
			  solarized-theme
			  use-package
                          web-mode
			  xref
			  yasnippet)
  "Default packages")
                                        ; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

                                        ; install the missing packages
(dolist (package gbhat/packages)
  (unless (package-installed-p package)
    (package-install package)))

(setq solarized-distinct-fringe-background t)
(setq solarized-use-variable-pitch nil)
(setq solarized-high-contrast-mode-line t)
(setq solarized-use-less-bold t)
(setq solarized-use-less-italic t)
(setq solarized-emphasize-indicators nil)
(load-theme 'solarized-dark t)

;; Take me to the scratch mode straight away
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Hide the various bars
;; (scroll-bar-mode -1)
(if window-system (tool-bar-mode -1))
(menu-bar-mode -1)

;; Marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Shows empty line on left
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq-default
 frame-title-format
 (list '((buffer-file-name " %f" (dired-directory
                                  dired-directory
                                  (revert-buffer-function " %b"
                                                          ("%b - Dir:  " default-directory)))))))


(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Sane tab handling. Change to spaces

(setq tab-width 4
      indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 4 200 4))

;; Backup files
(setq make-backup-files nil)
;; :)
(defalias 'yes-or-no-p 'y-or-n-p)

;; scroll line by line
(setq scroll-step 1)

;; Some nice key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-G") 'what-line)
(global-set-key (kbd "<f11>") 'undo)
(global-set-key (kbd "C-S-s") 'isearch-forward-symbol-at-point)


;; Misc
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)

(setq show-paren-delay 0)
(show-paren-mode t)

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-log-done t)
(setq org-todo-keywords
      '((sequence "TODO" "INPROGRESS" "DONE")))
(setq org-todo-keyword-faces
      '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(setq org-agenda-files (list "~/Dropbox/org/personal.org"))

(setq org-src-fontify-natively t)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(setq column-number-mode t)

(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                   scheme-mode
                   clojure-mode))

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(defvar lisp-power-map (make-keymap))
(define-minor-mode lisp-power-mode "Fix keybindings; add power."
  :lighter " (power)"
  :keymap lisp-power-map
  (paredit-mode t))
(define-key lisp-power-map [delete] 'paredit-forward-delete)
(define-key lisp-power-map [backspace] 'paredit-backward-delete)

(require 'auto-complete-config)
(ac-config-default)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(defalias 'perl-mode 'cperl-mode)

;; perldoc
(add-hook 'cperl-mode-hook
          (lambda ()
            (local-set-key (kbd "<f5>") 'cperl-perldoc)))
(setq cperl-invalid-face (quote off))
(setq cperl-electric-keywords t)
(setq cperl-hairy t)

;;Set line width to 80 columns
(setq fill-column 80)
(setq auto-fill-mode t)

;;
;;=============== Indent entire file =================
;;
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;============== Line number mode =======================
(global-linum-mode 1)
(setq linum-format "%4d \u2502 ")
;; (global-display-line-numbers-mode)

;;============== Remove trailing whitespace ============
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key (kbd "C-x C-r") (lambda () (interactive) (revert-buffer nil t)))

(require 'fill-column-indicator)
;; (setq fci-rule-character-color "#073642")
(setq fci-rule-character-color "#404040")
(setq fci-rule-column 80)
(setq fci-rule-character 9474)
(setq fci-always-use-textual-rule t)

(defun my-change-window-divider ()
  (let ((display-table (or buffer-display-table
			   standard-display-table
			   (make-display-table))))
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table)))

(add-hook 'window-configuration-change-hook 'my-change-window-divider)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(setq debug-on-error t)

;; When using this directly, you will need to have use-package installed:
;; M-x package-install, select use-package. But if you start via
;; `standalone.el', this is being taken care of automatically.


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; rustic = basic rust-mode + additions

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for rust-analyzer integration

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; inline errors

(use-package flycheck :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; auto-completion and code snippets

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :ensure
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for Cargo.toml and other config files

(use-package toml-mode :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; setting up debugging support with dap-mode

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(when (executable-find "lldb-mi")
  (use-package dap-mode
    :ensure
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
	   :gdbpath "rust-lldb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
           ))))


(require 'powerline)
(powerline-default-theme)
(sml/setup)
