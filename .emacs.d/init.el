(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
    (url-retrieve-synchronously
     "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
     'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq visible-bell 1)

(straight-use-package 'use-package)
(use-package straight
         :custom (straight-use-package-by-default t))

;;;; UI
(global-display-line-numbers-mode)
(setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(column-number-mode)
(show-paren-mode)

(add-to-list 'default-frame-alist '(font . "Hack-11"))
(set-face-attribute 'default t :font "Hack-11")

(setq-default frame-title-format '("emacs: %b"))

(straight-use-package 'gruvbox-theme)
(if (or (display-graphic-p) (daemonp))
	(progn (load-theme 'gruvbox t))
	(progn (load-theme 'wombat t)))

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-scope 'frame)
(setq aw-background nil)
(setq aw-ignore-current t)

;;(setq inhibit-startup-screen nil)
;;(setq initial-buffer-choice 'vterm)

;; evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (use-package evil-leader
		:config
		(global-evil-leader-mode t)
		(evil-leader/set-leader "<SPC>"))
  
  (use-package evil-surround
    :config (global-evil-surround-mode))

  (use-package evil-indent-textobject)

  (use-package powerline-evil
    :config
    (powerline-evil-vim-color-theme)))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-collection
  :config
  (evil-collection-init))


;;(defun evil-collection-vterm-escape-stay ()
;;  (setq-local evil-move-cursor-back nil))

;;(add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)

;;;; MISC PACKAGES
(straight-use-package 'vterm)
(straight-use-package 'magit)

(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(straight-use-package 'company-ctags)

(straight-use-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(straight-use-package 'counsel)
(straight-use-package 'counsel-etags)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(straight-use-package 'libmpdee)
(straight-use-package 'mingus)

(straight-use-package 'hl-todo)
(global-hl-todo-mode)

(straight-use-package 'highlight-indent-guides)

(straight-use-package 'ace-window)
(global-set-key [remap other-window] 'ace-window)
(straight-use-package 'powerline)
(powerline-center-evil-theme)

(straight-use-package 'vimish-fold)
(straight-use-package 'evil-vimish-fold)

(straight-use-package 'flycheck)

;;;; CONFIG

;; user
(setq user-full-name "Eshan Ramesh"
      user-mail-address "esrh@gatech.edu")

;; org
(straight-use-package 'org-superstar)
(straight-use-package 'org-fragtog)
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/"))
(setq org-hide-emphasis-markers t)
(setq org-deadline-warning-days 2)
(setq org-hide-emphasis-markers t)
(setq org-startup-with-latex-preview t)
(evil-leader/set-key "o" 'org-agenda)
(add-hook 'org-mode-hook (lambda ()
			   (org-superstar-mode 1)
			   (org-indent-mode 1)
			   (org-fragtog-mode 1)))
;; lsp
(straight-use-package 'company-lsp)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)

(setq lsp-ui-doc-show-with-mouse nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-modeline-code-actions-enable 1)
(add-hook 'lsp-mode-hook (lambda ()
			   (local-set-key (kbd "C-c C-j") 'lsp-execute-code-action)))

;; java
(straight-use-package 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 4)
			(setq tab-width 4)
            ;; use code format
            ))

;; haskell
(straight-use-package 'lsp-haskell)
(straight-use-package 'haskell-mode)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)
(add-hook 'haskell-mode-hook (lambda ()
			       (interactive-haskell-mode 1)
			       (setq lsp-lens-enable nil)
			       ))

;; C++
(straight-use-package 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

;; elisp
(add-hook 'lisp-mode-hook 'flycheck)

;; auctex
(straight-use-package 'auctex)
(setq TeX-view-program-selection '((output-pdf "Zathura")))

;; irc
;; store in other file bc passwd
(load (expand-file-name "ircconfig" user-emacs-directory))

;; term
(straight-use-package 'vterm-toggle)
(setq vterm-toggle-hide-method 'delete-window)

(setq vterm-kill-buffer-on-exit t)
(setq vterm-buffer-name-string "vt//%s")

(global-set-key (kbd "<C-return>") 'vterm-toggle-cd)
(global-set-key (kbd "<C-S-return>") 'vterm-toggle)

(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                (display-buffer-reuse-window display-buffer-at-bottom)
                ;;(display-buffer-reuse-window display-buffer-in-direction)
                ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                ;;(direction . bottom)
                ;;(dedicated . t) ;dedicated is supported in emacs27
                (reusable-frames . visible)
                (window-height . 0.3)))

(defun maybe-delete-frame-buffer (frame)
  "When a dedicated FRAME is deleted, also kill its buffer.
A dedicated frame contains a single window whose buffer is not
displayed anywhere else."
  (let ((windows (window-list frame)))
    (when (eq 1 (length windows))
      (let ((buffer (window-buffer (car windows))))
        (when (eq 1 (length (get-buffer-window-list buffer nil t)))
          (kill-buffer buffer))))))

(add-hook 'delete-frame-functions #'maybe-delete-frame-buffer)
;;(define-key vterm-mode-map (kbd "C-o") nil)

;; misc
(setq yas-indent-line 'fixed)

;; don't ask for killing process buffers
(setq kill-buffer-query-functions
	  (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(global-set-key (kbd "C-o") 'execute-extended-command)
(define-key evil-motion-state-map (kbd "C-o") nil)

(global-set-key (kbd "C-\;") 'ace-window)
(global-set-key (kbd "C-p") 'ace-window)
(define-key evil-motion-state-map (kbd "C-\;") nil)
(define-key evil-normal-state-map (kbd "C-p") nil)

(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

(defun load-init ()
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "4eb6fa2ee436e943b168a0cd8eab11afc0752aebb5d974bba2b2ddc8910fca8f" default))
 '(haskell-interactive-popup-errors t)
 '(org-export-backends '(ascii beamer html icalendar latex md odt))
 '(tab-width 4)
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
