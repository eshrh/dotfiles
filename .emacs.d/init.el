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

(straight-use-package 'use-package)
(use-package straight
         :custom (straight-use-package-by-default t))

(setq user-full-name "Eshan Ramesh"
      user-mail-address "esrh@gatech.edu")

(defalias 'yes-or-no-p 'y-or-n-p)

(setq kill-buffer-query-functions
	  (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

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

(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode mingus-playlist-mode dashboard-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq ring-bell-function 'ignore)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(column-number-mode)
(show-paren-mode)

(defvar emacs-english-font "Hack")
(defvar emacs-cjk-font "IPAGothic")

(defvar emacs-font-size-pair '(17 . 20))
(defvar emacs-font-size-pair-list
  '(( 5 .  6) (10 . 12)
    (13 . 16) (15 . 18) (17 . 20)
    (19 . 22) (20 . 24) (21 . 26)
    (24 . 28) (26 . 32) (28 . 34)
    (30 . 36) (34 . 40) (36 . 44))
  "This list is used to store matching (english . japanese) font-size.")

(defun set-font-frame (english japanese size-pair frame)
  "Setup emacs English and Japanese font on x window-system."
  (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t (list frame))
  ;;(set-face-attribute 'default nil :font english)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
	(set-fontset-font (frame-parameter frame 'font) charset
					  (font-spec :family japanese :size (cdr size-pair)))))

(defun emacs-step-font-size (step)
  "Increase/Decrease emacs's font size."
  (let ((scale-steps emacs-font-size-pair-list))
    (if (< step 0) (setq scale-steps (reverse scale-steps)))
    (setq emacs-font-size-pair
          (or (cadr (member emacs-font-size-pair scale-steps))
              emacs-font-size-pair))
    (when emacs-font-size-pair
      (message "emacs font size set to %.1f" (car emacs-font-size-pair))
      (set-font-frame emacs-english-font emacs-cjk-font emacs-font-size-pair (selected-frame)))))

(defun configure-fonts (frame)
  (when (display-graphic-p frame)
	(progn 
	  (set-font-frame emacs-english-font emacs-cjk-font emacs-font-size-pair frame))))

(add-hook 'after-make-frame-functions #'configure-fonts)
(add-hook 'dashboard-mode-hook (lambda ()
                                 (configure-fonts (selected-frame))))

(straight-use-package 'kaolin-themes)
  (if (or (display-graphic-p) (daemonp))
      (progn (load-theme 'kaolin-galaxy t))
      (progn (load-theme 'wombat t)))

(setq-default frame-title-format '("emacs: %b"))

(straight-use-package 'highlight-defined)
(straight-use-package 'highlight-numbers)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'highlight-quoted)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (highlight-numbers-mode)
                                  (highlight-defined-mode)
                                  (highlight-quoted-mode)
                                  (rainbow-delimiters-mode)))

(straight-use-package 'smart-mode-line)
(sml/setup)
(setq sml/theme 'respectful)

(display-time-mode 1)
(setq display-time-24hr-format t)

(straight-use-package 'ace-window)
(global-set-key [remap other-window] 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-scope 'frame)
(setq aw-background nil)
(setq aw-ignore-current t)

(use-package dashboard
  :config (dashboard-setup-startup-hook))

(setq initial-buffer-choice (get-buffer "*dashboard*"))
;;(setq dashboard-startup-banner 1)
(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)
(setq dashboard-set-init-info nil)
(setq dashboard-set-footer nil)

(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 5)))

(setq dashboard-item-names '(("Recent Files:" . "recent:")
                             ("Projects:" . "projects:")
                             ("Agenda for the coming week:" . "agenda:")))

;; (setq dashboard-banner-logo-title (concat "GNU emacsへようこそ。今日は"
;;                                           (format-time-string "%m")
;;                                           "月"
;;                                           (format-time-string "%e")
;;                                           "日です"))
(setq dashboard-banner-logo-title "GNU emacsへようこそ。")

(if (or (display-graphic-p) (daemonp))
    (progn (setq dashboard-startup-banner (expand-file-name "hiten_render_rsz.png" user-emacs-directory)))
    (progn (setq dashboard-startup-banner (expand-file-name "gnu.txt" user-emacs-directory))))

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

  (use-package evil-indent-textobject))

(use-package evil-collection
  :config
  (evil-collection-init))

(straight-use-package 'vimish-fold)
(straight-use-package 'evil-vimish-fold)

(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(straight-use-package 'company-ctags)

(straight-use-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(straight-use-package 'selectrum)
(straight-use-package 'prescient)
(straight-use-package 'selectrum-prescient)
(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

(straight-use-package 'marginalia)
(marginalia-mode)

(straight-use-package 'anki-editor)
(straight-use-package 'sdcv)
(straight-use-package 'clipmon)

(straight-use-package 'migemo)
(straight-use-package 'ivy-migemo)
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(migemo-init)

(straight-use-package '(mecab :type git
                              :repo "https://github.com/syohex/emacs-mecab"
                              :pre-build ("make")))

(straight-use-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq nov-text-width 100)

(straight-use-package 'libmpdee)
(straight-use-package 'mingus)

(defun mingus-redraw-to-hook (FRAME)
  (mingus-redraw-all FRAME))

(add-hook 'window-size-change-functions
          'mingus-redraw-to-hook)

(straight-use-package 'hl-todo)
(global-hl-todo-mode)

(straight-use-package 'highlight-indent-guides)

(straight-use-package 'which-key)
(which-key-mode)

(straight-use-package 'format-all)

(straight-use-package 'vterm)

(setq vterm-kill-buffer-on-exit t)
(setq vterm-buffer-name-string "vt//%s")

(global-set-key (kbd "<C-return>") 'vterm-toggle-cd)
(global-set-key (kbd "<C-S-return>") 'vterm-toggle)

(straight-use-package 'vterm-toggle)
(setq vterm-toggle-hide-method 'delete-window)
(setq vterm-toggle-fullscreen-p nil)

(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                (display-buffer-reuse-window display-buffer-at-bottom)
                (dedicated . t) ;dedicated is supported in emacs27
                (reusable-frames . visible)
                (window-height . 0.3)))

(defun vterm--kill-vterm-buffer-and-window (process event)
    "Kill buffer and window on vterm process termination."
    (when (not (process-live-p process))
      (let ((buf (process-buffer process)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (kill-buffer)
            (ignore-errors (delete-window))
            (message "VTerm closed."))))))
(add-hook 'vterm-mode-hook
          (lambda ()
            (set-process-sentinel (get-buffer-process (buffer-name))
                                  #'vterm--kill-vterm-buffer-and-window)))

(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/"))
(setq org-hide-emphasis-markers t)
(setq org-startup-with-latex-preview t)
(evil-leader/set-key "o" 'org-agenda)
(add-hook 'org-mode-hook (lambda ()
               ;;(org-superstar-mode 1)
               (org-indent-mode 1)
               (org-fragtog-mode 1)
               (setq electric-quote-mode 'nil)))

(setq org-deadline-warning-days 2)

(straight-use-package 'org-fragtog)

(when (file-exists-p "ircconfig.elc")
  (load (expand-file-name "ircconfig" user-emacs-directory)))

(straight-use-package 'yasnippet)
(yas-global-mode)
(setq yas-indent-line 'fixed)

(straight-use-package 'dired+)
(diredp-toggle-find-file-reuse-dir 1)

(straight-use-package 'aurel)
(setq aurel-info-download-function 'aurel-download-unpack-pkgbuild)
(setq aurel-list-download-function 'aurel-download-unpack-pkgbuild)

(setq electric-pair-pairs '(
                           (?\{ . ?\})
                           (?\( . ?\))
                           (?\[ . ?\])
                           (?\" . ?\")
                           ))
(electric-pair-mode)
(electric-quote-mode)

(setq-default indent-tabs-mode nil)

(setq mode-require-final-newline nil)

(straight-use-package 'flycheck)

(straight-use-package 'magit)

(straight-use-package 'company-lsp)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)

(setq lsp-ui-doc-show-with-mouse nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-modeline-code-actions-enable 1)

(add-hook 'lsp-mode-hook (lambda ()
			   (local-set-key (kbd "C-c C-j") 'lsp-execute-code-action)))

(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

(straight-use-package 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 4)
			(setq tab-width 4)
            ))

(straight-use-package 'haskell-mode)
(straight-use-package 'lsp-haskell)
(require 'lsp-mode)
(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(setq haskell-interactive-popup-errors t)

(setq-default tab-width 4
              c-basic-offset 4
              kill-whole-line t
              indent-tabs-mode nil)

(add-hook 'lisp-mode-hook 'flycheck-mode)
(straight-use-package 'slime)
(setq inferior-lisp-program "sbcl")

(straight-use-package 'auctex)

(setq TeX-view-program-selection '((output-pdf "Zathura")))

(add-hook 'tex-mode #'lsp)
(add-hook 'tex-mode (lambda ()
					  (setq lsp-lens-enable nil)))

(straight-use-package 'lsp-jedi)
(add-hook 'python-mode #'lsp)

(straight-use-package 'polymode)
(straight-use-package 'ein)
(setq ein:polymode t)

(straight-use-package 'sage-shell-mode)
(setq sage-shell:sage-executable "/usr/bin/sage")

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
 (balance-windows)
 (other-window 1))
(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

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

(global-set-key (kbd "C-o") 'execute-extended-command)
(define-key evil-motion-state-map (kbd "C-o") nil)

(global-set-key (kbd "C-\;") 'ace-window)
(global-set-key (kbd "C-p") 'ace-window)
(define-key evil-motion-state-map (kbd "C-\;") nil)
(define-key evil-normal-state-map (kbd "C-p") nil)

(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

(global-set-key (kbd "C-h C-f") 'xref-find-definitions)
(global-set-key (kbd "C-h C-j") 'xref-pop-marker-stack)

(defun load-init ()
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-export-backends '(ascii beamer html icalendar latex md odt))
 '(tab-width 4)
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-items-face ((t (:inherit widget-button :overline nil :underline nil)))))
