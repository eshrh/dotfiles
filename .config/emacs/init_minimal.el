(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

(setq straight-check-for-modifications nil)
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

(defalias 'sup 'straight-use-package)

(setq comp-deferred-compilation t)
(setq warning-suppress-log-types '((comp)))

(defmacro add-fs-to-hook (hook &rest funcs)
  "Add functions to hook. A function is either an unquoted token, or a form.
If it's a token, then its treated as a function and enabled. Otherwise, the form is run."
  `(add-hook ,hook
             (fn ,@(mapcar (lambda (el)
                             (if (listp el)
                                 el
                               (list el 1)))
                           funcs))))

(defmacro add-to-hooks (f &rest hooks)
  "Add a single function to many quoted hooks"
  `(progn ,@(mapcar (lambda (hook)
                      `(add-hook ,hook ,f))
                    hooks)))

(defmacro fn (&rest forms)
  (declare (indent 0))
  `(lambda () ,@forms))

(sup 's)
(sup 'dash)

(global-set-key [?\C-h] 'delete-backward-char)
(global-set-key [?\C-x ?h] 'help-command)

(global-set-key [?\C-z] #'kill-whole-line)

(setq ring-bell-function 'ignore)

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(scroll-bar-mode -1)

(column-number-mode)
(show-paren-mode)
(defun show-paren--locate-near-paren-ad ()
  "Locate an unescaped paren \"near\" point to show.
If one is found, return the cons (DIR . OUTSIDE), where DIR is 1
for an open paren, -1 for a close paren, and OUTSIDE is the buffer
position of the outside of the paren.  Otherwise return nil."
  (let* ((before (show-paren--categorize-paren (point))))
    (when (or
       (eq (car before) 1)
       (eq (car before) -1))
      before)))

(advice-add 'show-paren--locate-near-paren
            :override #'show-paren--locate-near-paren-ad)

(sup 'rainbow-mode)
(add-hook 'prog-mode #'rainbow-mode)

(global-hl-line-mode)

(add-fs-to-hook 'prog-mode-hook
                (add-hook 'after-save-hook
                          (fn (whitespace-cleanup))))

(sup 'gruvbox-theme)
(load-theme 'gruvbox-dark-hard t nil)

(sup 'telephone-line)
(require 'telephone-line)
(setq telephone-line-primary-left-separator 'telephone-line-cubed-left
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
      telephone-line-primary-right-separator 'telephone-line-cubed-right
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)

(setq telephone-line-height 24
      telephone-line-evil-use-short-tag t)

(telephone-line-defsegment* telephone-line-simpler-major-mode-segment ()
  (concat "["
          (if (listp mode-name)
              (car mode-name)
            mode-name) "]"))

(telephone-line-defsegment* telephone-line-simple-pos-segment ()
  (concat "%c : " "%l/" (number-to-string (count-lines (point-min) (point-max))) ))

(setq telephone-line-lhs
      '((nil . (telephone-line-projectile-buffer-segment))
        (accent . (telephone-line-simpler-major-mode-segment))
        (nil . (telephone-line-meow-tag-segment
                telephone-line-misc-info-segment)))
      telephone-line-rhs
      '((nil . (telephone-line-simple-pos-segment))
        (accent . (telephone-line-buffer-modified-segment))))

(telephone-line-mode 1)

(sup '(nyaatouch
       :repo "https://github.com/eshrh/nyaatouch"
       :fetcher github))
(turn-on-nyaatouch)

(meow-leader-define-key
 '("d" . vterm-toggle-cd))

(meow-normal-define-key '("r" . meow-delete))

(meow-normal-define-key
 '("`" . fill-paragraph))

(unless (display-graphic-p)
  (setq meow-esc-delay 0))

(setq meow-expand-hint-counts
      (-map (lambda (el) `(,(car el) . 10)) meow-expand-hint-counts))

(sup 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history nil)

(sup 'ace-window)
(global-set-key [remap other-window] 'ace-window)
(setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)) ;; dvorak moment
(setq aw-scope 'frame) ;; don't hint me for things outside the frame
(setq aw-background nil) ;; don't change the buffer background
(setq aw-ignore-current t) ;; i never want to select the current buffer

(sup 'dashboard)
(dashboard-setup-startup-hook)

(setq recentf-exclude '("~/org/"))
(setq dashboard-agenda-release-buffers t)

(setq initial-buffer-choice (get-buffer "*dashboard*"))

(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)
(setq dashboard-set-footer nil)

(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 5)))
(setq dashboard-agenda-sort-strategy '(time-up))
(setq dashboard-item-names '(("Recent Files:" . "recent:")
                             ("Projects:" . "projects:")
                             ("Agenda for the coming week:" . "agenda:")))

(setq dashboard-banner-logo-title "GNU emacsへようこそ。")

(defmacro set-dashboard-banner (name)
  `(setq dashboard-startup-banner
         (expand-file-name ,name user-emacs-directory)))

(sup 'company)
(add-hook 'after-init-hook #'global-company-mode)

(sup 'projectile)
(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(defun find-file-or-projectile ()
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'projectile-find-file)
    (call-interactively 'find-file)))

(global-set-key (kbd "C-x C-f") 'find-file-or-projectile)
;; just in case i need to use standard find file, probably to make a file.
(meow-leader-define-key '("U" . find-file))

(sup 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(sup 'prescient)
(sup 'ivy-prescient)
(ivy-prescient-mode)

(sup 'marginalia)
(marginalia-mode)

(sup 'which-key)
(which-key-mode)

(sup 'vterm)

(add-fs-to-hook 'vterm-mode-hook (setq-local global-hl-line-mode
                                             (null global-hl-line-mode)))

(setq vterm-kill-buffer-on-exit t)
(setq vterm-buffer-name-string "vt")

(add-to-list 'meow-mode-state-list '(vterm-mode . insert))

(sup 'vterm-toggle)
(setq vterm-toggle-hide-method 'delete-window)
(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda (bufname _)
                 (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                (display-buffer-reuse-window display-buffer-at-bottom)
                (dedicated . t)
                (reusable-frames . visible)
                (window-height . 0.4)))

(defun vterm--kill-vterm-buffer-and-window (process event)
  "Kill buffer and window on vterm process termination."
  (when (not (process-live-p process))
    (let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (kill-buffer)
          (ignore-errors (delete-window))
          (message "VTerm closed."))))))

(add-fs-to-hook 'vterm-mode-hook
                (set-process-sentinel (get-buffer-process (buffer-name))
                                      #'vterm--kill-vterm-buffer-and-window))

(setq-default c-basic-offset 4
              kill-whole-line t
              indent-tabs-mode nil)

(smartparens-global-mode)
(defun sp-disable (mode str)
  (sp-local-pair mode str nil :actions nil))
(sp-disable 'lisp-data-mode "'")

(sp-disable 'emacs-lisp-mode "'")
(sp-disable 'emacs-lisp-mode "`")
(sp-disable 'org-mode "'")

(sup 'aggressive-indent-mode)
(add-hook 'lisp-data-mode-hook #'aggressive-indent-mode 1)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

(defun my-asm-mode-hook ()
  (setq tab-always-indent (default-value 'tab-always-indent)))
(add-fs-to-hook 'asm-mode-hook
                (local-unset-key (vector asm-comment-char))
                (setq tab-always-indent (default-value 'tab-always-indent)))

(setq user-full-name "Eshan Ramesh"
      user-mail-address "esrh@gatech.edu")

(defalias 'yes-or-no-p 'y-or-n-p)
(setq vc-follow-symlinks nil)
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
(defconst emacs-tmp-dir
  (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
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

(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

(global-set-key (kbd "C-c /") #'comment-or-uncomment-region)

(cond
 ;; try hunspell at first
  ;; if hunspell does NOT exist, use aspell
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

  ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
  ;; If it's nil, Emacs tries to automatically set up the dictionaries.
  (when (boundp 'ispell-hunspell-dictionary-alist)
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))

 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

(global-set-key (kbd "C-x w") 'ace-swap-window)

(defun load-init ()
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun load-this-file ()
  (interactive)
  (load-file (buffer-file-name)))

(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'load-this-file)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(setq-default indent-tabs-mode nil)

(setq mode-require-final-newline nil)

(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message "")

(setq use-dialog-box nil)

(define-key key-translation-map [?\C-x] [?\C-u])
(define-key key-translation-map [?\C-u] [?\C-x])

(setq confirm-kill-processes nil)

(when (executable-find "rg")
  (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))

(define-key comint-mode-map (kbd "C-p") #'comint-previous-input)
(define-key comint-mode-map (kbd "C-n") #'comint-next-input)
(define-key comint-mode-map (kbd "C-w") #'backward-kill-word)
