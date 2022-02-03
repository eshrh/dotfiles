(setq straight-check-for-modifications '(find-when-checking))
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

(setq comp-deferred-compilation t)
(setq warning-suppress-log-types '((comp)))

(setq ring-bell-function 'ignore)

(menu-bar-mode -1)
(tool-bar-mode -1)
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

(straight-use-package 'rainbow-mode)
(rainbow-mode)

(global-hl-line-mode)

(add-hook 'prog-mode-hook
          (lambda () (setq show-trailing-whitespace t)))

(defvar emacs-english-font "Iosevka Hane Sans")
(defvar emacs-cjk-font "IPAGothic")

(setq my-font (concat emacs-english-font "-12"))
(set-face-attribute 'default t :font my-font)
(set-frame-font my-font nil t)

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

;;(add-hook 'after-make-frame-functions #'configure-fonts)
;;(add-hook 'dashboard-mode-hook (lambda ()
;;                                 (configure-fonts (selected-frame))))

(straight-use-package 'gruvbox-theme)
(if (or (display-graphic-p) (daemonp))
    (load-theme 'gruvbox-dark-hard t nil)
    (load-theme 'tsdh-dark t nil))

(setq-default frame-title-format '("emacs: %b"))

(straight-use-package 'highlight-defined)
(straight-use-package 'highlight-numbers)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'highlight-quoted)
(defun highlight-lisp-things ()
  (highlight-numbers-mode)
  (highlight-defined-mode)
  (highlight-quoted-mode)
  (rainbow-delimiters-mode))
(add-hook 'emacs-lisp-mode-hook #'highlight-lisp-things)

(straight-use-package 'smart-mode-line)
(setq sml/theme 'respectful)
(setq sml/no-confirm-load-theme t)
(sml/setup)

(defun meow-insert-right ()
  (interactive)
  (meow-right)
  (meow-insert))

(defun meow-negative-find ()
  (interactive)
  (let ((current-prefix-arg -1))
    (call-interactively 'meow-find)))

(put 'upcase-region 'disabled nil)
(defun upcase-dwiam ()
  "upcase, do what i actually mean"
  (interactive)
  (if (region-active-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-char 1)))

(defun replace-bounds (strt end content)
  (delete-region strt end)
  (insert (number-to-string content)))

(defun add-number (arg)
  (interactive "P")
  (let* ((num (thing-at-point 'number t))
         (bounds (bounds-of-thing-at-point 'word))
         (strt (car bounds))
         (end (cdr bounds)))
    (message "%s" arg)
    (if arg
        (replace-bounds strt end (+ num arg))
      (replace-bounds strt end (+ num 1)))))

(defun subtract-one ()
  (interactive)
  (let ((current-prefix-arg -1))
    (call-interactively 'add-number)))

(straight-use-package 'meow)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("*" . upcase-dwiam)
   '("+" . add-number)
   '("_" . subtract-one)
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("F" . meow-negative-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("/" . meow-insert-right)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . swiper)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("/" . avy-goto-word-1)
   '("<escape>" . ignore)))

(require 'meow)

(setq meow-paren-keymap (make-keymap))
(meow-define-state paren
  "paren state"
  :lighter " [P]"
  :keymap meow-paren-keymap)

(meow-normal-define-key
 '("Z" . meow-paren-mode))

(meow-define-keys 'paren
  '("<escape>" . meow-normal-mode)
  '("l" . sp-forward-sexp)
  '("h" . sp-backward-sexp)
  '("j" . sp-down-sexp)
  '("k" . sp-up-sexp)
  '("w s" . sp-wrap-square)
  '("w r" . sp-wrap-round)
  '("w c" . sp-wrap-curly)
  '("W" . sp-unwrap-sexp)
  '("n" . sp-forward-slurp-sexp)
  '("b" . sp-forward-barf-sexp)
  '("v" . sp-backward-barf-sexp)
  '("c" . sp-backward-slurp-sexp)
  '("s" . sp-splice-sexp-killing-forward)
  '("S" . sp-splice-sexp-killing-backward)
  '("e" . sp-end-of-sexp)
  '("a" . sp-beginning-of-sexp)
  '("t" . sp-transpose-hybrid-sexp)
  '("u" . meow-undo))

(setq meow-cursor-type-paren 'hollow)

(setq latex-thing-regexp
      '(regexp "\\\\begin{.*?}\\(.*?\\)\n\\|\\$"
               "\\\\end{.*?}\n\\|\\$"))

(meow-thing-register 'latex
		             latex-thing-regexp
                   latex-thing-regexp)

(add-to-list 'meow-char-thing-table
	         (cons ?x 'latex))

(require 'meow)
(meow-setup)
(meow-global-mode 1)

(straight-use-package 'ace-window)
(global-set-key [remap other-window] 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-scope 'frame)
(setq aw-background nil)
(setq aw-ignore-current t)

(straight-use-package 'avy)

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

(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(straight-use-package 'company-ctags)

(straight-use-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(straight-use-package 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(straight-use-package 'prescient)
(straight-use-package 'ivy-prescient)
(ivy-prescient-mode)

(straight-use-package 'swiper)

(straight-use-package 'marginalia)
(marginalia-mode)

(straight-use-package 'anki-editor)
(straight-use-package 'posframe)
(straight-use-package '(sdcv2 :type git
                              :repo "https://github.com/manateelazycat/sdcv"
                              :files ("sdcv.el")))

(cond ((string= (system-name) "himawari")
       (progn
         (setq sdcv-dictionary-simple-list '("jmdict-ja-en"))
         (setq sdcv-dictionary-complete-list '("jmdict-ja-en"
                                               "J_PLACES"))))
      ((string= (system-name) "shiragiku")
       (progn
         (setq sdcv-dictionary-simple-list '("JMdict_e"))
         (setq sdcv-dictionary-complete-list '("daijisen.tab" "JMdict_e")))))

(setq sdcv-dictionary-data-dir "/usr/share/stardict/dic/")
(setq sdcv-env-lang "ja_JP.UTF-8")
(straight-use-package 'clipmon)

  (straight-use-package 'migemo)
  (straight-use-package 'ivy-migemo)
  (straight-use-package 's)

  (unless (executable-find "cmigemo")
    (if (yes-or-no-p "install")
    (make-directory (concat user-emacs-directory "japanese") t)
    (let ((clonedir (concat user-emacs-directory "japanese" "/cmigemo/")))
      (unless (file-directory-p clonedir)
        (magit-clone-internal "https://github.com/koron/cmigemo"
                              nil)))
    (let ((default-directory
            (concat
             user-emacs-directory "japanese" "/cmigemo/")))
      (shell-command "make gcc")
      (shell-command "make gcc-dict")
      (shell-command "cd dict ; make utf-8")
      (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password? "))
                             " | sudo -S make gcc-install")))))
(if (executable-find "cmigemo")
  (require 'migemo)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (if (file-directory-p "/usr/share/migemo")
      (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
    (setq migemo-dictionary (concat user-emacs-directory
                                 "japanese/cmigemo/dict/utf-8.d/migemo-dict")))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init))

(if (executable-find "mecab")
    (straight-use-package '(mecab :type git
                                  :repo "https://github.com/syohex/emacs-mecab"
                                  :pre-build ("make")
                                  :files ("mecab-core.so"
                                          "mecab-core.o"
                                          "mecab-core.c"
                                          "mecab.el"))))

(straight-use-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq nov-text-width 100)

(straight-use-package 'emms)
(require 'emms-setup)
(emms-all)

(setq emms-player-list '(emms-player-mpd))
(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")
(setq emms-player-mpd-music-directory "~/mus")
(emms-player-mpd-connect)

(defun emms-info-mpd-process (track info)
  (dolist (data info)
    (let ((name (car data))
	  (value (cdr data)))
      (setq name (cond ((string= name "artist") 'info-artist)
		       ((string= name "albumartist") 'info-albumartist)
		       ((string= name "composer") 'info-composer)
		       ((string= name "performer") 'info-performer)
		       ((string= name "title") 'info-title)
		       ((string= name "album") 'info-album)
		       ((string= name "track") 'info-tracknumber)
		       ((string= name "disc") 'info-discnumber)
		       ((string= name "date") 'info-year)
		       ((string= name "genre") 'info-genre)
		       ((string= name "time")
			(setq value (string-to-number value))
			'info-playing-time)
		       (t nil)))
      (when name
	(emms-track-set track name value)))))

(straight-use-package 'hl-todo)
(global-hl-todo-mode)

(straight-use-package 'highlight-indent-guides)

(straight-use-package 'which-key)
(which-key-mode)

(straight-use-package 'format-all)

(straight-use-package 'vterm)
(straight-use-package 'fish-mode)

(setq vterm-kill-buffer-on-exit t)
(setq vterm-buffer-name-string "vt//%s")

(add-hook 'vterm-mode-hook (lambda ()
                             (setq-local global-hl-line-mode
                                         (null global-hl-line-mode))))

(global-set-key (kbd "<C-return>") 'vterm-toggle-cd)
(global-set-key (kbd "<C-S-return>") 'vterm-toggle)

(straight-use-package 'vterm-toggle)
(setq vterm-toggle-hide-method 'delete-window)
(setq vterm-toggle-fullscreen-p nil)

(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                (display-buffer-reuse-window display-buffer-at-bottom)
                ;;(dedicated . t) ;dedicated is supported in emacs27
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

(straight-use-package 'org)
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/"))
(setq org-hide-emphasis-markers t)
(setq org-list-allow-alphabetical t)
(add-hook 'org-mode-hook (lambda ()
                           (org-indent-mode 1)
                           (electric-quote-mode -1)
                           (auto-fill-mode 1)))

(setq org-export-backends '(latex beamer md html odt ascii org-ref))

(setq org-edit-src-content-indentation 0)

(setq org-deadline-warning-days 2)

(straight-use-package 'org-fragtog)

(defun org-inside-latex-block ()
  (eq (nth 0 (org-element-at-point)) 'latex-environment))


(setq org-fragtog-ignore-predicates '(org-at-table-p org-inside-latex-block))

(straight-use-package 'org-ref)
(straight-use-package 'ivy-bibtex)
;;(require 'org-ref-ivy)

(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-src-preserve-indentation t)

(setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
      org-ref-insert-label-function 'org-ref-insert-label-link
      org-ref-insert-ref-function 'org-ref-insert-ref-link
      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
  (define-key org-mode-map (kbd "S-]") 'org-ref-insert-link-hydra/body))
  ; (define-key org-mode-map (kbd "C-c C-e") 'org-ref-export-from-hydra))

(setq bibtex-completion-bibliography '("~/docs/library.bib"))

(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
	    "bibtex %b"
	    "pdflatex -interaction nonstopmode -output-directory %o %f"
	    "pdflatex -interaction nonstopmode -output-directory %o %f"))

(straight-use-package 'org-roam)
(setq org-roam-v2-ack t)

(unless (file-directory-p "~/roam")
  (make-directory "~/roam"))

(setq org-roam-directory (file-truename "~/roam"))
;(org-roam-db-autosync-mode)

(defun anki-description-transform ()
  (interactive)
  (let* ((begin (re-search-backward "^-"))
         (end (forward-sentence))
         (raw (buffer-substring-no-properties
               begin
               end))
         (split (s-split "::" raw))
         (q (substring (s-trim (car split)) 2))
         (a (s-trim (cadr split)))
         (depth (org-current-level)))
    (yas-expand-snippet
     (yas-lookup-snippet "anki-editor card")
     begin end)
    (insert q)
    (yas-next-field-or-maybe-expand)
    (insert a)
    (yas-end)
    (org-backward-element)))

(setq erc-default-server "irc.libera.chat")
(when (file-exists-p "ircconfig.elc")
  (load (expand-file-name "ircconfig" user-emacs-directory)))

(straight-use-package 'yasnippet)
(yas-global-mode)
(setq yas-indent-line 'fixed)

(straight-use-package 'dired+)

(straight-use-package 'aurel)
(setq aurel-info-download-function 'aurel-download-unpack-pkgbuild)
(setq aurel-list-download-function 'aurel-download-unpack-pkgbuild)

(straight-use-package 'elfeed)
(setq elfeed-feeds
      '("https://sachachua.com/blog/feed/"
        "https://hnrss.org/frontpage"))

(setq browse-url-browser-function 'eww-browse-url)

(straight-use-package 'flycheck)

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

(straight-use-package 'magit)

(straight-use-package 'telega)

;; custom entry in tex--prettify-symbols-alist. FIXME.
(global-prettify-symbols-mode)

(straight-use-package 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 4)
			(setq tab-width 4)))

(straight-use-package 'haskell-mode)
(straight-use-package 'lsp-haskell)
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

(straight-use-package 'slime-company)
(slime-setup '(slime-fancy slime-company))

(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ)))
(add-hook 'elisp-mode 'prettify-symbols-mode)
(add-hook 'lisp-mode 'prettify-symbols-mode)
(add-hook 'clojure-mode 'prettify-symbols-mode)
(add-hook 'python-mode 'prettify-symbols-mode)

(straight-use-package 'smartparens)
(smartparens-global-mode)

(defun sp-disable (mode str)
  (sp-local-pair mode str nil :actions nil))

(straight-use-package 'elisp-format)
(setq elisp-format-column 80)
(sp-disable 'emacs-lisp-mode "'")
(sp-disable 'emacs-lisp-mode "`")
(sp-disable 'org-mode "'")

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

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

(straight-use-package 'sage-shell-mode)
(setq sage-shell:sage-executable "/usr/bin/sage")

(straight-use-package 'janet-mode)
(straight-use-package
 '(ijanet
   :type git
   :host github
   :repo "serialdev/ijanet-mode"
))
(defun janet-key-config ()
    (interactive)
    (define-key janet-mode-map (kbd "C-c C-l") 'ijanet-eval-line)
    (define-key janet-mode-map (kbd "C-c C-p") 'ijanet)
    (define-key janet-mode-map (kbd "C-c C-b") 'ijanet-eval-buffer)
    (define-key janet-mode-map (kbd "C-c C-r") 'ijanet-eval-region))

(sp-disable 'janet-mode "'")

(straight-use-package
  '(janet-editor-elf :host github
                     :repo "sogaiu/janet-editor-elf"
                     :files ("*.el" "janet-editor-elf")))

(use-package janet-editor-elf
  :straight t
  :config
  (add-hook 'janet-mode-hook
            (lambda ()
              (setq-local indent-line-function
                          #'jee-indent-line))))

(straight-use-package
  '(ajrepl :host github
           :repo "sogaiu/ajrepl"
           :files ("*.el" "ajrepl")))

(use-package ajrepl
  :straight t
  :config
  (add-hook 'janet-mode-hook
            #'ajrepl-interaction-mode))

(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
(sp-disable 'clojure-mode "'")

(straight-use-package 'hy-mode)
(sp-disable 'hy-mode "'")

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

(global-set-key (kbd "C-\;") 'ace-window)

(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

(global-set-key (kbd "C-h C-f") (lambda ()
                                  (interactive)
                                  (if (> (count-windows) 1)
                                      (xref-find-definitions-other-window
                                       (thing-at-point 'symbol t))
                                    (xref-find-definitions
                                     (thing-at-point 'symbol t)))))

(global-set-key (kbd "C-h C-j") 'xref-pop-marker-stack)

(defun load-init ()
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(setq-default indent-tabs-mode nil)

(setq mode-require-final-newline nil)

(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)

(straight-use-package 'aggressive-indent-mode)
(global-aggressive-indent-mode 1)


(add-to-list
 'aggressive-indent-dont-indent-if
 '(and (derived-mode-p 'c++-mode)
       (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                           (thing-at-point 'line)))))

(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message "スクラッチ")
