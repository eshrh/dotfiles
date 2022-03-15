(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

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

(defalias 'sup 'straight-use-package)

(setq user-full-name "Eshan Ramesh"
      user-mail-address "esrh@gatech.edu")

(defalias 'yes-or-no-p 'y-or-n-p)

(setq vc-follow-symlinks nil)

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
(rainbow-mode)

(global-hl-line-mode)

(add-hook 'prog-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

(defvar emacs-english-font "Iosevka Hane Sans")
(defvar emacs-cjk-font "IPAGothic")

(setq my-font (concat emacs-english-font "-12"))

(add-to-list 'default-frame-alist `(font . ,my-font))
(set-face-attribute 'default t :font my-font)
;; (set-face-attribute 'default t :font my-font)
;; (set-frame-font my-font nil t)

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

(sup 'gruvbox-theme)
(if (or (display-graphic-p) (daemonp))
    (load-theme 'gruvbox-dark-hard t nil)
    (load-theme 'tsdh-dark t nil))

(setq-default frame-title-format '("emacs: %b"))

(sup 'highlight-defined)
(sup 'highlight-numbers)
(sup 'rainbow-delimiters)
(sup 'highlight-quoted)
(defun highlight-lisp-things-generic ()
  (highlight-numbers-mode)
  (highlight-defined-mode)
  (rainbow-delimiters-mode))

(defun highlight-lisp-things ()
  (highlight-lisp-things-generic)
  (highlight-quoted-mode))

(add-hook 'emacs-lisp-mode-hook #'highlight-lisp-things)
(add-hook 'lisp-data-mode-hook #'highlight-lisp-things-generic)
(add-hook 'clojure-mode-hook #'highlight-lisp-things-generic)

(sup 'telephone-line)

(require 'telephone-line)
(setq telephone-line-primary-left-separator 'telephone-line-cubed-left
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
      telephone-line-primary-right-separator 'telephone-line-cubed-right
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
(setq telephone-line-height 24
      telephone-line-evil-use-short-tag t)

;; patch submitted, waiting on upstream
(telephone-line-defsegment* telephone-line-simpler-major-mode-segment ()
  (concat "["
          (if (listp mode-name)
              (car mode-name)
            mode-name) "]"))

(telephone-line-defsegment* telephone-line-simple-pos-segment ()
  (concat "%c : " "%l/" (number-to-string (count-lines (point-min) (point-max))) ))

(count-lines (point-min) (point-max))

(setq telephone-line-evil-use-short-tag nil)

(setq telephone-line-lhs
      '((nil . (telephone-line-projectile-buffer-segment))
        (accent . (telephone-line-simpler-major-mode-segment))
        (nil . (telephone-line-meow-tag-segment
                telephone-line-misc-info-segment)))
      telephone-line-rhs
      '((nil . (telephone-line-simple-pos-segment))
        (accent . (telephone-line-buffer-modified-segment))))

(telephone-line-mode 1)

(sup 'ivy-posframe)
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))

(setq ivy-posframe-display-functions-alist
      '((swiper          . ivy-display-function-fallback)
        (org-ref-insert-link . ivy-display-function-fallback)
        (t               . ivy-posframe-display)))

(ivy-posframe-mode 1)

(sup 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
; (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)

(defun pixel-scroll-setup ()
  (interactive)
  (setq pixel-scroll-precision-large-scroll-height 40.0)
  (setq pixel-scroll-precision-interpolation-factor 10))

(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-setup)
  (add-hook 'prog-mode-hook #'pixel-scroll-precision-mode)
  (add-hook 'org-mode-hook #'pixel-scroll-precision-mode))

  (sup 's)
  (sup 'dash)

(defun meow-insert-right ()
  (interactive)
  (meow-right)
  (meow-insert))

(defun meow-negative-find ()
  (interactive)
  (let ((current-prefix-arg -1))
    (call-interactively 'meow-find)))

(put 'upcase-region 'disabled nil)

(defun uppercasep (c) (and (= ?w (char-syntax c)) (= c (upcase c))))

(defun downcase-char ()
  (interactive)
  (save-excursion
    (let ((ch (thing-at-point 'char t)))
      (delete-char 1)
      (insert (downcase ch)))))

(defun toggle-case-dwiam ()
  "toggle cases, do what i actually mean:

If no region is active, toggle between upcase and downcase on the
current character. If a region is active, then if there exists at
least one upcase char in the region, then downcase the whole
region. Otherwise, upcase the whole region."
  (interactive)
  (if (region-active-p)
      (let ((region (buffer-substring-no-properties
                     (region-beginning) (region-end))))
        (message "%s" region)
        (if (cl-remove-if-not #'uppercasep (string-to-list region))
            (downcase-region (region-beginning) (region-end))
          (upcase-region (region-beginning) (region-end))))
    (if (uppercasep (string-to-char (thing-at-point 'char t)))
        (downcase-char)
      (upcase-char 1))))

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

(defun sp-goto-top ()
  (interactive)
  (let ((res (sp-up-sexp)))
    (while res
      (setq res (sp-up-sexp)))))

(sup 'meow)
(require 'meow)

(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
(meow-motion-overwrite-define-key
 '("j" . meow-next)
 '("k" . meow-prev)
 '("<escape>" . ignore))


;; expansion
(meow-normal-define-key
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
 '(";" . meow-reverse))

;; movement
(meow-normal-define-key
 '("," . meow-inner-of-thing)
 '("." . meow-bounds-of-thing)
 '("[" . meow-beginning-of-thing)
 '("]" . meow-end-of-thing)
 '("a" . meow-append)
 '("b" . meow-back-word)
 '("B" . meow-back-symbol)
 '("e" . meow-next-word)
 '("E" . meow-next-symbol)
 '("f" . meow-find)
 '("F" . meow-negative-find)
 '("h" . meow-left)
 '("H" . meow-left-expand)
 '("t" . meow-next)
 '("T" . meow-next-expand)
 '("n" . meow-prev)
 '("N" . meow-prev-expand)
 '("s" . meow-right)
 '("S" . meow-right-expand)
 '("Q" . meow-goto-line)
 '("t" . meow-till)
 '("b" . (lambda () (interactive) (meow-bounds-of-thing 'round))))

;; extras
(meow-normal-define-key
 '("*" . toggle-case-dwiam)
 '("+" . add-number)
 '("_" . subtract-one)
 '("/" . avy-goto-word-1)
 '("v" . swiper))

;; actions
(meow-normal-define-key
 '("c" . meow-change)
 '("d" . meow-delete)
 '("g" . meow-cancel-selection)
 '("G" . meow-grab)
 '("i" . meow-insert)
 '("/" . avy-goto-word-1)
 '("m" . meow-join)
 '("o" . meow-open-below)
 '("O" . meow-open-above)
 '("p" . meow-yank)
 '("q" . meow-quit)
 '("r" . meow-replace)
 '("R" . meow-swap-grab)
 '("s" . meow-kill)
 '("g" . meow-undo)
 '("G" . meow-undo-in-selection)
 '("a" . meow-mark-word)
 '("A" . meow-mark-symbol)
 '("e" . meow-line)
 '("y" . meow-save)
 '("Y" . meow-sync-grab)
 '("z" . meow-pop-selection)
 '("r" . repeat)
 '("<escape>" . ignore))

(require 'meow)

(setq meow-paren-keymap (make-keymap))
(meow-define-state paren
  "paren state"
  :lighter " [P]"
  :keymap meow-paren-keymap)

(meow-normal-define-key
 '("Z" . meow-paren-mode))

(setq meow-cursor-type-paren 'hollow)

(meow-define-keys 'paren
  '("<escape>" . meow-normal-mode)
  '("h" . sp-backward-sexp)
  '("l" . sp-forward-sexp)
  '("j" . sp-down-sexp)
  '("k" . sp-up-sexp)
  '("w s" . sp-wrap-square)
  '("w r" . sp-wrap-round)
  '("w c" . sp-wrap-curly)
  '("w g" . (lambda () (interactive) (sp-wrap-with-pair "\"")))
  '("W" . sp-unwrap-sexp)
  '("n" . sp-slurp-hybrid-sexp)
  '("b" . sp-forward-barf-sexp)
  '("v" . sp-backward-barf-sexp)
  '("c" . sp-backward-slurp-sexp)
  '("r" . sp-raise-sexp)
  '("q" . sp-absorb-sexp)
  '("," . sp-split-sexp)
  '("s" . sp-splice-sexp-killing-forward)
  '("S" . sp-splice-sexp-killing-backward)
  '("e" . sp-end-of-sexp)
  '("a" . sp-beginning-of-sexp)
  '("G" . sp-goto-top)
  '("L" . sp-transpose-sexp)
  '("H" . (lambda () (interactive) (sp-transpose-sexp -1)))
  '("u" . meow-undo))

(meow-leader-define-key
   '("a" . "M-x")
   '("f" . "C-x C-f")
   '("j" . "H-j")
   '("k" . "H-k")
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

(setq latex-thing-regexp
      '(regexp "\\\\begin{.*?}\\(.*?\\)\n\\|\\$"
               "\\\\end{.*?}\n\\|\\$"))

(meow-thing-register 'latex
		             latex-thing-regexp
                   latex-thing-regexp)

(add-to-list 'meow-char-thing-table
	         (cons ?x 'latex))

(setq meow-use-clipboard t)

(defun meow-clipboard-toggle ()
  (interactive)
  (if meow-use-clipboard
      (progn
        (setq meow-use-clipboard nil)
        (message "Meow clipboard usage disabled"))
    (progn
      (setq meow-use-clipboard t)
      (message "Meow clipboard usage enabled"))))

(meow-leader-define-key '("t" . meow-clipboard-toggle))

(meow-normal-define-key '(":" . (lambda () (interactive) (call-interactively 'execute-extended-command))))

(setq meow-expand-exclude-mode-list '())

(meow-global-mode 1)

(sup 'undo-tree)
(global-undo-tree-mode)

(sup 'ace-window)
(global-set-key [remap other-window] 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-scope 'frame)
(setq aw-background nil)
(setq aw-ignore-current t)

(sup 'avy)

(sup 'dashboard)
(dashboard-setup-startup-hook)

(setq initial-buffer-choice (get-buffer "*dashboard*"))
(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)
(setq dashboard-set-init-info nil)
(setq dashboard-set-footer nil)

(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 5)))

(setq dashboard-agenda-sort-strategy '(time-up))

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

(add-to-list 'recentf-exclude
             (concat (getenv "HOME") "/org"))

(sup 'company)
(add-hook 'after-init-hook 'global-company-mode)
(sup 'company-ctags)

(sup 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(sup 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(sup 'prescient)
(sup 'ivy-prescient)
(ivy-prescient-mode)

(sup 'swiper)

(sup 'marginalia)
(marginalia-mode)

(sup 'helpful)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(sup 'anki-editor)
(sup 'posframe)
(sup '(sdcv2 :type git
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
(sup 'clipmon)

  (sup 'migemo)
  (sup 'ivy-migemo)
  (sup 's)

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
    (sup '(mecab :type git
                                  :repo "https://github.com/syohex/emacs-mecab"
                                  :pre-build ("make")
                                  :files ("mecab-core.so"
                                          "mecab-core.o"
                                          "mecab-core.c"
                                          "mecab.el"))))

(sup 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq nov-text-width 100)

(sup 'emms)
(require 'emms-setup)
(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-playlist-mode)
(require 'emms-browser)
(require 'emms-info)
(require 'emms-info-native)
(setq emms-playlist-default-major-mode #'emms-playlist-mode)
(add-to-list 'emms-track-initialize-functions #'emms-info-initialize-track)
(setq emms-info-functions '(emms-info-native))
(setq emms-track-description-function #'emms-info-track-description)
(add-hook 'emms-browser-mode-hook (lambda () (when (fboundp 'emms-cache)
                                               (emms-cache 1))))

(define-key emms-browser-mode-map (kbd "<tab>") 'emms-browser-toggle-subitems)

(defun emms-info-mpd-process-with-aa (track info)
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

(defun emms-mpd-setup ()
  (require 'emms-player-mpd)
  (setq emms-player-list '(emms-player-mpd))
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (setq emms-player-mpd-music-directory "~/mus")
  (advice-add 'emms-info-mpd-process :override 'emms-info-mpd-process-with-aa)
  (emms-player-mpd-connect))

(add-hook 'emms-browser-mode-hook 'emms-mpd-setup)
(add-hook 'emms-playlist-cleared-hook 'emms-player-mpd-clear)

(sup 'hl-todo)
(global-hl-todo-mode)

(sup 'highlight-indent-guides)

(sup 'which-key)
(which-key-mode)

(sup 'format-all)

(sup 'vterm)
(sup 'fish-mode)

(setq vterm-kill-buffer-on-exit t)
(setq vterm-buffer-name-string "vt//%s")

(add-hook 'vterm-mode-hook (lambda ()
                             (setq-local global-hl-line-mode
                                         (null global-hl-line-mode))))

(global-set-key (kbd "<C-return>") 'vterm-toggle-cd)
(global-set-key (kbd "<C-S-return>") 'vterm-toggle)

(sup 'vterm-toggle)
(setq vterm-toggle-hide-method 'delete-window)
(setq vterm-toggle-fullscreen-p nil)

(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
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
(add-hook 'vterm-mode-hook
          (lambda ()
            (set-process-sentinel (get-buffer-process (buffer-name))
                                  #'vterm--kill-vterm-buffer-and-window)))

(sup 'org)
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

(sup 'org-fragtog)

(defun org-inside-latex-block ()
  (eq (nth 0 (org-element-at-point)) 'latex-environment))


(setq org-fragtog-ignore-predicates '(org-at-table-p org-inside-latex-block))

(sup 'org-ref)
(sup 'ivy-bibtex)
(require 'org-ref-ivy)

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

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(define-key org-mode-map (kbd "C-c r") 'org-ref-citation-hydra/body)

(sup 'org-roam)
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

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("IEEEtran"
                 "\\documentclass{IEEEtran}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(setq erc-default-server "irc.libera.chat")
(add-hook 'erc-before-connect (lambda ()
                                (when (file-exists-p "ircconfig.elc")
                                  (load
                                   (expand-file-name
                                    "ircconfig"
                                    user-emacs-directory)))))

(sup 'yasnippet)
(yas-global-mode)
(setq yas-indent-line 'fixed)

(sup 'dired+)

(sup 'elfeed)
(setq elfeed-feeds
      '("https://sachachua.com/blog/feed/"
        "https://hnrss.org/frontpage"))

(setq browse-url-browser-function 'browse-url-firefox)

(sup 'flycheck)

(sup 'company-lsp)
(sup 'lsp-mode)
(sup 'lsp-ui)

(setq lsp-ui-doc-show-with-mouse nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-modeline-code-actions-enable 1)

(add-hook 'lsp-mode-hook (lambda ()
			   (local-set-key (kbd "C-c C-j") 'lsp-execute-code-action)))

(sup 'magit)

(sup 'telega)

;; custom entry in tex--prettify-symbols-alist. FIXME.
(global-prettify-symbols-mode)

(when (file-directory-p (concat
                         user-emacs-directory
                         "site-lisp/emacs-application-framework/"))
  (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
  (require 'eaf)
  (require 'eaf-pdf-viewer)
  (require 'eaf-org-previewer)
  (require 'eaf-browser)
  (require 'eaf-image-viewer)
  (require 'eaf-terminal))

(sup 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 4)
			(setq tab-width 4)))

(sup 'haskell-mode)
(sup 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(setq haskell-interactive-popup-errors t)

(setq-default tab-width 4
              c-basic-offset 4
              kill-whole-line t
              indent-tabs-mode nil)

(add-hook 'lisp-mode-hook 'flycheck-mode)
(sup 'slime)
(setq inferior-lisp-program "sbcl")

(sup 'slime-company)
(add-hook 'common-lisp-mode (lambda ()
                              (slime-setup '(slime-fancy slime-company))))

(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ)))
(add-hook 'elisp-mode 'prettify-symbols-mode)
(add-hook 'lisp-mode 'prettify-symbols-mode)
(add-hook 'clojure-mode 'prettify-symbols-mode)
(add-hook 'python-mode 'prettify-symbols-mode)

(sup 'smartparens)
(smartparens-global-mode)

(defun sp-disable (mode str)
  (sp-local-pair mode str nil :actions nil))

(sp-disable 'lisp-data-mode "'")

(sup 'elisp-format)
(setq elisp-format-column 80)
(sp-disable 'emacs-lisp-mode "'")
(sp-disable 'emacs-lisp-mode "`")
(sp-disable 'org-mode "'")

(sup 'auctex)

(add-hook 'tex-mode (lambda () (interactive) 
                      (add-to-list 'TeX-view-program-list
                                   '("Evince" "evince --page-index=%(outpage) %o"))
                      (setq TeX-view-program-selection
                            '((output-pdf "Evince")))))

(add-hook 'tex-mode #'lsp)
(add-hook 'tex-mode (lambda ()
					  (setq lsp-lens-enable nil)))

(sup 'lsp-jedi)
(add-hook 'python-mode #'lsp)

(sup 'polymode)
(sup 'ein)
(setq ein:polymode t)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

(sup 'sage-shell-mode)
(setq sage-shell:sage-executable "/usr/bin/sage")

(sup 'clojure-mode)
(sup 'cider)
(sp-disable 'clojure-mode "'")

(sup 'hy-mode)
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

(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)

(sup 'aggressive-indent-mode)
(global-aggressive-indent-mode 1)


(add-to-list
 'aggressive-indent-dont-indent-if
 '(and (derived-mode-p 'c++-mode)
       (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                           (thing-at-point 'line))))
 '(bound-and-true-p 'java-mode))

(add-hook 'java-mode-hook (lambda () (interactive) (aggressive-indent-mode -1)))

(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message "スクラッチ")

(global-set-key (kbd "<f19>") (lambda () (interactive)
                                (call-interactively 'execute-extended-command)))

(setq use-dialog-box nil)

(define-key key-translation-map [?\C-x] [?\C-u])
(define-key key-translation-map [?\C-u] [?\C-x])
