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

(global-set-key [?\C-h] 'delete-backward-char)
(global-set-key [?\C-x ?h] 'help-command)

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
(load-theme 'gruvbox-dark-hard t nil)
(setq solarized-high-contrast-mode-line t)
(setq solarized-use-less-bold t)
(setq solarized-use-more-italic t)
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)
(setq x-underline-at-descent-line t)

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

(sup 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
; (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)

(defun pixel-scroll-setup ()
  (interactive)
  (setq pixel-scroll-precision-large-scroll-height 30.0)
  (setq pixel-scroll-precision-interpolation-factor 20))

(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-setup)
  (add-hook 'prog-mode-hook #'pixel-scroll-precision-mode)
  (add-hook 'org-mode-hook #'pixel-scroll-precision-mode))

  (sup 's)
  (sup 'dash)

(sup 'neotree)
(setq neo-theme (if (display-graphic-p) 'ascii))

(sup '(nyaatouch
       :repo "https://github.com/eshrh/nyaatouch"
       :fetcher github))
(require 'nyaatouch)
(turn-on-nyaatouch)
(meow-normal-define-key '("r" . meow-delete))
(setq  x-meta-keysym 'super
       x-super-keysym 'meta)

(meow-leader-define-key
 '("d" . vterm-toggle-cd))

(sup 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history nil)

(sup 'ace-window)
(global-set-key [remap other-window] 'ace-window)

(setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
(setq aw-scope 'frame)
(setq aw-background nil)
(setq aw-ignore-current t)

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

(defun find-file-or-projectile ()
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'projectile-find-file)
    (call-interactively 'find-file)))

(global-set-key (kbd "C-x C-f") 'find-file-or-projectile)
(meow-leader-define-key
 '("U" . find-file))

(sup 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(sup 'prescient)
(sup 'ivy-prescient)
(ivy-prescient-mode)

(sup 'marginalia)
(marginalia-mode)

(sup 'posframe)
(sup 'ivy-posframe)
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
(setq ivy-posframe-display-functions-alist
      '((swiper          . ivy-display-function-fallback)
        (org-ref-insert-link . ivy-display-function-fallback)
        (t               . ivy-posframe-display)))

(ivy-posframe-mode 1)

(sup 'helpful)

(global-set-key (kbd "C-x h C-f") #'helpful-callable)
(global-set-key (kbd "C-x h C-v") #'helpful-variable)
(global-set-key (kbd "C-x h C-k") #'helpful-key)
(global-set-key (kbd "C-x h f") #'helpful-callable)
(global-set-key (kbd "C-x h v") #'helpful-variable)
(global-set-key (kbd "C-x h k") #'helpful-key)

(sup 'anki-editor)
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
  (when (featurep 'vterm)
    (define-key vterm-mode-map (kbd "C-h") #'vterm-send-backspace))

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
(sup 'ox-pandoc)
(when (file-exists-p "~/org/")
  (setq org-directory "~/org/")
  (setq org-agenda-files '("~/org/")))
(setq org-hide-emphasis-markers t)
(setq org-list-allow-alphabetical t)
(add-hook 'org-mode-hook (lambda ()
                           (org-indent-mode 1)
                           (electric-quote-mode -1)
                           (auto-fill-mode 1)))

(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(setq org-export-backends '(latex beamer md html odt ascii org-ref pandoc))

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
(global-set-key (kbd "C-c c i") #'org-roam-node-insert)
(global-set-key (kbd "C-c c f") #'org-roam-node-find)
(global-set-key (kbd "C-c c s") #'org-roam-db-sync)
(global-set-key (kbd "C-c c p") (lambda () (interactive)
                                  (load-file "~/roam/publish.el")))
(setq org-return-follows-link t)

(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :target
         (file+head "${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)))

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

(sup 'magit)

(setq ediff-diff-options "")
(setq ediff-custom-diff-options "-u")
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-vertically)

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

(add-hook 'eaf-mode-hook (lambda () (interactive)
			   (define-key eaf-mode-map (kbd "SPC") 'meow-keypad)))

;; custom entry in tex--prettify-symbols-alist. FIXME.
;; (global-prettify-symbols-mode)

(sup 'lsp-mode)
(sup 'lsp-ui)

(sup 'lsp-haskell)

(setq lsp-auto-guess-root t)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-lens-enable nil)
(setq lsp-headerline-breadcrumb-enable nil)

(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'haskell-mode-hook #'lsp-deferred)
(add-hook 'c-mode-hook #'lsp-deferred)

(sup 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 4)
			(setq tab-width 4))
          (lambda ()
            (add-hook 'before-save-hook #'delete-trailing-whitespace)))



(sup 'haskell-mode)
(add-hook 'haskell-mode-hook #'interactive-haskell-mode)

(setq haskell-interactive-popup-errors t)

(setq-default tab-width 4
              c-basic-offset 4
              kill-whole-line t
              indent-tabs-mode nil)

(add-hook 'lisp-mode-hook 'flycheck-mode)
(sup 'slime)
(setq inferior-lisp-program "sbcl")

(sup 'slime-company)
(add-hook 'common-lisp-mode-hook (lambda ()
                              (slime-setup '(slime-fancy slime-company))))

(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ)))
(add-hook 'elisp-mode-hook 'prettify-symbols-mode)
(add-hook 'lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)
(add-hook 'python-mode-hook 'prettify-symbols-mode)

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

(add-hook 'tex-mode-hook (lambda () (interactive) 
                           (setq TeX-view-program-selection
                                 '((output-pdf "sioyek")))
                           (prettify-symbols-mode)
                           (auto-fill-mode 1)))

(add-hook 'LaTeX-mode-hook #'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

(sup 'sage-shell-mode)
(setq sage-shell:sage-executable "/usr/bin/sage")

(sup 'clojure-mode)
(sup 'cider)
(sp-disable 'clojure-mode "'")

(defun my-asm-mode-hook ()
  ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
  
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
  (setq tab-always-indent (default-value 'tab-always-indent)))

(add-hook 'asm-mode-hook
          (lambda ()
            (local-unset-key (vector asm-comment-char))
            (setq tab-always-indent (default-value 'tab-always-indent))))

(when (file-exists-p (concat user-emacs-directory "kbd-mode.el"))
  (load-file "~/.emacs.d/kbd-mode.el")
  (add-hook 'kbd-mode-hook (lambda () (aggressive-indent-mode -1))))

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
(add-hook 'lisp-data-mode-hook (lambda () (interactive) (aggressive-indent-mode 1)))

(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message "スクラッチ")

(global-set-key (kbd "<f19>") (lambda () (interactive)
                                (call-interactively 'execute-extended-command)))

(setq use-dialog-box nil)

(define-key key-translation-map [?\C-x] [?\C-u])
(define-key key-translation-map [?\C-u] [?\C-x])

(when (executable-find "rg")
  (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))
