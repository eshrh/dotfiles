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

(defvar emacs-english-font "Iosevka Meiseki Sans")
(defvar emacs-cjk-font "IPAGothic")
(setq my-font (concat emacs-english-font "-12"))

(add-to-list 'default-frame-alist `(font . ,my-font))
(set-face-attribute 'default t :font my-font)

(sup 'gruvbox-theme)
(load-theme 'gruvbox-dark-hard t nil)

(setq-default frame-title-format '("emacs: %b"))

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

(defun pixel-scroll-setup ()
  (interactive)
  (setq pixel-scroll-precision-large-scroll-height 30.0)
  (setq pixel-scroll-precision-interpolation-factor 30))

(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-setup)
  (add-hook 'prog-mode-hook #'pixel-scroll-precision-mode)
  (add-hook 'org-mode-hook #'pixel-scroll-precision-mode))

(sup '(nyaatouch
       :repo "https://github.com/eshrh/nyaatouch"
       :fetcher github))
(require 'nyaatouch)
(turn-on-nyaatouch)

(meow-leader-define-key
 '("d" . vterm-toggle-cd))

(meow-normal-define-key '("r" . meow-delete))

(meow-normal-define-key
 '("`" . fill-paragraph))

(unless (display-graphic-p)
  (setq meow-esc-delay 0))

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

(setq initial-buffer-choice (get-buffer "*dashboard*"))

(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)
(setq dashboard-set-init-info nil) ;; don't show me that sad stuff...
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
(if (or (display-graphic-p) (daemonp))
    (set-dashboard-banner "hiten_render_rsz.png")
  (set-dashboard-banner "gnu.txt"))

(add-to-list 'recentf-exclude
             (concat (getenv "HOME") "/org"))

(sup 'company)
(add-hook 'after-init-hook #'global-company-mode)
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

;; TODO Add yomenai.el code here.
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
(add-fs-to-hook 'emms-browser-mode-hook (when (fboundp 'emms-cache)
                                          (emms-cache 1)))

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

(add-hook 'emms-browser-mode-hook #'emms-mpd-setup)
(add-hook 'emms-playlist-cleared-hook #'emms-player-mpd-clear)

(sup 'highlight-defined)
(sup 'highlight-numbers)
(sup 'rainbow-delimiters)
(sup 'highlight-quoted)
(defun highlight-lisp-things-generic ()
  (highlight-numbers-mode)
  (highlight-defined-mode)
  (rainbow-delimiters-mode))

(add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
(add-to-hooks #'highlight-lisp-things-generic 'lisp-data-mode-hook 'clojure-mode-hook)

(sup 'hl-todo)
(global-hl-todo-mode)

(sup 'which-key)
(which-key-mode)

(sup 'format-all)

(sup 'vterm)
(sup 'fish-mode)

(add-fs-to-hook 'vterm-mode-hook (setq-local global-hl-line-mode
                                             (null global-hl-line-mode)))

(setq vterm-kill-buffer-on-exit t)
(setq vterm-buffer-name-string "vt//%s")

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

(sup 'org)

(when (file-exists-p "~/org/")
  (setq org-directory "~/org/")
  (setq org-agenda-files '("~/org/")))

(setq org-list-allow-alphabetical t)

(add-fs-to-hook 'org-mode-hook
                org-indent-mode
                (electric-quote-mode -1)
                auto-fill-mode)

(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(sup 'ox-pandoc)
(setq org-export-backends '(latex beamer md html odt ascii org-ref pandoc))

(setq org-edit-src-content-indentation 0)

(setq org-deadline-warning-days 2)

(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-src-preserve-indentation t)

(sup 'org-fragtog)

(defun org-inside-latex-block ()
  (eq (nth 0 (org-element-at-point)) 'latex-environment))
(setq org-fragtog-ignore-predicates '(org-at-table-p org-inside-latex-block))

(sup 'org-ref)
(sup 'ivy-bibtex)
(require 'org-ref-ivy) ; ivy integration

(setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
      org-ref-insert-label-function 'org-ref-insert-label-link
      org-ref-insert-ref-function 'org-ref-insert-ref-link
      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
  (define-key org-mode-map (kbd "S-]") 'org-ref-insert-link-hydra/body)
  (define-key org-mode-map (kbd "C-c r") 'org-ref-citation-hydra/body))
(setq bibtex-completion-bibliography '("~/docs/library.bib"))
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(sup 'org-roam)
(setq org-roam-v2-ack t)

(unless (file-directory-p "~/roam")
  (make-directory "~/roam"))
(setq org-roam-directory (file-truename "~/roam"))

(setq org-return-follows-link t)

(global-set-key (kbd "C-c c i") #'org-roam-node-insert)
(global-set-key (kbd "C-c c f") #'org-roam-node-find)
(global-set-key (kbd "C-c c s") #'org-roam-db-sync)
(global-set-key (kbd "C-c c p") (fn (interactive) (load-file "~/roam/publish.el")))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :target
         (file+head "${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)))

(sup 'anki-editor)
;; TODO improve this code!!!
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

(add-hook 'erc-before-connect (lambda (SERVER PORT NICK)
                                (when (file-exists-p "ircconfig.elc")
                                  (load
                                   (expand-file-name
                                    "ircconfig"
                                    user-emacs-directory)))))

(sup 'yasnippet)
(yas-global-mode)
(setq yas-indent-line 'fixed)

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
  (require 'eaf-org-previewer)
  (require 'eaf-browser)
  (require 'eaf-image-viewer))

(add-fs-to-hook 'eaf-mode-hook (define-key eaf-mode-map (kbd "SPC") 'meow-keypad))

(global-prettify-symbols-mode)
(add-fs-to-hook 'emacs-lisp-mode-hook
                (push '("fn" . ?∅) prettify-symbols-alist))

(sup '(ligature
       :type git
       :repo "https://github.com/mickeynp/ligature.el"))
(ligature-set-ligatures
 'prog-mode
 '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
   ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
   "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "-<<"
   "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
   "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
   "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
   "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
   "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
   ">=" ">>" ">-" "-~" "-|" "->" "-<" "<~" "<*" "<|" "<:"
   "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
   "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
   "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
   "\\\\" "://"))
(global-ligature-mode)

(sup 'lsp-mode)
(sup 'lsp-ui)

(sup 'lsp-haskell)

(setq lsp-auto-guess-root t)

(setq lsp-enable-symbol-highlighting nil)
(setq lsp-lens-enable nil)
(setq lsp-headerline-breadcrumb-enable nil)

(add-to-hooks #'lsp-deferred 'python-mode-hook 'haskell-mode-hook 'c-mode-hook)

(sup 'meghanada)
(add-fs-to-hook 'java-mode-hook
                meghanada-mode
                flycheck-mode
                (setq c-basic-offset 4)
                (setq tab-width 4))

(sup 'haskell-mode)
(add-hook 'haskell-mode-hook #'interactive-haskell-mode)

(setq haskell-interactive-popup-errors t)

(setq-default
              c-basic-offset 4
              kill-whole-line t
              indent-tabs-mode nil)

(sup 'slime)
(setq inferior-lisp-program "sbcl")
(sup 'slime-company)
(add-fs-to-hook 'common-lisp-mode-hook (slime-setup '(slime-fancy slime-company)))
(add-hook 'lisp-mode-hook #'flycheck-mode)

(smartparens-global-mode)

(defun sp-disable (mode str)
  (sp-local-pair mode str nil :actions nil))

(sp-disable 'lisp-data-mode "'")

(sup 'elisp-format)
(setq elisp-format-column 80)
(sp-disable 'emacs-lisp-mode "'")
(sp-disable 'emacs-lisp-mode "`")
(sp-disable 'org-mode "'")

(sup 'aggressive-indent-mode)
(add-hook 'lisp-data-mode-hook #'aggressive-indent-mode 1)

(sup 'auctex)

(setq my-pdf-viewer (-first #'executable-find
                            '("sioyek" "evince" "okular" "zathura" "firefox")))

(setq TeX-view-program-list nil)
(add-to-list
 'TeX-view-program-list
 `("sioyek" ("sioyek %o" (mode-io-correlate
                          ,(concat
                            " --reuse-instance"
                            " --forward-search-file \"%b\""
                            " --forward-search-line %n"
                            " --inverse-search \"emacsclient +%2 %1\"")))
   "sioyek"))

(add-fs-to-hook 'LaTeX-mode-hook
                (setq TeX-view-program-selection
                      `((output-pdf ,my-pdf-viewer)
                        (output-dvi ,my-pdf-viewer)
                        (output-html "xdg-open")))
                auto-fill-mode)

(add-hook 'LaTeX-mode-hook #'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(sup 'outline-magic)
(add-hook 'tex-mode #'outline-minor-mode)
(define-key outline-minor-mode-map (kbd "<tab>") 'outline-cycle)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

(sup 'sage-shell-mode)
(setq sage-shell:sage-executable "/usr/bin/sage")

(sup 'clojure-mode)
(sup 'cider)
(sp-disable 'clojure-mode "'")

(defun my-asm-mode-hook ()
  (setq tab-always-indent (default-value 'tab-always-indent)))

(add-fs-to-hook 'asm-mode-hook
                (local-unset-key (vector asm-comment-char))
                (setq tab-always-indent (default-value 'tab-always-indent)))

(when (file-exists-p (concat user-emacs-directory "kbd-mode.el"))
  (load-file "~/.emacs.d/kbd-mode.el")
  (add-hook 'kbd-mode-hook (fn (aggressive-indent-mode -1))))

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

(sup 'aggressive-indent-mode)
(add-hook 'lisp-data-mode-hook #'aggressive-indent-mode)

(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message "")

(setq use-dialog-box nil)

(define-key key-translation-map [?\C-x] [?\C-u])
(define-key key-translation-map [?\C-u] [?\C-x])

(when (executable-find "rg")
  (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))
