(defun edit-config ()
  "Edit config.org"
  (interactive)
  (find-file (expand-file-name "config.org" user-emacs-directory)))

(defun reload-config ()
  "Compile config.org and execute config.el"
  (interactive)
  (require 'org)
  (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
  (byte-compile-file (expand-file-name "config.el" user-emacs-directory)))

(setq gc-cons-threshold 64000000)

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 100000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

(setq custom-file (expand-file-name ".custom" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(setq package-enable-at-startup nil)

(when (not package-archive-contents)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (message "`use-package' not found.  Installing...")
  (package-install 'use-package))

(require 'use-package)
(setq use-package-minimum-reported-time 0
      use-package-verbose t
      use-package-compute-statistics t)

(setq use-package-always-ensure t)

(add-hook 'focus-out-hook #'garbage-collect)

(setq w32-get-true-file-attributes nil)
(setq read-process-output-max (* 1024 1024))

(use-package esup :commands esup)

(setq inhibit-startup-screen t)

(global-visual-line-mode 1)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(setq-default indent-tabs-mode nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default left-fringe-width 1)
(setq-default right-fringe-width 1)

(use-package doom-themes
  :custom (custom-enabled-themes '(doom-dracula))
  :config
  ;; Global settings (defaults)
  (setq-default doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)
  )

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

(set-face-attribute 'default nil :family "Cascadia Code" :height 110)
(set-face-attribute 'fixed-pitch nil :family "Cascadia Code" :height 110)
(set-face-attribute 'variable-pitch nil :family "Noto Sans" :height 120)
(set-face-attribute 'mode-line nil :family "Noto Sans" :height 120)

(set-fontset-font t 'unicode "Noto Emoji" nil 'prepend)

(global-hl-line-mode 1)

(setq-default display-line-numbers-type t
              display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package minions
  :config (minions-mode 1))

(use-package all-the-icons)

(setq-default find-file-visit-truename t)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config

  ;; How tall the mode-line should be (only respected in GUI Emacs).
  (setq doom-modeline-height 35)
  (set-face-attribute 'mode-line nil :family "Noto Sans")
  (set-face-attribute 'mode-line-inactive nil :family "Noto Sans")

  ;; How wide the mode-line bar should be (only respected in GUI Emacs).
  (setq doom-modeline-bar-width 3)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  ;; If you are expereicing the laggy issue, especially while editing remote files
  ;; with tramp, please try `file-name' style.
  ;; Please refer to https://github.com/bbatsov/projectile/issues/657.
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)

  ;; What executable of Python will be used (if nil nothing will be showed).
  (setq doom-modeline-python-executable "python")

  ;; Whether show `all-the-icons' or not (if nil nothing will be showed).
  (setq doom-modeline-icon t)

  ;; Whether show the icon for major mode. It respects `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)

  ;; Display color icons for `major-mode'. It respects `all-the-icons-color-icons'.
  (setq doom-modeline-major-mode-color-icon t)

  ;; Whether display minor modes or not. Non-nil to display in mode-line.
  (setq doom-modeline-minor-modes t)

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count t)

  ;; If non-nil, only display one number for checker information if applicable.
  (setq doom-modeline-checker-simple-format t)

  ;; Whether display perspective name or not. Non-nil to display in mode-line.
  (setq doom-modeline-persp-name t)

  ;; Whether display `lsp' state or not. Non-nil to display in mode-line.
  (setq doom-modeline-lsp t)

  ;; Whether display github notifications or not. Requires `ghub` package.
  (setq doom-modeline-github nil)

  ;; The interval of checking github.
  (setq doom-modeline-github-interval (* 30 60))

  ;; Whether display environment version or not.
  (setq doom-modeline-version nil)

  ;; Whether display mu4e notifications or not. Requires `mu4e-alert' package.
  (setq doom-modeline-mu4e nil)
  )

(column-number-mode 1)

(use-package company
  :diminish
  :config

  (setq company-begin-commands '(self-insert-command))
  (setq company-idle-delay .1)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations 't)
  (setq global-company-mode t)
  (setq company-backends '(company-capf))

                                        ; use tab to autocomplete
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)

                                        ; shift tab to go backwards
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
            (Method . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.15))
            (Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.02))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.15))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.02))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.15))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.1))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-octicon "zap" :height 0.85 :v-adjust 0 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.15))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
            (Template . ,(all-the-icons-material "format_align_left" :height 0.85 :v-adjust -0.15)))
          company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package flycheck)

(use-package evil
  :init
  (setq evil-want-integration t) ;; required by evil-collection
  (setq evil-want-keybinding nil) ;; required by evil-collection
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t) ;; like vim's 'splitright'
  (setq evil-split-window-below t) ;; like vim's 'splitbelow'
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

;; remap Escape to something else to quit insert mode
(use-package evil-escape
  :after evil
  :init
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-unordered-key-sequence t)
  (setq-default evil-escape-key-sequence "jk")
  (evil-escape-mode))


;; vim-like keybindings everywhere in emacs
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-company-use-tng nil)
  :init
  (evil-collection-init))

;; gc operator, like vim-commentary
(use-package evil-commentary
  :after evil)

;; visual hints while editing
(use-package evil-goggles
  :after evil
  :config
  (setq evil-goggles-duration 0.1)
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode))

;; like vim-surround
(use-package evil-surround
  :after evil
  :init
  (evil-define-key 'operator global-map "s" 'evil-surround-edit)
  (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
  (evil-define-key 'visual global-map "S" 'evil-surround-region)
  (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode t))

(use-package all-the-icons-ivy
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

(setq-default ivy-initial-inputs-alist nil)

(use-package ivy-posframe
  :config
  ;; display at `ivy-posframe-style'
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (ivy-posframe-mode 1))

(use-package ivy-rich
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1))

(use-package counsel
  :after ivy
  :config
  (use-package smex)
  (use-package flx)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  ;; intentional space before end of string
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((counsel-ag . ivy--regex-plus)
          (t . ivy--regex-fuzzy))))

(use-package swiper)

(use-package ivy-hydra
  :after ivy)

(use-package which-key
  :diminish which-key-mode
  :config
  (add-hook 'after-init-hook 'which-key-mode))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-return-follows-link t)
  :custom-face
  (org-document-title ((t (:weight bold :height 1.5))))
  (org-done ((t (:strike-through t :weight bold))))
  (org-headline-done ((t (:strike-through t))))
  (org-level-1 ((t (:weight bold :height 1.3 :background nil))))
  (org-level-2 ((t (:weight normal :height 1.2 :background nil))))
  (org-level-3 ((t (:weight normal :height 1.1 :background nil))))
  (org-image-actual-width '(600))
  :config
  (setq org-startup-indented t
        org-ellipsis " ⤵ " ;; folding symbol
        org-pretty-entities t
        org-hide-emphasis-markers t
        ;; show actually italicized text instead of /italicized text/
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)
  (add-to-list 'org-structure-template-alist '("el" "#+BEGIN_SRC emacs-lisp :tangle yes?\n\n#+END_SRC")))

(add-hook 'org-mode-hook
          '(lambda ()
             (setq line-spacing 0.2) ;; Add more line padding for readability
             (variable-pitch-mode 1) ;; All fonts with variable pitch.
             (display-line-numbers-mode -1)
             (mapc
              (lambda (face) ;; Other fonts with fixed-pitch.
                (set-face-attribute face nil :inherit 'fixed-pitch))
              (list 'org-code
                    'org-link
                    'org-block
                    'org-table
                    'org-verbatim
                    'org-block-begin-line
                    'org-block-end-line
                    'org-meta-line
                    'org-document-info-keyword))))

(setq-default org-hide-emphasis-markers t)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org-bullets
  :config
  (setq org-bullets-bullet-list '(" ")) ;; no bullets, needs org-bullets package
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config

  (set-face-attribute 'treemacs-file-face nil :family "Noto Sans")
  (set-face-attribute 'treemacs-directory-face nil :family "Noto Sans")
  (set-face-attribute 'treemacs-root-face nil :family "Noto Sans")
  (set-face-attribute 'treemacs-git-unmodified-face nil :family "Noto Sans")
  (set-face-attribute 'treemacs-git-modified-face nil :family "Noto Sans")
  (set-face-attribute 'treemacs-git-renamed-face nil :family "Noto Sans")
  (set-face-attribute 'treemacs-git-ignored-face nil :family "Noto Sans")
  (set-face-attribute 'treemacs-git-untracked-face nil :family "Noto Sans")
  (set-face-attribute 'treemacs-git-added-face nil :family "Noto Sans")
  (set-face-attribute 'treemacs-git-conflict-face nil :family "Noto Sans")

  (progn
    (setq-default treemacs-collapse-dirs                 (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

(use-package eyebrowse
  :config
  (eyebrowse-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package projectile
  :config
  (projectile-mode))

(setq projectile-completion-system 'ivy)

(use-package counsel-projectile
  :config
  (add-hook 'after-init-hook 'counsel-projectile-mode))

(make-variable-buffer-local 'compile-command)

(use-package magit)

(use-package evil-magit
  :after magit
  :init
  (setq evil-magit-state 'normal
        evil-magit-use-z-for-folds t))

(use-package origami)

(use-package ivy-xref
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package yasnippet
:config
(yas-global-mode 1))
  (use-package ivy-yasnippet)

(setq c-default-style "bsd"
      c-basic-offset 4)

(use-package cmake-mode
  :after projectile
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :config
  (projectile-register-project-type 'cmake-ninja '("CMakeLists.txt")
                                    :compilation-dir "build"
                                    :src-dir "src"
                                    :compile "ninja"
                                    :run "ninja run"
                                    :configure "cd %s/build && cmake .."))

(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(when (eq system-type 'windows-nt)
  (setenv "PATH"
          (concat
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Tools\\MSVC\\14.27.29110\\bin\\HostX64\\x64"
           path-separator
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\Common7\\IDE\\VC\\VCPackages"
           path-separator
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\Common7\\IDE\\CommonExtensions\\Microsoft\\TestWindow"
           path-separator
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\Common7\\IDE\\CommonExtensions\\Microsoft\\TeamFoundation\\Team Explorer"
           path-separator
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\MSBuild\\Current\\bin\\Roslyn"
           path-separator
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\Team Tools\\Performance Tools\\x64"
           path-separator
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\Team Tools\\Performance Tools"
           path-separator
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\Shared\\Common\\VSPerfCollectionTools\\vs2019\\\\x64"
           path-separator
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\Shared\\Common\\VSPerfCollectionTools\\vs2019\\"
           path-separator
           "C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v10.0A\\bin\\NETFX 4.8 Tools\\x64\\"
           path-separator
           "C:\\Program Files (x86)\\Windows Kits\\10\\bin\\10.0.18362.0\\x64"
           path-separator
           "C:\\Program Files (x86)\\Windows Kits\\10\\bin\\x64"
           path-separator
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\\\MSBuild\\Current\\Bin"
           path-separator
           "C:\\Windows\\Microsoft.NET\\Framework64\\v4.0.30319"
           path-separator
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\Common7\\IDE\\"
           path-separator
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\Common7\\Tools\\"
           path-separator
           (getenv "PATH")))

  (setenv "INCLUDE"
          (concat
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Tools\\MSVC\\14.27.29110\\include"
           path-separator
           "C:\\Program Files (x86)\\Windows Kits\\NETFXSDK\\4.8\\include\\um"
           path-separator
           "C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.18362.0\\ucrt"
           path-separator
           "C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.18362.0\\shared"
           path-separator
           "C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.18362.0\\um"
           path-separator
           "C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.18362.0\\winrt"
           path-separator
           "C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.18362.0\\cppwinrt"
           path-separator))

  (setenv "LIB"
          (concat
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Tools\\MSVC\\14.27.29110\\lib\\x64"
           path-separator
           "C:\\Program Files (x86)\\Windows Kits\\NETFXSDK\\4.8\\lib\\um\\x64"
           path-separator
           "C:\\Program Files (x86)\\Windows Kits\\10\\lib\\10.0.18362.0\\ucrt\\x64"
           path-separator
           "C:\\Program Files (x86)\\Windows Kits\\10\\lib\\10.0.18362.0\\um\\x64"
           path-separator))

  (setenv "LIBPATH"
          (concat
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Tools\\MSVC\\14.27.29110\\lib\\x64"
           path-separator
           "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Tools\\MSVC\\14.27.29110\\lib\\x86\\store\\references"
           path-separator
           "C:\\Program Files (x86)\\Windows Kits\\10\\UnionMetadata\\10.0.18362.0"
           path-separator
           "C:\\Program Files (x86)\\Windows Kits\\10\\References\\10.0.18362.0"
           path-separator
           "C:\\Windows\\Microsoft.NET\\Framework64\\v4.0.30319")))

(use-package glsl-mode
  :mode "\\.\\(vert\\|frag\\)\\'")

(require 'cc-mode)

(setq maniascript-mode-syntax-table
      (let ( (synTable (make-syntax-table c-mode-syntax-table)))
        ;; syntax-table things
        synTable))

(setq maniascript-font-lock-keywords
      (let* (
             ;; define several category of keywords
             (x-keywords '("break" "case" "continue" "default" "else" "for" "foreach" "if" "return" "switchtype" "switch" "while"))
             (x-declare '("declare" "metadata" "netread" "netwrite" "persistent" "as" "in"))
             (x-types '("Void" "Integer" "Real" "Boolean" "Text" "Vec2" "Vec3" "Int3" "Ident"))
             (x-constants '("NullId" "Null" "True" "False"))
             (x-functions '("_" "log" "wait" "sleep" "assert" "count" "sortkeyreverse" "sortkey" "sort" "reverse" "removekey" "remove"  "addfirst" "add" "existskey" "exists" "keyof" "containsonly" "containsoneof" "slice" "tojson" "fromjson" "clear"))
             (x-variables '("This"))

             ;; generate regex string for each category of keywords
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-declare-regexp (regexp-opt x-declare 'words))
             (x-types-regexp (regexp-opt x-types 'words))
             (x-constants-regexp (regexp-opt x-constants 'words))
             (x-functions-regexp (regexp-opt x-functions 'words))
             (x-variables-regexp (regexp-opt x-variables 'words))

             (x-directives-regexp "\\#\\(Include\\|Setting\\|RequireContext\\|Const\\|Struct\\|Extends\\)")
             (x-multistring-regexp "\"\"\" \\.*?\"\"\"")
             )

        `(
          (,x-keywords-regexp . font-lock-keyword-face)
          (,x-declare-regexp . font-lock-keyword-face)
          (,x-types-regexp . font-lock-type-face)
          (,x-constants-regexp . font-lock-constant-face)
          (,x-functions-regexp . font-lock-builtin-face)
          (,x-variables-regexp . font-lock-variable-name-face)

          (,x-directives-regexp . font-lock-preprocessor-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

(define-derived-mode maniascript-mode prog-mode "maniascript"
  (setq font-lock-defaults '((maniascript-font-lock-keywords)))

  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")
  )

(add-to-list 'auto-mode-alist '("\\.Script.txt\\'" . maniascript-mode))
(add-to-list 'auto-mode-alist '("\\.ms\\'" . maniascript-mode))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package lsp-mode
  :hook (prog-mode . lsp)
  :init
  (setq lsp-prefer-flymake nil)
  (setq lsp-prefer-capf t)
  (setq lsp-clients-clangd-args '("-cross-file-rename"))
  :config
  (setq lsp-idle-delay 0.500)
  (add-to-list 'lsp-language-id-configuration '(maniascript-mode . "maniascript"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "manialsp.exe")
                    :major-modes '(maniascript-mode)
                    :server-id 'manialsp))
  (setq lsp-rust-server 'rust-analyzer)
  )

(use-package lsp-treemacs)
(use-package dap-mode)
(use-package lsp-origami
  :config
  (add-hook 'origami-mode-hook #'lsp-origami-mode)
  (global-origami-mode))

;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package rust-mode)

(use-package general
  :config
  ;; replace default emacs keybindings
  (general-define-key
   "C-s" 'counsel-grep-or-swiper ; search for string in current buffer
   "C-x C-f" 'counsel-find-file  ; C-x C-f use counsel-find-file
   "M-x" 'counsel-M-x            ; replace default M-x with ivy backend

   ;; Window configs shortcuts
   "M-q" 'eyebrowse-prev-window-config
   "M-w" 'eyebrowse-next-window-config
   "M-1" 'eyebrowse-switch-to-window-config-1
   "M-2" 'eyebrowse-switch-to-window-config-2
   "M-3" 'eyebrowse-switch-to-window-config-3
   "M-4" 'eyebrowse-switch-to-window-config-4
   "M-5" 'eyebrowse-switch-to-window-config-5
   )

  (general-define-key
   :states '(normal visual emacs)
   ;; LSP
   "gr"  '(lsp-find-references :which-key "find references")
   "gd"  '(lsp-find-definition :which-key "find definition")
   )

  ;; define normal state keybindings
  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"

   ;; simple command
   "/"   '(counsel-ag :which-key "find")
   "TAB" '(evil-prev-buffer :which-key "prev buffer")
   "SPC" 'counsel-M-x

   ;; Config
   "c"   '(:ignore t :which-key "Config")
   "ce"  '(edit-config :which-key "edit")
   "cr"  '(reload-config :which-key "reload")

   ;; Project
   "p"   '(:ignore t :which-key "Project")
   "pp"  '(counsel-projectile-switch-project :which-key "switch project")
   "pb"  '(counsel-projectile-switch-to-buffer :which-key "switch buffer")
   "pf"  '(counsel-projectile-find-file :which-key "find file")
   "p/"  '(counsel-projectile-ag :which-key "find in project")
   "p."  '(projectile-find-file-dwim :which-key "browse project")
   "p."  '(projectile-find-file-dwim :which-key "browse project")
   "pc"  '(projectile-compile-project :which-key "compile")
   "pr"  '(projectile-run-project :which-key "run")

   ;; Files
   "f"   '(:ignore t :which-key "Files")
   "ff"  '(counsel-find-file :which-key "Find file")
   "fo"  '(projectile-find-other-file :which-key "Find other file")

   ;; LSP
   "l"   '(:ignore t :which-key "LSP")
   "ls"  '(counsel-imenu :which-key "list symbols")
   "ln"  '(lsp-rename :which-key "rename symbol")

   ;; Buffer
   "b"   '(counsel-ibuffer :which-key "switch buffer")

   ;; Git
   "g"   '(:ignore t :which-key "Git")
   "gs"  '(magit-status :which-key "status")

   ;; Applications
   "a"   '(:ignore t :which-key "Applications")
   "ad"  'dired
   "at"  'treemacs)


  ;; define insert state key bindings
  (general-define-key
   "C-SPC"  'company-complete))
