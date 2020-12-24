;; -*- lexical-binding: t; -*-

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(defun toggle-dark-light-theme ()
  "toggles between dark and light theme."
  (interactive (if (eq current-theme 'mon-theme)
      (progn (load-theme 'mon-theme-light t) (setq current-theme 'mon-theme-light))
    (progn (load-theme 'mon-theme t) (setq current-theme 'mon-theme)))))

(require 'project)

(defun project-build (command &optional comint)
  "Run `compile' in the ${project root}/build directory.
Arguments the same as in `compile'."
  (interactive
   (list
    (let ((command (eval compile-command)))
      (require 'compile)
      (if (or compilation-read-command current-prefix-arg)
	  (compilation-read-command command)
	command))
    (consp current-prefix-arg)))
  (let* ((pr (project-current t))
         (default-directory (concat (project-root pr) "build")))
    (compile command comint)))

(cl-defmethod project-root ((project (head local)))
  (cdr project))

(defun my--project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Override `project-files' to use `fd' in local projects."
  (mapcan #'my--project-files-in-directory
          (or dirs (list (project-root project)))))

(defun my-project-try-local (dir)
  "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'local root))))

(defun my--backend (dir)
  "Check if DIR is under Git, otherwise return nil."
  (when (locate-dominating-file dir ".git")
    'Git))

(defun my-project-try-vc (dir)
  "Determine if DIR is a project.
This is a thin variant of `project-try-vc':
- It takes only Git into consideration
- It does not check for submodules"
  (let* ((backend (my--backend dir))
         (root
          (when (eq backend 'Git)
            (or (vc-file-getprop dir 'project-git-root)
                (let ((root (vc-call-backend backend 'root dir)))
                  (vc-file-setprop dir 'project-git-root root))))))
    (and root (cons 'vc root))))

(add-to-list 'project-find-functions 'my-project-try-local)

;; ----------------------------

;; Avoid GC stalls during loading
(setq gc-cons-threshold 64000000
      gc-cons-percentage 0.6)

(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore to a reasonable value after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1
                  file-name-handler-alist my--file-name-handler-alist)))

;; --- Emacs behaviour

; 😀 😃 😄 👋 🤚 🖐 ✋ 🖖 👌🧳 🌂 ☂️ 🧵 🧶 👓⚽️ 🏀 🏈 ⚾️ 🥎 🎾  🚕 🚙 🚌 🚎 🏎
(setq use-default-font-for-symbols nil)
; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

;; Put custom settings in .custom dir
(setq custom-file (expand-file-name ".custom" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(setq-default indent-tabs-mode nil)
(setq ring-bell-function 'ignore)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq w32-get-true-file-attributes nil) ;; fix win32 perf?

(setq tab-always-indent 'complete)
(setq c-tab-always-indent 'complete)
(setq-default c-basic-offset 4)
(setq indent-tabs-mode nil)
(setq c-default-style "linux")

;; Set backup files in the tmp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; ---


;; --- use-package initialization

(require 'package)
(setq ackage-enable-at-startup nil)
(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(package-initialize)
(require 'use-package)

(setq use-package-always-ensure t
      use-package-verbose t)
;; ---


;; --- Packages

(defface my-evil-tag-face:normal '((t ())) "Face for evil normal tag.")
(defface my-evil-tag-face:emacs '((t ())) "Face for evil normal tag.")
(defface my-evil-tag-face:insert '((t ())) "Face for evil normal tag.")
(defface my-evil-tag-face:replace '((t ())) "Face for evil normal tag.")
(defface my-evil-tag-face:motion '((t ())) "Face for evil normal tag.")
(defface my-evil-tag-face:visual '((t ())) "Face for evil normal tag.")
(defface my-evil-tag-face:operator '((t ())) "Face for evil normal tag.")

;; vim inside emacs
(use-package evil
  :custom
  (evil-undo-system 'undo-redo)
  :init
  (setq evil-want-integration t) ;; required by evil-collection
  (setq evil-want-keybinding nil) ;; required by evil-collection
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t) ;; like vim's 'splitright'
  (setq evil-split-window-below t) ;; like vim's 'splitbelow'
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)

  ;; modeline format
  (setq evil-mode-line-format nil)
  (setq evil-normal-state-tag   (propertize "N" 'face 'my-evil-tag-face:normal)
        evil-emacs-state-tag    (propertize "E" 'face 'my-evil-tag-face:emacs)
        evil-insert-state-tag   (propertize "I" 'face 'my-evil-tag-face:insert)
        evil-replace-state-tag  (propertize "R" 'face 'my-evil-tag-face:replace)
        evil-motion-state-tag   (propertize "M" 'face 'my-evil-tag-face:motion)
        evil-visual-state-tag   (propertize "V" 'face 'my-evil-tag-face:visual)
        evil-operator-state-tag (propertize "O" 'face 'my-evil-tag-face:operator))

  :config
  (evil-mode 1))

;; escape using jk or kj
(use-package evil-escape
  :after evil
  :init
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-unordered-key-sequence t)
  (setq-default evil-escape-key-sequence "jk")
  (evil-escape-mode))

;; vim-like bindings everywhere in emacs
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-company-use-tng nil) ;; tng is not compatible with lsp/yasnippet
  :init
  (setq evil-magit-state 'normal
        evil-magit-use-z-for-folds t)
  (evil-collection-init))

;; like vim-surround
(use-package evil-surround
  :after evil
  :init
  (add-hook 'c++-mode-hook (lambda ()
                             (push '(?< . ("< " . " >")) evil-surround-pairs-alist)))
  (evil-define-key 'operator global-map "s" 'evil-surround-edit)
  (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
  (evil-define-key 'visual global-map "S" 'evil-surround-region)
  (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

;; Incremental narrowing (autocompletion in M-x for example)
(use-package selectrum
  :init
  (selectrum-mode))

;; Add better functions using selectrum
(use-package consult
  :init
  (fset 'multi-occur #'consult-multi-occur)
  :config
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; Enable previes
  (consult-preview-mode))
(use-package consult-selectrum
  :demand t)

;; "Fuzzy" searching for selectrum and company
(use-package prescient
  :custom
  (prescient-filter-method '(literal fuzzy)))
(use-package company-prescient
  :after company
  :init
  (company-prescient-mode))
(use-package selectrum-prescient
  :init
  (selectrum-prescient-mode))

;; i3 like workspaces
(use-package eyebrowse
  :config
  (setq eyebrowse-mode-line-style 'current)
  (setq eyebrowse-mode-line-left-delimiter "")
  (setq eyebrowse-mode-line-right-delimiter "")
  (eyebrowse-mode))

;; git integration
(use-package magit)

(defun my/eglot-ensure ()
  "Eglot ensure AND replace the format function to eglot-format"
  (eglot-ensure)
  (setq indent-region-function 'eglot-format))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'c-mode-hook 'my/eglot-ensure)
  (add-hook 'c++-mode-hook 'my/eglot-ensure))


;; Snippets system (to have better autocompletion from lsp servers that support snippets)
(use-package yasnippet
  :config
  (yas-global-mode 1))


(define-fringe-bitmap 'circle-bmp "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")
(setq flymake-error-bitmap '(circle-bmp compilation-error)
      flymake-warning-bitmap '(circle-bmp compilation-warning)
      flymake-note-bitmap '(circle-bmp compilation-info))

;; completion package
(use-package company
  :config
  (setq company-begin-commands '(self-insert-command))
  (setq company-idle-delay .1)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations 't)
  ;; (setq global-company-mode t)
  ;; (setq company-backends '(company-capf))
  (add-hook 'after-init-hook 'global-company-mode))

;; tree-sitter based syntax highlighting
(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; TODO: make it work on windows
(use-package tree-sitter-langs)

;; (use-package ligature
;;   :load-path "elisp/ligature/"
;;   :config
;;   ;; Enable all Cascadia Code ligatures in programming modes
;;   (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
;;                                        ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
;;                                        "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
;;                                        "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
;;                                        "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
;;                                        "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
;;                                        "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
;;                                        "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
;;                                        ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
;;                                        "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
;;                                        "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
;;                                        "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
;;                                        "\\" "://"))
;;   ;; Enables ligature checks globally in all buffers. You can also do it
;;   ;; per mode with `ligature-mode'.
;;   (global-ligature-mode t))

;; display parens in different colors
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


; https://org-roam.discourse.group/t/update-a-field-last-modified-at-save/321/19
(add-hook 'org-mode-hook (lambda ()
                           (setq-local time-stamp-active t
                                       time-stamp-start "#\\+LAST_MODIFIED:[ \t]*"
                                       time-stamp-end "$"
                                       time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
                           (add-hook 'before-save-hook 'time-stamp nil 'local)))

; https://stackoverflow.com/questions/17590784/how-to-let-org-mode-open-a-link-like-file-file-org-in-current-window-inste
(defun org-force-open-current-window ()
  (interactive)
  (let ((org-link-frame-setup (quote
                               ((vm . vm-visit-folder)
                                (vm-imap . vm-visit-imap-folder)
                                (gnus . gnus)
                                (file . find-file)
                                (wl . wl)))
                              ))
    (org-open-at-point)))
;; Depending on universal argument try opening link
(defun org-open-maybe (&optional arg)
  (interactive "P")
  (if arg
      (org-open-at-point)
    (org-force-open-current-window)
    )
  )

(use-package org
  :ensure org-plus-contrib :pin org
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode))
  :config
  ;; Redefine file opening without clobbering universal argumnet
  (define-key org-mode-map "\C-c\C-o" 'org-open-maybe)
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  )

(use-package evil-org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Link notes and display graph
(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :config
  (setq org-roam-directory "~/Dropbox/org/roam/")
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title} \n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+HUGO_BASE_DIR: ~/Dropbox/website/\n#+HUGO_SECTION: notes\n\n- tags :: "
           :unnarrowed t))

        org-roam-capture-ref-templates '(("r" "ref" plain (function org-roam--capture-get-point)
                                          "%?"
                                          :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                          :head "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+ROAM_KEY: ${ref}\n#+HUGO_BASE_DIR: ~/Dropbox/website/\n#+HUGO_SECTION: notes\n\n- link :: [[${ref}]]\n- tags :: "
                                          :unnarrowed t)))
  )

(use-package company-org-roam
  :config
  (push 'company-org-roam company-backends))

(use-package deft
  :config
  (setq deft-recursive t
        deft-use-filter-string-for-filename t
        deft-default-extension "org"
        deft-directory "~/Dropbox/org/roam/"))

(use-package org-roam-server
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(use-package ox-hugo :after ox)

;; Start the server to make the org-protocol bookmark works
(require 'org-protocol)
(require 'org-roam-protocol)
(server-start)

(use-package deadgrep)

(use-package mini-frame
  :custom
  ((mini-frame-show-parameters
   '((top . 40)
     (width . 0.7)
     (left . 0.5)
     (height . 15))))
  :config
  (setq mini-frame-resize nil)
  )

;; ---

;; --- Programming languages

(require 'cc-mode)

;; GLSL major mode
(use-package glsl-mode
  :mode "\\.\\(vert\\|frag\\|comp\\|geom\\)\\'")

;; Specify files to search for `ff-other-file`
(setq cc-search-directories '("."
                              "../include" "../include/*" "../include/*/*"
                              "../../include" "../../include/*" "../../include/*/*"
                              "../../../include ../../../include/*" "../../../include/*/*"

                              "../src" "../src/*" "../src/*/*"
                              "../../src" "../../src/*" "../../src/*/*"
                              "../../../src ../../../src/*" "../../../src/*/*"))

;; ---

;; --- Keybindings

;; display keybindings
(use-package which-key
  :config
  (setq which-key-idle-delay 0.1)
  (add-hook 'after-init-hook 'which-key-mode))

;; set bindings
(use-package general
  :config


  (general-define-key
   :states '(insert emacs)
   "C-SPC"  'company-complete)

  (general-define-key
   :states '(normal visual emacs)
   ;; LSP
   "gr"  '(xref-find-references :which-key "find references")
   "gd"  '(xref-find-definitions :which-key "find definition")

   ;; Workspaces
   "M-1" 'eyebrowse-switch-to-window-config-1
   "M-2" 'eyebrowse-switch-to-window-config-2
   "M-3" 'eyebrowse-switch-to-window-config-3
   "M-4" 'eyebrowse-switch-to-window-config-4
   "M-5" 'eyebrowse-switch-to-window-config-5
   )

  ;; define normal state keybindings
  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"

   ;; simple command
   ;; "/"   '(counsel-ag :which-key "find")
   "TAB" '(evil-prev-buffer :which-key "prev buffer")
   "SPC" 'execute-extended-command

   ;; Config
   ;; "c"   '(:ignore t :which-key "Config")
   ;; "ce"  '(edit-config :which-key "edit")
   ;; "cr"  '(reload-config :which-key "reload")

   ;; Project
   "p"   '(:ignore t :which-key "Project")
   "pp"  '(project-switch-project :which-key "switch project")
   "pb"  '(project-switch-to-buffer :which-key "switch buffer")
   "pf"  '(project-find-file :which-key "find file")
   "p/"  '(deadgrep :which-key "find in project")
   "pc"  '(project-build :which-key "compile")

   ;; Files
   "f"   '(:ignore t :which-key "Files")
   "ff"  '(ido-find-file :which-key "Find file")
   "fo"  '(ff-find-other-file :which-key "Find other file")

   ;; LSP
   "l"   '(:ignore t :which-key "LSP")
   "ls"  '(consult-imenu :which-key "list symbols")
   "ln"  '(eglot-rename :which-key "rename symbol")
   "lc"  '(eglot-code-actions :which-key "trigger code action")

   ;; Buffer
   "b"   '(consult-buffer :which-key "switch buffer")

   ;; Git
   "g"   '(:ignore t :which-key "Git")
   "gs"  '(magit-status :which-key "status")

   ;; Theme
   "t" '(:ignore t :which-key "Themes")
   "tt" '(toggle-dark-light-theme :which-key "toggle")

   ;; Applications
   "a"   '(:ignore t :which-key "Applications")
   "ad"  'dired

   ;; Org
   "n" '(:ignore t :which-key "Notes")
   "nl" '(org-roam :which-key "Org Roam")
   "nf" '(org-roam-find-file :which-key "Roam: find file")
   "nb" '(org-roam-switch-buffer :which-key "Roam: switch buffer")
   "ng" '(org-roam-graph :which-key "Roam: graph")
   "ni" '(org-roam-insert :which-key "Roam: insert")
   "nI" '(org-roam-insert-immediate :which-key "Roam: insert immediate")
   "nc" '(org-roam-capture :which-key "Roam: capture")
   "nd" '(deft :which-key "navigate")
   "nt" '(org-roam-dailies-find-today :which-key "Today")
   )
  )

;; ---
