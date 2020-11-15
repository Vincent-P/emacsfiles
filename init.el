(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

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

;; ----------------------------

;; Avoid GC stalls during loading
(setq gc-cons-threshold 64000000)

;; Restore to a reasonable value after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 100000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

;; Put custom settings in .custom dir
(setq custom-file (expand-file-name ".custom" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))


;; --- appearance
(setq inhibit-startup-screen t)

(global-visual-line-mode 1) ;; wrap lines
(global-hl-line-mode 1) ;; highlight current line
(column-number-mode 1) ;; display the column in the modeline

;; hide default ui
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; remove borders
(setq-default left-fringe-width 1)
(setq-default right-fringe-width 1)


(set-face-attribute 'default nil :family "Cascadia Code" :height 110)
(set-face-attribute 'fixed-pitch nil :family "Cascadia Code" :height 110)
(set-face-attribute 'variable-pitch nil :family "Noto Sans" :height 120)
(set-face-attribute 'mode-line nil :family "Noto Sans" :height 120)

;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
(set-fontset-font t 'symbol "Noto Color Emoji" nil)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
;; ---

;; --- Emacs behaviour

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
;; ---

;; --- use-package initialization

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

;; ---

;; --- Packages

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
  (evil-collection-init))

;; like vim-surround
(use-package evil-surround
  :after evil
  :init
  (evil-define-key 'operator global-map "s" 'evil-surround-edit)
  (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
  (evil-define-key 'visual global-map "S" 'evil-surround-region)
  (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

;; Incremental narrowing (autocompletion in M-x for example)
(use-package selectrum
  :init
  (selectrum-mode))

;; "Fuzzy" searching for selectrum and company
(use-package prescient
  :custom
  (prescient-filter-method '(literal regexp initialism fuzzy)))

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
  (eyebrowse-mode))

;; git integration
(use-package magit)

(use-package evil-magit
  :after magit
  :init
  (setq evil-magit-state 'normal
        evil-magit-use-z-for-folds t))

;; LSP support
(use-package lsp-mode
  :hook (prog-mode . lsp)
  :init
  (setq lsp-prefer-flymake nil)
  (setq lsp-prefer-capf t)
  (setq lsp-clients-clangd-args '("-cross-file-rename"))
  :config
  (setq lsp-idle-delay 0.500)
  )

;; Snippets system (to have better autocompletion from lsp servers that support snippets)
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Error reporting
(use-package flycheck)

;; completion package
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
  ;; use tab to autocomplete
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  ;; shift tab to go backwards
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

;; tree-sitter based syntax highlighting
(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package ligature
  :load-path "elisp/ligature/"
  :config
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; display parens in different colors
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; modus themes
(use-package modus-operandi-theme)
(use-package modus-vivendi-theme)

;; modeline theme
(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; from https://protesilaos.com/modus-themes/#h:1777c247-1b56-46b7-a4ce-54e720b33d06
(defmacro modus-themes-format-sexp (sexp &rest objects)
  `(eval (read (format ,(format "%S" sexp) ,@objects))))

(dolist (theme '("operandi" "vivendi"))
  (modus-themes-format-sexp
   (defun modus-%1$s-theme-load ()
     (setq modus-%1$s-theme-slanted-constructs t
           modus-%1$s-theme-bold-constructs t
           modus-%1$s-theme-fringes 'subtle ; {nil,'subtle,'intense}
           modus-%1$s-theme-mode-line 'moody ; {nil,'3d,'moody}
           modus-%1$s-theme-syntax 'alt-syntax ; {nil,faint,'yellow-comments,'green-strings,'yellow-comments-green-strings,'alt-syntax,'alt-syntax-yellow-comments}
           modus-%1$s-theme-intense-hl-line nil
           modus-%1$s-theme-intense-paren-match nil
           modus-%1$s-theme-links 'faint ; {nil,'faint,'neutral-underline,'faint-neutral-underline,'no-underline}
           modus-%1$s-theme-no-mixed-fonts nil
           modus-%1$s-theme-prompts nil ; {nil,'subtle,'intense}
           modus-%1$s-theme-completions 'moderate ; {nil,'moderate,'opinionated}
           modus-%1$s-theme-diffs nil ; {nil,'desaturated,'fg-only}
           modus-%1$s-theme-org-blocks 'grayscale ; {nil,'grayscale,'rainbow}
           modus-%1$s-theme-headings  ; Read further below in the manual for this one
           '((1 . section)
             (2 . line)
             (t . rainbow-line-no-bold))
           modus-%1$s-theme-variable-pitch-headings nil
           modus-%1$s-theme-scale-headings t
           modus-%1$s-theme-scale-1 1.1
           modus-%1$s-theme-scale-2 1.15
           modus-%1$s-theme-scale-3 1.21
           modus-%1$s-theme-scale-4 1.27
           modus-%1$s-theme-scale-5 1.33)
     (load-theme 'modus-%1$s t))
   theme))

(defun modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
        (disable-theme 'modus-operandi)
        (modus-vivendi-theme-load))
    (disable-theme 'modus-vivendi)
    (modus-operandi-theme-load)))

(modus-vivendi-theme-load)

;; ---

;; --- Programming languages

;; Setup msvc toolchain
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

(require 'cc-mode)

(use-package glsl-mode
  :mode "\\.\\(vert\\|frag\\)\\'")

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
  :diminish which-key-mode
  :config
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
   "gr"  '(lsp-find-references :which-key "find references")
   "gd"  '(lsp-find-definition :which-key "find definition")

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
   "p/"  '(project-find-regexp :which-key "find in project")
   "pc"  '(project-build :which-key "compile")

   ;; Files
   "f"   '(:ignore t :which-key "Files")
   "ff"  '(ido-find-file :which-key "Find file")
   "fo"  '(ff-find-other-file :which-key "Find other file")

   ;; LSP
   "l"   '(:ignore t :which-key "LSP")
   "ls"  '(imenu :which-key "list symbols")
   "ln"  '(lsp-rename :which-key "rename symbol")

   ;; Buffer
   "b"   '(ido-switch-buffer :which-key "switch buffer")

   ;; Git
   "g"   '(:ignore t :which-key "Git")
   "gs"  '(magit-status :which-key "status")

   ;; Theme
   "t" '(:ignore t :which-key "Themes")
   "tt" '(modus-themes-toggle :which-key "toggle")

   ;; Applications
   "a"   '(:ignore t :which-key "Applications")
   "ad"  'dired
   )
  )

;; ---
