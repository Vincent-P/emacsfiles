;; -*- lexical-binding: t; -*-

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

;; fonts
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'dracula t)

(set-face-attribute 'default nil :family "Cascadia Code" :height 110)
(set-face-attribute 'fixed-pitch nil :family "Cascadia Code" :height 110)
(set-face-attribute 'variable-pitch nil :family "Segoe UI" :height 120)
(set-face-attribute 'mode-line nil :family "Segoe UI" :height 120)

(set-fontset-font t 'symbol "Segoe UI Emoji" nil)
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)

;; show whitespace
;; (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))
;;
;; (setq whitespace-display-mappings
;;       '(
;;         (space-mark 32 [183] [46]) ; 183 middle dot
;;         (newline-mark 10 [172 10]) ; 172 ¬ not sign
;;         (tab-mark 9 [187 9] [92 9]) ; tab
;; ))
;;
;;
;; (global-whitespace-mode)

(setq-default mode-line-format (list
                        ""
                        'mode-line-modified
                        'mode-line-buffer-identification
                        " "
                        'mode-line-position
                        " "
                        '(vc-mode vc-mode)
                        " "
                        'mode-name
                        " "
                        "["
                        '(:eval (symbol-name buffer-file-coding-system))
                        "]"
                        ))

(setq mode-line-format (default-value 'mode-line-format))

;; ---
