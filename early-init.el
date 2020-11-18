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
(custom-set-variables '(modus-vivendi-theme-syntax 'alt-syntax))
(custom-set-variables '(modus-vivendi-theme-intense-hl-line t))
(load-theme 'modus-vivendi t)

(set-face-attribute 'default nil :family "Cascadia Code" :height 110)
(set-face-attribute 'fixed-pitch nil :family "Cascadia Code" :height 110)
(set-face-attribute 'variable-pitch nil :family "Noto Sans" :height 110)

(set-face-attribute 'mode-line nil
                    :family "Noto Sans Mono"
                    :height 110
                    :background "grey20"
                    :foreground "white"
                    :box '(:line-width 4 :color "grey20")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :family "Noto Sans Mono"
                    :height 110
                    :background "black"
                    :foreground "white"
                    :box '(:line-width 4 :color "black")
                    :overline nil
                    :underline nil)

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

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
   Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(setq-default
 mode-line-format
 '((:eval
    (simple-mode-line-render
     ;; Left.
     (quote (" "
             (eyebrowse-mode (:eval (eyebrowse-mode-line-indicator)))
             evil-mode-line-tag
             "%*"
             mode-line-buffer-identification
             " %02l:%02c "
             ))
     ;; Right.
     (quote (""
             " ["
             (:eval (symbol-name buffer-file-coding-system))
             "] "
             mode-name
             "  "
             ))))))

(setq mode-line-format (default-value 'mode-line-format))
;; ---


;; --- package
(require 'package)

(setq ackage-enable-at-startup nil)
(setq package-archives
                        '(("org" . "\`https://orgmode.org/elpa/``")
                          ("melpa" . "https://melpa.org/packages/")
                          ("gnu" . "https://elpa.gnu.org/packages/")))
;; ---
