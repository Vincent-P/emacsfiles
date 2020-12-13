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

(set-face-attribute 'default nil :family "JetBrains Mono" :height 120)
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 120)
(set-face-attribute 'variable-pitch nil :family "Noto Sans" :height 120)

(set-face-attribute 'mode-line nil
                    :family "JetBrains Mono"
                    :height 110
                    :background "grey20"
                    :foreground "white"
                    :box '(:line-width 4 :color "grey20")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :family "JetBrains Mono"
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

(defvar my-mode-line-project-root
  '(:propertize
    (:eval (when (project-current nil) (concat (directory-file-name (project-root (project-current nil))) "/")))
    face mode-line-emphasis))

(defvar my-mode-line-selection
  '(:propertize
    (:eval (when (use-region-p) (concat (number-to-string (count-lines (region-beginning) (region-end))) " line(s)")))
    face mode-line-emphasis))

(put 'my-mode-line-project-root 'risky-local-variable t)
(put 'my-mode-line-selection 'risky-local-variable t)

(setq-default mode-line-position
              '((line-number-mode ("%04l" (column-number-mode ":%02c")))))

(setq mode-line-align-left
      '(" "
        (eyebrowse-mode (:eval (eyebrowse-mode-line-indicator)))
        (:eval evil-mode-line-tag)
        my-mode-line-project-root
        mode-line-buffer-identification
        "%*"
        " "
        mode-line-position
        "  "
        my-mode-line-selection
        ))

(setq mode-line-align-middle
      '(""
        ))

(setq mode-line-align-right
      '(""
        (:eval (symbol-name buffer-file-coding-system))
        " "
        mode-name))

;; = (https://emacs.stackexchange.com/questions/16654/how-to-re-arrange-things-in-mode-line) =================================
;; DONT CHANGE BELOW

(defun mode-line-fill-right (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))


(defun mode-line-fill-center (face reserve)
  "Return empty space using FACE to the center of remaining space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ center (.5 . right-margin)) ,reserve
                                             (.5 . left-margin))))
              'face face))




(defconst RIGHT_PADDING 1)

(defun reserve-left/middle ()
  (/ (length (format-mode-line mode-line-align-middle)) 2))

(defun reserve-middle/right ()
  (+ RIGHT_PADDING (length (format-mode-line mode-line-align-right))))

(setq-default mode-line-format
              (list
               mode-line-align-left
               '(:eval (mode-line-fill-center nil (reserve-left/middle)))
               mode-line-align-middle
               '(:eval (mode-line-fill-right nil (reserve-middle/right)))
               mode-line-align-right
               ))

(setq mode-line-format
      (list
       mode-line-align-left
       '(:eval (mode-line-fill-center nil (reserve-left/middle)))
       mode-line-align-middle
       '(:eval (mode-line-fill-right nil (reserve-middle/right)))
       mode-line-align-right
       ))

;; ===========================================================================================================================


;; ---


;; --- package
(require 'package)

(setq ackage-enable-at-startup nil)
(setq package-archives
                        '(("org" . "\`https://orgmode.org/elpa/``")
                          ("melpa" . "https://melpa.org/packages/")
                          ("gnu" . "https://elpa.gnu.org/packages/")))
;; ---
