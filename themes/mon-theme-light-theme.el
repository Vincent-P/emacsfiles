;;; package --- My Ideal theme  -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;;
;;; Code:


(deftheme mon-theme-light
  "My Ideal theme.")

(let ((fg_base "black")
      (fg2 "dim gray")
      (bg_base "white")
      (bg_region "gray95")
      (hl_bg "gray90")
      (str_fg "dark goldenrod")
      (str_bg "light goldenrod yellow")
      (blue0 "#0069a8")
      (blue1 "#265d73")
      (constant_fg "dark goldenrod")
      (type_fg "#0069a8")
      (error_color "indian red")
      (warning_color "coral")
      (info_color "black")
      (search_bg "#52beff")
      )

  ;; Set faces
  (custom-theme-set-faces
   'mon-theme-light ;; you must use the same theme name here...

   ;; --- Default Emacs GUI faces
   `(default ((t (:foreground ,fg_base :background ,bg_base))))
   `(cursor  ((t (:background ,fg2))))
   `(fringe  ((t (:background ,bg_base))))

   `(mode-line ((t (:foreground ,fg_base :box ,`(:line-width 1 :color ,fg2)))))
   `(mode-line-inactive ((t (:foreground ,fg2 :box ,`(:line-width 1 :color ,fg2)))))
   `(mode-line-emphasis ((t (:foreground ,constant_fg))))

   `(minibuffer-prompt  ((t (:foreground ,fg_base :weight bold))))

   `(link  ((t (:foreground ,blue0 :underline t :background nil))))
   `(link-visited  ((t (:foreground ,blue1))))
   `(button ((t (:background nil))))

   `(custom-button ((t (:box t))))
   `(custom-button-mouse ((t (:inherit custom-button))))
   `(custom-button-pressed ((t (:inherit custom-button))))
   `(custom-button-pressed_unraised ((t (:inherit custom-button))))
   `(custom-button-unraised ((t (:inherit custom-button))))

   `(highlight  ((t (:background ,hl_bg))))
   `(lazy-highlight  ((t (:background ,hl_bg :underline t))))
   `(isearch ((t (:background ,search_bg))))

   ;; --- Programming faces

   `(region  ((t (:background ,bg_region))))

   ;; Errors
   `(flycheck-error ((t (:underline (:color ,error_color :style line)))))
   `(flycheck-fringe-error ((t (:foreground ,error_color))))
   `(flyspell-incorrect ((t (:underline `(:color ,error_color :style line)))))

   `(flycheck-warning ((t (:underline (:color ,warning_color :style line)))))
   `(flycheck-fringe-warning ((t (:foreground ,warning_color))))
   `(flyspell-duplicate ((t (:underline (:color ,warning_color :style line)))))

   `(flycheck-info ((t (:underline (:color ,info_color :style line)))))
   `(flycheck-fringe-info ((t (:foreground ,info_color))))


   ;; Basic emacs programming faces
   `(font-lock-doc-face ((t (:foreground ,fg2 :weight bold :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg2))))
   `(font-lock-comment-face ((t (:foreground ,fg2))))

   `(font-lock-keyword-face ((t (:foreground ,fg_base :weight bold))))
   `(font-lock-constant-face ((t (:foreground ,constant_fg))))
   `(font-lock-type-face ((t (:foreground ,type_fg))))
   `(font-lock-string-face ((t (:foreground ,str_fg :background ,str_bg))))

   `(font-lock-builtin-face ((t (:foreground ,fg_base))))
   `(font-lock-function-name-face ((t (:foreground ,fg_base))))
   `(font-lock-negation-char-face ((t (:foreground ,fg_base))))
   `(font-lock-preprocessor-face ((t (:foreground ,fg_base :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,fg_base))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,fg_base))))
   `(font-lock-variable-name-face ((t (:foreground ,fg_base))))
   `(font-lock-warning-face ((t (:foreground ,fg_base))))

   ;; Treesitter faces
   `(tree-sitter-hl-face:type ((t (:inherit font-lock-type-face))))
   `(tree-sitter-hl-face:type.argument ((t (:inherit tree-sitter-hl-face:type))))
   `(tree-sitter-hl-face:type.builtin ((t (:inherit tree-sitter-hl-face:type))))
   `(tree-sitter-hl-face:type.parameter ((t (:inherit tree-sitter-hl-face:type))))
   `(tree-sitter-hl-face:type.supe ((t (:inherit tree-sitter-hl-face:type))))

   `(tree-sitter-hl-face:constant.builtin ((t (:inherit tree-sitter-hl-face:constant))))
   `(tree-sitter-hl-face:variable\.special ((t (:inherit tree-sitter-hl-face:constant))))

   `(tree-sitter-hl-face:property ((t (:slant italic))))
   `(tree-sitter-hl-face:property.definition ((t (:inherit tree-sitter-hl-face:property))))
   `(tree-sitter-hl-face:method ((t (:slant italic))))
   `(tree-sitter-hl-face:method.call ((t (:inherit tree-sitter-hl-face:method))))

   `(tree-sitter-hl-face:function.call ((t ())))

   ;; --- Company faces
   `(company-scrollbar-fg ((t (:background ,fg_base))))
   `(company-scrollbar-bg ((t (:background ,hl_bg))))
   `(company-tooltip ((t (:foreground ,fg_base :background ,bg_base))))
   `(company-tooltip-common ((t (:foreground ,fg_base :underline t))))
   `(company-tooltip-selection ((t (:background ,hl_bg))))
   `(company-preview ((t (:background ,hl_bg))))
   `(company-preview-common ((t (:background ,hl_bg :underline t))))
   `(company-preview-search ((t (:inherit company-preview-common))))

   ;; --- Rainbow delimiters, more satured colors
   `(rainbow-delimiters-base-error-face ((t (:foreground "indian red" :underline `(:color ,error_color :style line)))))
   `(rainbow-delimiters-base-face ((t ())))
   `(rainbow-delimiters-depth-1-face ((t (:foreground "#4b50a8"))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground "#5c7bed"))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground "#b8c054"))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground "#3fc93f"))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground "#c34040"))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground "#2957f3"))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground "#b7b74e"))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground "#55d355"))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground "#ba3e3e"))))

   ;; --- Org
   `(org-document-title ((t (:foreground ,fg_base :height 2.0))))
   `(outline-1 ((t (:foreground ,fg_base :height 1.20 :underline t))))
   `(outline-2 ((t (:foreground ,fg_base :height 1.15 :underline t))))
   `(outline-3 ((t (:foreground ,fg_base :height 1.10 :underline t))))

   `(org-list-dt ((t (:foreground ,fg_base :slant italic :weight normal))))
   )

  (custom-theme-set-variables
   'mon-theme-light))

(provide-theme 'mon-theme-light)

;;; mon-theme-light-theme.el ends here
