;;; package --- My Ideal theme

;;; Commentary:
;;;
;;;
;;; Code:

; -*- lexical-binding: t; -*-

(deftheme mon-theme
  "My Ideal theme.")

(let ((fg_base "white")
      (fg2 "dim gray")
      (bg_base "black")
      (bg_region "gray15")
      (hl_bg "gray20")
      (hl_bold "gray25")
      (str_fg "LightGoldenrod1")
      (str_bg "#332f1c")
      (blue0 "#52beff")
      (blue1 "#66afcc")
      (constant_fg "LightGoldenrod1")
      (type_fg "#52beff")
      )

  ;; Set faces
  (custom-theme-set-faces
   'mon-theme ;; you must use the same theme name here...

   ;; --- Default Emacs GUI faces
   `(default ((t (:foreground ,fg_base :background ,bg_base))))
   `(cursor  ((t (:background ,fg_base))))
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

   ;; --- Programming faces

   `(region  ((t (:background ,bg_region))))

   ;; Errors
   `(flycheck-error ((t (:underline (:color "indian red" :style line)))))
   `(flyspell-incorrect ((t (:underline (:color "indian red" :style line)))))
   `(flycheck-warning ((t (:underline (:color "coral" :style line)))))
   `(flyspell-duplicate ((t (:underline (:color "coral" :style line)))))
   `(flycheck-info ((t (:underline `(:color ,fg_base :style line)))))

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
   )

  (custom-theme-set-variables
   'mon-theme))

(provide-theme 'mon-theme)

;;; mon-theme-theme.el ends here
