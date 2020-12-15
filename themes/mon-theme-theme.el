;;; package --- My Ideal theme  -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;;
;;; Code:

(deftheme mon-theme
  "My Ideal theme.")

(let* (
       (fg_base "white")
       (fg_dim "dim gray")
       (bg_0 "black")
       (bg_1 "gray15")
       (bg_2 "gray20")
       (bg_3 "gray30")
       (yellow_0 "LightGoldenrod1")
       (yellow_1 "#332f1c")
       (blue_0 "#52beff")
       (blue_1 "#66afcc")
       (blue_2 "#0069a8")
       (red_0 "indian red")
       (red_1 "todo")
       (orange_0 "coral")
       (red_1 "todo")

       (bg_base bg_0)
       (region_bg bg_1)
       (hl_bg bg_2)
       (str_fg yellow_0)
       (str_bg yellow_1)
       (link_fg blue_0)
       (link_visited_fg blue_1)
       (constant_fg yellow_0)
       (modeline_emphasis_fg blue_0)
       (type_fg blue_0)
       (error_color red_0)
       (warning_color orange_0)
       (info_color fg_base)
       (search_bg blue_2)
       (whitespace_fg bg_3)
       )

  ;; Set faces
  (custom-theme-set-faces
   'mon-theme ;; you must use the same theme name here...

   ;; --- Default Emacs GUI faces
   `(default ((t (:foreground ,fg_base :background ,bg_base))))
   `(cursor  ((t (:background ,fg_dim))))
   `(fringe  ((t (:background ,bg_base))))

   `(mode-line ((t (:foreground ,fg_base :box ,`(:line-width 1 :color ,fg_dim)))))
   `(mode-line-inactive ((t (:foreground ,fg_dim :box ,`(:line-width 1 :color ,fg_dim)))))
   `(mode-line-emphasis ((t (:foreground ,modeline_emphasis_fg))))

   `(minibuffer-prompt  ((t (:foreground ,fg_base :weight bold))))

   `(link  ((t (:foreground ,link_fg :underline t :background nil))))
   `(link-visited  ((t (:foreground ,link_visited_fg))))
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

   `(region  ((t (:background ,region_bg))))
   `(whitespace-space ((t (:foreground ,whitespace_fg))))
   `(whitespace-newline ((t (:inherit whitespace-space))))

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
   `(font-lock-doc-face ((t (:foreground ,fg_dim :weight bold :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg_dim))))
   `(font-lock-comment-face ((t (:foreground ,fg_dim))))

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

   ;; --- Org
   `(org-document-title ((t (:foreground ,fg_base :height 2.0))))
   `(outline-1 ((t (:foreground ,fg_base :height 1.20 :underline t))))
   `(outline-2 ((t (:foreground ,fg_base :height 1.15 :underline t))))
   `(outline-3 ((t (:foreground ,fg_base :height 1.10 :underline t))))

   `(org-list-dt ((t (:foreground ,fg_base :slant italic :weight normal))))
   )

  (custom-theme-set-variables
   'mon-theme))

(provide-theme 'mon-theme)

;;; mon-theme-theme.el ends here
