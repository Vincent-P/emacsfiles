(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f66ffeadda7b52d40c8d698967ae9e9836f54324445af95610d257fa5e3e1e21" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(git-gutter:added-sign " ")
 '(git-gutter:deleted-sign " ")
 '(git-gutter:modified-sign " ")
 '(package-selected-packages
   (quote
    (flymake-diagnostic-at-point ivy-xref panda-theme lsp-ui cquery company-lsp company moody minions git-timemachine git-gutter magit fzf counsel-projectile projectile rainbow-mode rainbow-delimiters which-key ivy-hydra counsel ivy smex evil-surround evil-goggles evil-commentary evil-collection evil-escape evil benchmark-init use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#292A2B" :foreground "#E6E6E6"))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))

(package-initialize)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
