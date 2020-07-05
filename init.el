;; Don't attempt to find/apply special file handlers to files loaded during startup.
(let ((file-name-handler-alist nil)
	  (config-regular (expand-file-name "config.el" user-emacs-directory))
	  (config-compiled (expand-file-name "config.elc" user-emacs-directory))
	  (config-org (expand-file-name "config.org" user-emacs-directory)))

  ;; If config is pre-compiled, then load that
  (if (file-exists-p config-regular)
      (load-file config-regular)
    ;; Otherwise use org-babel to tangle and load the configuration
    (require 'org)
    (org-babel-load-file config-org)))
