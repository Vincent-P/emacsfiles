(setq make-backup-files nil)
(require 'package)
(package-initialize)
(require 'use-package)

(use-package org)

(use-package org-roam
  :config
  (setq org-roam-directory "~/Dropbox/org/roam/")
)

(defun my/org-roam--backlinks-list (file)
  (if (org-roam--org-roam-file-p file)
      (--reduce-from
       (concat acc (format "- [[file:%s][%s]]\n"
                           (file-relative-name (car it) org-roam-directory)
                           (org-roam-db--get-title (car it))))
       ""
       (org-roam-db-query
        [:select :distinct [links:source]
                 :from links
                 :left :outer :join tags :on (= links:source tags:file)
                 :where (and (= dest $s1)
                             (or (is tags:tags nil)
                                 (and
                                  (not-like tags:tags '%private%)
                                  (not-like tags:tags '%draft%))))]
        file))
    ""))

(defun my/org-export-preprocessor (_backend)
  (let ((links (my/org-roam--backlinks-list buffer-file-name)))
    (unless (string= links "")
      (goto-char (point-max))
      (insert (concat "\n* Links to this note\n") links))))

(add-hook 'org-export-before-processing-hook #'my/org-export-preprocessor)
