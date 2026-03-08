;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with ‘C-x C-f’ and enter text in its buffer.

;;; email->profile launcher

(defgroup my-browser-pf nil
  "Open URLs in per-email browser profiles."
  :group 'external)

(defcustom my-browser-pf-browser (or (executable-find "google-chrome-stable")
                                     (executable-find "google-chrome")
                                     (executable-find "chromium")
                                     (executable-find "microsoft-edge")
                                     "google-chrome")
  "Browser executable."
  :type 'string)



(defcustom my-browser-pf-root (expand-file-name "~/.config/chrome-pf/")
  "Root directory for per-email browser profiles (user-data-dir)."
  :type 'directory)

(defun my--email-at-point-or-org ()
  "Try to get email from Org property EMAIL, or email at point, or in current line."
  (let (email)
    ;; 1) Org property: :EMAIL:
    (when (derived-mode-p 'org-mode)
      (setq email (or (org-entry-get (point) "EMAIL" t)
                      (org-entry-get nil "EMAIL" t))))
    ;; 2) thing-at-point email
    (unless (and email (string-match-p "@" email))
      (setq email (thing-at-point 'email t)))
    ;; 3) regex scan current line
    (unless (and email (string-match-p "@" email))
      (save-excursion
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
          (when (string-match
                 "\\b[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]\\{2,\\}\\b"
                 line)
            (setq email (match-string 0 line))))))
    email))

(defun my--sanitize-email-for-path (email)
  "Make EMAIL safe to use as a directory name."
  ;; keep it readable; just replace path-hostile chars.
  (replace-regexp-in-string "[^a-zA-Z0-9@._+-]" "_" email))

(defun my--url-at-point-or-read ()
  "Try URL at point; fallback to prompt."
  (or (thing-at-point 'url t)
      (read-string "Open URL: ")))

(defun my-open-url-in-email-profile (url &optional email)
  "Open URL in a browser profile keyed by EMAIL (directory named by email)."
  (interactive (list (my--url-at-point-or-read)
                     nil))
  (let* ((email (or email (my--email-at-point-or-org)
                    (read-string "Email (profile name): ")))
         (email (string-trim email))
         (safe  (my--sanitize-email-for-path email))
         (udir  (expand-file-name safe my-browser-pf-root))
         (exe   my-browser-pf-browser))
    (unless (and exe (file-executable-p exe))
      (user-error "Browser not found: %S (set my-browser-pf-browser)" exe))
    (make-directory udir t)
    ;; Use make-process to avoid shell quoting issues
    (make-process
     :name (format "browser-%s" safe)
     :command (list exe
                    (concat "--user-data-dir=" udir)
                    "--profile-directory=Default"
                    "--new-window"
                    url)
     :noquery t)
    (message "Opened %s in profile %s (%s)" url email udir)))

;; 额外：只按当前邮箱打开“该服务商后台/固定网址”
(defcustom my-email-profile-default-url "https://example.com"
  "Default URL to open for the current email profile."
  :type 'string)

(defun my-open-default-in-email-profile ()
  (interactive)
  (my-open-url-in-email-profile my-email-profile-default-url))
(global-set-key (kbd "C-c o") #'my-open-url-in-email-profile)
(global-set-key (kbd "C-c O") #'my-open-default-in-email-profile)



(provide 'multi-profile)
