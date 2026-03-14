;;; -*- lexical-binding: t -*-

;;; clean

(setq gc-cons-threshold 100000000) ; 100 mb
(setq read-process-output-max (* 1024 1024)) ; 1mb
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)
(blink-cursor-mode -1)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-list-file-name nil)
(setq use-short-answers t)
(delete-selection-mode t)
(setq ring-bell-function 'ignore)
(setq save-silently t)

(setq read-extended-command-predicate
      #'command-completion-default-include-p)


;;; better
(setq column-number-mode t
      mode-line-in-non-selected-windows t)

(setq switch-to-buffer-obey-display-actions t)

(setq-default truncate-lines t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; 自动保存
;; (setq auto-save-visited-interval 1) ; 先设定
;; (auto-save-visited-mode 1) ; 再打开

(setq scroll-conservatively 101)   ; 光标靠近边界才滚动
(setq scroll-margin 2)             ; 顶部/底部预留2行
(setq scroll-step 1)               ; 每次滚动一行
(setq redisplay-dont-pause t)
;; (electric-pair-mode 0)
;; (show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; (global-display-line-numbers-mode)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)

;; (setq completions-format 'one-column)
;; (setq completions-max-height 15)
;; (setq completion-auto-help 'visible
;;         completion-auto-select 'second-tab
;;         completion-show-help nil
;;         completions-sort 'historical
;;         completions-header-format nil)

;; (setq icomplete-in-buffer t)
;; (setq icomplete-prospects-height 10)
;; (advice-add 'completion-at-point :after #'minibuffer-hide-completions)
;; (global-completion-preview-mode)

;;(icomplete-vertical-mode t)
;;(fido-vertical-mode 1)
;; (setq completion-styles '(initials flex substring))
;; (setq completion-category-overrides
;;       '((file (styles partial-completion))))

;;dired
(setq dired-kill-when-opening-new-dired-buffer t)
;; (add-hook 'dired-mode-hook #'dired-hide-details-mode)
(setq dired-listing-switches "-alh")
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq dired-dwim-target t) ;当两个 dired 窗口打开时，自动从一个位置复制到另一个位置


(use-package dabbrev
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(setq load-prefer-newer t)
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))


(require 'uniquify) ; 重复 buffer 区分成 index.js 和 index.js<2>
(setq uniquify-buffer-name-style 'forward)

;; org
(setq org-ellipsis "…")
(setq org-hide-leading-stars t)
(add-hook 'org-mode-hook #'org-indent-mode)
(org-babel-do-load-languages
 'org-babel-load-languages '
 ((js . t)))
(setq org-confirm-babel-evaluate nil)

(which-key-mode 1)
;;; manual package
(require 'vv-mode)

;; package init
(setq gnutls-use-ipv6 nil)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;; theme
(load-theme 'wombat t)


(require 'my-package)
;; (require 'my-meow)


(require 'bracket-content-flash)
(bracket-content-flash-mode 1)
(require 'my-pyisearch)

(define-key isearch-mode-map (kbd "DEL") #'isearch-del-char)
(define-key isearch-mode-map (kbd "C-g") #'isearch-cancel)

;; 你
(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; (defun +org/opened-buffer-files ()
;;   "Return the list of files currently opened in emacs"
;;   (delq nil
;;         (mapcar (lambda (x)
;;                   (if (and (buffer-file-name x)
;;                            (string-match "\\.org$"
;;                                          (buffer-file-name x)))
;;                       (buffer-file-name x)))
;;                 (buffer-list))))
                                        ;(setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 9)))
(setq org-refile-targets
      '((("~/agarden" . nil) :maxlevel . 3)))


(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; (defun +org-search ()
;;   (interactive)
;;   (org-refile '(4)))
;; (setq org-refile-use-cache t)
;; (run-with-idle-timer 300 t (lambda ()
;;                             (org-refile-cache-clear)
;;                             (org-refile-get-targets)))

(require 'org-super-links)
(global-set-key (kbd "C-c s s") #'org-super-links-link)
(global-set-key (kbd "C-c s l") #'org-super-links-store-link)
(global-set-key (kbd "C-c s C-l") #'org-super-links-insert-link)
;; (use-package org-super-links
;;   :vc (:url "https://github.com/toshism/org-super-links")
;;   :bind (("C-c s s" . org-super-links-link)
;;          ("C-c s l" . org-super-links-store-link)
;;          ("C-c s C-l" . org-super-links-insert-link)
;;          ("C-c s d" . org-super-links-quick-insert-drawer-link)
;;          ("C-c s i" . org-super-links-quick-insert-inline-link)
;;          ("C-c s C-d" . org-super-links-delete-link)))


(use-package google-translate
  :config
  (global-set-key (kbd "C-c t") 'google-translate-smooth-translate)
  (setq google-translate-translation-directions-alist '(("en" . "zh-CN")))
  )
(use-package google-this
  :config (google-this-mode 1)
  (global-set-key (kbd "C-x g") 'google-this-mode-submap))


(setq org-attach-id-dir "~/agarden/assets")
(defun my/org-insert-asset ()
  (interactive)
  (let* ((src (read-file-name "选择文件: "))
         (assets-dir (expand-file-name "assets"
                                       (file-name-directory (buffer-file-name))))
         (dst (expand-file-name
               (file-name-nondirectory src)
               assets-dir)))
    (unless (file-directory-p assets-dir)
      (make-directory assets-dir))
    (copy-file src dst t)
    (insert (format "[[file:assets/%s]]"
                    (file-name-nondirectory src)))))
(setq org-agenda-files '("~/agarden"))
(setq org-todo-keywords '((sequence "TODO" "DONE")))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-return-follow-link t)


(use-package howm
  :config
  (setq howm-file-name-format "%Y-%m-%d.org")
  (add-hook 'org-mode-hook 'howm-mode)
  )
