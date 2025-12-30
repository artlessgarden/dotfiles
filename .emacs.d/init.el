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


;;; better
(setq column-number-mode t
      mode-line-in-non-selected-windows t)

(setq switch-to-buffer-obey-display-actions t)

(setq-default truncate-lines t)
(load-theme 'wombat t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq auto-save-visited-interval 1) ; 先设定
(auto-save-visited-mode 1) ; 再打开
(setq scroll-conservatively 101)   ; 光标靠近边界才滚动
(setq scroll-margin 2)             ; 顶部/底部预留2行
(setq scroll-step 1)               ; 每次滚动一行
(setq redisplay-dont-pause t)
(electric-pair-mode t)
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


(use-package dabbrev
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;;(icomplete-vertical-mode t)
;;(fido-vertical-mode 1)
;; (setq completion-styles '(initials flex substring))
;; (setq completion-category-overrides
;;       '((file (styles partial-completion))))

(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(setq load-prefer-newer t)
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;;dired
(setq dired-kill-when-opening-new-dired-buffer t)
;; (add-hook 'dired-mode-hook #'dired-hide-details-mode)
(setq dired-listing-switches "-alh")
(add-hook 'dired-mode-hook 'auto-revert-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq dired-dwim-target t) ;当两个 dired 窗口打开时，自动从一个位置复制到另一个位置
;;; org
(setq org-ellipsis "…")
(setq org-hide-leading-stars t)
(add-hook 'org-mode-hook #'org-indent-mode)

(which-key-mode 1)
;;; manual package
;; (require 'vv-mode)

;; package init
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;;(unless package-archive-contents
;;  (package-refresh-contents))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(require 'my-package)
