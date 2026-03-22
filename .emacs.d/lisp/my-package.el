
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )


;; Enable Vertico.
(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package sis
  ;; :hook
  ;; 为指定的缓冲区启用 /context/ 和 /inline region/ 模式
  ;; (((text-mode prog-mode) . sis-context-mode)
  ;;  ((text-mode prog-mode) . sis-inline-mode))
  :config
  (sis-ism-lazyman-config "1" "2" 'fcitx5)
  ;; 启用 /光标颜色/ 模式
  (sis-global-cursor-color-mode t)
  ;; 启用 /respect/ 模式
  (sis-global-respect-mode t)
  ;; 为所有缓冲区启用 /context/ 模式
  (sis-global-context-mode t)
  ;; 为所有缓冲区启用 /inline english/ 模式
  (sis-global-inline-mode t)
  )

(use-package avy)

(use-package link-hint
  :config
  (setq browse-url-generic-args '("--target" "window"))
  )



(use-package activities
  :init
  (activities-mode)
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          ;; "\\*scratch\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
          "^\\*EGLOT .*\\*$"
          "\\*eldoc.*\\*"
          "\\*Flymake diagnostics.*\\*"
          "\\*xref\\*"))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

;;; org package

(use-package org-appear
  :commands (org-appear-mode)
  :hook     (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t)  ; Must be activated for org-appear to work
  (setq org-appear-autoemphasis   t   ; Show bold, italics, verbatim, etc.
        org-appear-autolinks      t   ; Show links
	    org-appear-autosubmarkers t)) ; Show sub- and superscripts

;; (use-package cnfonts
;;  :config (cnfonts-mode))

(use-package magit
  :bind (("C-x g" . magit-status)))


(use-package idle-highlight-mode
  :config (setq idle-highlight-idle-time 0.2)
  :hook ((prog-mode text-mode) . idle-highlight-mode))

;;npm -g path
(add-to-list 'exec-path (expand-file-name "~/.local/share/npm/bin"))
(setenv "PATH" (concat (expand-file-name "~/.local/share/npm/bin")
                       path-separator
                       (getenv "PATH")))

(defun my/web-mode-eglot-ensure ()
  (when buffer-file-name
    (setq-local eglot-server-programs
                (cond
                 ((string-match-p "\\.vue\\'" buffer-file-name)
                  '((web-mode . ("vue-language-server" "--stdio"))))
                 ((string-match-p "\\.html?\\'" buffer-file-name)
                  '((web-mode . ("vscode-html-language-server" "--stdio"))))))
    (eglot-ensure)))
(use-package eglot
  :hook ((web-mode . my/web-mode-eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (css-mode . eglot-ensure)
         (css-ts-mode . eglot-ensure))
  :config
  (setq eglot-ignored-server-capabilities
        '(:documentOnTypeFormattingProvider
          :inlayHintProvider))  
  (setq eglot-events-buffer-size 0)
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((eglot (styles orderless))))
  )

;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :hook ((js-mode . lsp)
;;          (js-ts-mode . lsp)
;;          (typescript-mode . lsp)
;;          (typescript-ts-mode . lsp)
;;          (web-mode . lsp)
;;          (css-mode . lsp)
;;          (css-ts-mode . lsp))
;;   :custom
;;   (lsp-keymap-prefix "C-c l")
;;   (lsp-headerline-breadcrumb-enable nil)
;;   (lsp-enable-symbol-highlighting t)
;;   (lsp-enable-on-type-formatting nil)
;;   (lsp-idle-delay 0.8)
;;   (lsp-log-io nil)
;;   :config
;;   (setq lsp-completion-provider :capf))
;; (use-package lsp-ui
;;   :ensure t
;;   :after lsp-mode
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-enable t)
;;   (lsp-ui-doc-show-with-cursor t)
;;   (lsp-ui-doc-show-with-mouse nil)
;;   (lsp-ui-doc-delay 0.8)
;;   (lsp-ui-sideline-enable nil)
;;   (lsp-ui-doc-position 'at-point)
;;   )

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install t)
  :config
  (global-treesit-auto-mode)
  (setq treesit-font-lock-level 4))

(use-package web-mode
  :ensure t
  :mode ("\\.vue\\'" "\\.html?\\'")
  :config
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-content-types-alist
        '(("vue" . "\\.vue\\'"))))


(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)       ;; 选中下一个同名词
   ("C-<" . mc/mark-previous-like-this)   ;; 选中上一个
   ("C-c C-<" . mc/edit-lines)))          ;; 每行行首添加光标

(use-package corfu
  :init
  (global-corfu-mode) ;; 全局开启
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto-prefix 2) ;; 打2个字母才弹
  (corfu-auto t)                 ;; 自动弹出，不用按键
  (corfu-cycle t)                ;; 列表循环
  (corfu-quit-no-match 'separator) ;; 没匹配时自动退出，不挡视线
  (corfu-preselect 'prompt)      ;; 默认选中第一项
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)     ;; TAB 往下选 (习惯问题，可选)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("RET" . nil)
        ("<return>" . nil)
        ("C-n" . nil)
        ("C-p" . nil)
        ("<down>" . nil)
        ("<up>" . nil)
        ([remap previous-line] . nil)
        ([remap next-line] . nil)
        ))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(add-hook 'prog-mode-hook #'flymake-mode)

(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t)

(use-package ace-pinyin)
(use-package avy-zap)
(defun orderless-regexp-pinyin (str)
  (setf (car str) (pinyinlib-build-regexp-string (car str)))
  str)
(advice-add 'orderless-regexp :filter-args #'orderless-regexp-pinyin)


;; (use-package elfeed-org)
;; (use-package elfeed
;;   :init
;;   ;; (setq elfeed-log-level 'error)
;;   (setq rmh-elfeed-org-files '("~/.emacs.d/elfeed.org"))
;;   :config
;;   (require 'elfeed-org)
;;   (elfeed-org)
;;   (global-set-key (kbd "C-x w") #'elfeed))

(use-package apheleia
  :config
  (apheleia-global-mode +1))

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  )

(use-package vterm)
(use-package vterm-toggle
  :config
  (global-set-key [f2] 'vterm-toggle)
  (global-set-key [C-f2] 'vterm-toggle-cd)

  ;; you can cd to the directory where your previous buffer file exists
  ;; after you have toggle to the vterm buffer with `vterm-toggle'.
  (define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)

                                        ;Switch to next vterm buffer
  (define-key vterm-mode-map (kbd "s-n")   'vterm-toggle-forward)
                                        ;Switch to previous vterm buffer
  (define-key vterm-mode-map (kbd "s-p")   'vterm-toggle-backward)

  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3)))
  )

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (global-set-key [f8] 'dired-sidebar-toggle-sidebar)
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

;; (use-package nerd-icons
;;   :custom
;;   (nerd-icons-font-family "Symbols Nerd Font Mono")
;;   )
;; (use-package dirvish
;;   :config
;;   (dirvish-override-dired-mode)
;;   (add-hook 'window-setup-hook
;;             (lambda ()
;;               (delete-other-windows)
;;               (select-window (frame-root-window))
;;               (dirvish "~")))
;;   (dirvish-peek-mode)
;;   (dirvish-side-follow-mode)
;;   (setq dirvish-attributes
;;         '(vc-state file-size git-msg subtree-state nerd-icons collapse file-time))
;;   (setq dirvish-mode-line-format '(:left (sort symlink) :right (vc-info yank index)))
;;   (setq dirvish-header-line-height '(25 . 35))
;;   (setq dirvish-side-width 38)
;;   (setq dirvish-header-line-format '(:left (path) :right (free-space)))
;;   (bind-keys ("C-c f" . dirvish-fd)
;;              :map 'dirvish-mode-map
;;              ;; left click for expand/collapse dir or open file
;;              ("<mouse-1>" . dirvish-subtree-toggle-or-open)
;;              ;; middle click for opening file / entering dir in other window
;;              ("<mouse-2>" . dired-mouse-find-file-other-window)
;;              ;; right click for opening file / entering dir
;;              ("<mouse-3>" . dired-mouse-find-file)
;;              ([remap dired-sort-toggle-or-edit] . dirvish-quicksort)
;;              ([remap dired-do-redisplay] . dirvish-ls-switches-menu)
;;              ([remap dired-do-copy] . dirvish-yank-menu)
;;              ("?"   . dirvish-dispatch)
;;              ("q"   . dirvish-quit)
;;              ("a"   . dirvish-quick-access)
;;              ("f"   . dirvish-file-info-menu)
;;              ("x"   . dired-do-delete)
;;              ("X"   . dired-do-flagged-delete)
;;              ("y"   . dirvish-yank-menu)
;;              ("s"   . dirvish-quicksort)
;;              ("TAB" . dirvish-subtree-toggle)
;;              ("M-t" . dirvish-layout-toggle)
;;              ("M-b" . dirvish-history-go-backward)
;;              ("M-f" . dirvish-history-go-forward)
;;              ("M-n" . dirvish-narrow)
;;              ("M-m" . dirvish-mark-menu)
;;              ("M-s" . dirvish-setup-menu)
;;              ("M-e" . dirvish-emerge-menu))
;;   )



(provide 'my-package)


