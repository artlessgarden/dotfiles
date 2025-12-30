;; -*- lexical-binding: t; -*-

(setq native-comp-deferred-compilation nil)
;;(setq native-comp-async-report-warnings-errors nil)
;;(setq native-comp-speed 3
;;      native-comp-deferred-compilation t
;;      package-native-compile t)
;;(setq warning-minimum-level :error)
;;(setq native-comp-jit-compilation-deny-list '(".*-loaddefs.el.gz"))
;;(setq completion-auto-help nil)

;;启动优化
(setq gc-cons-threshold (* 50 1000 1000))



(prefer-coding-system 'utf-8-unix)
;;设置字体
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font t charset
                    (font-spec :family "WenQuanYi Micro Hei Mono")))

(set-face-attribute 'default nil :family "Inconsolata" :height 140)





;;启动界面
(setq inhibit-startup-screen t)
(setq initial-buffer-choice (lambda () (progn   (dired "~/")   )))

;;(add-hook 'emacs-startup-hook (lambda ()
;;                                (when (get-buffer "*scratch*")
;;                                  (kill-buffer "*scratch*"))))


;;隐藏message
;;(setq-default message-log-max nil)
;;(kill-buffer "*Messages*")

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp")
