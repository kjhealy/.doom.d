;;; custom.el -*- lexical-binding: t; -*-

;; Fix giant title bar
(add-hook 'doom-after-init-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0)))
