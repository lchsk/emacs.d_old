;; (require-package 'tidy)
(require-package 'web-mode)
;; (add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map)))

;; (require-package 'tagedit)
;; (after-load 'sgml-mode
;; (tagedit-add-paredit-like-keybindings)
;; (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-auto-mode 'web-mode "\\.\\(jsp\\|tmpl\\)\\'")

;; Note: ERB is configured in init-ruby-mode

(provide 'init-html)
