(require-package 'diff-hl)
(require-package 'neotree)
(require-package 'web-mode)
;;(require-package 'ace-window)
(require-package 'smartparens)
(require-package 'monokai-theme)
(require-package 'jazz-theme)
(require-package 'material-theme)
(require-package 'zenburn-theme)
(require-package 'rainbow-mode)
(require-package 'vkill)
(require-package 'xkcd)
(require-package 'nyan-mode)
(require-package 'helm)
(require-package 'bind-key)
(require-package 'recentf)

(unless (version<= emacs-version "24.4")
  (require-package 'ranger))

;;(require 'smartparens-config)
(require 'helm-config)
(require 'sr-speedbar)
(require 'bind-key)

;; ------------
;; Functions
;; ------------

(defun go-to-speedbar ()
  (interactive)
  (let ((f-name (buffer-name)))
    (sr-speedbar-open)
    (sr-speedbar-select-window)
    (goto-line 1)
    (search-forward-regexp (concat f-name "$"))
    (speedbar-expand-line)
    ))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defun toggle-comment-on-line ()
  "Comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun revert-this-buffer ()
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " (buffer-name))))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;; ---------------
;; End Functions
;; ---------------

(global-linum-mode 1)
(global-diff-hl-mode 1)
;; (rainbow-delimiters-mode 1)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'nyan-mode)

(unless (version<= emacs-version "24.4")
  (global-prettify-symbols-mode 0))

(autoload 'vkill "vkill" nil t)
(autoload 'list-unix-processes "vkill" nil t)

;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode 1)

(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'js-indent-level 'tab-width)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

(add-hook 'python-mode-hook
		  (lambda ()
			(setq-default indent-tabs-mode t)
			(setq-default tab-width 4)
			(setq-default py-indent-tabs-mode t)
			(rainbow-delimiters-mode 1)
			))

(setq speedbar-use-images nil)
(setq fci-rule-color "navy")
(blink-cursor-mode 0)
(global-hl-line-mode 1)

(setq org-replace-disputed-keys t)
(setq recentf-max-saved-items 50)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

;; ---------------
;; Key Bindings
;; ---------------

;;(windmove-default-keybindings 'meta)
;;(global-set-key (kbd "C-c j") 'windmove-left)
;;(global-set-key (kbd "C-c l") 'windmove-right)
;;(global-set-key (kbd "C-c i") 'windmove-up)
;;(global-set-key (kbd "C-c k") 'windmove-down)


(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)
(global-set-key (kbd "C-M-s") 'go-to-speedbar)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(global-set-key (kbd "C-M-,") 'select-current-line)
(global-set-key (kbd "C-M-.") 'toggle-comment-on-line)
(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-x w") 'whitespace-mode)
(global-set-key (kbd "M-]") 'previous-buffer)
(global-set-key (kbd "M-[") 'next-buffer)

(global-set-key (kbd "C-M-{")
(lambda () (interactive) (next-line 5)))

(global-set-key (kbd "C-M-}")
(lambda () (interactive) (previous-line 5)))

(global-set-key (kbd "C-M-[")
(lambda () (interactive) (scroll-up-line 5)))

(global-set-key (kbd "C-M-]")
(lambda () (interactive) (scroll-down-line 5)))

;; Helm key bindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-RET") 'helm-imenu)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)

(global-set-key [f1] 'shell)
(global-set-key [f2] 'rgrep)
(global-set-key [f3] 'dired-find-file)
(global-set-key [f6] 'revert-this-buffer)

;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)
;; (bind-key* "<C-x C-r>" 'ido-recentf-open) ;

(global-set-key (kbd "C-S-c") 'auto-complete)
(global-set-key (kbd "C-c d") 'duplicate-line)

(add-hook 'c-mode-common-hook
		  (lambda() 
			(local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; ------------------
;; End Key Bindings
;; ------------------

(load-theme 'material t)

(provide 'init-local)
