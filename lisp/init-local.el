(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil)

(electric-indent-mode 0)

(global-unset-key (kbd "C-z"))

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
;; (require-package 'nyan-mode)
(require-package 'helm)
(require-package 'helm-swoop)
;;(require 'helm)
(require 'helm-swoop)
(require-package 'spotify)
(lambda () (spotify-enable-song-notifications))
(require-package 'helm-spotify)
(require 'spotify)
(require-package 'bind-key)
(require-package 'recentf)
(require-package 'magit)
(require-package 'key-chord)
;; (require-package 'hl-todo)
(require-package 'gh-md)
(require-package 'sql-indent)

	(eval-after-load "sql"
  '(load-library "sql-indent"))
;;(key-chord-mode 1)
;; (require-package 'autopair)
;; (require 'autopair)
;; (autopair-global-mode)

(require-package 'hackernews)
(require-package 'wiki-summary)

(require-package 'go-mode)
(require 'go-mode)

;;(require-package 'ensime)
;;(use-package ensime
;;  :commands ensime ensime-mode)
;;(add-hook 'scala-mode-hook 'ensime-mode)

(require-package 'golden-ratio)
(require 'golden-ratio)
(golden-ratio-mode 1)

(setq golden-ratio-auto-scale t)

;; Spaceline

;; (require-package 'spaceline)
;; (require 'spaceline-config)
;; (spaceline-spacemacs-theme)
;; (setq spaceline-minor-modes-p nil)
;; (setq spaceline-buffer-encoding-abbrev-p nil)
;; (setq spaceline-nyan-cat-p nil)

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

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
		(end (line-end-position arg)))
	(when mark-active
	  (if (> (point) (mark))
		  (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
		(setq end (save-excursion (goto-char (mark)) (line-end-position)))))
	(if (eq last-command 'copy-line)
		(kill-append (buffer-substring beg end) (< end beg))
	  (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not '(
                               lambda (x) (
                                           or (buffer-file-name x)
                                              (eq 'dired-mode
                                                  (buffer-local-value 'major-mode x))))
                             (buffer-list)))))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun move-to-window-line-middle ()
  (interactive)
  (let* ((wb-height (window-buffer-height (get-buffer-window)))
        (actual-height (if (> wb-height (window-height))
                           (window-height)
                         wb-height)))
    (move-to-window-line (/ actual-height 2))))

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
;; ---------------
;; End Functions
;; ---------------

(global-linum-mode 1)
(global-diff-hl-mode 1)
;; (rainbow-delimiters-mode 1)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'prog-mode-hook 'nyan-mode)

(unless (version<= emacs-version "24.4")
  (global-prettify-symbols-mode 0))

(autoload 'vkill "vkill" nil t)
(autoload 'list-unix-processes "vkill" nil t)

;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode 1)

(setq tab-width 4)

(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'js-indent-level 'tab-width)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defvaralias 'c-basic-offset 'tab-width)
(setq c-default-style "linux")

(defun my-c-mode-common-hook ()
  (c-set-offset 'substatement-open 0)

  ;; (lambda () (c-toggle-electric-state 1))
  (setq c++-tab-always-indent nil)
  (setq c-basic-offset 4)                  ;; Default is 2
  (setq c-indent-level 4)                  ;; Default is 2

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(add-hook 'python-mode-hook
		  (lambda ()
			(setq-default indent-tabs-mode t)
			(setq-default tab-width 4)
			(setq-default py-indent-tabs-mode t)
			(setq c-basic-offset 4)
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

;; (global-set-key (kbd "C-M-,") 'select-current-line)

;; TODO: Merge those two
(bind-key* "M-;" 'toggle-comment-on-line)
(bind-key* "C-c ;" 'comment-or-uncomment-region)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-x w") 'whitespace-mode)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

;; (global-set-key (kbd "C-M-{")
;; (lambda () (interactive) (next-line 5)))

;; (global-set-key (kbd "C-M-}")
;; 				(lambda () (interactive) (previous-line 5)))

;; (global-set-key (kbd "C-M-[")
;; (lambda () (interactive) (scroll-up-line 5)))

;; (global-set-key (kbd "C-M-]")
;; 				(lambda () (interactive) (scroll-down-line 5)))

(bind-key* "M-n" (lambda () (interactive) (scroll-up-line 5)))
(bind-key* "M-p" (lambda () (interactive) (scroll-down-line 5)))
;; (bind-key* "C-c n" (lambda () (interactive) (next-line 5)))
;; (bind-key* "C-c p" (lambda () (interactive) (previous-line 5)))

(bind-key* "C-c w" (lambda () (interactive) (kill-whole-line)))

(bind-key* "C-c t" (lambda () (interactive) (move-to-window-line 0)))
(bind-key* "C-c b" (lambda () (interactive) (move-to-window-line -1)))
(bind-key* "C-c m" (lambda () (interactive) (move-to-window-line-middle)))
;; (key-chord-define-global "hh" (lambda () (interactive) (move-to-window-line 0)))
;; (key-chord-define-global "ll" (lambda () (interactive) (move-to-window-line -1)))
;;(key-chord-define-global "mm" (lambda () (interactive) (move-to-window-line-middle)))
;;(key-chord-define-global "cc" (lambda () (interactive) (keyboard-escape-quit)))

;; Helm key bindings
(global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "M-RET") 'helm-imenu)
(bind-key* "M-q" (lambda () (interactive) (helm-imenu)))

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(bind-key "M-z" 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(bind-key* "C-c i" 'helm-swoop)
(bind-key* "M-i" 'helm-multi-swoop-all)

(global-set-key [f1] 'shell)
(global-set-key [f2] 'rgrep)
(global-set-key [f3] 'dired-find-file)
(global-set-key [f6] 'revert-this-buffer)

(bind-key* "C-c a" 'auto-complete)
(bind-key* "C-c d" 'duplicate-line)
(bind-key* "C-c p" 'switch-to-previous-buffer)

(bind-key* "C-c c l" 'copy-line)
(bind-key* "C-c k b" 'backward-kill-line)
(bind-key* "C-c k i" (lambda() (interactive) (backward-kill-line 1) (indent-relative)))
(bind-key* "C-c k a" (lambda() (interactive) (smarter-move-beginning-of-line 1) (kill-line)))

(add-hook 'c-mode-common-hook
		  (lambda()
			(local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(global-set-key (kbd "TAB") 'self-insert-command)


;; spotify
(global-set-key (kbd "<pause>") #'spotify-playpause)
(global-set-key (kbd "M-<pause>") #'spotify-next)
;; ------------------
;; End Key Bindings
;; ------------------

(load-theme 'material t)

(provide 'init-local)
