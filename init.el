;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc. configuration options.                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI deuglyfication.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(load-theme 'wombat)

(column-number-mode)
(global-display-line-numbers-mode 1)
;; Disables line numbers for certain major modes.
(dolist (mode '(shell-mode-hook
		eshell-mode-hook
		term-mode-hook
		dired-mode-hook
		apropos-mode-hook
		help-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq display-time-default-load-average nil) ; Removes display of system load from display-time-mode
(display-time-mode)
(display-battery-mode)

;; Run dired and open home directory on starup.
(setq inhibit-startup-screen t)
(setq initial-buffer-choice (expand-file-name "~"))
;; Starts emacs in fullscreen mode.
(add-to-list 'default-frame-alist '(fullscreen . maximized))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-package keybinds.                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-S-SPC") 'mark-whole-buffer)

;; Duplicates a line with C-c d.
;; https://stackoverflow.com/a/4717026
(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))
(global-set-key (kbd "C-c d") 'duplicate-line-or-region)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package system setup.                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets up package sources.
(require 'package)

(dolist (package '(("melpa" . "https://melpa.org/packages/")))
		   ;;("nongnu-elpa"  . "https://elpa.nongnu.org/nongnu/")))
  (add-to-list 'package-archives package t))

;; Initializes the package system.
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Auto-installs use-package for auto-installing required packages if not already installed.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package configuration options.                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :bind (("<C-S-up>"   . 'mc/mark-previous-like-this)
	 ("<C-S-down>" . 'mc/mark-next-like-this)))


;; Make sure to run M-x nerd-icons-install-fonts.
(use-package nerd-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

  
(use-package cobol-mode)
(setq auto-mode-alist
      (append
       '(("\\.[cC][oO][bB]\\'" . cobol-mode)
         ("\\.[cC][bB][lL]\\'" . cobol-mode)
         ("\\.[cC][pP][yY]\\'" . cobol-mode))
       auto-mode-alist))

;; TODO set up dyalog key combos.
(use-package dyalog-mode)
(setq auto-mode-alist
      (append
       '(("\\.apl\\'" . dyalog-mode))
       auto-mode-alist))

(use-package haskell-mode)

(use-package arduino-mode)

(use-package basic-mode)

(use-package typescript-mode)



;; Baby gets put in the corner))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (typescript-mode basic-mode arduino-mode haskell-mode rainbow-delimiters dyalog-mode cobol-mode use-package multiple-cursors doom-modeline))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
