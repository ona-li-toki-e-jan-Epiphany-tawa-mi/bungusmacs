;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is part of bungusmacs.                                           ;;
;;                                                                            ;;
;; Bungusmacs is free software: you can redistribute it and/or modify it      ;;
;; under the terms of the GNU General Public License as published by the Free ;;
;; Software Foundation, either version 3 of the License, or (at your option)  ;;
;; any later version.                                                         ;;
;;                                                                            ;;
;; Bungusmacs is distributed in the hope that it will be useful, but WITHOUT  ;;
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or      ;;
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for   ;;
;; more details.                                                              ;;
;;                                                                            ;;
;; You should have received a copy of the GNU General Public License along    ;;
;; with bungusmacs. If not, see <https://www.gnu.org/licenses/>.              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bungusmacs is my personal emacs configuration.
;;
;; Here's the features:
;; - No auto-saving + excess whitespace removal on file save.
;; - Removes the various ugly bars and tooltips.
;; - Random dark theme for pleasent viewing.
;; - Battery level (if applicable) and time in mode line.
;; - Line numbers except where they do not belong (if I notice, that is) +
;;   column numbers.
;; - Rainbow delimiters (though they kinda hard to see fr fr.)
;; - Autoboot into dired on your home directory in fullscreen mode.
;; - C-S-SPC to mark the whole file.
;; - C-c d to duplicate a line/selection (thank the heavens for stackoverflow.)
;; - Create extra cursor above/below with <C-S-up>/<C-S-down>.
;; - <C-S-backspace> to delete text from the current position to the start of a
;;   line.
;; - Completion suggestions for keybinds.
;; - Highlighting of trailing whitespace.
;; - Projectile, with C-c p as the base keybind.
;; - Magit, with C-c m to open magit-status.
;; - lsp-mode, with C-c l to active and C-c l as the base keybind + C-c C-i for
;;   a flymake buffer diagnostics buffer.
;; - Autocompletion.
;; - Indentation set to 4 spaces, minus the following exceptions:
;;    > 3 spaces in cobol-mode.
;; - Modes for the following non-builtin languages:
;;    > Haskell.
;;    > Typescript.
;;    > Arduino-C++.
;;    > Minecraft function files (the command language.)
;;    > COBOL.
;;    > APL.
;;    > BASIC.
;;
;; Author: ona li toki e jan Epiphany tawa mi.
;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc. configuration options.                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI deuglyfication.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; UI enhancification.
; Why does Emacs display the line number without the column number by default???
(column-number-mode)
; Removes display of system load from display-time-mode
(setq display-time-default-load-average nil)
(display-time-mode 1)
(display-battery-mode 1)

;; Run dired and open home directory on starup.
(setq inhibit-startup-screen t)
(setq initial-buffer-choice (expand-file-name "~"))
;; Starts emacs in fullscreen mode.
(add-to-list 'default-frame-alist #'(fullscreen . maximized))

;; Sets general indentation to 4 spaces
(setq-default tab-width 4)
(setq-default indent-line-function #'insert-tab)
(setq-default indent-tabs-mode nil)
;; Special sauce for c++
(setq c-set-style "k&r")
(setq c-basic-offset 4)

(defun bungusmacs/prog-mode-setup ()
  "Runs various setup functions for prog-mode."
  (display-line-numbers-mode 1)
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook #'bungusmacs/prog-mode-setup)

;; No more shitty *~ files.
(setq make-backup-files nil)

;; Automatically removes excess whitespace before saving.
(add-hook 'before-save-hook #'whitespace-cleanup)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-package keybinds.                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-S-SPC") #'mark-whole-buffer)

;; Duplicates a line with C-c d.
;; https://stackoverflow.com/a/4717026
(defun bungusmacs/duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region ; Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ; Go to beginning of next line,
                                                 ; or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1))) ; Insert N times, or once if not specified
          (insert text))))
    (if use-region nil ; Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ; Save column
        (if (> 0 n) ; Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))
(global-set-key (kbd "C-c d") #'bungusmacs/duplicate-line-or-region)

;; Deletes text from the cursor to the start of a line with C-S-backspace
(fset 'bungusmacs/delete-from-here-to-start-of-line
   [?\C-  ?\C-a backspace])
(global-set-key (kbd "<C-S-backspace>") #'bungusmacs/delete-from-here-to-start-of-line)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library and package system setup.                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets up package sources.
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Initializes the package system.
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Auto-installs use-package for auto-installing required packages if not already installed.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


;; Local-elisp-file load path.
(setq load-path (cons "~/.emacs.d/mcf" load-path))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library and package configuration options.                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :bind ("<C-S-up>"   . #'mc/mark-previous-like-this)
        ("<C-S-down>" . #'mc/mark-next-like-this))

(use-package which-key
  :config (which-key-mode 1))



(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes
  :custom (doom-themes-enable-bold t)
          (doom-themes-enable-italic t)
  :config (load-theme 'doom-Iosvkem t))



(defun bungusmacs/cobol-mode-setup ()
  "Runs various setup functions for cobol-mode."
  ;; Disables auto indentation and sets custom sizing.
  (electric-indent-mode -1)
  (setq tab-width 3))
(use-package cobol-mode
  :hook (cobol-mode . bungusmacs/cobol-mode-setup)
  :mode ("\\.[cC][oO][bB]\\'"
         "\\.[cC][bB][lL]\\'"
         "\\.[cC][pP][yY]\\'")
  :init (setq cobol-tab-width 3))

;; TODO set up dyalog key combos.
(use-package dyalog-mode
  :mode "\\.apl\\'")

(use-package haskell-mode)

(use-package arduino-mode)

(use-package basic-mode)

(use-package typescript-mode)

(require 'mcf-mode)
(defun bungusmacs/mcf-mode-setup ()
  "Runs various setup functions for mcf-mode."
  (electric-indent-mode -1))
(add-hook 'mcf-mode-hook #'bungusmacs/mcf-mode-setup)

(use-package scad-mode)

(use-package cmake-mode)


(defun bungusmacs/lsp-mode-setup ()
  "Runs various setup functions for lsp-mode."
  ;; Cool breadcrumb stuff at top of file.
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode 1)
  ;; Easy keybind for diagnostics buffer
  (local-set-key (kbd "C-c C-i") #'flymake-show-buffer-diagnostics))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom (lsp-keymap-prefix "C-c l")
  :bind ("C-c l" . lsp-mode)
  :hook (lsp-mode . bungusmacs/lsp-mode-setup)
  :config (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package company
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-suggestion))
  :custom (company-idle-delay 0.0))



(use-package projectile
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom (projectile-project-search-path '(("~/Proyekty/" . 2))))

(use-package magit
  :bind ("C-c m" . magit-status))



;; Baby gets put in the corner))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(mcf/mcf-mode lsp-ui company cmake-mode which-key-posframe which-key scad-mode lsp-mode magit projectile typescript-mode basic-mode arduino-mode haskell-mode rainbow-delimiters dyalog-mode cobol-mode use-package multiple-cursors doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
