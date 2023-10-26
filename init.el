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
;; Make sure to run <M-x nerd-icons-install-fonts> after installing.
;;
;; NOTE: The garbage collector is set to not run until 100 MB of memory usage
;; during startup and switches to 1 MB afterwards.
;;
;; Here's the features:
;; - Garbage collector tweaking to speed up Emacs at the cost of memory usage.
;; - No auto-saving + excess whitespace removal on file save.
;; - Removes the various ugly bars and tooltips.
;; - Random dark theme for pleasent viewing.
;; - TODO (and similiar) highlighting.
;; - Battery level (if applicable), time, and columns in mode line.
;; - Sexy doom mode line (if in graphical mode.)
;; - Display of keypresses/other in modeline with keycast.
;; - Line numbers in prog-mode.
;; - Automatic import of enviroment variables from .bashrc or similar.
;; - Rainbow delimiters (though they kinda hard to see fr fr.)
;; - Autoboot into dired on your home directory in fullscreen mode.
;; - C-S-SPC to mark the whole file.
;; - C-c d to duplicate a line/selection (thank the heavens for stackoverflow.)
;; - Create extra cursor above/below with <C-S-up>/<C-S-down>. Create cursors on
;;   each match of a selection with C-c s.
;; - <C-S-backspace> to delete text from the current position to the start of a
;;   line.
;; - Completion suggestions for keybinds with which-key.
;; - Highlighting of trailing whitespace.
;; - Projectile, with C-c p as the base keybind.
;; - Magit, with C-c m to open magit-status.
;; - lsp-mode, with C-c l to activate + automatically runs in prog-mode with
;;   C-c l as the base keybind.
;;   Flycheck is added for better integration with lsp-ui.
;; - Autocompletion with company.
;; - Indentation set to 4 spaces, minus the following exceptions:
;;    > 3 spaces in cobol-mode.
;; - Modes for the following non-builtin languages:
;;    > Haskell.
;;    > Typescript.
;;    > Arduino-C++.
;;    > Minecraft functions (the command language.)
;;    > COBOL.
;;    > APL (automatically in APL-Z input mode with ` as the prefix.)
;;    > BASIC.
;;    > OpenSCAD.
;;    > CMake.
;;    > FASM.
;;    > Rust.
;; - Displays startup time on startup.
;;
;; Author: ona li toki e jan Epiphany tawa mi.



;; Temporarily sets garbage collector threshold high to speed up startup times.
(setq gc-cons-threshold (* 100 1000 1000)) ; 100 MB.



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

(defun bungusmacs/display-startup-time ()
  (message "Emacs loaded in %s seconds with %d garbage collections."
           (emacs-init-time "%.2f")
           gcs-done))
(add-hook 'emacs-startup-hook #'bungusmacs/display-startup-time)



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
(add-to-list 'load-path "~/.emacs.d/mcf")
(add-to-list 'load-path "~/.emacs.d/fasm")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library and package configuration options.                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensures enviroment variables are avalible from .bashrc or similar.
(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize)))



(use-package multiple-cursors
  :bind ("<C-S-up>"   . mc/mark-previous-like-this)
        ("<C-S-down>" . mc/mark-next-like-this)
        ("C-c s"      . mc/mark-all-like-this))

(use-package which-key
  :defer 0
  :config (which-key-mode 1))



(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package keycast
  :custom (keycast-mode-line-insert-after '(:eval (doom-modeline-format--main)))
  :init (add-to-list 'global-mode-string '("" keycast-mode-line))
  :hook (doom-modeline-mode . keycast-mode-line-mode))

; Nerd-icons doesn't work right in a terminal.
(when (display-graphic-p)
  (use-package nerd-icons)
  (use-package doom-modeline
    :after nerd-icons
    :init (doom-modeline-mode 1)))

(use-package doom-themes
  :custom (doom-themes-enable-bold t)
          (doom-themes-enable-italic t)
  :config (load-theme 'doom-Iosvkem t))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))



(defun bungusmacs/cobol-mode-setup ()
  "Runs various setup functions for cobol-mode."
  (electric-indent-mode -1)
  (setq tab-width 3))
(use-package cobol-mode
  :hook (cobol-mode . bungusmacs/cobol-mode-setup)
  :mode ("\\.[cC][oO][bB]\\'"
         "\\.[cC][bB][lL]\\'"
         "\\.[cC][pP][yY]\\'")
  :init (setq cobol-tab-width 3))

(defun bungusmacs/gnu-apl-mode-setup ()
  "Runs various setup functions for gnu-apl-mode."
  (electric-indent-mode -1)
  (set-input-method "APL-Z")
  (customize-set-variable 'gnu-apl-key-prefix 96))
(use-package gnu-apl-mode
  ; The value(s) for :mode and :interpreter were pulled from gnu-apl-mode.el to
  ; defer startup.
  :mode "\\.apl\\'"
  :interpreter "apl"
  :hook (gnu-apl-mode . bungusmacs/gnu-apl-mode-setup)
        (gnu-apl-interactive-mode . bungusmacs/gnu-apl-mode-setup)
  :custom (gnu-apl-indent-amounts '(0 2 2 2))
          (gnu-apl-show-keymap-on-startup nil))

(use-package haskell-mode
  ; The value(s) for :mode and :interpreter were pulled from haskell-mode.el to
  ; defer startup.
  :mode ("\\.[gh]s\\'"
         "\\.hsig\\'"
         "\\.l[gh]s\\'"
         "\\.hsc\\'")
  :interpreter ("runghc"
                "runhaskell"))

(use-package arduino-mode
  ; The value(s) for :mode was pulled from arduino-mode.el to defer startup.
  :mode ("\\.pde\\'"
         "\\.ino\\'"))

(use-package basic-mode
  :mode "\\.bas\\'")

(use-package typescript-mode
  ; The value(s) for :mode was pulled from typescript-mode.el to defer startup.
  :mode "\\.tsx?\\'")

(defun bungusmacs/mcf-mode-setup ()
  "Runs various setup functions for mcf-mode."
  (electric-indent-mode -1))
(use-package mcf-mode
  :load-path "mcf-mode.el"
  :hook (mcf-mode-hook . bungusmacs/mcf-mode-setup)
  ; The value(s) for :mode was pulled from mcf-mode.el to defer startup.
  :mode "\\.mcfunction\\'")

(use-package scad-mode
  ; The value(s) for :mode was pulled from scad-mode.el to defer startup.
  :mode "\\.scad\\'")

(use-package cmake-mode
  ; The value(s) for :mode was pulled from cmake-mode.el to defer startup.
  :mode ("CMakeLists\\.txt\\'"
         "\\.cmake\\'"))

(use-package fasm-mode
  :load-path "fasm-mode.el"
  :mode "\\.asm\\'"
        "\\.inc\\'"
  :hook (asm-mode . fasm-mode)) ; makes fasm-mode the default for .asm files.

(use-package rust-mode
  :mode "\\.rs\\'")


(defun bungusmacs/lsp-mode-setup ()
  "Runs various setup functions for lsp-mode."
  ;; Cool breadcrumb stuff at top of file.
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode 1))

(use-package lsp-mode
  :custom (lsp-keymap-prefix "C-c l")
          (lsp-warn-no-matched-clients nil)
          (lsp-pylsp-plugins-pyflakes-enabled t)
          (lsp-pylsp-plugins-pylint-enabled t)
  :bind ("C-c l" . lsp-mode)
  :hook (prog-mode . lsp-mode)
        (lsp-mode . bungusmacs/lsp-mode-setup)
  :config (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package flycheck
  :after lsp-mode)



(use-package company
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-suggestion))
  :custom (company-idle-delay 0.0))



(use-package projectile
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom (projectile-project-search-path '(("~/Proyekty/" . 2)))
          (projectile-switch-project-action #'projectile-dired))



(use-package magit
  :bind ("C-c m" . magit-status))



;; Baby gets put in the corner))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(keycast exec-path-from-shell flycheck rust-mode gnu-apl-mode hl-todo mcf-mode mcf/mcf-mode lsp-ui company cmake-mode which-key-posframe which-key scad-mode lsp-mode magit projectile typescript-mode basic-mode arduino-mode haskell-mode rainbow-delimiters cobol-mode use-package multiple-cursors doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;; Resets the garbage collector threshold so that emacs' memory usage doesn't
;; explode whilst running.
(setq gc-cons-threshold (* 1 1000 1000)) ; 1 MB.
