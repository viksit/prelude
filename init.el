;;; init.el --- Prelude's configuration entry point.
;;
;; Copyright (c) 2011-2018 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://batsov.com/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Prelude.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Prelude is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "25.1")
  (error "Prelude requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar prelude-core-dir (expand-file-name "core" prelude-dir)
  "The home of Prelude's core functionality.")
(defvar prelude-modules-dir (expand-file-name  "modules" prelude-dir)
  "This directory houses all of the built-in Prelude modules.")
(defvar prelude-personal-dir (expand-file-name "personal" prelude-dir)
  "This directory is for your personal configuration.

Users of Emacs Prelude are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Prelude.")
(defvar prelude-personal-preload-dir (expand-file-name "preload" prelude-personal-dir)
  "This directory is for your personal configuration, that you want loaded before Prelude.")
(defvar prelude-vendor-dir (expand-file-name "vendor" prelude-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar prelude-savefile-dir (expand-file-name "savefile" prelude-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar prelude-modules-file (expand-file-name "prelude-modules.el" prelude-dir)
  "This files contains a list of modules that will be loaded by Prelude.")

(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

(defun prelude-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (prelude-add-subfolders-to-load-path name)))))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path prelude-core-dir)
(add-to-list 'load-path prelude-modules-dir)
(add-to-list 'load-path prelude-vendor-dir)
(prelude-add-subfolders-to-load-path prelude-vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `prelude-personal-preload-dir'
(when (file-exists-p prelude-personal-preload-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-preload-dir)
  (mapc 'load (directory-files prelude-personal-preload-dir 't "^[^#\.].*el$")))

(message "Loading Prelude's core...")

;; the core stuff
(require 'prelude-packages)
(require 'prelude-custom)  ;; Needs to be loaded before core, editor and ui
(require 'prelude-ui)
(require 'prelude-core)
(require 'prelude-mode)
(require 'prelude-editor)
(require 'prelude-global-keybindings)

;; macOS specific settings
(when (eq system-type 'darwin)
  (require 'prelude-macos))

(message "Loading Prelude's modules...")

;; the modules
(if (file-exists-p prelude-modules-file)
    (load prelude-modules-file)
  (message "Missing modules file %s" prelude-modules-file)
  (message "You can get started by copying the bundled example file from sample/prelude-modules.el"))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p prelude-personal-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-dir)
  (mapc 'load (directory-files prelude-personal-dir 't "^[^#\.].*el$")))

(message "Prelude is ready to do thy bidding, Master %s!" current-user)

;; Patch security vulnerability in Emacs versions older than 25.3
(when (version< emacs-version "25.3")
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end))))


;; @viksit's changes

;; we like font size 13
(set-face-attribute 'default nil :height 130)


;; get the arrow keys back
(global-unset-key (vector (list 'shift 'left)))
(global-unset-key (vector (list 'shift 'right)))
(global-unset-key (vector (list 'shift 'up)))
(global-unset-key (vector (list 'shift 'down)))

;; please don't force me to use your set up.
(defun disable-guru-mode ()
  (guru-mode -1)
)
(add-hook 'prelude-prog-mode-hook 'disable-guru-mode t)

;; enable elpy
(elpy-enable)


;; use a good theme
(load-theme 'wombat t)

;; (disable-theme 'zenburn)
;; (prelude-require-package 'wombat)
;; (setq prelude-theme 'wombat)

;; load the desktop
(desktop-save-mode 1)

;; To set fonts, use M-x customize-face RET default
;; and then set things.

;; some sane commenting techniques
(global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
(global-set-key [C-tab] 'other-window)

;; (toggle-scroll-bar -1)
(scroll-bar-mode -1)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . nil))

;; To use python3 and ipython for REPL
(setq elpy-rpc-python-command "python3")
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i")
(setq elpy-rpc-backend "jedi")

(global-set-key [f8] 'neotree-toggle)


(add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . (lambda () (elpy-enable) (python-mode))))


;; highlight matching parenthesis
(show-smartparens-mode)

(global-auto-revert-mode t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq python-shell-enable-font-lock nil)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)
(setq markdown-command "/usr/local/bin/pandoc")

;;(setq sml/no-confirm-load-theme t)
;;(setq sml/theme 'dark) ;; or 'dark
;;(add-hook 'after-init-hook #'sml/setup)
;;(sml/setup)

;; via https://github.com/bastibe/.emacs.d/blob/master/init.el
;; things to try
;; (ido-mode t)
;; (setq ido-enable-flex-matching t)
;; (ido-vertical-mode)
;; (setq ido-auto-merge-delay-time 1)


;; enable sensible undo
;; (require 'undo-tree)
;; (global-undo-tree-mode)
;; ;; make undo work the same way on the EN and DE keymap
;; (define-key undo-tree-map (kbd "C--") 'undo-tree-undo)
;; (define-key undo-tree-map (kbd "C-_") 'undo-tree-redo)


;; (setq ring-bell-function #'ignore)

;(require 'julia-repl)
;(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode
;; Add path for jupyter integration as well
;; (setq julia-jupyter-conda-path "/Users/viksit/.julia/conda/3/bin:")
;; (setenv "PATH" (concat julia-jupyter-conda-path (getenv "PATH")))
;; (setq exec-path (append exec-path '("/Users/viksit/.julia/conda/3/bin")))

;; Rust moed
;; (add-hook 'rust-mode-hook 'cargo-minor-mode)
;; (with-eval-after-load 'rust-mode
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)

;; (require 'rust-mode)
;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;; (setq company-tooltip-align-annotations t)

(helm-mode 1)
;; (setq helm-projectile-fuzzy-match nil)
(require 'helm-projectile)
(helm-projectile-on)

(require 'helm-config)

(when (package-installed-p 'helm)
  ;; change default prefix key
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  ;; helm-M-x
  (setq helm-M-x-fuzzy-match t)
  (global-set-key (kbd "M-x") 'helm-M-x)

  ;; helm-kill-ring
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)

  ;; helm-mini
  (global-set-key (kbd "C-x b") 'helm-mini)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)

  ;; helm-find-files
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
)

;;; -----------------------------
;;; helm-projectile
(when (package-installed-p 'helm-projectile)
  (projectile-global-mode)
  (helm-projectile-on)
)

;; get back the old helm behavior of having arrow keys
;; move multiple directories back and forward for quicker navigation
;; https://github.com/emacs-helm/helm/issues/2175
;; (define-key helm-map (kbd "<left>") 'helm-previous-source)
;; (define-key helm-map (kbd "<right>") 'helm-next-source)
(customize-set-variable 'helm-ff-lynx-style-map t)


;; Typescript and javascript stuff
;; We want to use - rjsx mode for all JS files


(use-package rjsx-mode
  :mode ("\\.js\\'"
         "\\.jsx\\'")
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-basic-offset 2
        js-indent-level 2)
  (setq-local flycheck-disabled-checkers (cl-union flycheck-disabled-checkers
                                                   '(javascript-jshint))) ; jshint doesn't work for JSX
  (electric-pair-mode 1))


;; (use-package prettier-js
;;   :defer t
;;   :diminish prettier-js-mode
;;   :hook (((js2-mode rjsx-mode) . prettier-js-mode)))

(use-package lsp-mode
 :defer t
 :diminish lsp-mode
 :hook (((js2-mode rjsx-mode) . lsp))
 :commands lsp
 :config
 (setq lsp-auto-configure t
       lsp-auto-guess-root t
       lsp-intelephense-multi-root nil
       ;; don't set flymake or lsp-ui so the default linter doesn't get trampled
       lsp-diagnostic-package :none))

(use-package company-lsp
 :defer t
 :config
 (setq company-lsp-cache-candidates 'auto
       company-lsp-async t
       company-lsp-enable-snippet nil
       company-lsp-enable-recompletion t))

(use-package lsp-ui
 :defer t
 :config
 (setq lsp-ui-sideline-enable t
       ;; disable flycheck setup so default linter isn't trampled
       lsp-ui-flycheck-enable t
       lsp-ui-sideline-show-symbol t
       lsp-ui-sideline-enable t
       lsp-ui-sideline-show-diagnostics t
       lsp-ui-sideline-show-hover t
       lsp-ui-sideline-show-code-actions t
       lsp-ui-peek-enable t
       lsp-ui-imenu-enable t
       lsp-enable-file-watchers nil
       lsp-ui-doc-enable nil))

(setq lsp-signature-auto-activate nil)

;; End JS/TS stuff
(show-paren-mode)
;; end @viksit's edits



(prelude-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'prelude-tip-of-the-day))

;;; init.el ends here

