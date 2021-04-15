;; (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
;; (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; (defun web-mode-init-hook ()
;;   "Hooks for Web mode.  Adjust indent."
;;   (setq web-mode-markup-indent-offset 2))
;; (add-hook 'web-mode-hook  'web-mode-init-hook)

;; ;; typescript settings
;; (setq-default typescript-indent-level 2)
;; (add-to-list 'auto-mode-alist '("\\.tsx?$" . typescript-mode)) ;; auto-enable for .ts/.tsx files
;; (add-to-list 'auto-mode-alist '("\\.js?$" . typescript-mode)) ;; auto-enable for .ts/.tsx files
;; (setq typescript-mode-content-types-alist '(("tsx" . "\\.ts[x]?\\'")))
;; (setq typescript-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; (setq package-check-signature nil)

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))

;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; ;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))
;; ;; enable typescript-tslint checker
;; (flycheck-add-mode 'typescript-tslint 'web-mode)

;; (add-hook 'js2-mode-hook #'setup-tide-mode)
;; ;; configure javascript-tide checker to run after your default javascript checker
;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "jsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))
;; ;; configure jsx-tide checker to run after your default jsx checker
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

;; (use-package tide
;;              :ensure t
;;              :after (typescript-mode company flycheck)
;;              :hook ((typescript-mode . tide-setup)
;;                     (typescript-mode . tide-hl-identifier-mode)
;;                     (before-save . tide-format-before-save)))


;;; ---- Changes branch ---
;; Javascript/Typescript stuff

(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
(setq-default flycheck-temp-prefix ".flycheck")

(add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode)) ;; auto-enable for .js/.jsx files
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)



;; (use-package rjsx-mode
;;   :mode ("\\.js\\'"
;;          "\\.jsx\\'")
;;   :config
;;   (setq js2-mode-show-parse-errors nil
;;         js2-mode-show-strict-warnings nil
;;         js2-basic-offset 2
;;         js-indent-level 2)
;;   (setq-local flycheck-disabled-checkers (cl-union flycheck-disabled-checkers
;;                                                    '(javascript-jshint))) ; jshint doesn't work for JSX
;;   (electric-pair-mode 1))


(use-package prettier-js
  :defer t
  :diminish prettier-js-mode
  :hook (((js2-mode rjsx-mode) . prettier-js-mode)))

;; (add-hook 'js2-mode-hook 'rjsx-mode 'prettier-js-mode)
;; (add-hook 'js2-mode-hook 'rjsx-mode
;;           (lambda ()
;;             (add-hook 'before-save-hook 'prettier-js nil 'make-it-local)))

;; (use-package tide
;;              :ensure t
;;              :after (typescript-mode company flycheck)
;;              :hook ((typescript-mode . tide-setup)
;;                     (typescript-mode . tide-hl-identifier-mode)
;;                     (before-save . tide-format-before-save)))


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; ;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

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
        lsp-ui-flycheck-enable nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-peek-enable nil
        lsp-ui-imenu-enable nil
        lsp-enable-file-watchers nil
        lsp-ui-doc-enable nil))

;; End javascript/typescript section
