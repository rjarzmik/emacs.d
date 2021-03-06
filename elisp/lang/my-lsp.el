;;; My personnal LSP-MODE configuration

(package-initialize)
(require 'use-package)

(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp-deferred)
  :custom
  (lsp-clients-clangd-executable "clangd") ;; or use ccls package to get call
                                         ;; hierarchy lsp extension
  (lsp-auto-guess-root t)                ;; auto guess root
  (lsp-prefer-capf t)                    ;; using `company-capf' by default
  (lsp-keymap-prefix "C-c l"))

(use-package helm-lsp)

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :bind (:map company-mode-map
              ([remap completion-at-point] . company-complete))
  :bind (:map lsp-mode-map
	      ([remap xref-find-apropos] . helm-lsp-workspace-symbol))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-show-numbers t)
  (company-require-match nil)
  (company-tooltip-align-annotations t)
  (company-backends '(company-capf)))

(provide 'my-lsp)

