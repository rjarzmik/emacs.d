;;; My personnal LSP-MODE configuration

(package-initialize)
(require 'use-package)

(defun dotfiles--lsp-deferred-if-supported ()
  "Run `lsp-deferred' if it's a supported mode."
  (unless (derived-mode-p 'emacs-lisp-mode)
    (lsp-deferred)))

(use-package lsp-mode
  :ensure t
  :hook (prog-mode . #'dotfiles--lsp-deferred-if-supported)
  :custom
  (lsp-clients-clangd-executable "clangd") ;; or use ccls package to get call
                                         ;; hierarchy lsp extension
  (lsp-auto-guess-root t)                ;; auto guess root
  (lsp-prefer-capf t)                    ;; using `company-capf' by default
  (lsp-keymap-prefix "C-c l")
  :config
  (progn
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                      :major-modes '(c-mode c++-mode)
                      :remote? t
                      :server-id 'clangd-remote))))

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

