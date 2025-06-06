;;; My personnal LSP-MODE configuration

(require 'use-package)

(use-package lsp-mode
  :ensure t
  :hook
  (c-mode . lsp)
  (rust-mode . lsp)
  (java-mode . lsp)
  :custom
  (lsp-clients-clangd-executable "clangd") ;; or use ccls package to get call
                                         ;; hierarchy lsp extension
  (lsp-auto-guess-root t)                ;; auto guess root
  (lsp-prefer-capf t)                    ;; using `company-capf' by default
  (lsp-keymap-prefix "C-c l")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  :config
  (progn
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                      :major-modes '(c-mode c++-mode)
                      :remote? t
                      :server-id 'clangd-remote))))

(use-package lsp-ui
  :ensure
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; inline errors
(use-package flycheck :ensure)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; auto-completion and code snippets
(use-package helm-lsp :ensure)

;;; Completion in LSP mode
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete))
  (:map company-mode-map
        ([remap completion-at-point] . company-complete))
  (:map lsp-mode-map
	([remap xref-find-apropos] . helm-lsp-workspace-symbol))
  :custom
  (company-idle-delay 5)
  (company-echo-delay 0)
  (company-show-numbers t)
  (company-require-match nil)
  (company-tooltip-align-annotations t)
  (company-tng-configure-default)        ; Tab cycle
  (setq company-selection-wrap-around t) ; Wrap around last/first
  (company-backends '(company-capf)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; setting up yassnippet
(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))
(use-package yasnippet-snippets
  :requires yasnippet)

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(provide 'my-lsp)

