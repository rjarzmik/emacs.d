;; Copilot
(use-package copilot
  :hook (copilot-mode . copilot-clear-overlay)
  :bind (:map copilot-completion-map
	      ("C-<return>" . copilot-accept-completion)
	      ("M-<return>" . copilot-next-completion))
  :config
  (setq copilot-idle-delay 0.5)
  (add-to-list 'copilot-indentation-alist '(rustic-mode rustic-indent-offset))
  (add-hook 'prog-mode-hook 'copilot-mode))

;; AI Assited Coding
(use-package gptel
  :ensure t
  :config
  (gptel-make-gh-copilot "Copilot" :host "api.business.githubcopilot.com")
  )
