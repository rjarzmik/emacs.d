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
