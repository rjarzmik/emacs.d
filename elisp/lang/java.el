;;; My personnal Java modes
(add-hook 'java-mode-hook
	  (lambda ()
	  (setq indent-tabs-mode t)
	  (setq tab-width 4)
	  (setq c-basic-offset 4)))
