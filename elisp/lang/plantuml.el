;;; plantuml.el --- Configuration Emacs -*- lexical-binding: t; elisp-autofmt-load-packages-local: ("use-package" "use-package-core"); -*-
;;; Robert Jarzmik
;;; 2025-03-28
;;; Plantuml configuration

(use-package plantuml-mode
  :config
  (setq plantuml-jar-path (concat user-emacs-directory "bin/plantuml.jar"))
  (setq plantuml-default-exec-mode 'jar))
