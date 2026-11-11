;;; tree-sitter.el --- Configuration Emacs -*- lexical-binding: t; elisp-autofmt-load-packages-local: ("use-package" "use-package-core"); -*-
;;; My personnal treesitter configuration

(use-package treesit
  :custom
  ;; Enable treesitter for every available mode
  (treesit-enabled-modes t))
