(use-package kotlin-ts-mode
  :ensure t
  :straight t
  :mode "\\.kt\\'")

(use-package nix-ts-mode
  :ensure t
  :straight t
  :mode "\\.nix\\'")

(use-package gradle-mode
  :ensure t
  :straight t
  :diminish gradle-mode
  :init (gradle-mode 1))

(use-package go-mode
  :ensure t
  :straight t)

(use-package jsonnet-mode
  :ensure t
  :straight t)
