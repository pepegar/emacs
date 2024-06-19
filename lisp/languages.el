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

(use-package eglot
  :custom
  (eglot-connect-timeout (* 30 60))
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs '(zig-mode . ("zls")))
  (add-to-list 'eglot-server-programs '((rust-ts-mode rust-mode) . ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  :hook (python-mode . eglot-ensure)
  (zig-mode . eglot-ensure)
  :bind (("M-RET" . eglot-code-actions)))

(use-package zig-mode
  :ensure
  :straight t
  :mode (("\\.zig\\'" . zig-mode)
         ("\\.zon\\'" . zig-mode))
  )

(use-package treesit-auto
  :ensure t
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package markdown-ts-mode
  :ensure t
  :straight t
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer 't
  :config
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))
