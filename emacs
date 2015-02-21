(require 'ido)
(ido-mode t)

    (setq ns-alternate-modifier 'meta
	  ns-command-modifier 'meta)

(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))

(global-set-key (kbd "C-x p") 'previous-multiframe-window)

(add-to-list 'load-path "~/.emacs.d/modes/")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.profiles$" . yaml-mode))

(require 'powershell-mode)
(add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode))


(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("7427913d387463ca1aaf75bd09e9b3392c11ba77f468ad3e9c40ec071291db30" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'zenburn t)


(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

(setq default-directory "~/")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(exec-path-from-shell-copy-env "GOPATH")
(exec-path-from-shell-copy-env "GOBIN")
(exec-path-from-shell-copy-env "GOROOT")

(add-to-list 'load-path "~/gocode/src/github.com/dougm/goflymake")
(require 'go-flymake)

(require 'go-eldoc) ;; Don't need to require, if you install by package.el
(add-hook 'go-mode-hook 'go-eldoc-setup)

(set-face-attribute 'eldoc-highlight-function-argument nil
                    :underline t :foreground "green"
                    :weight 'bold)
(require 'auto-complete-config)
(require 'go-autocomplete)
