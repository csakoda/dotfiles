(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'go-mode 'py-autopep8 'go-eldoc 'company 'company-go 'exec-path-from-shell 'flymake-cursor)

;; activate installed packages
(package-initialize)

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'py-autopep8)
(add-hook 'python-mode-hook
          (lambda ()
             (add-hook 'before-save-hook 'py-autopep8-enable-on-save)))

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

(setq jedi:complete-on-dot t)
;; don't use default keybindings from jedi.el; keep C-. free
(setq jedi:setup-keys nil)
(setq jedi:tooltip-method nil)
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)

(defvar jedi:goto-stack '())
(defun jedi:jump-to-definition ()
  (interactive)
  (add-to-list 'jedi:goto-stack
               (list (buffer-name) (point)))
  (jedi:goto-definition))
(defun jedi:jump-back ()
  (interactive)
  (let ((p (pop jedi:goto-stack)))
    (if p (progn
            (switch-to-buffer (nth 0 p))
            (goto-char (nth 1 p))))))

(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-.") 'jedi:jump-to-definition)
             (local-set-key (kbd "C-,") 'jedi:jump-back)
             (local-set-key (kbd "C-c d") 'jedi:show-doc)
             (local-set-key (kbd "C-<tab>") 'jedi:complete)))

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



(require 'go-mode)

(add-hook 'go-mode-hook '(lambda ()
  (local-set-key (kbd "C-c C-k") 'godoc)
  (local-set-key (kbd "C-c C-j") 'godef-jump-other-window)))

;; (setq default-directory "~/")

;; (when (memq window-system '(mac ns))
;;   (progn
;;     (exec-path-from-shell-initialize)


(exec-path-from-shell-initialize)
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

(require 'company)
(require 'company-go)
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)))
