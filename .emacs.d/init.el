;; -*- mode: lisp -*-

;; package handling
(package-initialize)
(require 'cask (car (file-expand-wildcards "~/.emacs.d/*/cask-*/cask.el")))
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; add legacy
(add-to-list 'load-path "~/.emacs.d/fdlcap")

;; turn on good shit
(set-language-environment "ASCII")
(show-paren-mode t)
(transient-mark-mode t)
(global-font-lock-mode t)
(delete-selection-mode t)
(column-number-mode t)
(ido-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;; turn off bad shit
(if (featurep 'tool-bar)   (tool-bar-mode   -1))
(if (featurep 'tabbar)     (tabbar-mode     -1))
(if (featurep 'tooltip)    (tooltip-mode    -1))
(if (featurep 'scroll-bar) (scroll-bar-mode -1))
(if (featurep 'menu-bar)   (menu-bar-mode   -1))
(defun ido-kill-emacs-hook () (ignore-errors (ido-save-history)))
(setq-default indent-tabs-mode nil)
(setq
 special-display-regexps  nil
 align-to-tab-stop        nil
 inhibit-startup-screen   t
 utf-translate-cjk-mode   nil
 visible-bell             t)

;; set theme
(if (fboundp 'custom-available-themes)
    (if (member 'tango-dark (custom-available-themes))
        (load-theme 'tango-dark)))

;; configs
(setq
 default-input-method     "swedish-postfix"
 max-lisp-eval-depth      40000
 scroll-down-aggressively 0.1
 scroll-up-aggressively   0.1
 user-mail-address        "mats.cronqvist@gmail.com")

;; keybindings
(global-set-key (kbd "M-'")     'flycheck-next-error)
(global-set-key (kbd "C-c a")   'align-regexp)
(global-set-key (kbd "C-c b")   'bury-buffer)
(global-set-key (kbd "C-c p")   'point-to-register)
(global-set-key (kbd "C-c r")   'register-to-point)
(global-set-key (kbd "M--")     `shrink-window)
(global-set-key (kbd "M-+")     `enlarge-window)
(global-set-key (kbd "M-[")     `previous-error)
(global-set-key (kbd "M-]")     `next-error)
(global-set-key (kbd "M-d")     'backward-delete-char-untabify)
(global-set-key (kbd "C-M-u")   `fdlcap-change-case-current-word)
(global-set-key (kbd "C-M-t")   `transpose-lines)
(global-set-key (kbd "C-M-v")   `scroll-down)
(global-set-key (kbd "C-M-c")   `compile)
(global-set-key (kbd "C-M-d")   `kill-current-word)
(global-set-key (kbd "C-z")     'undo) ; be like a mac
(global-set-key (kbd "M-z")     'undo) ; if screen eats C-z
(global-set-key (kbd "C-x C-r") 'revert-buffer)

;; erlang stuff
(defun my-after-init-hook ()
  (require 'edts-start))
(add-hook 'after-init-hook 'my-after-init-hook)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;(require 'flycheck-rebar3)
;;(flycheck-rebar3-setup)

(defun my-erlang-setup ()
  (setq flycheck-erlang-include-path '("../include"))
  (setq safe-local-variable-values
        (quote ((allout-layout . t)
                (erlang-indent-level . 4)
                (erlang-indent-level . 2))))
  (defun erl-file-header ()
    "insert my very own erlang file header"
    (interactive)
    (insert "%% -*- mode: erlang; erlang-indent-level: 2 -*-\n")
    (insert "%% @doc\n")
    (insert "%% @end\n\n")
    (insert (concat "-module(" (erlang-get-module-from-file-name) ").\n\n"))
    (insert (concat "-export([]).\n\n")))

  (add-hook 'erlang-new-file-hook 'my-erlang-new-file-hook)
  (defun my-erlang-new-file-hook ()
    (erl-file-header))

    ;; stupid electricity
    (set-variable 'erlang-electric-commands nil)
    ;; make hack for compile command
    ;; uses Makefile if it exists, else looks for ../inc & ../ebin
    (unless (null buffer-file-name)
      (make-local-variable 'compile-command)
      (setq compile-command
            (cond ((file-exists-p "Makefile")  "make -k")
                  ((file-exists-p "../Makefile")  "make -kC..")
                  (t (concat
                      "erlc "
                      (if (file-exists-p "../ebin") "-o ../ebin " "")
                      (if (file-exists-p "../include") "-I ../include " "")
                      "+debug_info -W " buffer-file-name))))))

;; javascript stuff
(defun my-js-setup()
  (autoload 'js2-mode "js2" nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (autoload 'json-mode "json" nil t)
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))

(add-hook 'json-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(defun my-js2-mode-hook ()
  (js2-leave-mirror-mode)
  (setq js2-mirror-mode nil
        js2-bounce-indent-p t
        js2-cleanup-whitespace t
        js2-mode-indent-ignore-first-tab t
        js2-basic-offset 2))

(defun my-whitespace-setup()
  (require 'whitespace)
  (setq whitespace-style (list 'face 'tabs 'trailing 'lines-tail)
        whitespace-line-column 79)
  (global-whitespace-mode t))

;; this is default in emacs 24.4
(if (locate-library "uniquify")
    (progn
      (require 'uniquify)
      (setq uniquify-buffer-name-style 'post-forward-angle-brackets)))

(if (locate-library "fdlcap")
    (require 'fdlcap))

(if (locate-library "magit")
    (require 'magit))

(if (locate-library "js2")
    (my-js-setup))

(if (locate-library "erlang-start")
    (progn
      (require 'erlang-start)
      (my-erlang-setup)))

(if (locate-library "whitespace")
    (my-whitespace-setup))

(if (locate-library "git")
    (require 'git))

(if (locate-library "git-blame")
    (progn
      (require 'format-spec)
      (require 'git-blame)))

(if (locate-library "sml-modeline")
    (progn
      (require 'sml-modeline)
      (setq sml-modeline-numbers 'line-numbers)
      (define-globalized-minor-mode global-sml-modeline-mode
        sml-modeline-mode
        (lambda ()
          (sml-modeline-mode t)))
      (global-sml-modeline-mode t)))

(if (locate-library "highlight-parentheses")
    (progn
      (require 'highlight-parentheses)
      (setq hl-paren-colors '("firebrick1" "color-160" "color-88"
                              "IndianRed4" "brightred" "white"))
      (define-globalized-minor-mode global-highlight-parentheses-mode
        highlight-parentheses-mode
        (lambda ()
          (highlight-parentheses-mode t)))
      (global-highlight-parentheses-mode t)))

(defun my-outline ()
  (setq outline-minor-mode-prefix "")
  (outline-minor-mode))

(add-hook 'text-mode-hook 'my-text-mode-hook)
(defun my-text-mode-hook ()
  (setq fill-column 79)
  (setq ispell-program-name "aspell")
  (if (locate-library "highlight-parentheses")
      (highlight-parentheses-mode -1))
  (if (locate-library "flyspell")
      (progn
        (flyspell-mode)
        (setq flyspell-dictionaries (quote ("american" "svenska"))))))

(defun indent-buffer ()
  "indent current buffer"
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(if window-system
    (set-background-color "black"))

;; automatically added stuff

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#000"))))
 '(ediff-current-diff-A ((t (:background "color-23"))))
 '(ediff-current-diff-B ((t (:background "color-52"))))
 '(magit-diff-add ((t (:foreground "green"))))
 '(magit-diff-del ((t (:foreground "color-169"))))
 '(magit-item-highlight ((t (:background "color-234"))))
 '(sml-modeline-end-face ((t (:inherit match :foreground "black"))))
 '(sml-modeline-vis-face ((t (:inherit region :foreground "black")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(purescript-mode-hook (quote (turn-on-purescript-indentation))))
