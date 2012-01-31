;; -*- mode: lisp -*-

; turn on shit
(set-language-environment "Latin-1")
(show-paren-mode t)
(transient-mark-mode t)
(global-font-lock-mode t)
(delete-selection-mode t)
(column-number-mode t)
(iswitchb-mode t)
(if (fboundp 'load-theme) (load-theme 'tango-dark))
(if (locate-library "package")
    (progn
      (require 'package)
      (package-initialize)
      (add-to-list 'package-archives
		   '("ELPA" . "http://tromey.com/elpa/"))
      (add-to-list 'package-archives
		   '("marmalade" . "http://marmalade-repo.org/packages/"))))

; turn off shit
(if (featurep 'tool-bar) (tool-bar-mode -1))
(if (featurep 'tooltip) (tooltip-mode -1))
(if (featurep 'scroll-bar) (scroll-bar-mode -1))
(if (featurep 'menu-bar) (menu-bar-mode -1))

(setq
 align-to-tab-stop        nil
 default-input-method     "swedish-postfix"
 indent-tabs-mode         nil
 indent-tabs-mode         nil
 inhibit-startup-screen   t
 max-lisp-eval-depth      40000
 scroll-down-aggressively 0.1
 scroll-up-aggressively   0.1
 special-display-regexps  nil
 user-mail-address        "masse@klarna.com"
 utf-translate-cjk-mode   nil
 visible-bell             t)

(global-set-key (kbd "C-c a") 'align-regexp)
(global-set-key (kbd "C-c b") 'bury-buffer)
(global-set-key (kbd "C-c p") 'point-to-register)
(global-set-key (kbd "C-c r") 'register-to-point)
(global-set-key (kbd "M--") `shrink-window)
(global-set-key (kbd "M-+") `enlarge-window)
(global-set-key (kbd "M-[") `previous-error)
(global-set-key (kbd "M-]") `next-error)
(global-set-key (kbd "M-d") 'backward-delete-char-untabify)
(global-set-key (kbd "C-M-u") `fdlcap-change-case-current-word)
(global-set-key (kbd "C-M-t") `transpose-lines)
(global-set-key (kbd "C-M-v") `scroll-down)
(global-set-key (kbd "C-M-c") `compile)
(global-set-key (kbd "C-M-d") `kill-current-word)
(global-set-key (kbd "C-z") 'undo) ; be like a mac
(global-set-key (kbd "M-z") 'undo) ; if screen eats C-z
(global-set-key (kbd "C-x C-r") 'revert-buffer)

(defun add-paths (ps)
  (dolist (f (nreverse ps))
  (when (and (stringp f) (file-exists-p f))
    (add-to-list 'load-path f))))

(add-paths (list "~/elisp"))

(defun my-erlang-setup ()

  (setq safe-local-variable-values
	(quote ((erlang-indent-level . 4)
		(erlang-indent-level . 2))))

  (defvar erlang-erl-path
    (shell-command-to-string "echo -n `brew --prefix erlang`"))
  (defvar erlang-distel-path "~/git/distel")
  (defvar erlang-erlmode-path "~/elisp")

  (add-paths (list 
	      (car (file-expand-wildcards erlang-erlmode-path))
	      (car (file-expand-wildcards
		    (concat erlang-erl-path "/lib/erlang/lib/tools-*/emacs")))
	      (concat erlang-distel-path "/elisp")))

  ;; use to start an erlang shell with boot flags

  (defun erl-shell (flags)
    "Start an erlang shell with flags"
    (interactive (list (read-string "Flags: ")))
    (set 'inferior-erlang-machine-options (split-string flags))
    (erlang-shell))

  (defun erl-file-header ()
    "insert my very own erlang file header"
    (interactive)
    (insert "%% -*- mode: erlang; erlang-indent-level: 2 -*-\n")
    (insert (concat "%%% Created : " (erlang-skel-dd-mmm-yyyy) " by "
                    (user-full-name) " <" erlang-skel-mail-address ">\n\n"))
    (insert "%% @doc\n")
    (insert "%% @end\n\n")
    (insert (concat "-module('" (erlang-get-module-from-file-name) "').\n"))
    (insert (concat "-author('" user-full-name "').\n"))
    (insert (concat "-export([]).\n\n")))

  (add-hook 'erlang-load-hook 'my-erlang-load-hook)
  (defun my-erlang-load-hook ()
    (setq
     ;; syntax haighlighting
     erl-atom-face              'default         ;'font-lock-doc-face
     erl-quotes-face            'font-lock-doc-string-face
     erl-list-operator-face     'font-lock-warning-face
     erl-match-operator-face    'font-lock-warning-face
     erl-operator-face          'font-lock-warning-face
     erl-arrow-face             'font-lock-keyword-face
     erl-ext-function-call-face 'font-lock-constant-face
     erl-int-function-call-face 'font-lock-constant-face
     erl-macro-face             'font-lock-preprocessor-face
     erl-record-face            'font-lock-preprocessor-face

     ;; i need some space
     erlang-indent-level 2
     ;; find the man pages
     setq erlang-root-dir erlang-erl-path))

  (add-hook 'erlang-new-file-hook 'my-erlang-new-file-hook)
  (defun my-erlang-new-file-hook ()
    (erl-file-header))

  (add-hook 'erlang-shell-mode-hook 'my-erlang-shell)
  (defun my-erlang-shell ()
    (setq comint-dynamic-complete-functions
          '(my-erl-complete  comint-replace-by-expanded-history)))

  (defun my-erl-complete ()
    "Call erl-complete if we have an Erlang node name"
    (if erl-nodename-cache
        (erl-complete erl-nodename-cache)
      nil))

  (add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
  (defun my-erlang-mode-hook ()
    (defun erl-align-arrows ()
      (interactive)
      (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)->" 1 1))
    ;; run flymake iff buffer has a file
    (local-set-key (kbd "M-L") 'erl-show-arglist)
    (local-set-key (kbd "M-A") 'erl-align-arrows)
    (if (and (locate-library "erlang-flymake")
             buffer-file-truename)
        (progn
          (setq flymake-no-changes-timeout 3)
          (load "erlang-flymake")
          (flymake-mode)
          (local-set-key (kbd "M-'") 'erlang-flymake-next-error)))
    ;; stupid electricity
    (set-variable 'erlang-electric-commands nil)
    ;; stupid default
    ;; (set-variable 'erlang-indent-level 2)
    ;; make hack for compile command
    ;; uses Makefile if it exists, else looks for ../inc & ../ebin
    (unless (null buffer-file-name)
      (make-local-variable 'compile-command)
      (setq compile-command
            (if (file-exists-p "Makefile")
                "make -k"
              (concat
               "erlc "
               (if (file-exists-p "../ebin") "-o ../ebin " "")
               (if (file-exists-p "../inc") "-I ../inc " "")
               "+debug_info -W " buffer-file-name))))))

(defun my-js-setup()
  (autoload 'js2-mode "js2" nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (setq js2-mirror-mode nil)
  (setq js2-basic-offset 2))

(defun my-whitespace-setup()
  (require 'whitespace)
  (setq whitespace-style (list 'tabs 'trailing 'lines-tail)
        whitespace-line-column 79)
  (global-whitespace-mode t))

(defun my-distel-setup ()
  (require 'distel)
  (distel-setup)
  (setq erl-reload-dwim t))

(defun my-svn-setup ()
  (require 'psvn)
  (setq svn-status-custom-hide-function 'my-svn-status-hide))

(if (locate-library "fdlcap")
    (require 'fdlcap))

(if (locate-library "magit")
    (require 'magit))

(if (locate-library "js2")
    (my-js-setup))

(if (locate-library "whitespace")
    (my-whitespace-setup))

(if (locate-library "psvn")
    (my-svn-setup))

(if (locate-library "git")
    (require 'git))

(if (locate-library "git-blame")
    (progn
      (require 'format-spec)
      (require 'git-blame)))

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

(if (locate-library "erlang-start")
    (progn
      (require 'erlang-start)
      (my-erlang-setup)
      (if (locate-library "distel")
          (my-distel-setup))))

(defun my-svn-status-hide (line-info)
  "Hide externals."
  (eq (svn-status-line-info->filemark line-info) ?X))

(add-hook 'text-mode-hook 'my-text-mode-hook)
(defun my-text-mode-hook ()
  (setq fill-column 79)
  (longlines-mode t)
  (setq outline-minor-mode-prefix "")
  (outline-minor-mode)
  (highlight-parentheses-mode -1)
  (setq flyspell-dictionaries (quote ("american" "svenska")))
  (flyspell-mode))

(add-hook 'comint-mode-hook 'my-comint)
(defun my-comint ()
  ;; try to make the shell more like the real shell
  (local-set-key [tab] 'comint-dynamic-complete)
  (local-set-key [(control up)] 'previous-line)
  (local-set-key [(control down)] 'next-line)
  (local-set-key [up] 'comint-previous-input)
  (local-set-key [down] 'comint-next-input))

(defun indent-buffer ()
  "indent current buffer"
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(if window-system
    (set-background-color "black"))

(defun my-erc ()
  "start erc, connect to some servers, join some channels"
  (interactive)
  (set-language-environment "utf-8")
  (setq default-input-method "swedish-postfix"
	erc-hide-list '("JOIN" "NICK" "PART" "QUIT")
	erc-modules '(autojoin completion fill irccontrols match noncommands
			       readonly ring scrolltobottom stamp spelling
			       track truncate)
	erc-autojoin-channels-alist '(("freenode.net" "#erlang")
				      ("hq.kred" "#tech")))
  (erc :server "irc.freenode.net" :nick "massemanet")
  (erc :server "irc.hq.kred" :nick "masse"))

(defun my-elpa ()
  (interactive)
  (package-refresh-contents)
  (dolist (p '(magit highlight-parentheses clojure-mode js2-mode slime))
    (progn
      (if (package-installed-p p)
          (message "already installed %s" p)
        (package-install p)))))


;; automatically added stuff

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-add ((t (:foreground "green"))))
 '(magit-diff-del ((t (:foreground "red"))))
 '(magit-item-highlight ((t (:background "color-239")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
