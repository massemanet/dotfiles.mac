;; -*- mode: lisp -*-

(set-default-font "-adobe-courier-medium-r-normal--14-*-----iso8859-1")
(set-language-environment "Latin-1")
(show-paren-mode t)
(transient-mark-mode t)
(global-font-lock-mode t)
(delete-selection-mode t)
(column-number-mode t)
(iswitchb-mode t)
(global-hl-line-mode -1)
(set-face-background 'hl-line "#333")
(if (featurep 'tool-bar) (tool-bar-mode -1))
(if (featurep 'tooltip) (tooltip-mode -1))
(if (featurep 'scroll-bar) (scroll-bar-mode -1))
(menu-bar-mode -1)

(setq
 align-to-tab-stop nil
 indent-tabs-mode nil
 browse-url-browser-function 'w3m-browse-url
 user-mail-address "masse@klarna.com"
 inhibit-startup-screen t
 special-display-regexps nil
 visible-bell t)

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

(defvar erlang-erl-path "/usr/local")
(defvar erlang-distel-path "~/git/distel")
(defvar erlang-erlmode-path 
  "/usr/local/otp/current/lib/erlang/lib/tools-*/emacs")

(defvar paths
  (list
   "~/elisp"
   (car (file-expand-wildcards erlang-erlmode-path))
   "/usr/share/doc/git/contrib/emacs"
   (concat erlang-distel-path "/elisp")))

(dolist (f (nreverse paths))
  (when (and (stringp f) (file-exists-p f))
    (add-to-list 'load-path f)))

(defun my-erlang-setup ()
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
    ;; i need some space
    (setq erlang-indent-level 2)
    ;; find the man pages
    (setq erlang-root-dir erlang-erl-path))

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
    ;; run flymake iff buffer has a file
    (local-set-key (kbd "M-L") 'erl-show-arglist)
    (local-set-key (kbd "M-A") 'erl-align-arrows)
    (if (and (locate-library "erlang-flymake")
             buffer-file-truename)
        (progn
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

(defun erl-align-arrows ()
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)->" 1 1))

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

(if (locate-library "erlang")
    (progn
      (require 'erlang-start)
      (my-erlang-setup)
      (if (locate-library "distel")
          (my-distel-setup))))

(if (locate-library "color-theme")
    (progn
      (require 'color-theme)
      (if (load-library "color-theme-masse")
          (color-theme-masse))))

(if (locate-library "fdlcap")
    (require 'fdlcap))

(defun my-svn-status-hide (line-info)
  "Hide externals."
  (eq (svn-status-line-info->filemark line-info) ?X))

(add-hook 'comint-mode-hook 'my-comint)
(defun my-comint ()
  ;; try to make the shell more like the real shell
  (local-set-key [tab] 'comint-dynamic-complete)
  (local-set-key [(control up)] 'previous-line)
  (local-set-key [(control down)] 'next-line)
  (local-set-key [up] 'comint-previous-input)
  (local-set-key [down] 'comint-next-input))

(defun k-find (word &optional ext)
  (grep-find
   (concat "find "
           (concat (car (split-string (buffer-file-name) "lib")) "lib/")
           (concat " -name '.svn' -prune -o -name '*~' -prune -o -name '*beam'"
                   " -prune -o -name '*html' -prune -o -type f -name '*"
                   ext
                   "' -print0 | xargs -0 -e grep -n -e "
                   word))))


(defun kfind (word)
  (interactive "MFind: ")
  (k-find word))

(defun kefind (word)
  (interactive "MFind: ")
  (k-find word "rl"))

(defun indent-buffer ()
  "indent current buffer"
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(if window-system
    (set-background-color "black"))

;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal
;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html

(unless (or (not (getenv "DISPLAY")) window-system)
  (defun xsel-cut-function (text &optional push)
    (interactive "MPush to X selection: ")
    (with-temp-buffer
      (insert text)
      (call-process-region
       (point-min) (point-max) "xsel" nil 0 nil "--input")))
  (defun xsel-paste-function()
    (interactive)
    (let ((xsel-output (shell-command-to-string "xsel --output")))
      (unless (string= (car kill-ring) xsel-output)
        xsel-output )))
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function))

(defun my-erc ()
  "start erc, connect to some servers, join some channels"
  (interactive)
  (set-language-environment "utf-8")
  (setq default-input-method "swedish-postfix")
  (setq erc-autojoin-channels-alist
        '(("freenode.net" "#erlang"); "#nitrogen")
;          ("foonetic.net" "#xkcd")
          ("hq.kred" "#tech")))
;  (setq erc-spelling-dictionaries '(("irc.hq.kred:6667" "svenska")))
  (erc :server "irc.freenode.net" :nick "massemanet")
;  (erc :server "irc.foonetic.net" :nick "masse")
  (erc :server "irc.hq.kred" :nick "masse"))

;; automatically added stuff


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default-input-method "swedish-postfix")
 '(erc-hide-list (quote ("JOIN" "NICK" "PART" "QUIT")))
 '(erc-modules (quote (autojoin completion fill irccontrols match noncommands readonly ring scrolltobottom stamp spelling track truncate)))
 '(flymake-no-changes-timeout 3)
 '(flyspell-dictionaries (quote ("american" "svenska")))
 '(gnus-novice-user nil)
 '(indent-tabs-mode nil)
 '(max-lisp-eval-depth 40000)
 '(safe-local-variable-values (quote ((erlang-indent-level . 4) (erlang-indent-level . 2))))
 '(scroll-down-aggressively 0.1)
 '(scroll-up-aggressively 0.1)
 '(utf-translate-cjk-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(comint-highlight-prompt ((t (:foreground "green1"))))
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(ediff-current-diff-B ((((class color) (min-colors 16)) (:background "Yellow" :foreground "black"))))
 '(ediff-even-diff-B ((((class color) (min-colors 16)) (:background "Grey" :foreground "black"))))
 '(ediff-odd-diff-A ((((class color) (min-colors 16)) (:background "Grey" :foreground "black"))))
 '(flymake-errline ((t (:background "red4"))))
 '(flymake-warnline ((t (:background "blue4"))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown4"))))
 '(magit-diff-add ((((class color) (background light)) (:foreground "green3"))))
 '(mode-line ((((class color) (min-colors 88)) (:stipple nil :background "red1" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:background "white" :foreground "darkred" :box (:line-width 1 :style released-button)))))
 '(region ((t (:background "color-58"))))
 '(svn-status-directory-face ((t (:foreground "white")))))
