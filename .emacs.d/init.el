;;; -*- mode: lisp -*-

;;; package handling
(package-initialize)
(require 'cask (car (sort (file-expand-wildcards
                           "~/.emacs.d/.cask/*/elpa/cask-*/cask.el")
                          'string>)))
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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
(display-time)
(fset 'yes-or-no-p 'y-or-n-p)
(server-start)
(nyan-mode 1)
(global-flycheck-mode)

(add-hook 'after-init-hook 'sml/setup)

;; turn off bad shit
(if (featurep 'tool-bar)   (tool-bar-mode   -1))
(if (featurep 'tabbar)     (tabbar-mode     -1))
(if (featurep 'tooltip)    (tooltip-mode    -1))
(if (featurep 'scroll-bar) (scroll-bar-mode -1))
(if (featurep 'menu-bar)   (menu-bar-mode   -1))
(defun ido-kill-emacs-hook () (ignore-errors (ido-save-history)))
(setq-default indent-tabs-mode nil)
(setq
 align-to-tab-stop           nil
 display-time-24hr-format    t
 ediff-window-setup-function 'ediff-setup-windows-plain
 inhibit-startup-screen      t
 ring-bell-function          #'blink-mode-line
 special-display-regexps     nil
 utf-translate-cjk-mode      nil
 visible-bell                nil)

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (window-resize (selected-window) (- 81 (window-width)) t))

(defun blink-mode-line ()
   "Blink the mode line."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))

;; configs
(setq
 default-input-method     "swedish-postfix"
 max-lisp-eval-depth      40000
 scroll-down-aggressively 0.1
 scroll-up-aggressively   0.1
 user-mail-address        "mats.cronqvist@gmail.com")

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun prev-window ()
  (interactive)
  (select-window (previous-window (selected-window) nil nil)))

;; keybindings
(global-set-key (kbd "C-%")     `query-replace)
(global-set-key (kbd "C-:")     'flycheck-previous-error)
(global-set-key (kbd "C-<")     'beginning-of-buffer)
(global-set-key (kbd "C->")     'end-of-buffer)
(global-set-key (kbd "C-S-c")   `compile)
(global-set-key (kbd "C-S-g")   `goto-line)
(global-set-key (kbd "C-S-n")   `forward-list)
(global-set-key (kbd "C-S-o")   `switch-to-previous-buffer)
(global-set-key (kbd "C-S-p")   `backward-list)
(global-set-key (kbd "C-S-t")   `transpose-lines)
(global-set-key (kbd "C-S-u")   `fdlcap-change-case-current-word)
(global-set-key (kbd "C-S-v")   `scroll-down)
(global-set-key (kbd "C-S-w")   `kill-ring-save)
(global-set-key (kbd "C-\"")    'flycheck-next-error)
(global-set-key (kbd "C-c a")   'align-regexp)
(global-set-key (kbd "C-c b")   'bury-buffer)
(global-set-key (kbd "C-c p")   'point-to-register)
(global-set-key (kbd "C-c r")   'register-to-point)
(global-set-key (kbd "C-v")     `scroll-up)
(global-set-key (kbd "C-x C-r") 'revert-buffer)
(global-set-key (kbd "C-x O")   'prev-window)
(global-set-key (kbd "C-x c")   'execute-extended-command)
(global-set-key (kbd "C-x |")   'set-80-columns)
(global-set-key (kbd "C-z")     'undo) ; be like a mac
(global-set-key (kbd "C-{")     `previous-error)
(global-set-key (kbd "C-}")     `next-error)
(global-set-key (kbd "M-z")     'undo) ; if screen eats C-z

(let ((map minibuffer-local-map))
  (define-key map (kbd "C-n")   'next-history-element)
  (define-key map (kbd "C-p")   'previous-history-element))

;; go stuff
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq whitespace-style (list 'face 'trailing 'lines-tail 'empty))
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet && go install"))
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; erlang stuff
(defun my-erlang-setup ()
  (require 'edts-start)
  (setq edts-xref-checks nil)
  (require 'flycheck-rebar3)
  (flycheck-rebar3-setup)
  (flycheck-define-checker erlang
    "awesome erlang checker"
    :command ("erlc"
              "-o" temporary-directory
              (option-list "-I" flycheck-erlang-include-path)
              (option-list "-pa" flycheck-erlang-library-path)
              "-Wall"
              source)
    :error-patterns
    ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
     (error line-start (file-name) ":" line ": " (message) line-end))
    :modes erlang-mode
    :predicate (lambda ()
                 (string-suffix-p ".erl" (buffer-file-name))))
  (setq flycheck-erlang-include-path '("../include"))
  (setq flycheck-erlang-library-path '("../_build/default/lib/*/ebin"))
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
  (setq whitespace-style (list 'face 'tabs 'trailing 'lines-tail 'empty)
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

(if (locate-library "alchemist")
    (require 'alchemist))

(if (locate-library "whitespace")
    (my-whitespace-setup))

(if (locate-library "git")
    (require 'git))

(if (locate-library "git-blame")
    (progn
      (require 'format-spec)
      (require 'git-blame)))

(if (locate-library "highlight-parentheses")
    (progn
      (require 'highlight-parentheses)
      (setq hl-paren-colors
            (quote
             ("orange" "yellow3" "green3" "blue1" "cyan3" "purple2")))
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
  "Indent current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

;; automatically added stuff

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "82fce2cada016f736dbcef237780516063a17c2436d1ee7f42e395e38a15793b" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "b67cb8784f6a2d1a3f605e39d2c376937f3bf8460cb8a0d6fc625c0331c00c83" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (flycheck-julia julia-mode julia-repl color-theme-sanityinc-solarized yaml-mode solarized-theme smart-mode-line purescript-mode pallet nyan-mode markdown-mode magit json-mode js2-mode highlight-parentheses gruvbox-theme go-mode flymake-jshint flycheck-rebar3 flycheck-elixir flycheck-demjsonlint exec-path-from-shell eproject edts alchemist)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sml/theme (quote respectful))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-warning-face ((t (:inherit error :background "blue" :weight bold))))
 '(mode-line ((t (:background "#661111" :foreground "#839496" :inverse-video nil :box (:line-width 1 :color "#073642" :style unspecified) :overline "#073642" :underline "#284b54")))))
