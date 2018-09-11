;;; -*- mode: lisp -*-

;;; package handling
(package-initialize)
(require 'cask (car (sort (file-expand-wildcards
                           "~/.emacs.d/.cask/*/elpa/cask-*/cask.el")
                          'string>)))
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(if (memq window-system '(mac ns x))
    (progn
      (exec-path-from-shell-initialize)
      (load-theme 'gruvbox-dark-hard t)))

;; add legacy
(add-to-list 'load-path "~/.emacs.d/fdlcap")
(add-to-list 'load-path "~/.emacs.d/masserlang")

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
(global-set-key (kbd "C-S-q")   `fill-paragraph)
(global-set-key (kbd "C-S-t")   `transpose-lines)
(global-set-key (kbd "C-S-u")   `fdlcap-change-case-current-word)
(global-set-key (kbd "C-S-v")   `scroll-down)
(global-set-key (kbd "C-S-w")   `kill-ring-save)
(global-set-key (kbd "C-S-y")   `yank-pop)
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

(defun my-dark ()
  "Set dark theme."
  (interactive)
  (load-theme 'gruvbox-dark-hard t))

(defun my-lite ()
  "Set light theme."
  (interactive)
  (load-theme 'gruvbox-light-hard t))

(defun my-whitespace-setup()
  (require 'whitespace)
  (setq whitespace-style (list 'face 'tabs 'trailing 'lines-tail 'empty)
        whitespace-line-column 79))

;; this is default in emacs 24.4
(if (locate-library "uniquify")
    (progn
      (require 'uniquify)
      (setq uniquify-buffer-name-style 'post-forward-angle-brackets)))

(if (locate-library "projectile")
    (progn
      (projectile-mode +1)
      (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(if (locate-library "masserlang")
    (require 'masserlang))

(if (locate-library "fdlcap")
    (require 'fdlcap))

(if (locate-library "magit")
    (require 'magit))

(if (locate-library "whitespace")
    (my-whitespace-setup))

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

(custom-set-variables
 '(term-default-fg-color nil)
 '(term-default-bg-color nil))

(custom-set-faces)
