;;; flyspell-guess.el --- flyspell dictionary guessing engine

;; Copyright (C) 2007 Alessandro Di Marco

;; Author:          Alessandro Di Marco (dmr@c0nc3pt.com)
;; Maintainer:      Alessandro Di Marco (dmr@c0nc3pt.com)
;; Created:         Oct 27, 2007
;; Keywords:        convenience
;; Latest Version:

;; This file is not part of Emacs

;; COPYRIGHT NOTICE

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  Still too lazy to write one ;-)

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path, then add one of the following
;;  to your ~/.emacs startup file.  You can load hints every time you start
;;  Emacs:
;;
;;     (require 'flyspell-guess)

;;; Usage:
;;
;;  Do you really need help for this?

;;; Known Bugs:
;;
;;  None at the moment ;-)

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Alessandro Di Marco (dmr@c0nc3pt.com).
;;
;;  This version of hints was developed and tested with GNU Emacs 22.1.50.1
;;  under Linux. Please, let me know if it works with other OS and versions of
;;  Emacs.

;;; Code:

(require 'flyspell)

(defcustom flyspell-guess-dictionaries
  '("american" "svenska")
  "List of dictionaries considered in the guess."
  :group 'flyspell
  :type '(repeat (string)))

(defvar flyspell-guess-size 80
  "The guessing region size.")

(defvar flyspell-guess-slots 1
  "The number of guessing regions.")

(defvar flyspell-origin)

(make-variable-buffer-local 'flyspell-origin)

(defun flyspell-count-errors-range (dict left right)
  (let ((errors (make-vector flyspell-guess-slots 0))
	(flyspell-issue-message-flag nil))
    (defun flyspell-error-counter (left right undef)
      (let ((slotl (/ (- left flyspell-origin) flyspell-guess-size))
	    (slotr (/ (- right flyspell-origin) flyspell-guess-size)))
	(if (and (/= slotl slotr) (< slotr flyspell-guess-slots))
	    (aset errors slotr
		  (1+ (aref errors slotr))))
	(if (< slotl flyspell-guess-slots)
	    (aset errors slotl
		  (1+ (aref errors slotl))))))
    (condition-case nil
	(progn
	  (ispell-change-dictionary dict)
	  (add-hook 'flyspell-incorrect-hook
		    'flyspell-error-counter nil t)
	  (flyspell-region (max (point-min) left)
			   (min (point-max) right))
	  (remove-hook 'flyspell-incorrect-hook
		       'flyspell-error-counter t)
	  )
      (error ((lambda ()
		(message "Skipping dictionary '%s'" dict)
		(setq errors nil)))))
    errors))

(defun flyspell-guess-dictionary-range (dicts left right)
  (let ((dict)
        (errors)
        (min-dict)
	(min nil))
    (while dicts
      (setq dict (car dicts))
      (setq dicts (cdr dicts))
      (setq errors (aref (flyspell-count-errors-range dict left right) 0))
      (if (or (eq min nil) (< errors min))
          (progn
            (setq min errors)
            (setq min-dict dict)))
;      (message "%s %s %s %s %s %s" dict min dicts left right (not min))
      )
    min-dict))

(defun flyspell-do-guess-dictionary (buffer beginning end)
  (let ((old (current-buffer)))
    (set-buffer buffer)
    (let ((new (clone-indirect-buffer "guess" nil t)))
      (set-buffer new)
      (setq flyspell-origin beginning)
      (let ((guess
	     (flyspell-guess-dictionary-range
	      flyspell-guess-dictionaries
	      flyspell-origin
              end)))
	(set-buffer buffer)
	(kill-buffer new)
	(if guess
            (progn
              (message "Guessed language '%s'" guess)
              (ispell-change-dictionary guess)
              (turn-on-flyspell))
          (message "Could not guess a dictionary"))))
    (set-buffer old)))

(defun flyspell-guess-dictionary (beginning &optional end)
  (interactive "d")
  (if (eq end nil) 
      (setq end (min (+ beginning flyspell-guess-size) (point-max))))
  (message "%s %s" beginning end)
  (flyspell-do-guess-dictionary (current-buffer) beginning end))

(provide 'flyspell-guess)
