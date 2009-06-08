;;; multi-comment.el --- multi-mode handling of comments and strings

;; Copyright (C) 2009  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: languages
;; $Revision: 1.2 $
;; URL: http://www.loveshack.ukfsn.org/emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides `multi-comment-mode', which is suitable for putting on a
;; programming language major mode hook.  It uses the multi-mode
;; facility to edit comments and strings in Text mode.  Thus, when
;; editing such regions of the buffer, you'll get, for instance, the
;; normal text-mode syntax, keymap, abbrevs, and hook functions
;; (e.g. flyspell).  Keybindings in the text-mode buffer inherit from
;; the base buffer, although they act with the wrong syntax table.
;; There's a special case attempt to make M-q, M-j, and C-M-j DTRT by
;; acting in the base buffer; there may be others worth doing.
;;
;; The behaviour depends on functions from newcomment.el, which may
;; not be robust and rely on `comment-start-skip' being defined in the
;; base major mode.

;;; Code:

(require 'multi-mode)
(require 'syntax)			; need add-on for Emacs 21
(require 'newcomment)

(defun multi-comment-chunk-region (pos)
  "Function for `multi-alist'."
  (save-excursion
    (with-current-buffer (multi-base-buffer)
      (if pos (goto-char pos))
      (let ((syntax (syntax-ppss))
	    start end syntax-start syntax-end)
	(cond
	 ;; Maybe string region -- see below.
	 ((eq (syntax-ppss-context syntax) 'string)
	  (goto-char (setq syntax-start (nth 8 syntax)))
	  (save-excursion
	    ;; E.g. in Python, the syntax might be `|""' to start the
	    ;; string, and `""|' to finish it.
	    (skip-syntax-forward "\"|")
	    (setq start (point)))
	  (setq end (condition-case nil
			(progn (forward-sexp)
			       (setq syntax-end (point))
			       (skip-syntax-backward "\"|" start)
			       (point))
		      (error (point-max))))
	  ;; This accounts for POS, say, in the middle of a Python
	  ;; triple quote, which we regard as being outside the string
	  ;; region.
	  (when (and (>= pos start) (<= pos end))
	    (multi-make-list 'text-mode start end)))
	 ;; Maybe comment region.
	 ((and (eq (syntax-ppss-context syntax) 'comment)
	       (setq syntax-start (comment-beginning)))
	  (setq start (point)
		end (condition-case nil
			(progn (goto-char (nth 8 syntax))
			       (forward-comment 1)
			       (setq syntax-start (point))
			       (comment-enter-backward)
			       (max start (point)))
		      (error (point-max))))
	  ;; Per string case, for multi-char comment delimiters.
	  (when (and (>= pos start) (<= pos end))
	    (multi-make-list 'text-mode start end)))
	 ;; Otherwise code region.
	 (t nil))))))

;; (defun multi-test ()
;;   (interactive)
;;   (let ((chunk (multi-comment-chunk-region (point))))
;;     (goto-char (nth 1 chunk))
;;     (push-mark (nth 2 chunk) nil t)
;;     (message "%s" (car chunk))))

;; Create a wrapped version of COMMAND which operates in the base
;; buffer (with appropriate syntax, for instance).
(eval-when-compile
(defmacro multi-comment-wrap (command)
  (let ((interactive-form (interactive-form command)))
    `(lambda (&rest args)
       ,interactive-form
       (goto-char (let ((point (point)))
		    (with-current-buffer (multi-base-buffer)
		      (goto-char point)
		      (call-interactively ',command)
		      (point))))))))

(defvar multi-comment-recursing nil)

;;;###autoload
(defun multi-comment-mode ()
  "Turn on Text-mode editing of comments and strings in the buffer.
The local keymap used will inherit from the map in the base buffer,
although the inherited bindings may not behave as expected, given
that the syntax table is the text-mode one.
Suitable for putting on a major mode hook."
  (interactive)
  ;; Break recursion from being on a major mode hook.
  (unless multi-comment-recursing
    (let ((multi-comment-recursing t))
      ;; The comment- functions we use need this.
      (if (not comment-start-skip)
	  (progn
	    (message
	     "comment-start-skip not defined in %s; multi-comment inactive"
	     major-mode)
	    (sit-for 1))
	(unless (and multi-alist (local-variable-p 'multi-alist))
	  (set (make-local-variable 'multi-alist)
	       `((,major-mode . nil))))
	(setq multi-alist
	      (append multi-alist '((text-mode . multi-comment-chunk-region))))
	(multi-mode-install-modes)
	(multi-comment-setup)))))

(defun multi-comment-setup ()
  "Setup additional local state in text-mode buffer."
  (with-current-buffer (cdr (assq 'text-mode
				  multi-indirect-buffers-alist))
    ;; Various keybindings that will probably work better in the base
    ;; buffer.  (Don't use substitute-key-definition, since some modes
    ;; ignore the relevant hooks and set up their own bindings.)
    (let ((map (copy-keymap (current-local-map))))
      (set-keymap-parent map
			 (with-current-buffer (multi-base-buffer)
			   (current-local-map)))
      (define-key map [?\M-j] (multi-comment-wrap indent-new-comment-line))
      (define-key map [(control meta ?j)]
	(multi-comment-wrap indent-new-comment-line))
      (define-key map [?\M-q] (multi-comment-wrap fill-paragraph))
      (use-local-map map))
    ;; Get the text-mode regions fontified.
    (set (make-local-variable 'font-lock-defaults)
	 (with-current-buffer (multi-base-buffer)
	   font-lock-defaults))
    (set (make-local-variable 'font-lock-mode)
	 (with-current-buffer (multi-base-buffer)
	   font-lock-mode))))

(provide 'multi-comment)
;;; multi-comment.el ends here
