;;; multi-mako.el --- multi-mode for Mako templates

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

;;; Code:

(require 'multi-mode)

(defun multi-mako-mode ()
  "Mode for editing PHP embedded in HTML, using multi-mode."
  (interactive)
  (set (make-local-variable 'multi-alist)
       '((html-mode)
	 (python-mode . mako-chunk-region)
	 ))
  (add-hook 'multi-indirect-buffer-hook
	    (lambda ()
	      (when (eq major-mode 'html-mode)
		(setq indent-line-function 'sgml-indent-line)))
	    t t)
  (multi-mode-install-modes))

(defun mako-chunk-region (pos)
  "Mode-selecting function for Python embedded in HTML.
See `multi-alist'."
  (let ((case-fold-search t)
	pi-start pi-end next-pi pi-mode pi-term)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char pos)
	(save-excursion
	  (save-excursion
	    (skip-chars-forward " \t\n")
	    (if (re-search-backward "\\(<%!?\\)[ \t\n]+" (first pi-start) t)
		(setq pi-start (list (match-end 1) "%>" 'python-mode))))
	  
	  (save-excursion
	    (if (re-search-backward "\\${" (first pi-start) t)
		(setq pi-start (list (match-end 0) "}" 'python-mode))))

	  ;; Ready to search for matching terminator or next
	  ;; processing instruction.
	  (goto-char (or (first pi-start) pos))
	  
	  (if pi-start
	      (progn
		(re-search-forward (second pi-start) nil t)
		(setq pi-end (match-beginning 0))
		(goto-char pos)))
	  
	  (if (and pi-start pi-end (< pos pi-end))
	      ;; We were between PI start and terminator.
	      (list (third pi-start) (first pi-start) pi-end)
	    ;; Otherwise, look forward for a PI to delimit the HTML
	    ;; region.
	    (progn
	      (save-excursion
		(search-backward "<" (or pi-end (point-min)) t)
		(if (re-search-forward "\\(<%!?\\)[ \t\n]+" nil t)
		    (setq next-pi (match-end 1))))
	      
	      (save-excursion
		(search-backward "$" (or pi-end (point-min)) t)
		(if (re-search-forward "\\${" next-pi t)
		    (setq next-pi (match-end 0))))

	      (list 'html-mode (or pi-end (point-min)) next-pi)
	      )))))))

(provide 'multi-mako)
;;; multi-mako.el ends here
