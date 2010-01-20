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
  "Mode for editing Python embedded in HTML, using multi-mode."
  (interactive)
  (set (make-local-variable 'multi-alist)
       '((html-mode . mako-chunk-at)
	 (css-mode)
	 (js-mode)
	 (python-mode)
	 ))
  (add-hook 'multi-indirect-buffer-hook
	    (lambda ()
	      (when (eq major-mode 'html-mode)
		(setq indent-line-function 'sgml-indent-line)))
	    t t)
  (multi-mode-install-modes))

(defvar mako-regions
  '(("\\(<%!?\\)" "%>" python-mode)
    ("\\${" "}" python-mode)
    ("<script[^>]*>" "</script>" js-mode)
    ("<style[^>]*>" "</style>" css-mode)))

(defun mako-region-mode (n)
  (third n))

(defun degroup-regexp (r)
  (replace-regexp-in-string
   "\\\\(" "\\\\(?:" (replace-regexp-in-string "\\\\(?:" "\\\\(" r)))

(defun joined-regexp (parts)
  (mapconcat (lambda (x)
               (concat "\\(" (degroup-regexp x) "\\)"))
             parts "\\|"))

(defvar mako-start-regexp
  (joined-regexp (mapcar 'car mako-regions)))

(defvar mako-start-end-regexp
  (joined-regexp
   (append (mapcar 'car mako-regions)
           (mapcar 'cadr mako-regions))))

(defun mako-match-group ()
  (/ (position-if-not 'null (cddr (match-data))) 2))

(defun mako-chunk-at (pos)
  (or (mako-chunk pos)
      (mako-default-chunk pos)))

(defun mako-default-chunk (pos)
  (save-excursion
    (goto-char pos)
    (let ((e (save-excursion
               (if (re-search-forward mako-start-regexp nil t)
                   (match-beginning 0) (point-max))))
          (b (save-excursion
               (cond ((looking-at mako-start-end-regexp) pos)
                     ((re-search-backward mako-start-end-regexp nil t) (match-end 0))
                     (t (point-min))))))
      (list 'html-mode b e 0))))

(defun mako-chunk (pos)
  (save-excursion
    (goto-char pos)
    (loop while (re-search-backward mako-start-regexp nil t)
          for n = (mako-match-group)
          for os = (match-beginning 0)
          for oe = (match-end 0)
          for e = (mako-chunk-end oe n)
          if (>= e pos)
            return (list
                    (mako-region-mode (nth n mako-regions))
                    (max (or last-end 0) oe)
                    e
                    (current-column))
          else
            maximize e into last-end)))

(defun mako-chunk-end (pos n)
  (let ((stack (list n)))
    (save-excursion
      (goto-char pos)
      (loop while (re-search-forward mako-start-end-regexp nil t)
            for n = (mako-match-group)
            if (and (>= n (length mako-regions))
                    (= n (+ (length mako-regions) (first stack))))
              do (pop stack) end
            if (< n (length mako-regions))
              do (push n stack) end
            if (= (length stack) 0)
              return (1- (match-beginning 0))))))

(provide 'multi-mako)
;;; multi-mako.el ends here
