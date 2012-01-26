;;; html-js.el --- multi-mode PHP embedded in HTML

;; Copyright (C) 2008  Dave Love

;; Author: Andreas Raster <lazor@affenbande.org>
;; URL: http://github.com/rakete

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

(require 'multi-mode)
(require 'multi-util)

(defun html-js-mode ()
  "Mode for editing PHP embedded in HTML, using multi-mode."
  (interactive)
  (set (make-local-variable 'multi-alist)
       '((html-mode)
         (javascript-mode . html-js-chunk-region)))
  (add-hook 'multi-indirect-buffer-hook
            (lambda ()
              (when (eq major-mode 'html-mode)
                (setq indent-line-function 'sgml-indent-line)))
            t t)
  (multi-mode-install-modes))

(defun html-js-chunk-region (pos)
  (interactive)
  (let* ((re-start "<script[^>]*>")
         (re-end  "<\/script>")
         (chunk-inside (condition-case nil (multi-mode-position-chunk pos re-start re-end) (error nil)))
         (inside-start (cdr (assoc :inner-start chunk-inside)))
         (inside-end (cdr (assoc :inner-end chunk-inside))))
    (if (and chunk-inside
             inside-start
             inside-end
             (/= inside-start inside-end)
             (>= pos inside-start)
             (< pos inside-end))
        (list 'javascript-mode inside-start inside-end)
      (let* ((chunk-outside (condition-case nil (multi-mode-position-chunk pos re-end re-start t) (error nil)))
             (outside-start (cdr (assoc :outer-start chunk-outside)))
             (outside-end (cdr (assoc :outer-end chunk-outside))))
        (list 'html-mode outside-start outside-end)))))

(defun test-html-js ()
  (interactive)
  (print (html-js-chunk-region (point))))

(provide 'html-js)
