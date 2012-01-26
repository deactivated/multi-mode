;;; html-multi.el --- multi-mode PHP embedded in HTML

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

(defun html-multi-mode ()
  (interactive)
  (set (make-local-variable 'multi-alist)
       '((html-mode)
         (css-mode . html-css-chunk-region)
         (javascript-mode . html-js-chunk-region)
         (php-mode . html-php-chunk-region)))
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

(defun html-css-chunk-region (pos)
  (interactive)
  (let* ((re-start "<style[^>]*>")
         (re-end  "<\/style>")
         (chunk-inside (condition-case nil (multi-mode-position-chunk pos re-start re-end) (error nil)))
         (inside-start (cdr (assoc :inner-start chunk-inside)))
         (inside-end (cdr (assoc :inner-end chunk-inside))))
    (if (and chunk-inside
             inside-start
             inside-end
             (/= inside-start inside-end)
             (>= pos inside-start)
             (< pos inside-end))
        (list 'css-mode inside-start inside-end)
      (let* ((chunk-outside (condition-case nil (multi-mode-position-chunk pos re-end re-start t) (error nil)))
             (outside-start (cdr (assoc :outer-start chunk-outside)))
             (outside-end (cdr (assoc :outer-end chunk-outside))))
        (list 'html-mode outside-start outside-end)))))

(defun html-php-chunk-region (pos)
  "Mode-selecting function for PHP embedded in HTML.
See `multi-alist'."
  (let ((case-fold-search t)
        pi-start pi-end next-pi)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char pos)
        (save-excursion
          (let* ((p1 (save-excursion
                       ;; Check whether we're on the processing
                       ;; instruction start.  Skip definitely clear of
                       ;; it and then search backwards.
                       (goto-char (min (point-max) (+ (point) 5)))
                       (re-search-backward "<\\?php\\|<\\?=" (- (point) 9) t)))
                 (p1 (if (and p1 (<= (match-beginning 0) pos))
                         (match-beginning 0) nil))
                 (match-end (if p1 (match-end 0)))
                 ;; Otherwise search backwards simply.
                 (p2 (unless p1 (re-search-backward "<\\?php\\|<\\?=" nil t))))

            (if p2 (setq match-end (match-end 0)))
            (setq pi-start (or p1 p2))

            ;; Ready to search for matching terminator or next
            ;; processing instruction.
            (goto-char (or match-end pos)))
          (if pi-start
              ;; Look forward for the PI terminator.
              (let* ((p1 (save-excursion
                           ;; Check whether we're on the terminator.
                           (backward-char 1)
                           (search-backward "?>" (- (point) 2) t)))
                     (p2 (unless p1 (search-forward "?>" nil t))))
                (setq pi-end (or p1 p2 (point-max))))
            (goto-char pos))
          (if (and pi-start pi-end (< pos pi-end))
              ;; We were between PI start and terminator.
              (list 'php-mode pi-start pi-end)
            ;; Otherwise, look forward for a PI to delimit the HTML
            ;; region.
            (setq next-pi (if (re-search-forward "<\\?php\\|<\\?=" nil t)
                              (match-beginning 0)
                            (point-max)))
            (if pi-start
                (list 'html-mode (or pi-end (point-min)) next-pi)
              (list 'html-mode (point-min) next-pi))))))))

(provide 'html-multi)
