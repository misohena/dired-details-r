;;; dired-details-r.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2016  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Show details right of filename in Dired.

;;; Code:


;;
;; Constants
;;

(defconst dired-details-r-regexp
  (concat
   "\\(\\([^ ][-r][-w][^ ][-r][-w][^ ][-r][-w][^ ]\\) \\)" ;1,2:permissions
   "\\( *\\([0-9]+\\) +\\)" ;3,4:link count
   "\\(\\([^ ]+\\) +\\)" ;5,6:user
   "\\(\\([^ ]+\\) +\\)" ;7,8:group (7 including space before size)
   "\\(\\([0-9]+\\) \\)" ;9,10:size
   "\\(\\(.+[^ ]\\) +\\)")) ;11,12:time

(defconst dired-details-r-parts
  ;; (name index subexp align-right)
  '((perms 0  2 nil)
    (links 1  4 t)
    (user  2  6 nil)
    (group 3  8 nil)
    (size  4 10 t)
    (time  5 12 nil))
  "Definitions of detail parts.")
(defun dired-details-r-part-name (part) (nth 0 part))
(defun dired-details-r-part-index (part) (nth 1 part))
(defun dired-details-r-part-subexp (part) (nth 2 part))
(defun dired-details-r-part-align-right (part) (nth 3 part))

(defcustom dired-details-r-max-filename-width 40 "" :group 'dired :type 'file)

(defcustom dired-details-r-combinations
  '((all        . (size time perms links user group))
    (size-time  . (size time))
    (no-details . ()))
  "Details combination list."
  :group 'dired
  :type 'file)

(defface dired-details-r-today '((t (:foreground "GreenYellow"))) nil)
(defvar dired-details-r-today 'dired-details-r-today)
(defface dired-details-r-dot-file '((t (:foreground "Gray50"))) nil)
(defvar dired-details-r-dot-file 'dired-details-r-dot-file)


;;
;; Variables
;;

(defvar dired-details-r-combination-name
  (caar dired-details-r-combinations) "Current details combination.")

(defvar dired-details-r-visible-parts
  (cdr (assq dired-details-r-combination-name dired-details-r-combinations))
  "Current visible parts list.")



;;
;; Set face to string
;;

(defun dired-details-r-set-face-part (str part-name)
  (cond
   ;; highlight today
   ((and (eq part-name 'time)
         (string-match (format-time-string "%b %e" (current-time)) str))
    (propertize str 'face 'dired-details-r-today))
   (t str)))

(defun dired-details-r-set-face-details (str part-strings)
  ;; gray dot file
  (cond
   ((string-match "^\\." (car (last part-strings)))
    (propertize str 'face 'dired-details-r-dot-file))
   (t str)))



;;
;; Process dired buffer
;;

(defun dired-details-r-foreach-filenames (beg end fun-at-filename)
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (ignore-errors
        (when (dired-move-to-filename)
          (if (looking-back dired-details-r-regexp)
              (funcall fun-at-filename))))
      (forward-line 1))))

(defun dired-details-r-match-part-strings ()
  "Return strings of text matched by looking-back dired-details-r-regexp."
  (append
   (mapcar #'(lambda (part) (match-string (dired-details-r-part-subexp part))) dired-details-r-parts)
   ;; last element is filename
   (list (buffer-substring (point) (point-at-eol)))))

(defun dired-details-r-max-part-widths (max-widths part-strings)
  "Calculate max width of parts and filename."
  (let* ((result (or max-widths (make-list (length part-strings) 0)))
         (r result)
         (s part-strings))
    (while (and r s)
      (setcar r (max
                 (car r)
                 (string-width (car s)) ))
      (setq r (cdr r))
      (setq s (cdr s)))
    result))


(defun dired-details-r-make-details-string (max-widths part-strings)
  (concat
   ;; spaces after filename
   (make-string
    (let* ((filename-curr-width (string-width (car (last part-strings))))
           (filename-max-width  (min (car (last max-widths))
                                     dired-details-r-max-filename-width))
           (filename-spaces (max
                             0
                             (- filename-max-width filename-curr-width -1))))
      filename-spaces)
    ? )
   ;; details
   (mapconcat
    #'(lambda (part-name)
        (let* ((part (assq part-name dired-details-r-parts))
               (index (dired-details-r-part-index part))
               (align-right (dired-details-r-part-align-right part))
               (spaces-width (-  ;; (max width) - (current width)
                              (nth index max-widths)
                              (string-width (nth index part-strings))))
               (spaces (make-string spaces-width ? ))
               (value (dired-details-r-set-face-part (nth index part-strings) part-name)))
          (if align-right (concat spaces value) (concat value spaces))))
    dired-details-r-visible-parts " ")))

(defun dired-details-r-set-text-properties (beg end)
  (let ((max-widths nil))

    ;; Calculate column width
    (dired-details-r-foreach-filenames
     beg end
     #'(lambda ()
         (setq max-widths
               (dired-details-r-max-part-widths
                max-widths
                (dired-details-r-match-part-strings)))))

    ;; Set text properties
    (dired-details-r-foreach-filenames
     beg end
     #'(lambda ()
         ;; put details overlay
         (let* ((details-beg (match-beginning 0))
                (details-end (match-end 0))
                (filename-beg (point))
                (filename-end (point-at-eol))
                (part-strings (dired-details-r-match-part-strings))

                (filename-ovl (dired-details-r-add-overlay filename-beg filename-end))
                (details-str (dired-details-r-set-face-details (dired-details-r-make-details-string max-widths part-strings) part-strings)))
           (overlay-put filename-ovl 'after-string details-str)
           (overlay-put filename-ovl 'intangible t)

           ;; erase details before filename
           (put-text-property details-beg details-end  'invisible t)
         )))))


;;
;; Overlay Management
;;

(defvar dired-details-r-overlay-list nil)
(make-variable-buffer-local 'dired-details-r-overlay-list)

(defun dired-details-r-add-overlay (beg end)
  (let ((ovl (make-overlay beg end)))
    (setq dired-details-r-overlay-list (cons ovl dired-details-r-overlay-list))
    (overlay-put ovl 'modification-hooks '(dired-details-r-modification-hook))
    ovl))
(defun dired-details-r-delete-overlays (&optional _arg _noconfirm)
  (mapc 'delete-overlay dired-details-r-overlay-list)
  (setq dired-details-r-overlay-list nil))

(defun dired-details-r-modification-hook (o changed beg end &optional len)
  (let ((inhibit-modification-hooks t))
    (when (and changed (= beg end))
      (delq o dired-details-r-overlay-list)
      (delete-overlay o))))


;;
;; Switch Combination
;;

(defun dired-details-r-toggle-combination ()
  (interactive)
  ;; Rotate combination
  (let* ((curr-combination-def
          (let ((combs dired-details-r-combinations))
            (while (and combs (not (eq (caar combs) dired-details-r-combination-name))) (setq combs (cdr combs)))
            combs))
         (next-combination-def (or (cadr curr-combination-def) (car dired-details-r-combinations))))
    (setq dired-details-r-combination-name (car next-combination-def))
    (setq dired-details-r-visible-parts (cdr next-combination-def)))
  ;; Refresh buffer
  (revert-buffer))



;;
;; Setup
;;

(defun dired-details-r-set-text-properties-after (beg end)
  (dired-details-r-set-text-properties beg end))

(defun dired-details-r-activate () (toggle-truncate-lines 1))

(defun dired-details-r-setup ()
  (interactive)
  (advice-add 'dired-insert-set-properties :after 'dired-details-r-set-text-properties-after)
  (define-key dired-mode-map "(" 'dired-details-r-toggle-combination)
  (add-hook 'dired-mode-hook 'dired-details-r-activate)
  (advice-add 'dired-revert :before 'dired-details-r-delete-overlays))

(defun dired-details-s-uninstall ()
  (interactive)
  (advice-remove 'dired-insert-set-properties 'dired-details-r-set-text-properties-after)
  (define-key dired-mode-map "(" 'dired-hide-details-mode)
  (remove-hook 'dired-mode-hook 'dired-details-r-activate)
  (advice-remove 'dired-revert :before 'dired-details-r-delete-overlays))

(dired-details-r-setup)


(provide 'dired-details-r)
;;; dired-details-r.el ends here
