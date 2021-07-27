;;; dired-details-r.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2021 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: Dired

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

;;; Usage:

;; (require 'dired-details-r)
;; (dired-details-r-setup)

;;; Code:



;;
;; Customizable variables
;;

(defgroup dired-details-r nil
  "Mode to display file information to the right of filename."
  :group 'dired)

(defcustom dired-details-r-max-filename-width 52 "" :group 'dired-details-r :type 'integer)
(defcustom dired-details-r-min-filename-width 40 "" :group 'dired-details-r :type 'integer)

(defcustom dired-details-r-combinations
  '((size-time  . (size time))
    (no-details . ())
    (disabled   . disabled)
    (all        . (size time perms links user group)))
  "Details combination list."
  :group 'dired-details-r
  :type '(repeat sexp))

(defcustom dired-details-r-truncate-lines t
  "" :group 'dired-details-r :type 'boolean)



(defgroup dired-details-r-faces nil
  "Faces used by dired-details-r."
  :group 'dired-details-r
  :group 'faces)

(defface dired-details-r-today
  '((t (:foreground "GreenYellow")))
  ""
  :group 'dired-details-r-faces)

(defface dired-details-r-dot-file
  '((t (:foreground "Gray50")))
  ""
  :group 'dired-details-r-faces)



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



;;
;; Variables
;;

(defvar-local dired-details-r-combination-name nil
  "Current details combination.")

(defvar-local dired-details-r-visible-parts nil
  "Current visible parts list.")

(defvar-local dired-details-r-max-widths nil)



;;
;; Minor Mode
;;

;;;###autoload
(define-minor-mode dired-details-r-mode
  "Display detailed information on the right side of the buffer."
  :group 'dired
  :keymap (let ((keymap (make-sparse-keymap)))
            (define-key keymap (kbd "(") 'dired-details-r-toggle-combination)
            keymap)
  (unless (derived-mode-p 'dired-mode)
    (error "Not a Dired buffer"))

  (dired-details-r-update-invisibility-spec)

  (cond
   ;; turn on
   (dired-details-r-mode
    ;; local variables
    (setq-local dired-details-r-combination-name
                (caar dired-details-r-combinations))
    (setq-local dired-details-r-visible-parts
                (cdr (assq dired-details-r-combination-name
                           dired-details-r-combinations)))
    (setq-local dired-details-r-max-widths nil)

    ;; hook
    (dired-details-r-enable-global-hooks)
    (add-hook 'wdired-mode-hook 'dired-details-r-update-invisibility-spec nil t)

    ;; change appearance
    (let ((inhibit-read-only t))
      (dired-details-r-set-appearance-changes (point-min) (point-max)))

    ;; truncate lines
    (when dired-details-r-truncate-lines
      (setq truncate-lines t)
      (force-mode-line-update)))

   ;; turn off
   (t
    ;; unhook
    (remove-hook 'wdired-mode-hook 'dired-details-r-update-invisibility-spec t)

    ;; change appearance
    (dired-details-r-remove-all-appearance-changes))))



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
          (if (looking-back dired-details-r-regexp (line-beginning-position))
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
           (filename-max-width  (max
                                 dired-details-r-min-filename-width
                                 (min (car (last max-widths))
                                      dired-details-r-max-filename-width)))
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

(defun dired-details-r-set-appearance-changes (beg end)
  "Set text properties and overlays on file information lines."
  (when (and (not (eq dired-details-r-visible-parts 'disabled))
             (listp dired-details-r-visible-parts))

    (let ((max-widths dired-details-r-max-widths))

      ;; Calculate column width
      (dired-details-r-foreach-filenames
       beg end
       (lambda ()
         (setq max-widths
               (dired-details-r-max-part-widths
                max-widths
                (dired-details-r-match-part-strings)))))

      (setq dired-details-r-max-widths max-widths)

      ;; Set text properties
      (dired-details-r-foreach-filenames
       beg end
       (lambda ()
         ;; put details overlay
         (let* ((eol-beg (line-end-position))
                (eol-end (1+ eol-beg))
                (ovl (dired-details-r-add-overlay eol-beg eol-end))
                (part-strings (dired-details-r-match-part-strings))
                (details-str (dired-details-r-set-face-details
                              (dired-details-r-make-details-string
                               max-widths part-strings)
                              part-strings)))
           (overlay-put ovl 'display (concat details-str "\n")))

         ;; erase details before filename
         (let ((details-beg (+ (line-beginning-position) 1)) ;; include second whitespace
               (details-end (1- (point)))) ;; keep whitespace after details. if not, wdired will not work properly
           (put-text-property details-beg details-end 'invisible 'dired-details-r-detail)))))))

(defun dired-details-r-remove-all-appearance-changes ()
  "Remove all invisible text properties and overlays for dired-details-r."
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max) '(invisible 'dired-details-r-detail)))
  (dired-details-r-remove-all-overlays))

(defun dired-details-r-update-invisibility-spec ()
  (if dired-details-r-mode
      (add-to-invisibility-spec 'dired-details-r-detail)
    (remove-from-invisibility-spec 'dired-details-r-detail)))



;;
;; Overlay Management
;;

(defun dired-details-r-add-overlay (beg end)
  (let ((ovl (make-overlay beg end nil t)))
    (overlay-put ovl 'dired-details-r t)
    (overlay-put ovl 'evaporate t)
    ovl))

(defun dired-details-r-remove-all-overlays ()
  (remove-overlays (point-min) (point-max) 'dired-details-r t))



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

(defun dired-details-r-mode-or-toggle-combination ()
  (interactive)
  (if dired-details-r-mode
      (dired-details-r-toggle-combination)
    (dired-details-r-mode)))



;;
;; Global Hooks
;;

(defvar dired-details-r-enabled-global-hooks nil)

(defun dired-details-r-enable-global-hooks ()
  (when (not dired-details-r-enabled-global-hooks)
    (advice-add 'dired-insert-set-properties
                :after
                'dired-details-r--dired-insert-set-properties-hook)
    (advice-add 'dired-revert
                :before
                'dired-details-r--dired-revert-hook)
    (setq dired-details-r-enabled-global-hooks t)))

(defun dired-details-r-disable-global-hooks ()
  (when dired-details-r-enabled-global-hooks
    (advice-remove 'dired-insert-set-properties
                   'dired-details-r--dired-insert-set-properties-hook)
    (advice-remove 'dired-revert
                   'dired-details-r--dired-revert-hook)
    (setq dired-details-r-enabled-global-hooks nil)))


(defun dired-details-r--dired-insert-set-properties-hook (beg end)
  (when dired-details-r-mode
    ;; Insert text properties and overlays from beg to end
    (dired-details-r-set-appearance-changes beg end)))

(defun dired-details-r--dired-revert-hook (&optional _arg _noconfirm)
  (when dired-details-r-mode
    ;; Reset column widths
    (setq dired-details-r-max-widths nil)
    ;; Remove all overlays (unnecessary? evaporate property is used)
    (dired-details-r-remove-all-overlays)))



;;
;; Setup
;;

;;;###autoload
(defun dired-details-r-setup ()
  (interactive)
  (add-hook 'dired-mode-hook 'dired-details--dired-mode-hook)
)

(defun dired-details-r-uninstall ()
  (interactive)
  (dired-details-r-disable-global-hooks)
  (remove-hook 'dired-mode-hook 'dired-details--dired-mode-hook)
  ;; turn off dired-details-r-mode for all Dired buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when dired-details-r-mode
        (dired-details-r-mode -1)))))


(defun dired-details--dired-mode-hook ()
  (dired-details-r-mode))



(provide 'dired-details-r)
;;; dired-details-r.el ends here
