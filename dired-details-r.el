;;; dired-details-r.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2023 AKIYAMA Kouhei

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
;; (global-dired-details-r-mode)

;;; Code:

(require 'dired)
(require 'cl-lib)


;;;; Customizable variables


(defgroup dired-details-r nil
  "Display file details to the right of the file name in dired."
  :group 'dired)

(defcustom dired-details-r-min-filename-width 40
  "Width always reserved for file names.

The area for displaying file names is never less than this width."
  :group 'dired-details-r :type 'integer)

(defcustom dired-details-r-max-filename-width 52
  "The maximum width to always reserve for file names in the entire buffer.

Even if there is a file name whose length exceeds this width, the
layout of the entire buffer will not change any further."
  :group 'dired-details-r :type 'integer)

(defcustom dired-details-r-combinations
  '((size-time  . (size time))
    ;;(time-size  . (time size)) ;; Which do you prefer?
    (no-details . ())
    (disabled   . disabled)
    (all        . (size time perms links user group))
    ;;(all        . (time size perms links user group))
    )
  "A list of combinations of file detail information to display.

 The combination is rotated by calling
 `dired-details-r-toggle-combination' command."
  :group 'dired-details-r
  :type '(repeat sexp))

(defcustom dired-details-r-truncate-lines t
  "If non-nil, truncate lines."
  :group 'dired-details-r
  :type 'boolean)

(defcustom dired-details-r-date-format
  "A format string to use when determining whether a date is today.

Used by the `dired-details-r-set-face-part' function to determine
whether to apply the `dired-details-r-today' face to the
timestamp part.

The value is specified as the first argument of `format-time-string'.

May be related to `ls-lisp-format-time-list'.

For example, if the date format output by ls is 2023-01-02 12:34,
specify %Y-%m-%d."
  "%b %e"
  :group 'dired-details-r-faces
  :type 'string)

(defcustom dired-details-r-consider-overlays-before-filename-p t
  "When non-nil, consider image-dired and all-the-icons-dired
overlays in front of the filename.

Set to nil if too slow or unstable."
  :group 'dired-details-r
  :type 'boolean)

(defcustom dired-details-r-truncate-filenames t
  "When non-nil, filenames are truncated to the maximum width."
  :group 'dired-details-r
  :type 'boolean)

(defcustom dired-details-r-ellipsis ".."
  "The ellipsis to use in the Dired buffer.

When nil, just use the standard three dots.  When a non-empty string,
use that string instead."
  :group 'org-startup
  :type '(choice (const :tag "Default" nil)
                 (string :tag "String" :value "...")))

;;;; Faces


(defgroup dired-details-r-faces nil
  "Faces used by dired-details-r."
  :group 'dired-details-r
  :group 'faces)

(defface dired-details-r-today
  '((t (:foreground "GreenYellow")))
  "Face for indicating today's timestamp.

Whether it is today's date or not is determined by whether the
string specified in `dired-details-r-date-format' is included."
  :group 'dired-details-r-faces)

(defface dired-details-r-dot-file
  '((t (:foreground "Gray50")))
  "Face for dot file."
  :group 'dired-details-r-faces)

(defface dired-details-r-ellipsis
  '((t (:foreground "Gray50")))
  "Face for ellipsis."
  :group 'dired-details-r-faces)


;;;; Constants


(defconst dired-details-r-regexp
  (concat
   "\\(\\([^ ][-r][-w][^ ][-r][-w][^ ][-r][-w][^ ]\\) \\)" ;1,2:permissions
   "\\( *\\([0-9]+\\) +\\)" ;3,4:link count
   "\\(\\([^ ]+\\) +\\)" ;5,6:user
   "\\(\\([^ ]+\\) +\\)" ;7,8:group (7 including space before size)
   "\\(\\([0-9]+\\) \\)" ;9,10:size
   "\\(\\(.+[^ ]\\) +\\)")) ;11,12:time

(defconst dired-details-r-part-info-list
  ;; (name index subexp align-right)
  '((perms 0  2 nil)
    (links 1  4 t)
    (user  2  6 nil)
    (group 3  8 nil)
    (size  4 10 t)
    (time  5 12 nil))
  "Definitions of detail parts.")
(defun dired-details-r-part-info-name (part-info) (nth 0 part-info))
(defun dired-details-r-part-info-index (part-info) (nth 1 part-info))
(defun dired-details-r-part-info-subexp (part-info) (nth 2 part-info))
(defun dired-details-r-part-info-align-right (part-info) (nth 3 part-info))


;;;; Variables


(defvar-local dired-details-r-combination-name nil
  "Current details combination.")

(defvar-local dired-details-r-visible-parts nil
  "Current visible parts list.")

(defvar-local dired-details-r-max-widths nil)

(defvar-local dired-details-r-overlay-method nil)

(defvar dired-details-r-display-table nil)

(defvar dired-details-r-ellipsis-width 3)

;;;; Minor Mode


;;;###autoload
(define-minor-mode dired-details-r-mode
  "Display file details to the right of the file name in dired."
  :group 'dired
  :keymap (let ((km (make-sparse-keymap)))
            (define-key km "(" 'dired-details-r-toggle-combination-in-dired-mode)
            km)
  (unless (derived-mode-p 'dired-mode)
    (error "Not a Dired buffer"))

  (dired-details-r-update-invisibility-spec)
  (dired-details-r-update-isearch-settings)

  (cond
   ;; turn on
   (dired-details-r-mode
    ;; Update ellipsis glyph
    (dired-details-r-update-display-table)
    ;; local variables
    (setq-local dired-details-r-combination-name
                (caar dired-details-r-combinations))
    (setq-local dired-details-r-visible-parts
                (cdr (assq dired-details-r-combination-name
                           dired-details-r-combinations)))
    (setq-local dired-details-r-max-widths nil)

    ;; hook
    ;;(dired-details-r-enable-global-hooks)
    (add-hook 'wdired-mode-hook 'dired-details-r--wdired-mode-hook nil t)
    (add-hook 'dired-after-readin-hook 'dired-details-r--after-readin-hook 100 t) ;; Ensure called after all-the-icons-dired--after-readin-hook and image-dired-dired-after-readin-hook

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
    (remove-hook 'wdired-mode-hook 'dired-details-r--wdired-mode-hook t)
    (remove-hook 'dired-after-readin-hook 'dired-details-r--after-readin-hook t)

    ;; change appearance
    (dired-details-r-remove-all-appearance-changes))))

(defun dired-details-r--wdired-mode-hook ()
  ;; If the display of dired-details-r interferes with wdired, turn it off.
  (if (memq dired-details-r-overlay-method '(textprop textprop-and-overlay))
      (dired-details-r-remove-all-appearance-changes)
    ;; Show truncated part of filenames
    (dired-details-r-set-filename-overflow-visibility t)))


;;;; Set face to string


(defun dired-details-r-set-face-part (str part-name)
  (cond
   ;; highlight today
   ((and (eq part-name 'time)
         (string-match
          (format-time-string dired-details-r-date-format (current-time)) str))
    (propertize str 'face 'dired-details-r-today))
   (t str)))

(defun dired-details-r-set-face-details (str parts)
  ;; gray dot file
  (cond
   ((string-match "^\\." (dired-details-r-filename-part-filename parts))
    (propertize str 'face 'dired-details-r-dot-file))
   (t str)))


;;;; Process dired buffer


(defun dired-details-r-initialize-buffer-settings ()
  ;; Reset column widths
  (setq dired-details-r-max-widths nil)
  ;; Reset overlay method
  (setq dired-details-r-overlay-method nil))

(defun dired-details-r-foreach-filenames (beg end fun-at-filename)
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (ignore-errors
        (when (dired-move-to-filename)
          (if (looking-back dired-details-r-regexp (line-beginning-position))
              (funcall fun-at-filename))))
      (forward-line 1))))

(defmacro dired-details-r-do-filenames (beg end &rest body)
  (declare (indent 2))
  `(dired-details-r-foreach-filenames
    ,beg ,end
    (lambda ()
      ,@body)))

(defun dired-details-r-text-pixel-width (from to)
  ;; See `shr-pixel-column' technic.
  (let ((bol (line-beginning-position)))
    (if (eq (window-buffer) (current-buffer))
        (- (car (window-text-pixel-size nil bol to 100000))
           (car (window-text-pixel-size nil bol from 100000)))
      (save-window-excursion
        (set-window-dedicated-p nil nil)
        (set-window-buffer nil (current-buffer))
        (- (car (window-text-pixel-size nil bol to 100000))
           (car (window-text-pixel-size nil bol from 100000)))))))

(defun dired-details-r-width-before-filename (beginning-of-filename)
  "Return width of objects preceding file name."
  (if (and dired-details-r-consider-overlays-before-filename-p
           (display-graphic-p)
           ;; Find overlays before filename, for example image-dired thumbnails.
           ;; I want to avoid calling window-text-pixel-size as much as possible
           (seq-some (lambda (ov)
                       (or
                        ;; for image-dired
                        (and (= (overlay-start ov) beginning-of-filename)
                             (= (overlay-end ov) beginning-of-filename)
                             (overlay-get ov 'before-string))
                        ;; for all-the-icons-dired
                        (and (= (overlay-start ov) (1- beginning-of-filename))
                             (= (overlay-end ov) beginning-of-filename)
                             (overlay-get ov 'after-string))
                        ))
                     (overlays-in (1- beginning-of-filename)
                                  (1+ beginning-of-filename))))
      (let* (;; (char-w (dired-details-r-text-pixel-width
             ;;          beginning-of-filename (1+ beginning-of-filename)))
             ;;slow?
             (char-w (frame-char-width)) ;;@todo OK?
             (prev-w (dired-details-r-text-pixel-width
                      (1- beginning-of-filename) beginning-of-filename))
             (prev-chars (max 0 (1- (floor (+ 0.5 (/ prev-w char-w)))))))
        prev-chars)
    0))

(defun dired-details-r-match-part-strings ()
  "Return strings of text matched by looking-back dired-details-r-regexp."
  (nconc
   (mapcar #'(lambda (part-info)
               (match-string (dired-details-r-part-info-subexp part-info)))
           dired-details-r-part-info-list)
   ;; last element
   (list
    (list
     ;; thumbnail, icon, etc.
     (dired-details-r-width-before-filename (point))
     ;; filename
     (buffer-substring (point) (point-at-eol))))))

;; Access last element

(defun dired-details-r-filename-part (parts)
  (car (last parts)))

(defun dired-details-r-filename-part-filename (parts)
  (cadr (dired-details-r-filename-part parts)))

(defun dired-details-r-filename-part-width (parts)
  (let ((part (dired-details-r-filename-part parts)))
    (+ (car part)
       (string-width (cadr part)))))

(defun dired-details-r-max-part-widths (max-widths parts)
  "Calculate max width of parts and filename."
  (let* ((result (or max-widths (make-list (length parts) 0)))
         (r result)
         (s parts))
    (while (and r s)
      (setcar r (max
                 (car r)
                 (let ((spec (car s)))
                   (cond
                    ((listp spec)
                     (cl-loop for elt in spec
                              sum (cond
                                   ((integerp elt) elt)
                                   (t (string-width elt)))))
                    ((integerp spec)
                     spec)
                    (t
                     (string-width spec))))))
      (setq r (cdr r))
      (setq s (cdr s)))
    result))

(defun dired-details-r-make-details-string (max-widths parts)
  (let* ((filename-curr-width (dired-details-r-filename-part-width parts))
         (filename-max-width  (max
                               dired-details-r-min-filename-width
                               (min (dired-details-r-filename-part max-widths)
                                    dired-details-r-max-filename-width)))
         (overflow (if dired-details-r-truncate-filenames
                       0
                     (max 0 (- filename-curr-width filename-max-width)))))
    (concat
     ;; spaces after filename
     (make-string
      (max 1 (- filename-max-width filename-curr-width -1))
      ? )
     ;; details
     (mapconcat
      #'(lambda (part-name)
          (let* ((part-info (assq part-name dired-details-r-part-info-list))
                 (index (dired-details-r-part-info-index part-info))
                 (align-right (dired-details-r-part-info-align-right part-info))
                 (spaces-width (-  ;; (max width) - (current width)
                                (nth index max-widths)
                                (string-width (nth index parts))))
                 (overflow-delta (min spaces-width overflow))
                 (spaces-width (- spaces-width overflow-delta))
                 (spaces (make-string spaces-width ? ))
                 (value (dired-details-r-set-face-part (nth index parts)
                                                       part-name)))
            (setq overflow (- overflow overflow-delta))
            (if align-right (concat spaces value) (concat value spaces))))
      dired-details-r-visible-parts " "))))

(defun dired-details-r-set-text-properties-on-file-line (parts max-widths)
  ;; put details overlay
  (let ((details-str (dired-details-r-set-face-details
                      (dired-details-r-make-details-string
                       max-widths parts)
                      parts)))
    (dired-details-r-add-overlay (line-end-position) details-str))

  ;; erase details before filename
  (let ((details-beg (+ (line-beginning-position) 1)) ;; include second whitespace
        (details-end (1- (point)))) ;; keep whitespace after details. if not, wdired will not work properly
    (put-text-property details-beg details-end
                       'invisible 'dired-details-r-detail))

  ;; Truncate file name
  (when (and dired-details-r-truncate-filenames
             ;; Don't truncate if all details are hidden
             dired-details-r-visible-parts)
    (let* ((filename-part-w
            ;;NOTE: Include thumbnail and icon width
            (dired-details-r-filename-part-width parts))
           (filename-excess-w
            (max 0 (- filename-part-w
                      dired-details-r-max-filename-width))))
      ;;(message "filename=%s w=%s excess=%s last=%s" (dired-details-r-filename-part-filename parts) filename-part-w filename-excess-w (dired-details-r-filename-part parts))
      (when (> filename-excess-w 0)
        (let* ((filename
                (dired-details-r-filename-part-filename parts))
               (filename-w
                (string-width filename))
               (truncated-filename-w
                (- filename-w
                   filename-excess-w
                   dired-details-r-ellipsis-width))
               (pos (point))
               (pos-eol (line-end-position)))
          (let ((curr-w 0))
            (while (and (< pos pos-eol)
                        (progn
                          (cl-incf curr-w (char-width (char-after pos)))
                          (<= curr-w truncated-filename-w)))
              (cl-incf pos)))
          (put-text-property pos pos-eol
                             'invisible 'dired-details-r-filename-overflow))))))

(defun dired-details-r-set-appearance-changes (beg end)
  "Set text properties and overlays on file information lines."

  (dired-details-r-set-filename-overflow-visibility nil)

  (when (and (< beg end)
             (not (eq dired-details-r-visible-parts 'disabled))
             (listp dired-details-r-visible-parts))

    (dired-details-r-update-overlay-method beg end)

    (let ((max-widths dired-details-r-max-widths)
          lines)

      ;; Cache parts (Avoid computing length of parts twice)
      (dired-details-r-do-filenames beg end
        (push (cons
               (point)
               (dired-details-r-match-part-strings))
              lines))
      (setq lines (nreverse lines))

      ;; Calculate column width
      (dolist (line lines)
        (setq max-widths
              (dired-details-r-max-part-widths max-widths (cdr line))))
      (setq dired-details-r-max-widths max-widths)

      ;; Set text properties
      (with-silent-modifications
        (save-excursion
          (dolist (line lines)
            (goto-char (car line))
            (dired-details-r-set-text-properties-on-file-line
             (cdr line)
             max-widths)))))))

(defun dired-details-r-remove-all-appearance-changes ()
  "Remove all invisible text properties and overlays for dired-details-r."
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max)
                            '(invisible 'dired-details-r-detail)))
  (dired-details-r-remove-all-overlays))

(defun dired-details-r-update-invisibility-spec ()
  (if dired-details-r-mode
      (add-to-invisibility-spec 'dired-details-r-detail)
    (remove-from-invisibility-spec 'dired-details-r-detail))

  (dired-details-r-set-filename-overflow-visibility (not dired-details-r-mode)))

(defun dired-details-r-set-filename-overflow-visibility (visible)
  (if visible
      (remove-from-invisibility-spec '(dired-details-r-filename-overflow . t))
    (add-to-invisibility-spec '(dired-details-r-filename-overflow . t))))

(defun dired-details-r-update-display-table ()
  (if (and (stringp dired-details-r-ellipsis)
           (not (equal "" dired-details-r-ellipsis)))
      (progn
        (unless dired-details-r-display-table
          (setq dired-details-r-display-table (make-display-table)))
        (set-display-table-slot
         dired-details-r-display-table 4
         (vconcat (mapcar (lambda (c) (make-glyph-code
                                       c 'dired-details-r-ellipsis))
		          dired-details-r-ellipsis)))
        (setq buffer-display-table
              dired-details-r-display-table)
        (setq dired-details-r-ellipsis-width
              (string-width dired-details-r-ellipsis)))
    (setq buffer-display-table nil)
    (setq dired-details-r-ellipsis-width 3)))


;;;; Overlay Management


(defconst dired-details-r-max-num-lines-to-use-overlay 1000)

(defun dired-details-r-update-overlay-method (_beg end)
  ;; Use the text property when there are more than a certain number of lines.
  ;; Fast with text properties but problem with cursor movement.
  ;; Use overlays when the number of lines is small (most of the day).
  (pcase dired-details-r-overlay-method
    ('nil
     (if (>= (line-number-at-pos end) dired-details-r-max-num-lines-to-use-overlay)
         (setq dired-details-r-overlay-method 'textprop)
       (setq dired-details-r-overlay-method 'overlay)))
    ('overlay
     (when (>= (line-number-at-pos end) dired-details-r-max-num-lines-to-use-overlay)
       (setq dired-details-r-overlay-method 'textprop-and-overlay)))
    ;; ('textprop) Keep method
    ;; ('textprop-and-overlay) Keep method
    )
  ;;(message "dired-details-r-overlay-method=%s beg=%s end=%s" dired-details-r-overlay-method beg end)
  )


(defun dired-details-r-add-overlay (eol details-str)
  ;; Replacing \n with the display property makes previous-line very slow.
  ;; (When there are thousands of lines)

  ;; Slow
  ;; (set-text-properties
  ;;  eol (1+ eol)
  ;;  (list 'display (concat details-str "\n")))

  ;; Slow
  ;; (let ((ovl (make-overlay eol (1+ eol) nil t)))
  ;;   (overlay-put ovl 'dired-details-r t)
  ;;   (overlay-put ovl 'evaporate t)
  ;;   (overlay-put ovl 'display (concat details-str "\n")))

  ;; Fast but unable to put cursor at the end of filename while using wdired.
  ;; (let ((ovl (make-overlay eol (1+ eol) nil t)))
  ;;   (overlay-put ovl 'dired-details-r t)
  ;;   (overlay-put ovl 'evaporate t)
  ;;   (overlay-put ovl 'before-string details-str))

  (pcase dired-details-r-overlay-method
    ;; Fastest but unable to put cursor at the end of filename while using wdired.
    ((or 'textprop 'textprop-and-overlay)
     (put-text-property
      (1- eol) eol
      'display (format "%c%s" (char-after (1- eol)) details-str)))

    ;; Possible to put cursor at the end of filename.
    ;; Slower than using text property.
    ('overlay
     (let ((ovl (make-overlay eol (1+ eol) nil t)))
       (overlay-put ovl 'dired-details-r t)
       (overlay-put ovl 'evaporate t)
       (overlay-put ovl 'before-string (propertize details-str 'cursor 1))))))

(defun dired-details-r-remove-all-overlays ()
  ;;(message "on dired-details-r-remove-all-overlays ovmethod=%s" dired-details-r-overlay-method)
  ;; Remove display text property at the last character of lines
  (when (memq dired-details-r-overlay-method '(textprop textprop-and-overlay))
    (let ((inhibit-read-only t))
      (save-excursion
        (save-restriction
          (widen)
          (dired-details-r-do-filenames (point-min) (point-max)
            (let ((eol (line-end-position)))
              (remove-text-properties (1- eol) eol '(display))))))))

  ;; Remove overlays
  (when (memq dired-details-r-overlay-method '(overlay textprop-and-overlay))
    (remove-overlays (point-min) (point-max) 'dired-details-r t))

  ;; Reset overlay method
  (setq dired-details-r-overlay-method nil))

(defun dired-details-r-remove-all-overlays-on-revert ()
  ;; Should have already been removed by the effect of the evaporate property.
  ;;  (remove-overlays (point-min) (point-max) 'dired-details-r t)

  ;; Should be automatically removed when using text properties.
  )


;;;; Switch Combination


(defun dired-details-r-toggle-combination-in-dired-mode ()
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (dired-details-r-toggle-combination)
    (call-interactively 'self-insert-command)))

(defun dired-details-r-toggle-combination ()
  (interactive)
  ;; Rotate combination
  (dired-details-r-rotate-combination-variable)
  ;; Refresh buffer
  (if (equal (buffer-name) "*Find*")
      ;;@todo Check revert-buffer-function?
      ;; Do not revert-buffer as it will re-run find.
      ;;@todo Always use this method? (never use revert-buffer?)
      (progn
        (dired-details-r-remove-all-appearance-changes)
        (let ((inhibit-read-only t))
          (dired-details-r-set-appearance-changes (point-min) (point-max))))
    (revert-buffer)))

(defun dired-details-r-rotate-combination-variable ()
  (let* ((curr-combination-def
          (let ((combs dired-details-r-combinations))
            (while (and combs
                        (not (eq (caar combs)
                                 dired-details-r-combination-name)))
              (setq combs (cdr combs)))
            combs))
         (next-combination-def
          (or (cadr curr-combination-def)
              (car dired-details-r-combinations))))
    (setq dired-details-r-combination-name (car next-combination-def))
    (setq dired-details-r-visible-parts (cdr next-combination-def))))

(defun dired-details-r-mode-or-toggle-combination ()
  (interactive)
  (if dired-details-r-mode
      (dired-details-r-toggle-combination)
    (dired-details-r-mode)))


;;;; isearch

(defun dired-details-r-update-isearch-settings ()
  (if dired-details-r-mode
      (progn
        (setq-local search-invisible t)
        (add-hook 'isearch-mode-hook 'dired-details-r-isearch-start nil t)
        (add-hook 'isearch-mode-end-hook 'dired-details-r-isearch-end nil t))
    (kill-local-variable 'search-invisible)
    (remove-hook 'isearch-mode-hook 'dired-details-r-isearch-start t)
    (remove-hook 'isearch-mode-end-hook 'dired-details-r-isearch-end t)))

(defun dired-details-r-isearch-start ()
  (dired-details-r-set-filename-overflow-visibility t))

(defun dired-details-r-isearch-end ()
  (dired-details-r-set-filename-overflow-visibility nil))


;;;; Observe Dired Buffer Changes


;; (defvar dired-details-r-enabled-global-hooks nil)

;; (defun dired-details-r-enable-global-hooks ()
;;   (when (not dired-details-r-enabled-global-hooks)
;;     (advice-add 'dired-insert-set-properties
;;                 :after
;;                 'dired-details-r--dired-insert-set-properties-hook)
;;     (advice-add 'dired-revert
;;                 :before
;;                 'dired-details-r--dired-revert-hook)
;;     (setq dired-details-r-enabled-global-hooks t)))

;; (defun dired-details-r-disable-global-hooks ()
;;   (when dired-details-r-enabled-global-hooks
;;     (advice-remove 'dired-insert-set-properties
;;                    'dired-details-r--dired-insert-set-properties-hook)
;;     (advice-remove 'dired-revert
;;                    'dired-details-r--dired-revert-hook)
;;     (setq dired-details-r-enabled-global-hooks nil)))


;; (defun dired-details-r--dired-insert-set-properties-hook (beg end)
;;   (when dired-details-r-mode
;;     ;; Insert text properties and overlays from beg to end
;;     (dired-details-r-set-appearance-changes beg end)))

;; (defun dired-details-r--dired-revert-hook (&optional _arg _noconfirm)
;;   (when dired-details-r-mode
;;     ;; Remove all overlays (unnecessary? evaporate property is used)
;;     (dired-details-r-remove-all-overlays-on-revert)
;;     ;; Reset column width and overlay method
;;     (dired-details-r-initialize-buffer-settings)))

(defun dired-details-r--after-readin-hook ()
  (when dired-details-r-mode
    ;; Remove all overlays (unnecessary? evaporate property is used)
    (dired-details-r-remove-all-overlays-on-revert)
    ;; Reset column width and overlay method
    (dired-details-r-initialize-buffer-settings)
    ;; Insert text properties and overlays from beg to end
    (dired-details-r-set-appearance-changes (point-min) (point-max))))


;;;; Support for find-dired


(defcustom dired-details-r-use-in-find-dired nil
  "If non-nil, the layout is updated after the execution of find-dired.

This is not recommended as the layout tends to be corrupted by
long path names.

You can change the layout by pressing `('."
  :group 'dired-details-r
  :type 'boolean)

(when dired-details-r-use-in-find-dired
  (defun dired-details-r--after-find-dired-sentinel (&rest _)
    (dired-details-r--after-readin-hook))

  (advice-add 'find-dired-sentinel :after
              'dired-details-r--after-find-dired-sentinel))

;;;; Setup


;;;###autoload
(defun dired-details-r-setup ()
  (interactive)
  ;; Ensure called after all-the-icons-dired-mode
  (add-hook 'dired-mode-hook 'dired-details-r--dired-mode-hook 100))

(defun dired-details-r-uninstall ()
  (interactive)
  ;; (dired-details-r-disable-global-hooks)
  (remove-hook 'dired-mode-hook 'dired-details-r--dired-mode-hook)
  ;; turn off dired-details-r-mode for all Dired buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when dired-details-r-mode
        (dired-details-r-mode -1)))))

(defun dired-details-r--dired-mode-hook ()
  (dired-details-r-mode))


;;;; Global Mode


;; NOTE: It is difficult to use define-globalized-minor-mode to
;; precisely control the timing of layout processing.

;;;###autoload
(define-minor-mode global-dired-details-r-mode
  "Global global-dired-details-r-mode."
  :group 'dired
  :global t
  (if global-dired-details-r-mode
      (dired-details-r-setup)
    (dired-details-r-uninstall)))


(provide 'dired-details-r)
;;; dired-details-r.el ends here
