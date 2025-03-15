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

(defconst dired-details-r-verbose nil)

;;;; Customizable variables


(defgroup dired-details-r nil
  "Display file details to the right of the file name in dired."
  :group 'dired)

(defcustom dired-details-r-max-width 'dired-details-r-max-width-auto
  "The maximum width to use for file names and details.

`Auto' (dired-details-r-max-width-auto) means the current window
width minus 3.

Whatever you specify, the maximum width cannot be less than the
sum of `dired-details-r-min-filename-width' and the total detail
width."
  :group 'dired-details-r
  :type '(choice (const :tag "Auto" dired-details-r-max-width-auto)
                 (const :tag "No Limit" 10000)
                 (integer :tag "Columns" 77)
                 (function :tag "Function")))

(defcustom dired-details-r-min-filename-width 28
  "Width always reserved for file names.

The area for displaying file names is never less than this width."
  :group 'dired-details-r :type 'integer)

(defcustom dired-details-r-max-filename-width 'auto
  "The maximum width to always reserve for file names in the entire buffer.

Even if there is a file name whose length exceeds this width, the
layout of the entire buffer will not change any further.

Symbol `auto' means calculate from details total width and
`dired-details-r-max-width'."
  :group 'dired-details-r
  :type '(choice (integer :tag "Columns" 52)
                 (const :tag "Auto" auto)))

(defcustom dired-details-r-combinations
  '((size-time  . (size time))
    ;;(time-size  . (time size)) ;; Which do you prefer?
    (no-details . ())
    (disabled   . disabled)
    (all        . (size time perms links user group))
    (left-right . (time size filename perms links user group))
    (left       . (time size filename))
    )
  "A list of combinations of file detail information to display.

The symbol `filename' can be specified only once. If not
specified, all parts will appear to the right of the file
name (for compatibility).

 The combination is rotated by calling
 `dired-details-r-toggle-combination' command."
  :group 'dired-details-r
  :type '(repeat sexp))

(defcustom dired-details-r-truncate-lines t
  "If non-nil, truncate lines."
  :group 'dired-details-r
  :type 'boolean)

(defcustom dired-details-r-highlight-today t
  "A non-nil value means to fontify today's date with the
`dired-details-r-today' face.

The `dired-details-r-date-format' variable must be set correctly in
order for dates to be recognized."
  :group 'dired-details-r
  :type 'boolean)

(defcustom dired-details-r-date-format "%b %e"
  "A format string to use when determining whether a date is today.

Used by the `dired-details-r-set-face-part' function to determine
whether to apply the `dired-details-r-today' face to the
timestamp part.

The value is specified as the first argument of `format-time-string'.

May be related to `ls-lisp-format-time-list'.

For example, if the date format output by ls is 2023-01-02 12:34,
specify %Y-%m-%d."
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
  :group 'dired-details-r
  :type '(choice (const :tag "Default" nil)
                 (string :tag "String" :value "...")))

(defcustom dired-details-r-preferred-overlay-method nil
  "The method by which detailed information is displayed.

nil means switch automatically. Use overlays for small number of
files and text properties for large number.

The symbol `textprop' means use only text properties.

The symbol `text' means to insert detailed information text directly
into the buffer, without using an overlay or text properties.
Inserting text before or after file names may cause some existing Emacs
Lisp code to malfunction. I have already added workaround code for some
issues I noticed.

The method is related to display speed and wdired behavior."
  :group 'dired-details-r
  :type '(choice (const :tag "Auto" nil)
                 (const :tag "Text property only" textprop)
                 (const :tag "Text insertion" text)))

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
   "\\(\\([^ ][-r][-w][^ ][-r][-w][^ ][-r][-w][^ ][?.+@]?\\) ?\\)" ;1,2:permissions
   "\\( *\\([0-9]+\\) +\\)" ;3,4:link count
   "\\(\\([^ ]+\\) +\\)" ;5,6:user
   "\\(\\([^ ]+\\) +\\)" ;7,8:group (7 including space before size)
   "\\(\\([0-9]+[.,0-9]*[BkKMGTPEZYRQ]?\\) \\)" ;9,10:size
   "\\(\\(.+\\)\\)")) ;11,12:time

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

(defvar-local dired-details-r-max-widths nil
  "The maximum width of each part of the file details measured
 the last time the entire buffer was updated.")

(defvar-local dired-details-r-filename-column-width nil
  "The width allocated for displaying filenames the last time the
entire buffer was updated.")

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
    (with-silent-modifications
      (dired-details-r-apply-appearance-changes-whole-buffer))

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
    (dired-details-r-remove-appearance-changes-whole-buffer))))

(defun dired-details-r--wdired-mode-hook ()
  ;; If the display of dired-details-r interferes with wdired, turn it off.
  (if (memq dired-details-r-overlay-method '(textprop textprop-and-overlay))
      (dired-details-r-remove-appearance-changes-whole-buffer)
    ;; Show truncated part of filenames
    (dired-details-r-set-filename-overflow-visibility t)))


;;;; Set face to string


(defun dired-details-r-set-face-part (str part-name)
  (cond
   ;; highlight today
   ((and (eq part-name 'time)
         dired-details-r-highlight-today
         (fboundp 'string-search) ;; Emacs 28 or later
         (string-search
          (format-time-string dired-details-r-date-format (current-time))
          str))
    (propertize str
                'face 'dired-details-r-today
                'font-lock-face 'dired-details-r-today))
   (t str)))

(defun dired-details-r-set-face-details (str parts)
  ;; gray dot file
  (cond
   ((string-match "^\\." (dired-details-r-filename-part-filename parts))
    (propertize str 'face 'dired-details-r-dot-file))
   (t str)))


;;;; Process dired buffer

;; File Line:
;; * lrwxrwxrwx  1 misohena users  4 24-12-10 12:45 latest -> v120
;; mark perms links user group size time L thumb-and-icon filename-and-symlink R
;;
;; L = Left insertion point for file details
;; R = Right insertion point for file details

(defun dired-details-r-initialize-buffer-settings ()
  ;; Reset column widths
  (setq dired-details-r-max-widths nil)
  ;; Reset overlay method
  (setq dired-details-r-overlay-method nil))

;; Visit File Names

(defun dired-details-r-foreach-filenames (beg end fun-at-filename)
  (save-excursion
    (goto-char beg)
    (while (< (point) (or end (point-max)))
      (ignore-errors
        (when (dired-move-to-filename)
          (if (save-excursion
                (dired-details-r-back-to-left-ins-point)
                (looking-back dired-details-r-regexp (line-beginning-position)))
              (funcall fun-at-filename))))
      (forward-line 1))))

(defmacro dired-details-r-do-filenames (beg end &rest body)
  (declare (indent 2))
  `(dired-details-r-foreach-filenames
    ,beg ,end
    (lambda ()
      ,@body)))

;; Measure Text Width

(defun dired-details-r-text-pixel-width-internal (from to)
  ;; Note: Narrow to region to avoid problems with `window-text-pixel-size'.
  ;; `window-text-pixel-size' may return a negative width if there
  ;; are full-width characters in the buffer.
  ;;
  ;; Note: To avoid changing the scroll position, narrow after
  ;; the window buffer changes.
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (car (window-text-pixel-size nil from to 100000)))))

(defun dired-details-r-text-pixel-width (from to)
  ;; See `shr-pixel-column' technic.
  (if (eq (window-buffer) (current-buffer))
      (dired-details-r-text-pixel-width-internal from to)
    (let ((old-wnb (window-next-buffers))
          (old-wpb (window-prev-buffers)))
      (prog1
          (save-window-excursion
            (set-window-dedicated-p nil nil)
            (set-window-buffer nil (current-buffer))
            (dired-details-r-text-pixel-width-internal from to))
        ;; `set-window-buffer' modifies window-prev-buffers!(*)
        ;; Then the first `switch-to-buffer' will move the point to the
        ;; location investigated here.
        ;; So prevent it.
        ;; * (`set-window-buffer' -> `record-window-buffer'
        ;;  -> `push-window-buffer-onto-prev' -> `set-window-prev-buffers')
        (set-window-next-buffers nil old-wnb)
        (set-window-prev-buffers nil old-wpb)
        ;; @todo (run-hooks 'buffer-list-update-hook)?
        ;;       See `record-window-buffer'.
        ))))

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
        (when dired-details-r-verbose
          (message "bofn=%s char-w=%s prev-w=%s prev-chars=%s"
                   beginning-of-filename
                   char-w prev-w prev-chars))
        prev-chars)
    0))

;;

(defun dired-details-r-collect-parts ()
  "Return strings of text matched by looking-back
dired-details-r-regexp and filename part on current line."
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
     (buffer-substring (point) (line-end-position))))))

;; Access last element

(defun dired-details-r-filename-part (parts)
  (car (last parts)))

(defun dired-details-r-filename-part-filename (parts)
  (cadr (dired-details-r-filename-part parts)))

(defun dired-details-r-filename-part-width (parts)
  (let ((part (dired-details-r-filename-part parts)))
    (+ (car part)
       (string-width (cadr part)))))

;; Divide Visible Parts

(defun dired-details-r-visible-parts-left ()
  (seq-take
   dired-details-r-visible-parts
   (cl-loop for part-name in dired-details-r-visible-parts
            for count from 0
            when (eq part-name 'filename) return count
            finally return 0)))

(defun dired-details-r-visible-parts-right ()
  (if-let* ((r (memq 'filename dired-details-r-visible-parts)))
      (cdr r)
    dired-details-r-visible-parts))

;; TEST
;; (setq dired-details-r-visible-parts '(size filename time))
;; (setq dired-details-r-visible-parts '(filename time))
;; (setq dired-details-r-visible-parts '(size filename))
;; (setq dired-details-r-visible-parts '(size time))
;; (dired-details-r-visible-parts-left)
;; (dired-details-r-visible-parts-right)

;; Layout Calculation

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

(defun dired-details-r-max-width-auto ()
  (- (window-width) 3))

(defun dired-details-r-max-width ()
  (pcase dired-details-r-max-width
    ((and (pred integerp) w) w)
    ((and (pred functionp) f) (funcall f))
    (_ 74)))

(defun dired-details-r-details-total-width (max-widths part-names)
  (if (or (null part-names) (null max-widths))
      0
    (+
     (cl-loop for part-name in part-names
              for part-info = (assq part-name dired-details-r-part-info-list)
              for index = (dired-details-r-part-info-index part-info)
              when index
              sum (nth index max-widths))
     (1- (length part-names)))))

(defun dired-details-r-max-filename-width (max-widths
                                           visible-parts-left
                                           visible-parts-right)
  (pcase dired-details-r-max-filename-width
    ((and (pred integerp) w) w)
    (_ ;;'auto
     (max
      dired-details-r-min-filename-width
      (- (dired-details-r-max-width)
         (dired-details-r-details-total-width max-widths visible-parts-left)
         (dired-details-r-details-total-width max-widths visible-parts-right)
         1))))) ;;space between filename and details

(defun dired-details-r-filename-extension-position-at ()
  ;; TODO: limit to (dired-move-to-end-of-filename t)
  ;;       1) foofoofoofoofoofoofoo.d -> barbarbarbar.d (symlink)
  ;;       2) foo.jpg  This is a description for foo.jpg generated by el-xmp
  (when (looking-at "[^\n]+\\(\\.[^./\n]+\\)")
    (match-beginning 1)))

(defun dired-details-r-truncate-filename-at (parts
                                             filename-column-width)
  "Truncate the width of the filename part of the current line to
less than FILENAME-COLUMN-WIDTH."
  (let* ((filename-part-w ;;NOTE: Include thumbnail and icon width
          (dired-details-r-filename-part-width parts))
         (filename-excess-w
          (max 0 (- filename-part-w filename-column-width))))
    (when (> filename-excess-w 0)
      (let* ((filename
              (dired-details-r-filename-part-filename parts))
             (filename-w
              (string-width filename))
             (beg-filename
              (point))
             (end-filename
              (line-end-position))
             (beg-extension
              (dired-details-r-filename-extension-position-at))
             (before-ext-w
              (when beg-extension
                (string-width (buffer-substring beg-filename
                                                beg-extension))))
             (truncate-before-ext-p
              (and beg-extension
                   (>  before-ext-w
                       (+ 4 ;; Minimum length before ellipsis (Avoid: ...ext)
                          filename-excess-w
                          dired-details-r-ellipsis-width))))
             (end-truncated
              (if truncate-before-ext-p beg-extension end-filename))
             (truncated-w-ideal
              (if truncate-before-ext-p
                  (- before-ext-w filename-excess-w dired-details-r-ellipsis-width)
                (- filename-w filename-excess-w dired-details-r-ellipsis-width)))
             (pos beg-filename)
             (truncated-w 0))
        (while (and (< pos end-truncated)
                    (let ((char-w (char-width (char-after pos))))
                      (if (> (+ truncated-w char-w) truncated-w-ideal)
                          nil
                        (cl-incf truncated-w char-w)
                        (cl-incf pos)
                        t))))
        (put-text-property pos end-truncated
                           'invisible 'dired-details-r-filename-overflow)
        (- truncated-w-ideal truncated-w)))))

(defun dired-details-r-make-details-string (max-widths
                                            parts
                                            visible-part-names
                                            left-overflow)
  (mapconcat
   #'(lambda (part-name)
       (let ((part-info (assq part-name dired-details-r-part-info-list)))
         (when part-info
           (let* ((index (dired-details-r-part-info-index part-info))
                  (align-right (dired-details-r-part-info-align-right
                                part-info))
                  (spaces-width (-  ;; (max width) - (current width)
                                 (nth index max-widths)
                                 (string-width (nth index parts))))
                  (overflow-delta (min spaces-width left-overflow))
                  (spaces-width (- spaces-width overflow-delta))
                  (spaces (make-string spaces-width ? ))
                  (value (dired-details-r-set-face-part (nth index parts)
                                                        part-name)))
             (cl-decf left-overflow overflow-delta)
             (if align-right (concat spaces value) (concat value spaces))))))
   visible-part-names " "))

(defun dired-details-r-make-details-string-for-right
    (max-widths
     parts
     visible-parts-right
     filename-column-width
     filename-truncated-shortage)
  (let* ((filename-curr-width (dired-details-r-filename-part-width parts))
         (filename-overflow (if dired-details-r-truncate-filenames
                                0
                              (max 0 (- filename-curr-width
                                        filename-column-width)))))
    (string-trim-right
     (concat
      ;; spaces after filename
      (make-string
       (+
        (or filename-truncated-shortage 0)
        (max 1 (- filename-column-width filename-curr-width -1)))
       ? )
      ;; details
      (dired-details-r-make-details-string max-widths
                                           parts
                                           visible-parts-right
                                           filename-overflow)))))

(defun dired-details-r-set-text-properties-on-file-line
    (parts
     max-widths visible-parts-left visible-parts-right filename-column-width)
  ;; Erase details before filename
  (let ((details-beg (+ (line-beginning-position) 1)) ;; include second whitespace
        (details-end (1- (point)))) ;; keep whitespace after details. if not, wdired will not work properly
    (put-text-property details-beg details-end
                       'invisible 'dired-details-r-detail))

  (let ((delta-buffer-size 0))
    ;; Put details on left
    (when visible-parts-left
      (cl-incf delta-buffer-size
               (dired-details-r-add-overlay-left
                (point)
                (dired-details-r-set-face-details
                 (dired-details-r-make-details-string max-widths parts
                                                      visible-parts-left 0)
                 parts))))

    ;; Filename and details on right
    (let* (;; Truncate file name
           (filename-truncated-shortage
            (when (and dired-details-r-truncate-filenames
                       ;; Don't truncate if details are not shown on right
                       visible-parts-right)
              (dired-details-r-truncate-filename-at parts filename-column-width))))

      ;; Put details on right
      (when visible-parts-right
        (cl-incf delta-buffer-size
                 (dired-details-r-add-overlay-right
                  (line-end-position)
                  (dired-details-r-set-face-details
                   (dired-details-r-make-details-string-for-right
                    max-widths parts visible-parts-right filename-column-width
                    filename-truncated-shortage)
                   parts)))))

    delta-buffer-size))

(defun dired-details-r-set-text-properties-for-each-file
    (lines
     max-widths visible-parts-left visible-parts-right filename-column-width)
  "LINES is the list obtained by `dired-details-r-get-file-lines'.

MAX-WIDTHS is the list obtained by
`dired-details-r-calculate-max-widths'.

visible-parts-left is the list obtained by
`dired-details-r-visible-parts-left'.

visible-parts-right is the list obtained by
`dired-details-r-visible-parts-right'."
  (with-silent-modifications
    (save-excursion
      (let ((delta-pos 0))
        (dolist (line lines)
          (goto-char (+ (car line) delta-pos))
          (cl-incf delta-pos
                   (dired-details-r-set-text-properties-on-file-line
                    (cdr line)
                    max-widths
                    visible-parts-left
                    visible-parts-right
                    filename-column-width)))))))

(defun dired-details-r-get-file-lines (beg end)
  (let (lines)
    (dired-details-r-do-filenames beg end
      (push (cons (point) (dired-details-r-collect-parts))
            lines))
    (nreverse lines)))

(defun dired-details-r-calculate-max-widths (lines max-widths)
  (dolist (line lines)
    (setq max-widths
          (dired-details-r-max-part-widths max-widths (cdr line))))
  max-widths)

(defun dired-details-r-update-max-widths (lines)
  (setq dired-details-r-max-widths
        (dired-details-r-calculate-max-widths lines
                                              dired-details-r-max-widths)))

(defun dired-details-r-update-filename-column-width ()
  (setq dired-details-r-filename-column-width
        (max
         ;; minimum width
         dired-details-r-min-filename-width
         (if dired-details-r-max-widths
             (min
              ;; maximum width of current file names
              (dired-details-r-filename-part dired-details-r-max-widths)
              ;; allocatable width for filenames in window
              (dired-details-r-max-filename-width
               dired-details-r-max-widths
               (dired-details-r-visible-parts-left)
               (dired-details-r-visible-parts-right)))
           0))))

(defun dired-details-r-apply-appearance-changes-whole-buffer ()
  (dired-details-r-set-filename-overflow-visibility nil)
  (dired-details-r-update-overlay-method (point-min)
                                         ;; nil means max-point
                                         nil)
  (dired-details-r-apply-appearance-changes (point-min)
                                            ;; nil means max-point
                                            nil
                                            ;; Update widths
                                            t))

(defun dired-details-r-apply-appearance-changes (beg end
                                                     &optional update-widths)
  "Set text properties and overlays on file information lines."

  (when dired-details-r-verbose
    (message "Enter dired-details-r-apply-appearance-changes beg=%s end=%s"
             beg end))

  (when (and (< beg (or end (point-max)))
             (not (eq dired-details-r-visible-parts 'disabled))
             (listp dired-details-r-visible-parts))

    (let (;; Cache parts (Avoid computing length of parts twice)
          (lines (dired-details-r-get-file-lines beg end)))

      ;; Calculate max column width
      (when update-widths
        ;; @todo The widths are also updated when adding subdirectories.
        ;;       The layout may break when use `dired-details-r-update-file'.

        ;; Calculate maximum width of each parts
        (dired-details-r-update-max-widths lines)

        ;; Update filename column display width
        (dired-details-r-update-filename-column-width))

      ;; Set text properties
      (dired-details-r-set-text-properties-for-each-file
       lines
       dired-details-r-max-widths
       (dired-details-r-visible-parts-left)
       (dired-details-r-visible-parts-right)
       dired-details-r-filename-column-width))))

(defun dired-details-r-update-appearance-changes (beg end)
  (dired-details-r-remove-appearance-changes beg end)
  (dired-details-r-apply-appearance-changes beg end))

(defun dired-details-r-update-current-line ()
  "Updates the appearance of the current line only. Useful for
fixing the misaligned appearance when shown/hidden thumbnail
in image-dired."
  (when dired-details-r-mode
    (let ((begin (line-beginning-position))
          ;; Include last "\n"
          (end (copy-marker (1+ (line-end-position)))))
      (dired-details-r-update-appearance-changes begin end)
      (set-marker end nil))))

(defun dired-details-r-update-file (file)
  "Updates the appearance of the specified FILE only. Useful for
fixing the misaligned appearance when shown/hidden thumbnail
in image-dired."
  (when dired-details-r-mode
    (save-excursion
      (when (dired-goto-file (expand-file-name file))
        (dired-details-r-update-current-line)))))


(defun dired-details-r-remove-appearance-changes-whole-buffer ()
  "Remove all invisible text properties and overlays for dired-details-r."
  (dired-details-r-remove-appearance-changes (point-min)
                                             ;; nil means point-max
                                             nil)
  ;; Reset overlay method
  (setq dired-details-r-overlay-method nil))

(defun dired-details-r-remove-appearance-changes (beg end)
  "Remove invisible text properties and overlays for dired-details-r."
  (with-silent-modifications
    (remove-text-properties beg (or end (point-max))
                            '(invisible 'dired-details-r-detail))
    (dired-details-r-remove-overlays beg end)))

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


;;;; Text Insertion

(defun dired-details-r-back-to-left-ins-point ()
  (goto-char (dired-details-r-left-ins-point (point))))

(defun dired-details-r-left-ins-range (bof)
  ;; timestamp|| BOFfilename
  ;; timestamp|LEFT-INSERTED-TEXT| filename
  (let* ((bol (save-excursion (goto-char bof) (line-beginning-position)))
         (pos (previous-single-property-change
               bof 'dired-details-r-left-ins-text nil bol)))
    (if (get-text-property pos 'dired-details-r-left-ins-text)
        (cons pos bof)
      (if (and (< bol pos)
               (get-text-property (1- pos) 'dired-details-r-left-ins-text))
          (cons (previous-single-property-change
                 pos 'dired-details-r-left-ins-text nil bol)
                pos)
        (cons (1- bof) (1- bof))))))

(defun dired-details-r-right-ins-range (point)
  ;; filename -> symlink||EOL
  ;; filename -> symlink|RIGHT-INSERTED-TEXT|EOL
  (save-excursion
    (goto-char point)
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (pos (previous-single-property-change
                 eol 'dired-details-r-right-ins-text nil bol)))
      (if (get-text-property pos 'dired-details-r-right-ins-text)
          (cons pos eol)
        (if (and (< bol pos)
                 (get-text-property (1- pos) 'dired-details-r-right-ins-text))
            (cons (previous-single-property-change
                   pos 'dired-details-r-right-ins-text nil bol)
                  pos)
          (cons eol eol))))))

(defun dired-details-r-left-ins-point (bof)
  (car (dired-details-r-left-ins-range bof)))

(defun dired-details-r-right-ins-point (point)
  (car (dired-details-r-right-ins-range point)))

(defun dired-details-r-remove-left-ins-text (bof)
  (dired-details-r-update-left-ins-text bof nil))

(defun dired-details-r-update-left-ins-text (bof new-text)
  (cl-destructuring-bind (begin . end) (dired-details-r-left-ins-range bof)
    (save-excursion
      (goto-char begin)
      (when (< begin end)
        (delete-region begin end))
      (when new-text
        (insert (propertize new-text 'dired-details-r-left-ins-text t))))
    ;; delta buffer size
    (- (length new-text) (- end begin))))

(defun dired-details-r-remove-right-ins-text (point)
  (dired-details-r-update-right-ins-text point nil))

(defun dired-details-r-update-right-ins-text (point new-text)
  (cl-destructuring-bind (begin . end) (dired-details-r-right-ins-range point)
    (save-excursion
      (goto-char begin)
      (when (< begin end)
        (delete-region begin end))
      (when new-text
        (insert (propertize new-text 'dired-details-r-right-ins-text t))))
    ;; delta buffer size
    (- (length new-text) (- end begin))))

;;;;; Fix various issues with text insertion

;;;;;; Can't mark with ~ and #

(defun dired-details-r--move-to-end-of-filename ()
  (let ((bol (pos-bol))
        (eol (pos-eol)))
    (when (< bol eol)
      (if (get-text-property (1- eol) 'dired-filename)
          (goto-char eol)
        (let ((pos (previous-single-property-change eol 'dired-filename
                                                    nil bol)))
          (if (< bol pos)
              (goto-char pos)
            (end-of-line)))))))

(defun dired-details-r--flag-auto-save-files:around (old-fun &rest args)
  (if (eq dired-details-r-overlay-method 'text)
      (apply #'dired-details-r--flag-auto-save-files args)
    (apply old-fun args)))

(defun dired-details-r--flag-auto-save-files (&optional unflag-p)
  "An alternative to the `dired-flag-auto-save-files' command.
Replaces the call to `end-of-line' with a jump to the end of the file."
  (let ((dired-marker-char (if unflag-p ?\s dired-del-marker)))
    (dired-mark-if
     (and (save-excursion
            ;; Changes start here
            (dired-details-r--move-to-end-of-filename)
            ;; Changes end here
            (or (eq (preceding-char) ?#)
                (if (eq (preceding-char) ?*)
                    (progn (forward-char -1) (eq (preceding-char) ?#)))))
          (not (looking-at-p dired-re-dir))
          (let ((fn (dired-get-filename t t)))
            (if fn (auto-save-file-name-p (file-name-nondirectory fn)))))
     "auto save file")))

(defun dired-details-r--flag-backup-files:around (old-fun &rest args)
  (if (eq dired-details-r-overlay-method 'text)
      (apply #'dired-details-r--flag-backup-files args)
    (apply old-fun args)))

(defun dired-details-r--flag-backup-files (&optional unflag-p)
  "An alternative to the `dired-flag-backup-files' command.
Replaces the call to `end-of-line' with a jump to the end of the file."
  (let ((dired-marker-char (if unflag-p ?\s dired-del-marker)))
    (dired-mark-if
     (and (save-excursion
            ;; Changes start here
            (dired-details-r--move-to-end-of-filename)
            ;; Changes end here
            (if (eq (preceding-char) ?*) (forward-char -1))
            (eq (preceding-char) ?~))
          (not (looking-at-p dired-re-dir))
          (let ((fn (dired-get-filename t t))) (if fn (backup-file-name-p fn))))
     "backup file")))

;;;;;; Font lock problems

;; - Suppress fontify on the right side of file names
;; - Fix regexp `$' in dired-font-lock-keywords for dired-ignored-face

(defun dired-details-r-match-file-name-regexp--text-ins (limit regexp)
  ;; Note: This function assumes that the `dired-filename' text
  ;; property is set correctly. You should not use
  ;; `dired-details-r-overlay-method' = 'text in environments where
  ;; the `dired-filename' text property is not set correctly.

  ;; Skip non-filename part
  (unless (get-text-property (point) 'dired-filename)
    (goto-char (next-single-property-change (point) 'dired-filename nil limit)))
  (when (< (point) limit)
    (let ((begin (if (get-text-property (1- (point)) 'dired-filename)
                     (previous-single-property-change (point) 'dired-filename
                                                      nil (pos-bol))
                   (point)))
          (end (next-single-property-change (point) 'dired-filename nil limit)))
      (unless (get-text-property end 'dired-filename) ;; END points end of fn
        (when (or (null regexp)
                  ;; Apply REGEXP only to the current filename
                  ;; ($ means the end of the filename)
                  (progn
                    (goto-char begin)
                    (save-restriction
                      (narrow-to-region begin end)
                      (re-search-forward regexp nil t))))
          (set-match-data (list begin end))
          (goto-char end)
          ;; Skip text after filename in the same line
          ;; (The number of calls to this function is suppressed)
          (end-of-line)
          t)))))

(defun dired-details-r-match-file-name-regexp--default (limit regexp)
  ;; Reproduce the following format:
  ;;           (".+" (dired-move-to-filename) nil SUBEXP-HIGHLIGHTERS)
  ;;   (REGEXP (".+" (dired-move-to-filename) nil SUBEXP-HIGHLIGHTERS))
  (when (or (null regexp)
            (re-search-forward regexp limit t))
    (when (dired-move-to-filename)
      (let ((begin (point)))
        (end-of-line)
        (set-match-data (list begin (point)))
        t))))

(defun dired-details-r-match-file-name-regexp (limit regexp)
  (when dired-details-r-verbose
    (message "match-file-name-regexp point=%s limit=%s regexp=%s"
             (point) limit (and regexp (truncate-string-to-width regexp 10))))

  (if (eq dired-details-r-overlay-method 'text)
      (dired-details-r-match-file-name-regexp--text-ins limit regexp)
    (dired-details-r-match-file-name-regexp--default limit regexp)))

(defun dired-details-r-match-file-name (limit)
  (dired-details-r-match-file-name-regexp limit nil))

(defvar dired-details-r-match-ignore-files-regexps nil)

(defun dired-details-r-match-ignore-files-regexps ()
  (if (and dired-details-r-match-ignore-files-regexps
           (eq (car dired-details-r-match-ignore-files-regexps)
               completion-ignored-extensions))
      dired-details-r-match-ignore-files-regexps
    (let ((common (concat
                   "\\(" (regexp-opt completion-ignored-extensions)
                   "\\|#\\|\\.#.+\\)[*=|]?")))
      (setq dired-details-r-match-ignore-files-regexps
            (cons completion-ignored-extensions
                  (cons (concat common "\\(?: \\|$\\)")
                        (concat common "$")))))))

(defun dired-details-r-match-ignored-files (limit)
  (dired-details-r-match-file-name-regexp
   limit
   (cddr (dired-details-r-match-ignore-files-regexps))))

(defconst dired-details-r-font-lock-keywords-to-add
  (list
   ;; Marked files.
   (list (concat "^[" (char-to-string dired-marker-char) "]")
         '(dired-details-r-match-file-name nil nil (0 dired-marked-face)))
   ;; Flagged files.
   (list (concat "^[" (char-to-string dired-del-marker) "]")
         '(dired-details-r-match-file-name nil nil (0 dired-flagged-face)))
   ;; Subdirectories.
   (list dired-re-dir
         '(dired-details-r-match-file-name nil nil (0 dired-directory-face)))
   ;; Files suffixed with `completion-ignored-extensions'.
   '(eval
     . (list
        (cadr (dired-details-r-match-ignore-files-regexps))
        '(dired-details-r-match-ignored-files (beginning-of-line) nil (0 dired-ignored-face))))
   ;; Sockets, pipes, block devices, char devices.
   (list dired-re-special
         '(dired-details-r-match-file-name nil nil (0 'dired-special)))))

(defconst dired-details-r-font-lock-keywords-to-delete
  (list
   ;; Marked files.
   (concat "^[" (char-to-string dired-marker-char) "]")
   ;; Flagged files.
   (concat "^[" (char-to-string dired-del-marker) "]")
   ;; Subdirectories.
   dired-re-dir
   ;; Files suffixed with `completion-ignored-extensions'.
   'eval
   'dired-details-r-match-ignored-files
   ;; Sockets, pipes, block devices, char devices.
   dired-re-special))

(defconst dired-details-r-font-lock-keywords
  (append
   dired-details-r-font-lock-keywords-to-add
   (seq-remove (lambda (keyword)
                 (member
                  (car keyword)
                  dired-details-r-font-lock-keywords-to-delete))
               dired-font-lock-keywords))
  "An alternative to `dired-font-lock-keywords'.

  `dired-font-lock-keywords' contains a regular expression that assumes
that the end of a line is the end of the filename, which causes
problems, so this fixes it.")

(defun dired-details-r-replace-font-lock-keywords ()
  "Replace `dired-font-lock-keywords' in `font-lock-defaults' with
`dired-details-r-font-lock-keywords'."
  (when (eq (car font-lock-defaults) 'dired-font-lock-keywords)
    (setq font-lock-defaults
          (cons 'dired-details-r-font-lock-keywords
                (cdr font-lock-defaults)))))

;;;;;; Setup and Teardown

(defun dired-details-r-text-ins-global-setup ()
  (advice-add #'dired-flag-auto-save-files
              :around #'dired-details-r--flag-auto-save-files:around)
  (advice-add #'dired-flag-backup-files
              :around #'dired-details-r--flag-backup-files:around)
  (add-hook 'dired-mode-hook #'dired-details-r-replace-font-lock-keywords))

(defun dired-details-r-text-ins-global-teardown ()
  (advice-remove #'dired-flag-auto-save-files
                 #'dired-details-r--flag-auto-save-files:around)
  (advice-remove #'dired-flag-backup-files
                 #'dired-details-r--flag-backup-files:around)
  (remove-hook 'dired-mode-hook #'dired-details-r-replace-font-lock-keywords))


;;;; Overlay Management


(defconst dired-details-r-max-num-lines-to-use-overlay 1000)

(defun dired-details-r-update-overlay-method (_beg end)
  (cond
   ((eq dired-details-r-preferred-overlay-method 'text)
    (setq dired-details-r-overlay-method 'text))
   ((eq dired-details-r-preferred-overlay-method 'textprop)
    ;; Use text properties only.
    (setq dired-details-r-overlay-method 'textprop))
   (t
    ;; Use the text property when there are more than a certain number of lines.
    ;; Fast with text properties but problem with cursor movement.
    ;; Use overlays when the number of lines is small (most of the day).
    (pcase dired-details-r-overlay-method
      ('nil
       (if (>= (line-number-at-pos (or end (point-max)))
               dired-details-r-max-num-lines-to-use-overlay)
           (setq dired-details-r-overlay-method 'textprop)
         (setq dired-details-r-overlay-method 'overlay)))
      ('overlay
       (when (>= (line-number-at-pos (or end (point-max)))
                 dired-details-r-max-num-lines-to-use-overlay)
         (setq dired-details-r-overlay-method 'textprop-and-overlay)))
      ;; ('textprop) Keep method
      ;; ('textprop-and-overlay) Keep method
      )))
  ;;(message "dired-details-r-overlay-method=%s beg=%s end=%s" dired-details-r-overlay-method beg end)
  )


(defun dired-details-r-add-overlay-right (eol details-str)
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
    ('text
     (dired-details-r-update-right-ins-text eol details-str))

    ;; Fastest but unable to put cursor at the end of filename while using wdired.
    ((or 'textprop 'textprop-and-overlay)
     (put-text-property
      (1- eol) eol
      'display (format "%c%s" (char-after (1- eol)) details-str))
     0)

    ;; Possible to put cursor at the end of filename.
    ;; Slower than using text property.
    ('overlay
     (let ((ovl (make-overlay eol (1+ eol) nil t)))
       (overlay-put ovl 'dired-details-r t)
       (overlay-put ovl 'evaporate t)
       (overlay-put ovl 'before-string
                    (propertize
                     details-str
                     'display (propertize details-str 'cursor 1) ;; See: https://misohena.jp/blog/2023-08-18-before-string-is-not-applied-to-another-overlay-face.html
                     'cursor 1 ;;See: https://misohena.jp/blog/2022-01-01-emacs-replacing-line-breaks-using-display-properties-is-very-slow.html
                     ))
       0))
    (_
     0)))

(defun dired-details-r-add-overlay-left (bof details-str)
  "Show DETAILS-STR on left of BOF (beginning of filename) !!
dired-details-l !?"
  (pcase dired-details-r-overlay-method
    ('text
     (dired-details-r-update-left-ins-text bof details-str))

    ((or 'textprop 'textprop-and-overlay)
     (put-text-property (- bof 2) (- bof 1) 'display details-str)
     (put-text-property (- bof 2) (- bof 1) 'invisible nil)
     0)

    ;; Slower than using text property.
    ('overlay
     (let ((ovl (make-overlay (1- bof) bof nil t)))
       (overlay-put ovl 'dired-details-r t)
       (overlay-put ovl 'evaporate t)
       (overlay-put ovl 'before-string (propertize details-str 'cursor 1))
       0))
    (_
     0)))

(defun dired-details-r-remove-overlays (beg end)
  ;;(message "on dired-details-r-remove-overlays ovmethod=%s" dired-details-r-overlay-method)

  ;; Called in with-silent-modification

  ;; Remove inserted text
  (when (eq dired-details-r-overlay-method 'text)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char beg)
        (while (< (point) (or end (point-max)))
          (when (dired-move-to-filename)
            (dired-details-r-remove-left-ins-text (point))
            (dired-details-r-remove-right-ins-text (point)))
          (forward-line)))))

  ;; Remove display text property at the last character of lines
  (when (memq dired-details-r-overlay-method '(textprop textprop-and-overlay))
    (save-excursion
      (save-restriction
        (widen)
        (dired-details-r-do-filenames beg end
          (let ((bof (point)) ;;beginning of filename
                (eol (line-end-position)))
            (remove-text-properties (1- bof) bof '(display)) ;;left
            (remove-text-properties (1- eol) eol '(display))))))) ;;right

  ;; Remove overlays
  (when (memq dired-details-r-overlay-method '(overlay textprop-and-overlay))
    (remove-overlays beg (or end (point-max)) 'dired-details-r t)))


(defun dired-details-r-remove-all-overlays-on-revert ()
  ;; Should have already been removed by the effect of the evaporate property.
  ;;  (with-silent-modifications
  ;;    (remove-overlays (point-min) (point-max) 'dired-details-r t))

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
      (with-silent-modifications
        (dired-details-r-remove-appearance-changes-whole-buffer)
        (dired-details-r-apply-appearance-changes-whole-buffer))
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
;;     (dired-details-r-apply-appearance-changes beg end)))

;; (defun dired-details-r--dired-revert-hook (&optional _arg _noconfirm)
;;   (when dired-details-r-mode
;;     ;; Remove all overlays (unnecessary? evaporate property is used)
;;     (dired-details-r-remove-all-overlays-on-revert)
;;     ;; Reset column width and overlay method
;;     (dired-details-r-initialize-buffer-settings)))

(defun dired-details-r--after-readin-hook ()
  (when dired-details-r-verbose
    (message "Enter dired-details-r--after-readin-hook"))
  (when dired-details-r-mode
    ;; Remove all overlays (unnecessary? evaporate property is used)
    (dired-details-r-remove-all-overlays-on-revert)
    ;; Reset column width and overlay method
    (dired-details-r-initialize-buffer-settings)
    ;; Insert text properties and overlays from beg to end
    (dired-details-r-apply-appearance-changes-whole-buffer))
  (when dired-details-r-verbose
    (message "Leave dired-details-r--after-readin-hook")))


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
  (add-hook 'dired-mode-hook 'dired-details-r--dired-mode-hook 100)
  ;; For text insertion method
  (dired-details-r-text-ins-global-setup))

(defun dired-details-r-uninstall ()
  (interactive)
  ;; (dired-details-r-disable-global-hooks)
  (remove-hook 'dired-mode-hook 'dired-details-r--dired-mode-hook)
  ;; turn off dired-details-r-mode for all Dired buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when dired-details-r-mode
        (dired-details-r-mode -1))))
  ;; For text insertion method
  (dired-details-r-text-ins-global-teardown))

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
