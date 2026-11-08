;;; emureader-synctex.el --- SyncTeX support for EMuReader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Correlate positions in a PDF shown by EMuReader with positions in
;; the LaTeX source that produced it, using the `synctex' command line
;; tool and the .synctex.gz file written by "pdflatex -synctex=1".
;;
;; - `emureader-synctex-forward-search' goes from a source line to the
;;   spot on the page, and briefly points an arrow at it.
;; - `emureader-synctex-inverse-search' (C-mouse-1 or C-c C-g in the
;;   document) goes from a spot on the page to the source line.
;;
;; With AUCTeX, select the viewer "EμReader" for PDF output,
;;
;;   (setq TeX-view-program-selection '((output-pdf "EμReader")))
;;
;; and with `TeX-source-correlate-mode' on, C-c C-v does a forward
;; search.

;;; Code:

(require 'emureader)

(defgroup emureader-synctex nil
  "SyncTeX support for EMuReader."
  :prefix "emureader-synctex-"
  :group 'emureader)

(defcustom emureader-synctex-program "synctex"
  "The SyncTeX command line tool."
  :type 'string)

(defcustom emureader-synctex-display-action
  '((display-buffer-reuse-window display-buffer-pop-up-window
     display-buffer-use-some-window)
    (inhibit-same-window . t)
    (reusable-frames . visible))
  "Action used to display the document on a forward search.
See `display-buffer' for its format."
  :type display-buffer--action-custom-type)

(defcustom emureader-synctex-source-display-action
  '((display-buffer-reuse-window display-buffer-use-some-window
     display-buffer-pop-up-window)
    (inhibit-same-window . t)
    (reusable-frames . visible))
  "Action used to display the source file on an inverse search.
See `display-buffer' for its format."
  :type display-buffer--action-custom-type)

(defun emureader-synctex--run (&rest args)
  "Run `emureader-synctex-program' with ARGS and parse its records.
Return a list of alists, one per record, mapping keys such as \"Page\"
to string values."
  (unless (executable-find emureader-synctex-program)
    (user-error "Cannot find the `%s' program" emureader-synctex-program))
  (with-temp-buffer
    (let ((status (apply #'call-process emureader-synctex-program
			 nil t nil args))
	  records in-result)
      (unless (eql status 0)
	(user-error "SyncTeX failed: %s" (string-trim (buffer-string))))
      (goto-char (point-min))
      (while (not (eobp))
	(let ((line (buffer-substring-no-properties
		     (point) (line-end-position))))
	  (forward-line 1)
	  (cond ((string= line "SyncTeX result begin") (setq in-result t))
		((string= line "SyncTeX result end") (setq in-result nil))
		((and in-result (string-match "\\`\\([A-Za-z]+\\):\\(.*\\)\\'" line))
		 (let ((key (match-string 1 line))
		       (value (match-string 2 line)))
		   (when (or (null records) (string= key "Output"))
		     (push nil records))
		   (push (cons key value) (car records)))))))
      (nreverse (mapcar #'nreverse records)))))

(defun emureader-synctex--number (record key)
  "Return the number stored under KEY in RECORD, or nil."
  (when-let* ((value (cdr (assoc key record))))
    (string-to-number value)))

(defun emureader-synctex--check-data (pdf)
  "Signal a `user-error' unless PDF has SyncTeX data."
  (unless (seq-some (lambda (ext)
		      (file-exists-p (concat (file-name-sans-extension pdf) ext)))
		    '(".synctex.gz" ".synctex"))
    (user-error "No SyncTeX data for %s; compile with -synctex=1"
		(file-name-nondirectory pdf))))

(defun emureader-synctex-view (pdf tex line &optional column)
  "Return where LINE (and COLUMN) of TEX appears in PDF.
The value is a plist (:page PAGE :x X :y Y :width W :height H), where
X and Y locate the top left corner of the box in PDF points from the
page's top left corner, or nil when SyncTeX knows no such place."
  (let ((pdf (expand-file-name pdf)))
    (emureader-synctex--check-data pdf)
    (when-let* ((record (car (emureader-synctex--run
			      "view" "-i"
			      (format "%d:%d:%s" line (max 0 (or column 0))
				      (expand-file-name tex))
			      "-o" pdf)))
		(page (emureader-synctex--number record "Page"))
		(_ (> page 0)))
      (let ((h (emureader-synctex--number record "h"))
	    (v (emureader-synctex--number record "v"))
	    (w (emureader-synctex--number record "W"))
	    (ht (emureader-synctex--number record "H")))
	(list :page page :x h :y (- v ht) :width w :height ht)))))

(defun emureader-synctex-edit (pdf page x y)
  "Return the source position of point X, Y on PAGE of PDF.
X and Y are in PDF points from the page's top left corner.  The value
is a list (FILE LINE COLUMN); COLUMN is nil when unknown."
  (let ((pdf (expand-file-name pdf)))
    (emureader-synctex--check-data pdf)
    (when-let* ((record (car (emureader-synctex--run
			      "edit" "-o"
			      (format "%d:%f:%f:%s" page x y pdf))))
		(input (cdr (assoc "Input" record)))
		(line (emureader-synctex--number record "Line"))
		(_ (> line 0)))
      (let ((column (emureader-synctex--number record "Column")))
	(list (expand-file-name input (file-name-directory pdf))
	      line
	      (and column (>= column 0) column))))))

(defun emureader-synctex--document-buffer (pdf)
  "Return an EMuReader buffer showing PDF, opening it when needed."
  (or (emureader-find-doc-noselect pdf)
      (user-error "Cannot open %s" pdf)))

;;;###autoload
(defun emureader-synctex-forward-search (pdf tex line &optional column)
  "Show where LINE of TEX appears in PDF and point at it.
COLUMN, when non-nil, refines the position.  Interactively, use the
current buffer's file and line, and the PDF of the same name.

The document is displayed with `emureader-synctex-display-action' and
reverted first when PDF changed on disk."
  (interactive
   (let ((tex (or buffer-file-name (user-error "Buffer is not visiting a file"))))
     (list (read-file-name "PDF: " nil nil t
			   (concat (file-name-base tex) ".pdf"))
	   tex
	   (save-restriction (widen) (line-number-at-pos))
	   (current-column))))
  (let* ((pdf (expand-file-name pdf))
	 (place (or (emureader-synctex-view pdf tex line column)
		    (user-error "SyncTeX found line %d of %s nowhere in %s"
				line (file-name-nondirectory tex)
				(file-name-nondirectory pdf))))
	 (buffer (emureader-synctex--document-buffer pdf))
	 (shown (get-buffer-window buffer 'visible))
	 (window (or shown
		     (display-buffer buffer emureader-synctex-display-action))))
    (unless (window-live-p window)
      (user-error "Cannot display %s" (buffer-name buffer)))
    ;; Window overlays are made when redisplay runs the window hooks.
    (redisplay t)
    (with-selected-window window
      (unless shown
	;; The page was fitted to whichever window was selected when
	;; the document was opened.
	(funcall emureader-default-fit))
      (unless (verify-visited-file-modtime buffer)
	(emureader--non-queue-refresh-doc-buffer)
	(redisplay t))
      (unless (eql (emureader-current-doc-pagenumber window)
		   (plist-get place :page))
	(emureader-goto-page (plist-get place :page)))
      (let* ((size (emureader-current-doc-page-size window))
	     (x (/ (plist-get place :x) (car size)))
	     (y (/ (plist-get place :y) (cdr size)))
	     (w (/ (plist-get place :width) (car size)))
	     (h (/ (plist-get place :height) (cdr size))))
	(emureader-scroll-to-page-fraction x (+ y (/ h 2)) window)
	(redisplay t)
	(emureader-flash-position x y window w h)
	place))))

(defun emureader-synctex--window-centre-posn (window)
  "Return the position at the centre of WINDOW's text area."
  (posn-at-x-y (/ (window-body-width window t) 2)
	       (/ (window-body-height window t) 2)
	       window))

;;;###autoload
(defun emureader-synctex-inverse-search (&optional event)
  "Visit the source line of the spot clicked in the document.
EVENT is the mouse event; without one (from the keyboard) the spot at
the centre of the selected window is used.  The source is shown with
`emureader-synctex-source-display-action'."
  (interactive (list (and (mouse-event-p last-nonmenu-event)
			  last-nonmenu-event)))
  (let* ((posn (if event
		   (event-start event)
		 (emureader-synctex--window-centre-posn (selected-window))))
	 (window (posn-window posn)))
    (unless (and (windowp window)
		 (with-current-buffer (window-buffer window)
		   (derived-mode-p 'emureader-mode)))
      (user-error "Not in an EMuReader document"))
    (with-current-buffer (window-buffer window)
      (let* ((fraction (or (emureader-posn-page-fraction posn)
			   (user-error "Not on a page")))
	     (size (emureader-current-doc-page-size window))
	     (page (emureader-current-doc-pagenumber window))
	     (source (or (emureader-synctex-edit
			  (emureader--local-file) page
			  (* (car fraction) (car size))
			  (* (cdr fraction) (cdr size)))
			 (user-error "SyncTeX knows no source for this spot")))
	     (file (nth 0 source))
	     (buffer (or (find-buffer-visiting file)
			 (find-file-noselect file))))
	;; Display the source from the document's window, so that it is
	;; the document that stays in view.
	(select-window window)
	(select-window
	 (display-buffer buffer emureader-synctex-source-display-action))
	(widen)
	(goto-char (point-min))
	(forward-line (1- (nth 1 source)))
	(when (nth 2 source)
	  (move-to-column (nth 2 source)))
	(when (fboundp 'pulse-momentary-highlight-one-line)
	  (pulse-momentary-highlight-one-line (point)))
	source))))

;;; AUCTeX

(declare-function TeX-active-master "tex" (&optional extension nondirectory ignore))
(declare-function TeX-output-extension "tex")
(defvar TeX-source-correlate-mode)

;;;###autoload
(defun emureader-synctex-tex-view ()
  "Show the output of the current AUCTeX document in EMuReader.
With `TeX-source-correlate-mode', do a forward search to the current
line; otherwise just display the PDF.  This is the command of the
\"EμReader\" entry in `TeX-view-program-list'."
  (let ((pdf (expand-file-name (TeX-active-master (TeX-output-extension)))))
    (if (and (bound-and-true-p TeX-source-correlate-mode) buffer-file-name)
	(emureader-synctex-forward-search
	 pdf buffer-file-name
	 (save-restriction (widen) (line-number-at-pos))
	 (current-column))
      (display-buffer (emureader-synctex--document-buffer pdf)
		      emureader-synctex-display-action))))

;;;###autoload
(with-eval-after-load 'tex
  (defvar TeX-view-program-list)
  (add-to-list 'TeX-view-program-list
	       '("EμReader" emureader-synctex-tex-view)))

(provide 'emureader-synctex)
;;; emureader-synctex.el ends here
