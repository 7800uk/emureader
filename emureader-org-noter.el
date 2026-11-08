;;; emureader-org-noter.el --- org-noter support for EMuReader  -*- lexical-binding: t; -*-

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

;; Lets org-noter (https://github.com/org-noter/org-noter) annotate
;; documents shown in `emureader-mode', in the way its own modules
;; support doc-view, pdf-tools, nov and djvu.
;;
;;   (with-eval-after-load 'org-noter
;;     (require 'emureader-org-noter))
;;
;; Locations are (PAGE . V) or (PAGE V . H), V and H being fractions
;; of the page's height and width, as for PDF Tools.  Notes therefore
;; carry over between EMuReader and PDF Tools.  The notes follow the
;; document as its pages change, `org-noter-insert-precise-note' asks
;; for a click on the page, and `org-noter-create-skeleton' turns the
;; document's outline into headings.
;;
;; Remote documents work through EMuReader's cache: a NOTER_DOCUMENT
;; such as "/ssh:host:/store/ab/ab12…" opens in EMuReader even without
;; an extension, the format being guessed from its contents.

;;; Code:

(require 'emureader)
(require 'org-noter-core nil t)

(defvar org-noter-supported-modes)
(defvar org-noter--doc-extensions)
(defvar org-noter--session)
(defvar org-noter-arrow-delay)
(defvar org-noter-property-note-location)
(defvar org-noter-property-doc-file)
(defvar org-noter--property-auto-save-last-location)
(defvar org-noter-doc-property-in-notes)
(defvar emureader-current-doc-outline)

(declare-function org-noter--valid-session "org-noter-core" (session))
(declare-function org-noter--session-doc-mode "org-noter-core" (session))
(declare-function org-noter--session-doc-buffer "org-noter-core" (session))
(declare-function org-noter--session-notes-buffer "org-noter-core" (session))
(declare-function org-noter--session-property-text "org-noter-core" (session))
(declare-function org-noter--get-location-page "org-noter-core" (location))
(declare-function org-noter--get-location-top "org-noter-core" (location))
(declare-function org-noter--get-location-left "org-noter-core" (location))
(declare-function org-noter--doc-location-change-handler "org-noter-core")
(declare-function org-noter--pretty-print-location "org-noter-core" (location))
(declare-function org-noter--parse-root "org-noter-core" (&optional info))
(declare-function org-noter--insert-heading "org-noter-core"
		  (level title &optional newlines-number location))
(declare-function org-noter--narrow-to-root "org-noter-core" (ast))
(declare-function org-element-property "org-element" (property node &optional dflt force-undefer))
(declare-function org-entry-put "org" (epom property value))
(declare-function org-at-heading-p "org" (&optional invisible-not-ok))
(declare-function org-fold-show-children "org-fold" (&optional level))
(declare-function outline-hide-subtree "outline")

(defun emureader-org-noter--session-p ()
  "Non-nil if the current org-noter session shows an EMuReader document."
  (and (boundp 'org-noter--session)
       (org-noter--valid-session org-noter--session)
       (eq (org-noter--session-doc-mode org-noter--session) 'emureader-mode)))

(defun emureader-org-noter--precise-info-p (info)
  "Non-nil if INFO is a vertical position V or a cons (V . H)."
  (or (numberp info)
      (and (consp info) (numberp (car info)) (numberp (cdr info)))))

(defun emureader-org-noter--approx-location (mode &optional precise-info _force-new-ref)
  "Return the location shown when MODE is `emureader-mode'.
The value is (PAGE . 0), or with PRECISE-INFO (PAGE . V) or
\(PAGE V . H)."
  (when (eq mode 'emureader-mode)
    (cons (emureader-current-doc-pagenumber (selected-window))
	  (if (emureader-org-noter--precise-info-p precise-info)
	      precise-info
	    0))))

(defun emureader-org-noter--setup (mode)
  "Set up the current buffer as an org-noter document when MODE is ours."
  (when (eq mode 'emureader-mode)
    (unless (derived-mode-p 'emureader-mode)
      (emureader-mode))
    (add-hook 'emureader-after-page-change-hook
	      #'org-noter--doc-location-change-handler nil t)
    t))

(defun emureader-org-noter--pretty-print-location (location)
  "Format LOCATION for the note location property."
  (when (emureader-org-noter--session-p)
    (let ((top (org-noter--get-location-top location)))
      (format "%s" (if (or (not top) (<= top 0))
		       (car location)
		     location)))))

(defun emureader-org-noter--pretty-print-location-for-title (location)
  "Format LOCATION for a note's default title."
  (when (emureader-org-noter--session-p)
    (let ((top (or (org-noter--get-location-top location) 0))
	  (left (or (org-noter--get-location-left location) 0)))
      (concat (format "%s" (org-noter--get-location-page location))
	      (when (or (> top 0) (> left 0))
		(format " V: %d%%" (round (* 100 top))))
	      (when (> left 0)
		(format " H: %d%%" (round (* 100 left))))))))

(defun emureader-org-noter--get-precise-info (mode window)
  "Ask for a click on the page in WINDOW when MODE is ours.
Return the clicked position as (V . H)."
  (when (eq mode 'emureader-mode)
    (let (fraction)
      (while (not fraction)
	(let ((event (read-event "Click where you want the start of the note to be!")))
	  (when (and (eq (car-safe event) 'mouse-1)
		     (eq window (posn-window (event-start event))))
	    (setq fraction (emureader-posn-page-fraction (event-start event))))))
      (cons (cdr fraction) (car fraction)))))

(defun emureader-org-noter--goto-location (mode location &optional window)
  "Show LOCATION in WINDOW when MODE is ours."
  (when (eq mode 'emureader-mode)
    (let ((window (or window (selected-window)))
	  (page (org-noter--get-location-page location))
	  (top (or (org-noter--get-location-top location) 0))
	  (left (or (org-noter--get-location-left location) 0)))
      (with-selected-window window
	;; org-noter may go to a location in a window that redisplay has
	;; not yet set up for the document.
	(unless (emureader--window-owned-p window)
	  (emureader--window-create-function window))
	(unless (eql page (emureader-current-doc-pagenumber window))
	  (emureader-goto-page page))
	(if (<= top 0)
	    (emureader--set-window-vscroll window 0 t)
	  (emureader-scroll-to-page-fraction left top window)
	  (when (and (numberp org-noter-arrow-delay) (>= org-noter-arrow-delay 0))
	    (run-with-idle-timer org-noter-arrow-delay nil
				 (lambda ()
				   (when (window-live-p window)
				     (emureader-flash-position
				      (max left 0.02) top window)))))))
      t)))

(defun emureader-org-noter--get-current-view (mode)
  "Return the paged view shown when MODE is ours."
  (when (eq mode 'emureader-mode)
    (vector 'paged (emureader-current-doc-pagenumber (selected-window)))))

(defun emureader-org-noter--get-selected-text (_mode)
  "Return nil: EMuReader has no text selection yet."
  nil)

(defun emureader-org-noter--outline-data (outline level)
  "Turn the EMuReader OUTLINE at LEVEL into skeleton entries.
Each entry is a vector [TITLE LOCATION LEVEL]."
  (mapcan (lambda (item)
	    (cons (vector (plist-get item :title)
			  ;; MuPDF counts pages from 0.
			  (cons (1+ (plist-get item :page)) 0)
			  level)
		  (emureader-org-noter--outline-data
		   (plist-get item :children) (1+ level))))
	  outline))

(defun emureader-org-noter--create-skeleton (mode)
  "Insert note headings from the document outline when MODE is ours."
  (when (and (eq mode 'emureader-mode) (emureader-org-noter--session-p))
    (let* ((session org-noter--session)
	   (outline (with-current-buffer (org-noter--session-doc-buffer session)
		      emureader-current-doc-outline))
	   (ast (org-noter--parse-root))
	   (top-level (or (org-element-property :level ast) 0))
	   (entries (cons (vector "Skeleton" nil 1)
			  (or (emureader-org-noter--outline-data outline 2)
			      (user-error "This document has no outline")))))
      (with-current-buffer (org-noter--session-notes-buffer session)
	;; Widen so that the narrowing includes the new headings.
	(widen)
	(save-excursion
	  (goto-char (org-element-property :end ast))
	  (dolist (entry entries)
	    (let ((location (aref entry 1)))
	      (org-noter--insert-heading (+ top-level (aref entry 2)) (aref entry 0))
	      (when location
		(org-entry-put nil org-noter-property-note-location
			       (org-noter--pretty-print-location location)))
	      (when org-noter-doc-property-in-notes
		(org-entry-put nil org-noter-property-doc-file
			       (org-noter--session-property-text session))
		(org-entry-put nil org-noter--property-auto-save-last-location "nil"))))
	  (setq ast (org-noter--parse-root))
	  (org-noter--narrow-to-root ast)
	  (goto-char (org-element-property :begin ast))
	  (when (org-at-heading-p) (outline-hide-subtree))
	  (org-fold-show-children 2)))
      entries)))

(defun emureader-org-noter-open-document (document)
  "Open remote DOCUMENT in EMuReader when its name has no known extension.
Such documents, typically kept under content-addressed names, would
not otherwise reach `emureader-mode'.  For `org-noter-open-document-functions'."
  (when (and (stringp document)
	     (file-remote-p document)
	     (not (emureader--supported-extension-p document))
	     (file-regular-p document))
    (emureader-find-doc-noselect document)))

(defun emureader-org-noter--enable ()
  "Register EMuReader's functions with org-noter."
  (add-to-list 'org-noter-supported-modes 'emureader-mode)
  (dolist (ext emureader-supported-formats)
    (add-to-list 'org-noter--doc-extensions ext))
  (add-hook 'org-noter-set-up-document-hook #'emureader-org-noter--setup)
  (add-hook 'org-noter--doc-approx-location-hook #'emureader-org-noter--approx-location)
  (add-hook 'org-noter--doc-goto-location-hook #'emureader-org-noter--goto-location)
  (add-hook 'org-noter--get-current-view-hook #'emureader-org-noter--get-current-view)
  (add-hook 'org-noter--get-precise-info-hook #'emureader-org-noter--get-precise-info)
  (add-hook 'org-noter--pretty-print-location-hook
	    #'emureader-org-noter--pretty-print-location)
  (add-hook 'org-noter--pretty-print-location-for-title-hook
	    #'emureader-org-noter--pretty-print-location-for-title)
  (add-hook 'org-noter-get-selected-text-hook #'emureader-org-noter--get-selected-text)
  (add-hook 'org-noter-create-skeleton-functions #'emureader-org-noter--create-skeleton)
  (add-hook 'org-noter-open-document-functions #'emureader-org-noter-open-document))

(when (featurep 'org-noter-core)
  (emureader-org-noter--enable))

(provide 'emureader-org-noter)
;;; emureader-org-noter.el ends here
