;;; emureader.el --- General-purpose document reader -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Divya Ranjan Pattanaik <divya@dialetheia.xyz>
;;         Tushar <tusharhero@sdf.org>
;; Keywords: lisp, files, tools
;; Version: 0.4.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://codeberg.org/MonadicSheep/emureader

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

;; This package provides a general purpose document reader within
;; Emacs by leveraging the use of dynamic modules.  It primarily
;; relies on MuPDF to do the rendering, and Emacs Lisp’s native image
;; displaying capabilities to view the rendered images and manipulate them.

;; To use,
;; - open a file using `find-file'
;; - Switch pages using `C-n', `C-p'
;; - Zoom using `+' and `-'.

;;; Code:

(require 'image)

(defconst emureader-directory
  (file-name-directory (or
			(macroexp-file-name)
			load-file-name
			default-directory))
  "The directory from where emureader was first loaded.")

(defun emureader--load-module ()
  "Load the `emureader-dyn' dynamic module from `emureader-directory'."
  (let ((load-path (cons emureader-directory load-path)))
    (require 'emureader-dyn)))

(defun emureader--module-built-p ()
  "Return non-nil if the dynamic module exists in `emureader-directory'."
  (file-exists-p
   (file-name-concat emureader-directory
		     (concat "emureader-dyn" module-file-suffix))))

(defun emureader--post-build (_buffer status)
  "Load the module if the build finished successfully, per STATUS."
  (if (string-prefix-p "finished" status)
      (progn (emureader--load-module)
	     (mapc (lambda (buff)
		     (with-current-buffer buff
		       (emureader-mode)))
		   (match-buffers '(major-mode . emureader-mode))))
    (message "emureader: module build failed, see *emureader-module-compilation*")))

;;;###autoload
(defun emureader-build ()
  "Build emureader's dynamic module, then load it."
  (interactive)
  (let ((default-directory emureader-directory)
	(compilation-buffer-name-function
	 (lambda (_) "*emureader-module-compilation*")))
    (compile (concat (or (executable-find "gmake") "make")
		     " module"))
    (with-current-buffer "*emureader-module-compilation*"
      ;; Buffer-local hook
      (add-hook 'compilation-finish-functions #'emureader--post-build nil t))
    ;; Let it compile.
    (sit-for 1)))

(defun emureader-ensure-module ()
  "Ensure emureader's dynamic module is loaded."
  (unless (featurep 'emureader-dyn)
    (if (emureader--module-built-p)
	(emureader--load-module)
      (when (yes-or-no-p "Build emureader's dynamic module? ")
	(emureader-build)))))

(defvar emureader--recent-pagenumber-fallback)
(defvar emureader-current-doc-state-ptr)
(defvar emureader--recent-pagenumber-fallback)
(defvar emureader-current-doc-render-status)

;;; Forward declarations for module functions
(declare-function emureader-dyn--load-doc "ext:emureader-dyn.so" (file &optional page))
(declare-function emureader-dyn--redisplay-doc "ext:emureader-dyn.so")
(declare-function emureader-dyn--close-doc "ext:emureader-dyn.so")
(declare-function emureader-dyn--next-page "ext:emureader-dyn.so")
(declare-function emureader-dyn--prev-page "ext:emureader-dyn.so")
(declare-function emureader-dyn--first-page "ext:emureader-dyn.so")
(declare-function emureader-dyn--last-page "ext:emureader-dyn.so")
(declare-function emureader-dyn--goto-page "ext:emureader-dyn.so" (page))
(declare-function emureader-dyn--scale-page "ext:emureader-dyn.so" (factor))
(declare-function emureader-dyn--doc-invert "ext:emureader-dyn.so" (invert))
(declare-function emureader-dyn--rotate-page "ext:emureader-dyn.so" (degree))
(declare-function emureader-dyn--window-create "ext:emureader-dyn.so" (window))
(declare-function emureader-dyn--window-close "ext:emureader-dyn.so" (window))

(defgroup emureader nil
  "Group for EMuReader’s customizations."
  :prefix "emureader-"
  :group 'custom)

(defcustom emureader-enlarge-factor 1.25
  "The fractional amount by which the page would be enlarged."
  :type 'number)

(defcustom emureader-shrink-factor 0.75
  "The fractional amount by which the page would be shrinked."
  :type 'number)

(defcustom emureader-default-fit #'emureader-fit-to-height
  "The default fitting for documents."
  :type '(radio (function-item emureader-fit-to-height)
		(function-item emureader-fit-to-width)))

(defvar-local emureader--recent-scale-fallback 1.0
  "Previous scaling factor applied to the document.")

(defvar-local emureader--fit-pending nil
  "Non-nil if the document was opened without being shown.
The page is then fitted to the first window showing it.")

(defvar-local emureader--reloading nil
  "Boolean flag to represent reloading status of a document.")

;; Setting of `auto-mode-list' fails if not autoloaded.
;;;###autoload
(defconst emureader-supported-formats (list "pdf" "epub" "mobi"
					    "fb2" "xps" "cbz"
					    "docx""pptx" "xlsx"
					    "odt" "ods" "odp" "odg")
  "File formats supported by the document emureader.")

(defun emureader-current-doc-overlay (&optional window)
  "Overlay parameter for WINDOW.

WINDOW must be a valid window and defaults to the selected one."
  (window-parameter (or window (selected-window)) 'overlay))

(defun emureader--window-owned-p (window)
  "Non-nil if WINDOW's emureader overlay belongs to WINDOW's current buffer."
  (let ((ov (window-parameter window 'overlay)))
    (and (overlayp ov)
	 (eq (overlay-buffer ov)
	     (window-buffer window)))))

(defun emureader--current-doc-window ()
  "Return a window showing the current buffer's document, or nil.
Prefer the selected window."
  (seq-find #'emureader--window-owned-p (get-buffer-window-list nil nil t)))

(defun emureader-current-doc-pagenumber (&optional window)
  "Page number of WINDOW.

WINDOW defaults to a window showing the current buffer.  Without
one fall back to the buffer-local page number."
  (let ((window (or window (emureader--current-doc-window))))
    (or (and window (emureader--window-owned-p window)
	     (window-parameter window 'page))
	emureader--recent-pagenumber-fallback)))

(defun emureader-current-doc-scale-value (&optional window)
  "Scaling factor of WINDOW.

WINDOW defaults to a window showing the current buffer.  Without
one fall back to the buffer-local scale value."
  (let ((window (or window (emureader--current-doc-window))))
    (or (and window (emureader--window-owned-p window)
	     (window-parameter window 'scale))
	emureader--recent-scale-fallback)))

;; We queue some commands because the user is expected to use the
;; commands repeatedly, such as by simply spamming a key. If we don't
;; queue the commands, Emacs may start skipping intermediate commands.

(defvar emureader--command-queue nil
  "Queue of emureader commands to be executed sequentially.")

(defun emureader--process-command-queue ()
  "Process the next command in the queue."
  (when-let* ((cmd (pop emureader--command-queue))
	      ;; Selected through trial and error.
	      (delay 0.0001))
    (funcall cmd)
    (redisplay)
    (run-with-idle-timer delay nil #'emureader--process-command-queue)))

(defun emureader--enqueue-command (cmd args)
  "Add CMD with ARGS to the queue and start processing if needed."
  (push (apply #'apply-partially cmd args) emureader--command-queue)
  (unless (or (active-minibuffer-window)
              (memq #'emureader--process-command-queue post-command-hook))
    (add-hook 'post-command-hook #'emureader--process-command-queue nil t)))

(defmacro emureader--define-queue-command (name arglist docstring interactive &rest body)
  "Define NAME as a emureader command.
Also define a necessary non-queue function.

Much like `defun', except that ARGLIST, DOCSTRING, and INTERACTIVE are
required.

The emureader--non-queue-NAME function is simply defined as a function with
body BODY.

The emureader-NAME command is simply a wrapper around
`emureader--enqueue-command' with emureader--non-queue-NAME as the argument."
  (declare (indent defun)
	   (doc-string 3))
  (let* ((name (symbol-name name))
	 (non-queue-function-name (concat "emureader--non-queue-" name))
	 (queue-function-name (concat "emureader-" name)))
    `(progn

       (defun ,(intern non-queue-function-name) ,arglist
	 ,(format "%s

This is the actual function.
See `%s',
for interactive version."
		  docstring queue-function-name)
	 ,(push 'progn body))

       (defun ,(intern queue-function-name) ,arglist
	 ,(format "%s

This is the queuing function.
See `%s',
for actual definition."
		  docstring non-queue-function-name)
	 ,interactive
	 (emureader--enqueue-command #',(intern non-queue-function-name)
				     ,(remq '&optional (push 'list arglist)))))))

(defun emureader--read-doc-name (&optional any)
  "Read a document file name.
Unless ANY is non-nil, only offer files with a supported extension."
  (let* ((regexp (concat "\\." (regexp-opt emureader-supported-formats t) "\\'"))
	 (prompt "Open document: ")
	 (file (read-file-name
		prompt nil nil t nil
		(lambda (f)
		  (or any
		      (file-directory-p f)
		      (string-match-p regexp f))))))
    (if (file-directory-p file)
	(emureader--read-doc-name any) file)))

;;; Remote documents
;;
;; MuPDF opens documents by local path, so a remote document (see
;; `file-remote-p') is copied into `emureader-cache-directory' and the
;; copy is rendered.  The buffer still visits the remote file:
;; `buffer-file-name' is the remote file name, while
;; `emureader-cache-file' names the local copy.  The copy is stored
;; under a real file name, NAME, so that documents kept on servers
;; under content-addressed names without an extension still get the
;; right format.

(defcustom emureader-cache-directory
  (expand-file-name "emureader-cache/" user-emacs-directory)
  "Directory holding local copies of remote documents.

Each remote document gets a subdirectory named after a hash of its
remote file name, holding the copy under the document's name."
  :type 'directory)

(defvar-local emureader-doc-name nil
  "Name of the document shown in the current buffer.
It names the buffer and, for remote documents, the cached copy.  When
nil, the non-directory part of the variable `buffer-file-name' is used.")
(put 'emureader-doc-name 'permanent-local t)

(defvar-local emureader-cache-file nil
  "Local copy of the remote document visited by the current buffer.
It is nil for local documents.")
(put 'emureader-cache-file 'permanent-local t)

(defun emureader--supported-extension-p (file)
  "Non-nil if FILE has an extension in `emureader-supported-formats'."
  (when-let* ((ext (file-name-extension file)))
    (member (downcase ext) emureader-supported-formats)))

(defun emureader--sniff-extension (file)
  "Guess the extension of local FILE from its first bytes, or return nil."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 128)
    (goto-char (point-min))
    (cond ((looking-at-p "%PDF-") "pdf")
	  ((looking-at-p "AT&TFORM") nil)
	  ((looking-at-p "PK\003\004")
	   (cond ((search-forward "mimetypeapplication/epub+zip" nil t) "epub")
		 ((search-forward "[Content_Types].xml" nil t) nil)
		 ((search-forward "mimetypeapplication/vnd.oasis.opendocument." nil t)
		  (cond ((looking-at-p "text") "odt")
			((looking-at-p "spreadsheet") "ods")
			((looking-at-p "presentation") "odp")
			((looking-at-p "graphics") "odg")))))
	  ((looking-at-p "BOOKMOBI") "mobi")
	  ((looking-at-p "\\(?:\357\273\277\\)?<\\?xml") "fb2"))))

(defun emureader--cache-name (name)
  "Return NAME made safe for use as a local file name."
  (let ((name (replace-regexp-in-string "[/\\:\0]" "_" name)))
    (if (string-match-p "\\`\\.*\\'" name) "document" name)))

(defun emureader--cache-subdirectory (file)
  "Return the cache subdirectory for remote FILE."
  (file-name-as-directory
   (expand-file-name (md5 file) emureader-cache-directory)))

(defun emureader--cache-fresh-p (cache attributes)
  "Non-nil if local CACHE matches the remote file ATTRIBUTES."
  (when-let* ((local (and cache (file-attributes cache))))
    (and (eql (file-attribute-size local)
	      (file-attribute-size attributes))
	 (time-equal-p (file-attribute-modification-time local)
		       (file-attribute-modification-time attributes)))))

(defun emureader--cache-candidates (dir name)
  "Return the cache file names in DIR that may hold document NAME."
  (cons (expand-file-name name dir)
	(unless (emureader--supported-extension-p name)
	  (mapcar (lambda (ext) (expand-file-name (concat name "." ext) dir))
		  emureader-supported-formats))))

(defun emureader--cache-write-buffer (buffer cache attributes)
  "Write the raw bytes held by BUFFER to CACHE, if that is safe.
Return non-nil when CACHE was written and has the size recorded in
the remote file ATTRIBUTES."
  (with-current-buffer buffer
    (when (and (> (buffer-size) 0)
	       buffer-file-coding-system
	       (memq (coding-system-base buffer-file-coding-system)
		     '(no-conversion raw-text))
	       (eql (coding-system-eol-type buffer-file-coding-system) 0))
      (save-restriction
	(widen)
	(let ((coding-system-for-write 'no-conversion)
	      (write-region-inhibit-fsync t))
	  (write-region nil nil cache nil 'silent)))
      (or (eql (file-attribute-size (file-attributes cache))
	       (file-attribute-size attributes))
	  (progn (delete-file cache) nil)))))

(defun emureader-cache-fetch (file &optional name buffer)
  "Return a local copy of remote FILE, fetching it when needed.

The copy lives in a subdirectory of `emureader-cache-directory' and is
called NAME, which defaults to FILE's non-directory name.  When
neither has a supported extension, the format is guessed from the
contents and its extension appended.  An existing copy is reused when
its size and modification time match FILE's.

If BUFFER is non-nil and holds FILE's undecoded bytes, they are
written to the cache instead of copying FILE again."
  (let* ((remote-file-name-inhibit-cache t)
	 (attributes (or (file-attributes file)
			 (user-error "Remote document not found: %s" file)))
	 (dir (emureader--cache-subdirectory file))
	 (name (emureader--cache-name
		(or name (file-name-nondirectory file))))
	 (cache (expand-file-name name dir)))
    (when (eq t (file-attribute-type attributes))
      (user-error "Not a document: %s" file))
    (or (seq-find (lambda (f) (emureader--cache-fresh-p f attributes))
		  (emureader--cache-candidates dir name))
	(let ((part (expand-file-name (concat ".fetch-" name) dir)))
	  (make-directory dir t)
	  ;; Old versions of the document are no longer needed.
	  (dolist (old (directory-files dir t directory-files-no-dot-files-regexp))
	    (when (file-regular-p old)
	      (delete-file old)))
	  (unless (and buffer (emureader--cache-write-buffer buffer part attributes))
	    (message "Fetching %s..." file)
	    (copy-file file part t))
	  (unless (emureader--supported-extension-p name)
	    (when-let* ((ext (emureader--sniff-extension part)))
	      (setq cache (concat cache "." ext))))
	  (rename-file part cache t)
	  (set-file-times cache (file-attribute-modification-time attributes))
	  cache))))

(defun emureader--local-file (&optional recheck)
  "Return the local file that renders the current buffer's document.

For a remote document this is the cached copy, fetched when missing.
If RECHECK is non-nil, also fetch it again when the remote file
changed."
  (when-let* ((file buffer-file-name))
    (if (not (file-remote-p file))
	file
      (when (or recheck
		(not (and emureader-cache-file
			  (file-exists-p emureader-cache-file))))
	(setq emureader-cache-file
	      (emureader-cache-fetch
	       file emureader-doc-name
	       ;; Bytes inserted by `find-file' can be reused once.
	       (and (not emureader-cache-file) (current-buffer)))))
      emureader-cache-file)))

;;;###autoload
(defun emureader-clear-cache (&optional all)
  "Delete the local copies of remote documents.

Copies shown by live EμReader buffers are kept unless ALL is non-nil
\(interactively, with a prefix argument)."
  (interactive "P")
  (let ((keep (unless all
		(delq nil (mapcar (lambda (buf)
				    (buffer-local-value 'emureader-cache-file buf))
				  (buffer-list)))))
	(count 0))
    (when (file-directory-p emureader-cache-directory)
      (dolist (dir (directory-files emureader-cache-directory t
				    directory-files-no-dot-files-regexp))
	(when (file-directory-p dir)
	  (dolist (file (directory-files dir t directory-files-no-dot-files-regexp))
	    (unless (member file keep)
	      (delete-file file)
	      (setq count (1+ count))))
	  (when (directory-empty-p dir)
	    (delete-directory dir)))))
    (when (called-interactively-p 'interactive)
      (message "Deleted %d cached document%s" count (if (= count 1) "" "s")))
    count))

;;;###autoload
(defun emureader-find-doc-noselect (document &optional name)
  "Return a buffer showing DOCUMENT in `emureader-mode', without selecting it.

DOCUMENT may be a remote file, in which case it is rendered from a
local copy; see `emureader-cache-fetch'.  NAME, when non-nil, names
the buffer and the local copy, and its extension sets the format.
This allows opening documents stored without an extension, as in
\(emureader-find-doc-noselect \"/ssh:host:/store/ab/ab12…\" \"Paper.pdf\").

An existing buffer visiting DOCUMENT is reused."
  (let ((document (expand-file-name document)))
    (or (seq-find (lambda (buf)
		    (and (equal (buffer-local-value 'buffer-file-name buf) document)
			 (eq (buffer-local-value 'major-mode buf) 'emureader-mode)))
		  (buffer-list))
	(let ((buffer (create-file-buffer
		       (or name (file-name-nondirectory document)))))
	  (with-current-buffer buffer
	    (setq buffer-file-name document
		  buffer-file-truename (abbreviate-file-name document)
		  emureader-doc-name name
		  default-directory (file-name-directory document))
	    (unless (file-remote-p document)
	      (set-visited-file-modtime))
	    ;; We need something to put the overlay on.
	    (insert "\n")
	    ;; go back to the previous point for page border.
	    (goto-char (1- (point)))
	    (set-buffer-modified-p nil)
	    (condition-case err
		(emureader-mode)
	      (error (when (buffer-live-p buffer)
		       (kill-buffer buffer))
		     (signal (car err) (cdr err)))))
	  (and (buffer-live-p buffer) buffer)))))

;;;###autoload
(defun emureader-open-doc (document &optional name)
  "Open DOCUMENT for viewing.

DOCUMENT may be local or remote.  NAME, when non-nil, is the name
under which the document is shown and, for a remote document, cached;
its extension determines the format.  Interactively, a prefix
argument offers every file and prompts for NAME.

The document is rendered by the dynamic module, see
`emureader-find-doc-noselect'.  Without a prefix argument, the only
formats offered are `emureader-supported-formats'."
  (interactive
   (let ((file (expand-file-name (emureader--read-doc-name current-prefix-arg))))
     (list file
	   (when current-prefix-arg
	     (let ((name (read-string "Document name: "
				      (file-name-nondirectory file))))
	       (unless (string-empty-p name) name))))))
  (when-let* ((buffer (emureader-find-doc-noselect document name)))
    (switch-to-buffer buffer)))

;;; Page changes

(defvar emureader-after-page-change-hook nil
  "Hook run after the page shown in a window changes.
It runs with the document's buffer current and the window selected.")

(defvar emureader--inhibit-page-change-hook nil
  "Non-nil while `emureader-after-page-change-hook' must not run.")

(defun emureader--page-changed (old)
  "Run `emureader-after-page-change-hook' unless the page is still OLD."
  (unless (or emureader--inhibit-page-change-hook
	      (eql old (emureader-current-doc-pagenumber (selected-window))))
    (run-hooks 'emureader-after-page-change-hook)))

(defmacro emureader--with-page-change (&rest body)
  "Run BODY, then the page change hook if the page changed.
Return the value of BODY."
  (declare (indent 0) (debug t))
  (let ((old (make-symbol "old")))
    `(let ((,old (emureader-current-doc-pagenumber (selected-window))))
       (prog1 (progn ,@body)
	 (emureader--page-changed ,old)))))

(define-queue-command next-page ()
  "Go to the next page of the document."
  (interactive)
  (when (emureader--with-page-change (emureader-dyn--next-page))
    ;; Must return `t' when the command succeeds.
    (force-mode-line-update) t))

(define-queue-command previous-page ()
  "Go to the previous page of the document."
  (interactive)
  (when (emureader--with-page-change (emureader-dyn--prev-page))
    ;; Must return `t' when the command succeeds.
    (force-mode-line-update) t))

(defun emureader-first-page ()
  "Go to the first page of the document."
  (interactive)
  (emureader--with-page-change
    (emureader-dyn--first-page)
    (emureader--center-page))
  (force-mode-line-update))

(defun emureader-last-page ()
  "Go to the last page of the document."
  (interactive)
  (emureader--with-page-change
    (emureader-dyn--last-page)
    (emureader--center-page))
  (force-mode-line-update))

(defun emureader-goto-page (n)
  "Go to page number N in the current document."
  (interactive "nGoto page: ")
  (emureader--with-page-change
    (emureader-dyn--goto-page (- n 1)) ; MuPDF does 0-indexing
    (emureader--center-page)))

(defun emureader--get-current-doc-image-size (&optional window)
  "Get the dimensions of the page in WINDOW.

WINDOW must be a valid window and defaults to the selected one."
  (let* ((cdr-image (cdr (overlay-get (emureader-current-doc-overlay window) 'display)))
	 (width (plist-get cdr-image :width))
	 (height (plist-get cdr-image :height)))
    (cons width height)))

(defun emureader-doc-scale-page (factor)
  "Scales the page by a given FACTOR.

It also updates `emureader--recent-scale-fallback' and `scale' property of
selected window to reflect the new, possibly clamped scale."
  (let ((inhibit-quit t))
    (when-let* ((factor-clamped (emureader-dyn--scale-page factor)))
      (setq emureader--recent-scale-fallback factor-clamped)
      (set-window-parameter nil 'scale factor-clamped))))

(defun emureader-enlarge-size (&optional scaling-factor)
  "Enlarge the size of the current page by the `emureader-enlarge-factor'.

Optionally scale it by the SCALING-FACTOR.  Scales on selected window."
  (interactive (list (float
		      (* (emureader-current-doc-scale-value) emureader-enlarge-factor))))
  (emureader-doc-scale-page scaling-factor)
  (emureader--center-page))

(defun emureader-shrink-size (&optional scaling-factor)
  "Shrink the size of the current page by the `emureader-shrink-factor'.

Optionally scale it by the SCALING-FACTOR.  Scales on selected window."
  (interactive (list (float
		      (* (emureader-current-doc-scale-value) emureader-shrink-factor))))
  (emureader-doc-scale-page scaling-factor)
  (emureader--center-page))

(defun emureader-reset-size ()
  "Reset the size of the current page to 1.0."
  (interactive)
  (emureader-doc-scale-page 1.0)
  (emureader--center-page))

(defun emureader-fit-to-height ()
  "Scale the current page in the selected window to fit its height."
  (interactive)
  (let* ((image-height (cdr (emureader--get-current-doc-image-size)))
	 (pixel-window-height (window-body-height nil t))
	 (unscaled-height (/ image-height (emureader-current-doc-scale-value)))
	 (scaling-factor (/ pixel-window-height unscaled-height)))
    (emureader-doc-scale-page scaling-factor)
    (emureader--center-page)
    (emureader--set-window-vscroll nil 0)))

(defun emureader-fit-to-width ()
  "Scale the current page in the selected window to fit its width."
  (interactive)
  (let* ((image-width (car (emureader--get-current-doc-image-size)))
	 (pixel-window-width (window-body-width nil t))
	 (unscaled-width (/ image-width (emureader-current-doc-scale-value)))
	 (scaling-factor (/ pixel-window-width unscaled-width)))
    (emureader-doc-scale-page scaling-factor)
    (emureader--center-page)))

(defun emureader--get-pixel-per-col (&optional window)
  "Get the no of pixels per column for WINDOW.

WINDOW must be a valid window and defaults to the selected one."
  (/ (window-body-width window t) (window-body-width window)))

;; We need to do this because scrolling is possible in one direction
;; (downwards) indefinitely.

(defun emureader--set-window-vscroll (window vscroll &optional pixels-p)
  "Set amount by which WINDOW should be scrolled vertically to VSCROLL.

If setting VSCROLL makes the document page's top disappear, it sets the
maximum vertical scroll possible without doing that.

If PIXELS-P is non-nil, VSCROLL is considered to be in pixels.
Also see `set-window-vscroll'."
  (let* ((image-height (cdr (emureader--get-current-doc-image-size window)))
	 (pixel-window-height (window-body-height window t))
	 (window-height (window-body-height window))
	 (pixel-per-col (/ pixel-window-height window-height))
	 (pixel-vscroll (* pixel-per-col vscroll))
	 (max-pixel-vscroll (- image-height pixel-window-height))
	 (corrected-pixel-vscroll (min pixel-vscroll max-pixel-vscroll))
	 (corrected-vscroll (/ corrected-pixel-vscroll pixel-per-col)))
    (set-window-vscroll window
			(if pixels-p corrected-pixel-vscroll corrected-vscroll)
			pixels-p)))

;; Most of the scrolling functions here exist because of our handling
;; of centering in `emureader--center-page'.

(defun emureader--get-prefix-width (&optional window)
  "Get the line prefix width set by `emureader--center-page'.

WINDOW must be a valid window and defaults to the selected one."
  (car
   (plist-get
    (cdr (overlay-get (emureader-current-doc-overlay window) 'line-prefix))
    :width)))

(defun emureader--right-most-window-hscroll (&optional window)
  "Get the maximum horizontal scroll value for WINDOW.

This position is at the rightmost point.
WINDOW must be a valid window and defaults to the selected one."
  (let* ((image-width (car (emureader--get-current-doc-image-size window)))
	 (line-prefix-width (emureader--get-prefix-width window))
	 (pixel-window-width (window-body-width window t))
	 (max-ncol (round (/ (max line-prefix-width
				  (- image-width pixel-window-width))
			     (emureader--get-pixel-per-col window)))))
    max-ncol))

(defun emureader--set-window-hscroll (window ncol &optional unconstrained)
  "Set number of columns WINDOW is scrolled from left margin to NCOL.

If setting NCOL makes the document page disappear, it sets the maximum
horizontal scroll possible without doing that.  If UNCONSTRAINED is
non-nil, it allows setting NCOL even if it makes the page disappear.

See also `set-window-hscroll'."
  (let* ((line-prefix-width (emureader--get-prefix-width window))
	 (pixel-per-col (emureader--get-pixel-per-col window))
	 (calibrated-ncol (round (- (/ line-prefix-width pixel-per-col) ncol)))
	 (max-ncol (emureader--right-most-window-hscroll window))
	 (ncol (if unconstrained
		   calibrated-ncol
		 (min calibrated-ncol max-ncol))))
    (set-window-hscroll window ncol)
    (emureader--window-hscroll window)))

(defun emureader--window-hscroll (&optional window)
  "Return the number of columns by which WINDOW is scrolled from left margin.

WINDOW must be a live window and defaults to the selected one.

This correctly handles the prefix width set by emureader documents and does
not return the actual horizontal scroll value; for that, see
`window-hscroll'."
  (let* ((line-prefix-width (emureader--get-prefix-width window))
	 (pixel-per-col (emureader--get-pixel-per-col window))
	 (hscroll (round (- (/ line-prefix-width pixel-per-col) (window-hscroll window)))))
    hscroll))

;; We need this hack involving line-prefix because Emacs' scrolling is
;; idiosyncratic, and doesn't allow arbitrary scrolling in every
;; direction.
(defun emureader--center-page (&optional window)
  "Center the document with respect to WINDOW.

WINDOW must be a valid window and defaults to the selected one."
  (when-let* ((overlay (emureader-current-doc-overlay window))
              (_ (overlayp overlay))
	      (window-width (window-body-width window t))
              (doc-image-width (car (emureader--get-current-doc-image-size window)))
	      (max-left-offset (max 0 (- window-width doc-image-width)))
	      (overlay-offset `(space :width (,max-left-offset)))
	      (pixel-per-col (emureader--get-pixel-per-col window))
	      (doc-left-offset (- window-width doc-image-width))
              (doc-center-offset (/ doc-left-offset 2))
              (scroll-offset (round (/ doc-center-offset pixel-per-col))))
    ;; Add prefix so that the page is at the leftmost point of the window.
    (overlay-put overlay 'line-prefix overlay-offset)
    ;; scroll window back to the center of the doc
    (emureader--set-window-hscroll window scroll-offset t)))

(defun emureader-scroll-up (&optional amount)
  "Scroll up the current page by AMOUNT.

AMOUNT defaults to 1."
  (interactive "p")
  (or amount (setq amount 1))
  (let* ((prev-scroll (window-vscroll))
	 (vscroll (- prev-scroll amount)))
    (- prev-scroll (emureader--set-window-vscroll nil vscroll))))

(defun emureader-scroll-down (&optional amount)
  "Scroll down the current page by AMOUNT.

AMOUNT defaults to 1."
  (interactive "p")
  (or amount (setq amount 1))
  (let* ((prev-scroll (window-vscroll))
	 (vscroll (+ prev-scroll amount)))
    (- (emureader--set-window-vscroll nil vscroll) prev-scroll)))

(defun emureader-scroll-up-screen (&optional amount)
  "Scroll up the current page by a screen.

Unless AMOUNT is specified in which case we scroll by that value."
  (interactive "P")
  (let ((amount (or amount (- (window-body-height)
			      next-screen-context-lines))))
    (when (= 0 (emureader-scroll-up amount))
      (message "Beginning of page"))))

(defun emureader-scroll-down-screen (&optional amount)
  "Scroll down the current page by a screen.

Unless AMOUNT is specified in which case we scroll by that value."
  (interactive "P")
  (let ((amount (or amount (- (window-body-height)
			      next-screen-context-lines))))
    (when (= 0 (emureader-scroll-down amount))
      (message "End of page"))))

(defun emureader-scroll-left (&optional amount)
  "Scroll to the left of the current page by AMOUNT.

AMOUNT defaults to 1.

Only scrolls when the document page width is larger then the window width."
  (interactive "p")
  (or amount (setq amount 1))
  (when-let* ((_ (< (window-body-width nil t) (car (emureader--get-current-doc-image-size))))
	      (prev-scroll (emureader--window-hscroll))
	      (hscroll (+ prev-scroll amount)))
    (- (emureader--set-window-hscroll nil hscroll) prev-scroll)))

(defun emureader-scroll-right (&optional amount)
  "Scroll to the right of the current page by AMOUNT (or 1).

Only scrolls when the document page width is larger then the window width."
  (interactive "p")
  (or amount (setq amount 1))
  (when-let* ((_ (< (window-body-width nil t) (car (emureader--get-current-doc-image-size))))
	      (prev-scroll (emureader--window-hscroll))
	      (hscroll (- prev-scroll amount)))
    (- prev-scroll (emureader--set-window-hscroll nil hscroll))))

(defun emureader-scroll-left-most ()
  "Scroll to the left most point of the current page.

Only scrolls when the document page width is larger then the window width."
  (interactive)
  (when (< (window-body-width nil t) (car (emureader--get-current-doc-image-size)))
    (emureader--set-window-hscroll nil 0)))

(defun emureader-scroll-right-most ()
  "Scroll to the right most point of the current page.

Only scrolls when the document page width is larger then the window width."
  (interactive)
  (when (< (window-body-width nil t) (car (emureader--get-current-doc-image-size)))
    ;; We use `set-window-hscroll' here because we need to go the right
    ;; most point directly, bypassing `emureader--set-window-hscroll' checks.
    (set-window-hscroll nil (emureader--right-most-window-hscroll))))

(define-queue-command scroll-up-or-prev-page (&optional amount)
  "Scroll up the current page by AMOUNT, otherwise switch to the previous page.

AMOUNT defaults to 1."
  (interactive "p")
  (or amount (setq amount 1))
  (when-let* ((_ (and (= 0 (emureader-scroll-up amount))
		      (emureader--non-queue-previous-page))) ; if succeeds
	      (image-height (cdr (emureader--get-current-doc-image-size)))
	      (pixel-window-height (window-body-height nil t))
	      (bottom-most-scroll-pixel
	       (- image-height pixel-window-height)))
    (emureader--set-window-vscroll nil bottom-most-scroll-pixel t)))

(define-queue-command scroll-down-or-next-page (&optional amount)
  "Scroll down the current page by AMOUNT, otherwise switch to the next page.

AMOUNT defaults to 1."
  (interactive "p")
  (or amount (setq amount 1))
  (when (and (= 0 (emureader-scroll-down amount))
	     (emureader--non-queue-next-page)) ; if succeeds
    (emureader--set-window-vscroll nil 0)))

(define-queue-command scroll-up-screen-or-prev-page ()
  "Scroll up the current page by screen, otherwise switch to the previous page."
  (interactive)
  (let ((scroll (- (window-body-height)
		   next-screen-context-lines)))
    (emureader--non-queue-scroll-up-or-prev-page scroll)))

(define-queue-command scroll-down-screen-or-next-page ()
  "Scroll down the current page by screen, otherwise switch to the next page."
  (interactive)
  (let ((scroll (- (window-body-height)
		   next-screen-context-lines)))
    (emureader--non-queue-scroll-down-or-next-page scroll)))

(define-queue-command mwheel-scroll-up (event)
  "Scroll up or switch to the previous page, but also handle mouse EVENT.

See also `emureader-non-queue-scroll-up-or-prev-page'."
  (interactive "e")
  (let* ((event-type (car event))
	 (amount (pcase event-type
		   ('wheel-up 1)
		   ('double-wheel-up 2)
		   ('triple-wheel-up 3)))
	 (scrolled-window (car (cadr event))))
    (with-selected-window scrolled-window
      (emureader--non-queue-scroll-up-or-prev-page amount))))

(define-queue-command mwheel-scroll-down (event)
  "Scroll down or switch to the next page, but also handle mouse EVENT.

See also `emureader--non-queue-scroll-down-or-next-page'."
  (interactive "e")
  (let* ((event-type (car event))
	 (amount (pcase event-type
		   ('wheel-down 1)
		   ('double-wheel-down 2)
		   ('triple-wheel-down 3)))
	 (scrolled-window (car (cadr event))))
    (with-selected-window scrolled-window
      (emureader--non-queue-scroll-down-or-next-page amount))))

(defun emureader-mwheel-scroll-left (event)
  "Scroll to the left, but also handle mouse EVENT.

See also `emureader-scroll-left'."
  (interactive "e")
  (let* ((event-type (car event))
	 (amount (pcase event-type
		   ('S-wheel-up 1)
		   ('S-double-wheel-up 2)
		   ('S-triple-wheel-up 3)))
	 (scrolled-window (car (cadr event))))
    (with-selected-window scrolled-window
      (emureader-scroll-left amount))))

(defun emureader-mwheel-scroll-right (event)
  "Scroll to the right, but also handle mouse EVENT.

See also `emureader-scroll-right'."
  (interactive "e")
  (let* ((event-type (car event))
	 (amount (pcase event-type
		   ('S-wheel-down 1)
		   ('S-double-wheel-down 2)
		   ('S-triple-wheel-down 3)))
	 (scrolled-window (car (cadr event))))
    (with-selected-window scrolled-window
      (emureader-scroll-right amount))))

(defun emureader-mwheel-enlarge-size (event)
  "Enlarge the current page, but also handle mouse EVENT.

See also `emureader-enlarge-size'."
  (interactive "e")
  (let* ((event-type (car event))
	 (scaling-factor (pcase event-type
			   ('C-wheel-up emureader-enlarge-factor)
			   ('C-double-wheel-up (+ emureader-enlarge-factor 0.1))
			   ('C-triple-wheel-up (+ emureader-enlarge-factor 0.2))))
	 (scrolled-window (car (cadr event))))
    (with-selected-window scrolled-window
      (emureader-enlarge-size
       (* scaling-factor (emureader-current-doc-scale-value))))))

(defun emureader-mouse-select-window (event)
  "Select the window.

EVENT must be a click event."
  (interactive "e")
  (when-let* ((end-window (posn-window (event-end event)))
	      (_ (window-live-p end-window)))
    (select-window end-window)))

(defun emureader-mwheel-shrink-size (event)
  "Shrink the current page, but also handle mouse EVENT.

See also `emureader-shrink-size'."
  (interactive "e")
  (let* ((event-type (car event))
	 (scaling-factor (pcase event-type
			   ('C-wheel-down emureader-shrink-factor)
			   ('C-double-wheel-down (- emureader-shrink-factor 0.1))
			   ('C-triple-wheel-down (- emureader-shrink-factor 0.2))))
	 (scrolled-window (car (cadr event))))
    (with-selected-window scrolled-window
      (emureader-shrink-size
       (* scaling-factor (emureader-current-doc-scale-value))))))

(defun emureader-rotate-clockwise ()
  "Rotate the current window's view by 90 degrees, clockwise."
  (interactive)
  (let ((inhibit-quit t))
    (when (emureader-dyn--rotate-page 90)
      (emureader--note-rotation 90))
    (emureader--center-page)))

(defun emureader-rotate-counter-clockwise ()
  "Rotate the current window's view by 90 degrees, counter-clockwise."
  (interactive)
  (let ((inhibit-quit t))
    (when (emureader-dyn--rotate-page -90)
      (emureader--note-rotation -90))
    (emureader--center-page)))

;;; Positions on the page
;;
;; The module renders a page of W x H points at scale S into an image
;; of W*S/F x H*S/F pixels, F being `frame-scale-factor', rotated
;; clockwise by the window's rotation.  These functions convert
;; between positions in a window and positions on the page, given as
;; fractions of the unrotated page's width and height (0 is the left
;; or top edge).

(defun emureader--note-rotation (degrees)
  "Record that the selected window's view was rotated by DEGREES."
  (set-window-parameter
   nil 'emureader-rotation
   (mod (+ (or (window-parameter nil 'emureader-rotation) 0) degrees) 360)))

(defun emureader-current-doc-rotation (&optional window)
  "Clockwise rotation of the page in WINDOW, in degrees."
  (or (window-parameter window 'emureader-rotation) 0))

(defun emureader--rotate-fraction (x y rotation)
  "Map the unrotated page fraction X, Y to the page shown at ROTATION.
Return a cons of fractions of the shown image."
  (pcase (mod rotation 360)
    (90  (cons (- 1 y) x))
    (180 (cons (- 1 x) (- 1 y)))
    (270 (cons y (- 1 x)))
    (_   (cons x y))))

(defun emureader--unrotate-fraction (x y rotation)
  "Map the fraction X, Y of the image shown at ROTATION to the page.
This is the inverse of `emureader--rotate-fraction'."
  (pcase (mod rotation 360)
    (90  (cons y (- 1 x)))
    (180 (cons (- 1 x) (- 1 y)))
    (270 (cons (- 1 y) x))
    (_   (cons x y))))

(defun emureader-current-doc-page-size (&optional window)
  "Size of the unrotated page shown in WINDOW, as (WIDTH . HEIGHT) in points."
  (let* ((window (or window (selected-window)))
	 (size (emureader--get-current-doc-image-size window))
	 (factor (/ (float (frame-scale-factor (window-frame window)))
		    (emureader-current-doc-scale-value window)))
	 (w (* (car size) factor))
	 (h (* (cdr size) factor)))
    (if (memq (emureader-current-doc-rotation window) '(90 270))
	(cons h w)
      (cons w h))))

(defun emureader-posn-page-fraction (posn)
  "Return the page position under POSN as fractions (X . Y) of the page.
POSN is a position as returned by `event-start' or `posn-at-x-y'.
Return nil unless POSN is on a document page."
  (when-let* ((window (posn-window posn))
	      (_ (windowp window))
	      (_ (emureader--window-owned-p window))
	      (object-xy (posn-object-x-y posn))
	      (_ (consp (posn-image posn)))
	      (size (emureader--get-current-doc-image-size window)))
    (emureader--unrotate-fraction
     (/ (float (car object-xy)) (car size))
     (/ (float (cdr object-xy)) (cdr size))
     (emureader-current-doc-rotation window))))

(defun emureader--image-origin (&optional window)
  "Return the position of the page image's top left corner in WINDOW.
The value is (X . Y) in pixels relative to WINDOW's text area; it is
negative when that corner is scrolled out of view."
  (let* ((window (or window (selected-window)))
	 (prefix (or (emureader--get-prefix-width window) 0))
	 (hscroll (* (window-hscroll window)
		     (emureader--get-pixel-per-col window))))
    (cons (- prefix hscroll)
	  (- (window-vscroll window t)))))

(defun emureader-page-fraction-to-window (x y &optional window)
  "Return the pixel position in WINDOW of the page fractions X, Y.
The value is (PX . PY), relative to WINDOW's text area."
  (let* ((window (or window (selected-window)))
	 (shown (emureader--rotate-fraction
		 x y (emureader-current-doc-rotation window)))
	 (size (emureader--get-current-doc-image-size window))
	 (origin (emureader--image-origin window)))
    (cons (round (+ (car origin) (* (car shown) (car size))))
	  (round (+ (cdr origin) (* (cdr shown) (cdr size)))))))

(defun emureader-scroll-to-page-fraction (x y &optional window)
  "Scroll WINDOW so that page fractions X, Y are visible.
The position is put about a third of the way down the window, and
horizontally into view when the page is wider than the window."
  (let* ((window (or window (selected-window)))
	 (shown (emureader--rotate-fraction
		 x y (emureader-current-doc-rotation window)))
	 (size (emureader--get-current-doc-image-size window))
	 (target-y (* (cdr shown) (cdr size)))
	 (height (window-body-height window t)))
    (with-selected-window window
      (emureader--set-window-vscroll
       window (max 0 (round (- target-y (/ height 3)))) t)
      (when (> (car size) (window-body-width window t))
	(let* ((target-x (* (car shown) (car size)))
	       (width (window-body-width window t))
	       (col (/ (max 0 (- target-x (/ width 3)))
		       (emureader--get-pixel-per-col window))))
	  (emureader--set-window-hscroll window (round col)))))))

(defface emureader-flash
  '((((background dark)) :background "#e5c07b" :foreground "#e5c07b")
    (t :background "#ffd700" :foreground "#c04000"))
  "Face of the marks put on the page by `emureader-flash-position'.
The background colours the box highlighted over the page, the
foreground the arrow pointing at a position."
  :group 'emureader)

(defcustom emureader-flash-duration 1.5
  "Seconds for which `emureader-flash-position' marks the page."
  :type 'number)

(defcustom emureader-flash-opacity 0.4
  "Opacity, from 0 to 1, of the box highlighted by `emureader-flash-position'."
  :type 'number)

(defvar emureader--flash-frame nil
  "Child frame used to mark positions on the page.")

(defvar emureader--flash-timer nil
  "Timer hiding `emureader--flash-frame'.")

(defun emureader--flash-hide ()
  "Hide the mark put on the page by `emureader-flash-position'."
  (when (timerp emureader--flash-timer)
    (cancel-timer emureader--flash-timer))
  (setq emureader--flash-timer nil)
  (when (frame-live-p emureader--flash-frame)
    (make-frame-invisible emureader--flash-frame t)))

(defun emureader--flash-frame (parent)
  "Return the child frame of PARENT used to mark positions."
  (unless (and (frame-live-p emureader--flash-frame)
	       (eq (frame-parent emureader--flash-frame) parent))
    (when (frame-live-p emureader--flash-frame)
      (delete-frame emureader--flash-frame t))
    (let ((buffer (get-buffer-create " *emureader-flash*")))
      (with-current-buffer buffer
	(setq-local mode-line-format nil
		    header-line-format nil
		    tab-line-format nil
		    cursor-type nil
		    cursor-in-non-selected-windows nil
		    truncate-lines t
		    ;; Global modes would draw in the frame.
		    display-line-numbers nil
		    display-fill-column-indicator nil
		    show-trailing-whitespace nil
		    indicate-empty-lines nil
		    indicate-buffer-boundaries nil
		    global-hl-line-mode nil))
      (setq emureader--flash-frame
	    (let ((after-make-frame-functions nil)
		  (display-buffer-alist nil))
	      (make-frame
	       `((parent-frame . ,parent)
		 (minibuffer . nil)
		 (undecorated . t)
		 (no-accept-focus . t)
		 (no-focus-on-map . t)
		 (no-other-frame . t)
		 (no-special-glyphs . t)
		 (cursor-type . nil)
		 (right-divider-width . 0)
		 (bottom-divider-width . 0)
		 (unsplittable . t)
		 (skip-taskbar . t)
		 (visibility . nil)
		 (internal-border-width . 0)
		 (child-frame-border-width . 0)
		 (left-fringe . 0)
		 (right-fringe . 0)
		 (vertical-scroll-bars . nil)
		 (horizontal-scroll-bars . nil)
		 (menu-bar-lines . 0)
		 (tool-bar-lines . 0)
		 (tab-bar-lines . 0)
		 (line-spacing . 0)
		 ;; A tiny font lets the frame be as short as a line of
		 ;; the document.
		 (font . ,(font-spec :family (face-attribute 'default :family parent)
				     :size 2))
		 (desktop-dont-save . t)
		 (width . 2)
		 (height . 1)))))
      (set-window-buffer (frame-root-window emureader--flash-frame) buffer)
      (set-window-dedicated-p (frame-root-window emureader--flash-frame) t)))
  emureader--flash-frame)

(defun emureader-flash-position (x y &optional window width height)
  "Briefly mark the page fractions X, Y in WINDOW.

With WIDTH and HEIGHT, also fractions of the page, highlight that box
with X, Y as its top left corner; otherwise point an arrow at X, Y.
The mark is shown for `emureader-flash-duration' seconds, in a child
frame of WINDOW's frame, and only on graphical displays."
  (let* ((window (or window (selected-window)))
	 (parent (window-frame window))
	 (edges (window-inside-pixel-edges window)))
    (when (and (display-graphic-p parent)
	       (not (frame-parent parent)))
      (emureader--flash-hide)
      (let* ((frame (emureader--flash-frame parent))
	     (box (and width height))
	     (from (emureader-page-fraction-to-window x y window))
	     (to (and box (emureader-page-fraction-to-window
			   (+ x width) (+ y height) window)))
	     (char-w (frame-char-width parent))
	     (char-h (frame-char-height parent))
	     (left (if box (min (car from) (car to)) (- (car from) (* 2 char-w))))
	     (top (if box (min (cdr from) (cdr to)) (- (cdr from) (/ char-h 2))))
	     (pixel-w (if box (max 4 (abs (- (car to) (car from)))) (* 2 char-w)))
	     (pixel-h (if box (max 4 (abs (- (cdr to) (cdr from)))) char-h))
	     (frame-resize-pixelwise t))
	(when (< -1 top (- (nth 3 edges) (nth 1 edges)))
	  (with-current-buffer (window-buffer (frame-root-window frame))
	    (let ((inhibit-read-only t))
	      (erase-buffer)
	      (unless box
		(insert (propertize "➤" 'face
				    `(:foreground ,(face-foreground 'emureader-flash nil t)
						  :height ,(face-attribute 'default :height parent)
						  :weight bold))))))
	  (modify-frame-parameters
	   frame
	   `((background-color . ,(if box
				      (face-background 'emureader-flash nil t)
				    (face-background 'default parent)))
	     (alpha . ,(if box emureader-flash-opacity 1.0))
	     (left . ,(max 0 (+ (nth 0 edges) left)))
	     (top . ,(max 0 (+ (nth 1 edges) top)))))
	  (set-frame-size frame pixel-w pixel-h t)
	  (make-frame-visible frame)
	  (setq emureader--flash-timer
		(run-with-timer emureader-flash-duration nil
				#'emureader--flash-hide)))))))

(defun emureader--clear-doc-memory ()
  "Clear document memory if we are in `emureader-mode'.

Intended for use in `kill-buffer-hook'."
  (when (derived-mode-p 'emureader-mode)
    (emureader-dyn--close-doc)))

(defun emureader-close-doc ()
  "Close the current document, also prompt the user for confirmation."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to close the current document?")
    (kill-buffer (current-buffer))))

(defun emureader--restore-display-on-quit (page scale)
  "Reload the document with PAGE at SCALE and rebuild window overlays on quit.

Quit is deferred inside so the recovery cannot itself be interrupted."
  (with-local-quit
    (condition-case nil
        (progn
          (emureader--render-buffer (1- page))
          (emureader-doc-scale-page scale)
          (emureader--center-page)
          (emureader--manage-window-overlays))
      (error nil))))

(define-queue-command refresh-doc-buffer (&optional ignore-auto noconfirm preserve-modes)
  "Refresh the buffer document with file on disk.

It reloads the entire document while preserving the previous state.
The arguments are ignored and are simply to satisfy the template for
`revert-buffer-function'.

This function is replaced as `revert-buffer-function' for
`emureader-mode' buffers."
  (interactive)
  ;; Can't just prefix with _ because they get used when constructing
  ;; the function definitions.
  (ignore ignore-auto noconfirm preserve-modes)
  ;; A buffer that no window shows is skipped.
  (when-let* ((_ (and buffer-file-name (not emureader--reloading)))
	      (win (get-buffer-window nil t)))
    (setq emureader--reloading t)
    (unwind-protect
	(with-selected-window win
	  (let ((page (or (emureader-current-doc-pagenumber) 1))
		(scale (emureader-current-doc-scale-value)))
	    (message "Reloading your document...Press C-g to stop.")
	    (condition-case err
		(progn
		  (emureader--render-buffer (1- page) t)
		  (emureader-doc-scale-page scale)
		  (emureader--center-page)
		  (emureader--manage-window-overlays)
		  ;; Record the new version, so that it is not
		  ;; reverted again, e.g. by `auto-revert-mode'.
		  (unless (file-remote-p buffer-file-name)
		    (set-visited-file-modtime)))
	      ((quit error)
	       (message "Reloading interrupted; restoring display...")
	       (emureader--restore-display-on-quit page scale)
	       (signal (car err) (cdr err))))))
      (setq-local emureader--reloading nil))))

(defun emureader--render-buffer (&optional page recheck)
  "Render the document file current buffer is associated with.
Load the PAGE if provided.  For a remote document, RECHECK non-nil
fetches it again if it changed; see `emureader--local-file'."
  (interactive)
  (if-let* ((file (emureader--local-file recheck)))
      (when-let* ((_ (file-exists-p file))
		  (page (if (numberp page) (max 0 page) 0)))
	(emureader-dyn--load-doc file page)
	;; emureader-dyn--load-doc resets the window state, which also
	;; needs to be reflected in the frontend.
	(set-window-parameter nil 'scale 1.0)
	(set-window-parameter nil 'emureader-rotation 0))
    (message "No file associated with buffer.")))

;; We have an overlay per window, since we (naturally) need to be able
;; show different pages on different windows.
;;
;; The window knows (as a window parameter):
;; * Which overlay it belongs to.
;; * Which page number it's currently displaying.
;; * What the current scale is.
;;
;; The overlay knows (as an overlay property):
;; * Which window it belongs to.
;; * user pointer to the underlying EmacsWinState from the native
;; module.
;;
;; Window and overlay need to know each other for efficient lookup.

(defun emureader--manage-window-overlays ()
  "Manage the creation and deletion of window overlays as needed.

It is hooked to `window-configuration-change-hook' to keep detecting."
  (let ((overlays (car (overlay-lists)))
	(windows (get-buffer-window-list (current-buffer) nil t)))
    (mapc #'emureader--window-close-function overlays)
    (mapc #'emureader--window-create-function windows)
    (mapc #'emureader--fit-if-pending windows)))

(defun emureader--window-create-function (window)
  "Create window overlay for WINDOW."
  ;; Only buffer-local state is used: the window's own  parameters
  ;; may be left over from another buffer.
  (with-selected-window window
    (unless (emureader--window-owned-p window)
      (when-let* ((old (window-parameter window 'overlay))
		  (_ (overlayp old)))
	(emureader-dyn--window-close old))
      (set-window-parameter window 'overlay nil)
      (emureader-dyn--window-create window)
      (set-window-parameter window 'emureader-rotation 0)
      ;; Scale first so goto-page renders and centers once at the final scale.
      (emureader-doc-scale-page emureader--recent-scale-fallback)
      (let ((emureader--inhibit-page-change-hook t))
	(emureader-goto-page (or emureader--recent-pagenumber-fallback 1)))))
  (emureader--fit-if-pending window))

(defun emureader--fit-if-pending (window)
  "Fit the page to WINDOW if the document was opened without being shown."
  (when (and (window-live-p window)
	     (buffer-local-value 'emureader--fit-pending (window-buffer window))
	     (emureader--window-owned-p window))
    (with-selected-window window
      (setq emureader--fit-pending nil)
      (funcall emureader-default-fit))))

(defun emureader--window-close-function (overlay)
  "Properly close the window belonging to OVERLAY."
  (let ((window (overlay-get overlay 'window)))
    (unless (window-valid-p window)
      (emureader-dyn--window-close overlay))))

(defun emureader--handle-window-buffer-change (window)
  "Handle overlay state when WINDOW's buffer changes.
Cleans up stale overlays if the window switched to a different buffer,
and ensures a valid overlay exists if the new buffer is a emureader buffer."
  (when-let* ((_ (windowp window))
	      (overlay (window-parameter window 'overlay))
	      (_ (overlayp overlay)))
    (unless (eq (overlay-buffer overlay) (window-buffer window))
      ;; Overlay belongs to a different buffer. Clean it up.
      (emureader-dyn--window-close overlay)
      (set-window-parameter window 'overlay nil))
    ;; If the new buffer is a emureader buffer and lacks an overlay, create one.
    (when (with-current-buffer (window-buffer window)
            (derived-mode-p 'emureader-mode))
      (unless (window-parameter window 'overlay)
        (emureader--window-create-function window))
      (emureader--fit-if-pending window))))


(declare-function emureader-outline-show "emureader-outline")
(declare-function emureader-synctex-inverse-search "emureader-synctex")
;; We explicitly bind default keys because relying on the rebinding
;; mechanism can lead to them not being bound at all if users have
;; overridden the defaults with other commands.
(defvar-keymap emureader-mode-map
  :doc "Keymap for `emureader-mode'."
  "n"       #'emureader-next-page
  "p"       #'emureader-previous-page

  "C-p"     #'emureader-scroll-up-or-prev-page
  "C-n"     #'emureader-scroll-down-or-next-page
  "<remap> <previous-line>" #'emureader-scroll-up-or-prev-page
  "<remap> <next-line>" #'emureader-scroll-down-or-next-page
  "<remap> <next>" #'emureader-scroll-down-or-next-page
  "<remap> <prior>" #'emureader-scroll-up-or-prev-page

  "<wheel-up>" #'emureader-mwheel-scroll-up
  "<wheel-down>" #'emureader-mwheel-scroll-down

  "S-<wheel-up>" #'emureader-mwheel-scroll-left
  "S-<wheel-down>" #'emureader-mwheel-scroll-right

  "C-v"      #'emureader-scroll-down-screen
  "M-v"      #'emureader-scroll-up-screen
  "<remap> <scroll-down-command>" #'emureader-scroll-up-screen
  "<remap> <scroll-up-command>" #'emureader-scroll-down-screen

  "SPC"     #'emureader-scroll-down-screen-or-next-page
  "DEL"     #'emureader-scroll-up-screen-or-prev-page
  "S-SPC"   #'emureader-scroll-up-screen-or-prev-page

  "C-f"     #'emureader-scroll-right
  "C-b"     #'emureader-scroll-left
  "<remap> <forward-char>" #'emureader-scroll-right
  "<remap> <backward-char>" #'emureader-scroll-left

  "C-e"     #'emureader-scroll-right-most
  "C-a"     #'emureader-scroll-left-most
  "<remap> <move-end-of-line>" #'emureader-scroll-right-most
  "<remap> <move-beginning-of-line>" #'emureader-scroll-left-most

  "M-<" #'emureader-first-page
  "M->" #'emureader-last-page
  "<remap> <beginning-of-buffer>" #'emureader-first-page
  "<remap> <end-of-buffer>" #'emureader-last-page

  "g" #'emureader-refresh-doc-buffer

  "M-g g"   #'emureader-goto-page
  "M-g M-g" #'emureader-goto-page
  "<remap> <goto-line>"   #'emureader-goto-page

  "="       #'emureader-enlarge-size
  "+"       #'emureader-enlarge-size
  "C-<wheel-up>" #'emureader-mwheel-enlarge-size

  "-"       #'emureader-shrink-size
  "C-<wheel-down>" #'emureader-mwheel-shrink-size

  "0"       #'emureader-reset-size

  "H"       #'emureader-fit-to-height
  "W"       #'emureader-fit-to-width

  "r"       #'emureader-rotate-clockwise
  "R"       #'emureader-rotate-counter-clockwise

  "o"       #'emureader-outline-show

  "C-c C-g" #'emureader-synctex-inverse-search
  "C-<down-mouse-1>" #'ignore
  "C-<mouse-1>" #'emureader-synctex-inverse-search

  "Q"       #'emureader-close-doc


  ;; Disable a few mouse commands to make sure the user is not able to
  ;; accidentally move the point to a weird place, which causes the
  ;; cursor (which we use as a page border) to be drawn after the
  ;; page.

  "<remap> <mouse-set-point>"   #'emureader-mouse-select-window
  "<remap> <mouse-set-region>"  #'emureader-mouse-select-window
  "<remap> <mouse-set-mark>"    #'emureader-mouse-select-window
  "<remap> <mouse-set-secondary>"  #'emureader-mouse-select-window
  "<remap> <mouse-drag-region>"    #'emureader-mouse-select-window
  "<remap> <mouse-drag-secondary>" #'emureader-mouse-select-window
  "<remap> <mouse-save-then-kill>" #'emureader-mouse-select-window
  "<remap> <mouse-secondary-save-then-kill>" #'emureader-mouse-select-window)

;;;###autoload
(define-derived-mode emureader-mode special-mode "EμReader"
  "Major mode for viewing documents in The EMuReader.

Keybindings:
\\{emureader-mode-map}"
  :group 'emureader
  :interactive nil

  (emureader-ensure-module)

  ;; For a remote document this fetches the local copy.
  (let ((file (condition-case err
		  (emureader--local-file)
		(error (message "%s" (error-message-string err)) nil))))
    (unless (and file
		 (file-exists-p file)
		 (file-regular-p file)
		 (file-readable-p file))
      (let ((name (or buffer-file-name "<no file>")))
	(kill-buffer (current-buffer))
	;; Stop here: the rest of the body would act on another buffer.
	(user-error "EMuReader did not open the document: %s is not a valid file"
		    name))))

  (setq-local buffer-read-only t
	      global-linum-mode nil
	      cursor-type 'hollow
	      display-line-numbers-mode nil
	      ;; messes up centering(line-prefix) otherwise, the
	      ;; default value is 8, so we keep it at that.
	      left-fringe-width 8)
  (set-buffer-modified-p nil)
  (blink-cursor-mode 0)
  (auto-revert-mode 1)
  (setq-local revert-buffer-function #'emureader--non-queue-refresh-doc-buffer)

  ;; Disable `pixel-scroll-precision-mode' locally because it doesn't
  ;; work nicely with `emureader-mode'.
  (when (bound-and-true-p pixel-scroll-precision-mode)
    (setq-local pixel-scroll-precision-mode nil))

  (declare-function emureader-bookmark-make-record "emureader-bookmark")
  (setq-local bookmark-make-record-function
	      #'emureader-bookmark-make-record)

  ;; When the document is opened without being shown, as by
  ;; `find-file-noselect', the selected window shows another buffer:
  ;; keep its scrolling, and fit the page once a window shows it.
  (let* ((window (selected-window))
	 (foreign (not (eq (window-buffer window) (current-buffer))))
	 (hscroll (window-hscroll window))
	 (vscroll (window-vscroll window t)))
    (setq emureader--fit-pending foreign)
    (unless (bound-and-true-p
	     emureader-current-doc-render-status)
      (emureader--render-buffer))
    (when foreign
      (set-window-hscroll window hscroll)
      (set-window-vscroll window vscroll t)))

  (declare-function emureader--outline-imenu-goto "emureader-outline")
  (declare-function emureader--outline-imenu-create-index "emureader-outline")
  (setq-local imenu-create-index-function #'emureader--outline-imenu-create-index
	      imenu-default-goto-function #'emureader--outline-imenu-goto)

  (run-hooks 'emureader-mode-hook)

  (unless emureader--fit-pending
    (funcall emureader-default-fit))
  (add-hook 'window-size-change-functions #'emureader--center-page nil t)

  ;;`bookmark--jump-via' messes up with centering, so we need to center again.
  (add-hook 'bookmark-after-jump-hook #'emureader--center-page nil t)
  ;; Depth 90 so that it runs after `save-place-to-alist' so scroll/page
  ;; are saved before the document state is cleared.
  (add-hook 'kill-buffer-hook #'emureader--clear-doc-memory 90 t)
  (add-hook 'window-configuration-change-hook #'emureader--manage-window-overlays nil t)
  (add-hook 'window-buffer-change-functions #'emureader--handle-window-buffer-change nil t))

(defun emureader-mode-line ()
  "Set custom mode-line interface when reading documents."
  (setq-local mode-line-position
	      '(" P" (:eval (number-to-string (emureader-current-doc-pagenumber)))
                "/" (:eval (number-to-string emureader-current-doc-pagecount)))))

(add-hook 'emureader-mode-hook #'emureader-mode-line)
(add-hook 'emureader-mode-hook #'imenu-add-menubar-index)

(defun emureader--redisplay-all-windows ()
  "Redisplay the current document in all windows showing this buffer."
  (let ((inhibit-quit t))
    (dolist (win (get-buffer-window-list nil nil t))
      (with-selected-window win
	(emureader-dyn--redisplay-doc)))))

(define-minor-mode emureader-dark-mode
  "Toggle dark-mode for current emureader document."
  :lighter " Dark"
  (when (derived-mode-p 'emureader-mode)
    (emureader-dyn--doc-invert (if emureader-dark-mode 1 0))
    (emureader--redisplay-all-windows)))

(define-minor-mode emureader-presentation-mode
  "Toggle presentation view for current emureader document."
  :lighter ""
  (when (derived-mode-p 'emureader-mode)
    (cond (emureader-presentation-mode
	   (setq-local mode-line-format nil)
	   (emureader-fit-to-height))
	  ((kill-local-variable 'mode-line-format)))))

;;;###autoload
(dolist (pattern emureader-supported-formats)
  (add-to-list 'auto-mode-alist (cons (concat "\\." pattern "\\'") 'emureader-mode)))

(provide 'emureader)
;;; emureader.el ends here.

;; Local Variables:
;; read-symbol-shorthands: (("define-queue-command" . "emureader--define-queue-command"))
;; End:
