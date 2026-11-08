;;; emureader-fork-tests.el --- Tests for the fork's features  -*- lexical-binding: t; -*-

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

;; Batch tests for the requirements in doc/fork-spec.org that need no
;; display.  Run them with "make test".  Tests that need a TeX
;; installation, org-noter or a remote host are skipped when those are
;; missing.  The remote host is named by the environment variable
;; EMUREADER_TEST_REMOTE, e.g. "/ssh:host:/path/to/a/pdf-without-ext";
;; the remote file is only read.

;;; Code:

(require 'ert)
(require 'emureader)
(require 'emureader-synctex)

(defvar emureader-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defmacro emureader-test--with-temp-dir (var &rest body)
  "Bind VAR to a new temporary directory while running BODY."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "emureader-test" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

;;; T: the cache

(ert-deftest emureader-test-sniff-extension ()
  "T3: formats are guessed from the first bytes."
  (emureader-test--with-temp-dir dir
    (let ((pdf (expand-file-name "a" dir))
	  (epub (expand-file-name "b" dir))
	  (other (expand-file-name "c" dir)))
      (let ((coding-system-for-write 'binary))
	(with-temp-file pdf (insert "%PDF-1.5\n%\342\343\n"))
	(with-temp-file epub
	  (insert "PK\003\004" (make-string 26 ?\0) "mimetypeapplication/epub+zip")))
      (with-temp-file other (insert "hello"))
      (should (equal (emureader--sniff-extension pdf) "pdf"))
      (should (equal (emureader--sniff-extension epub) "epub"))
      (should-not (emureader--sniff-extension other)))))

(ert-deftest emureader-test-cache-name ()
  "T2: names are made safe file names."
  (should (equal (emureader--cache-name "Kripke - Naming and Necessity.pdf")
		 "Kripke - Naming and Necessity.pdf"))
  (should (equal (emureader--cache-name "a/b:c.pdf") "a_b_c.pdf"))
  (should (equal (emureader--cache-name "..") "document")))

(ert-deftest emureader-test-supported-extension ()
  (should (emureader--supported-extension-p "x/Paper.PDF"))
  (should (emureader--supported-extension-p "b.epub"))
  (should-not (emureader--supported-extension-p "/store/ab/ab12cd"))
  (should-not (emureader--supported-extension-p "notes.org")))

(defun emureader-test--remote ()
  "Return the remote test document, or nil when none is configured."
  (let ((remote (getenv "EMUREADER_TEST_REMOTE")))
    (and remote (not (string-empty-p remote)) remote)))

(ert-deftest emureader-test-remote-fetch-named ()
  "T2, T5: a remote document is cached under NAME and reused."
  (skip-unless (emureader-test--remote))
  (let ((remote (emureader-test--remote)))
    (emureader-test--with-temp-dir dir
      (let* ((emureader-cache-directory dir)
	     (cache (emureader-cache-fetch remote "Some Paper.pdf"))
	     (attributes (file-attributes remote)))
	(should (equal (file-name-nondirectory cache) "Some Paper.pdf"))
	(should (string-prefix-p dir cache))
	(should (= (file-attribute-size (file-attributes cache))
		   (file-attribute-size attributes)))
	(should (equal (emureader--sniff-extension cache) "pdf"))
	;; Reused: the same file, not fetched again.
	(let ((inode (file-attribute-inode-number (file-attributes cache))))
	  (should (equal (emureader-cache-fetch remote "Some Paper.pdf") cache))
	  (should (equal (file-attribute-inode-number (file-attributes cache))
			 inode)))
	;; A stale copy is fetched again.
	(set-file-times cache 0)
	(let ((inode (file-attribute-inode-number (file-attributes cache))))
	  (should (equal (emureader-cache-fetch remote "Some Paper.pdf") cache))
	  (should-not (equal (file-attribute-inode-number (file-attributes cache))
			     inode)))))))

(ert-deftest emureader-test-remote-fetch-sniffed ()
  "T3: without NAME, an extension-less remote file gets a sniffed one."
  (skip-unless (emureader-test--remote))
  (let ((remote (emureader-test--remote)))
    (skip-unless (not (emureader--supported-extension-p remote)))
    (emureader-test--with-temp-dir dir
      (let* ((emureader-cache-directory dir)
	     (cache (emureader-cache-fetch remote)))
	(should (equal (file-name-nondirectory cache)
		       (concat (file-name-nondirectory remote) ".pdf")))
	(should (equal (emureader-cache-fetch remote) cache))))))

(ert-deftest emureader-test-clear-cache ()
  "T8: clearing keeps nothing when no buffer uses the cache."
  (emureader-test--with-temp-dir dir
    (let ((emureader-cache-directory dir))
      (make-directory (expand-file-name "abc" dir))
      (with-temp-file (expand-file-name "abc/x.pdf" dir) (insert "x"))
      (should (= (emureader-clear-cache) 1))
      (should-not (file-exists-p (expand-file-name "abc" dir))))))

;;; S: positions and SyncTeX

(ert-deftest emureader-test-rotation-round-trip ()
  "S3: rotating and unrotating fractions are inverse."
  (dolist (rotation '(0 90 180 270))
    (dolist (point '((0.1 . 0.2) (0.75 . 0.4) (0.0 . 1.0)))
      (let* ((shown (emureader--rotate-fraction (car point) (cdr point) rotation))
	     (back (emureader--unrotate-fraction (car shown) (cdr shown) rotation)))
	(should (< (abs (- (car back) (car point))) 1e-9))
	(should (< (abs (- (cdr back) (cdr point))) 1e-9)))))
  ;; Rotating clockwise by 90 moves the top left corner to the top right.
  (should (equal (emureader--rotate-fraction 0 0 90) '(1 . 0))))

(defun emureader-test--compile-tex (dir)
  "Compile a small LaTeX file with SyncTeX in DIR and return its name."
  (let ((tex (expand-file-name "doc.tex" dir))
	(default-directory dir))
    (with-temp-file tex
      (insert "\\documentclass{article}\n"
	      "\\begin{document}\n"
	      "First line of text.\n"
	      "\n"
	      "\\newpage\n"
	      "Second page paragraph on line six.\n"
	      "\\end{document}\n"))
    (should (eql 0 (call-process "pdflatex" nil nil nil "-synctex=1"
				 "-interaction=nonstopmode" "doc.tex")))
    tex))

(ert-deftest emureader-test-synctex-round-trip ()
  "S1, S2: forward then inverse search returns the source line."
  (skip-unless (and (executable-find "pdflatex") (executable-find "synctex")))
  (emureader-test--with-temp-dir dir
    (let* ((tex (emureader-test--compile-tex dir))
	   (pdf (expand-file-name "doc.pdf" dir))
	   (place (emureader-synctex-view pdf tex 6)))
      (should (equal (plist-get place :page) 2))
      (let ((source (emureader-synctex-edit
		     pdf 2
		     (+ (plist-get place :x) 5)
		     (+ (plist-get place :y) (/ (plist-get place :height) 2)))))
	(should (file-equal-p (nth 0 source) tex))
	(should (= (nth 1 source) 6))))))

(ert-deftest emureader-test-synctex-errors ()
  "S5: missing SyncTeX data is a `user-error'."
  (emureader-test--with-temp-dir dir
    (let ((pdf (expand-file-name "x.pdf" dir)))
      (let ((coding-system-for-write 'binary))
	(with-temp-file pdf (insert "%PDF-1.5\n")))
      (should-error (emureader-synctex-view pdf "x.tex" 1) :type 'user-error)
      (let ((emureader-synctex-program "emureader-no-such-program"))
	(with-temp-file (expand-file-name "x.synctex.gz" dir) (insert ""))
	(should-error (emureader-synctex-view pdf "x.tex" 1) :type 'user-error)))))

(ert-deftest emureader-test-auctex-viewer ()
  "S4: the AUCTeX viewer entry is registered once tex is loaded."
  (skip-unless (locate-library "tex"))
  (require 'tex)
  (should (assoc "EμReader" TeX-view-program-list)))

;;; N: org-noter

(ert-deftest emureader-test-org-noter-registration ()
  "N1: the hooks are registered."
  (skip-unless (locate-library "org-noter-core"))
  (require 'org-noter-core)
  (require 'emureader-org-noter)
  (should (memq 'emureader-mode org-noter-supported-modes))
  (should (member "pdf" org-noter--doc-extensions))
  (dolist (hook '(org-noter-set-up-document-hook
		  org-noter--doc-approx-location-hook
		  org-noter--doc-goto-location-hook
		  org-noter--get-current-view-hook
		  org-noter--get-precise-info-hook
		  org-noter--pretty-print-location-hook
		  org-noter--pretty-print-location-for-title-hook
		  org-noter-get-selected-text-hook
		  org-noter-create-skeleton-functions
		  org-noter-open-document-functions))
    (should (seq-some (lambda (f) (string-prefix-p "emureader-" (symbol-name f)))
		      (symbol-value hook)))))

(ert-deftest emureader-test-org-noter-skeleton-data ()
  "N5: the outline becomes skeleton entries with 1-based pages."
  (require 'emureader-org-noter)
  (let ((data (emureader-org-noter--outline-data
	       '((:title "One" :page 0 :children ((:title "One.a" :page 2)))
		 (:title "Two" :page 4))
	       2)))
    (should (equal (mapcar (lambda (v) (list (aref v 0) (aref v 1) (aref v 2))) data)
		   '(("One" (1 . 0) 2) ("One.a" (3 . 0) 3) ("Two" (5 . 0) 2))))))

(provide 'emureader-fork-tests)
;;; emureader-fork-tests.el ends here
