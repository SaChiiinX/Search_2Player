;; =============================================================
;;   FILE:  simple-defsys.lisp
;; =============================================================
;;  A very simple and portable compilation utility.  Inspired by Peter
;;  Norvig's Paradigms_of_AI_Programming, p.892.  Written by Wheeler
;;  Ruml (while at Harvard University) with help from the CS182 staff.
;;
;; Example of use:
;;
;; You have put this file at "---LOCATION---/simple-defsys.lisp" and
;; the file "asmt6.system" contains:
;;
;; (in-package cl-user)
;;
;; (load "---LOCATION---/simple-defsys")
;;
;; (define-system "asstX"
;;    "foo"
;;    "bar")
;;
;; Start Lisp and evaluate (load "asst1.system").  Now you should be
;; able to evaluate (mk).  This should compile and load foo and bar.
;; From then on, evaluating (mk) should compile any changed files and
;; load both foo and bar.

(in-package cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *systems* '() "list of all systems defined")

(defstruct system
  "a named collection of source files that can be compiled in one command"
  name
  src-files)

;;;;;;;;;;;;;;;;;;;;;;; user interface

(defmacro define-system (name &rest src-files)
  "Define a new system.  NAME can be a symbol or a string.  Filenames are
interpreted relative to the directory containing the DEFINE-SYSTEM expression"
  ;; This needs to be a macro so that we can use *load-pathname* to pick up
  ;; the filename in which the DEFINE-SYSTEM call is made
  `(do-define-system ',name ',src-files ,*load-pathname*))

;; can't call this MAKE due to ACL built-in
;;   -- note:  there is no built-in MAKE function in CMU Lisp --

(defun mk (&key (name (when *systems* (system-name (first *systems*))))
		(force-compile nil))
  "Compile and load the files in the system with the given NAME (which should
have already been defined using DEFINE-SYSTEM).  NAME defaults to the most
recently made system.  If FORCE-COMPILE is non-nil, will compile even if
object file is newer.  Defaults to nil.  Always loads all object files."
  (let ((system (find name *systems*
		      :key #'system-name :test #'equalp)))
    (when (null system)
      (error "Can't find system with name ~A!" name))
    ;; for compatibility with CLisp, don't use WITH-COMPILATION-UNIT!!
    (with-compilation-unit ()
      (dolist (src-file (system-src-files system))
	(let ((obj-file (make-pathname :type "fasl"
				       :defaults src-file)))
	  (unless (and (probe-file obj-file)
		       (> (file-write-date obj-file)
			  (file-write-date src-file))
		       (not force-compile))
	    (compile-file src-file :verbose t :print nil))
	  (load obj-file))))
    (push-system system)))

;;;;;;;;;;;;;;;;;;;;;;;; internal functions

(defun do-define-system (name src-files defaults)
  (push-system (make-system
		:name name
		:src-files
		(mapcar #'(lambda (name)
			    (merge-pathnames (merge-pathnames name
							      "*.lisp")
					     defaults))
			src-files))))

(defun push-system (system)
  (let ((name (system-name system)))
    ;; delete any old system w/ same name
    (setf *systems*
      (delete name *systems* :key #'system-name :test #'equalp))
    ;; ensure this one is first
    (push system *systems*)
    name))

;; EOF - simple-defsys.lisp
