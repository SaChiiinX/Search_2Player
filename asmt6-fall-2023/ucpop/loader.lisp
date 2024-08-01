;;; Compile and load UCPOP if no defsystem available

" (c) 1990-1995 Copyright (c) University of Washington

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to 
  bug-ucpop@cs.washington.edu; the same address should be used for problems."



(in-package cl-user)

(defpackage "VARIABLE"
  (:use common-lisp)
  (:export unify unify-args bind-variable instantiate-term
	   add-bind new-bindings variable? uniquify-var
	   test-equivalence
	   *unify-count* *compute-rank-unifies* 
	   *computing-rank* *add-bind-count*))

(defpackage "PTRACE"
  (:use common-lisp)
  (:nicknames "VCR" "PDB"))

(defpackage "CHOICE"
  (:use common-lisp)
  (:export reject select rank prefer partition p-clear p-sort p-best 
	   new-choice choice-entry *select-reject*))

(defpackage "RULE-NET"
  (:use common-lisp)
  (:export defclause defrule defdemon defnet trace-rule untrace-rule 
	   assertion variable$ clear-firings profile show-profile
	   collect-firings dump-firings ignore-firings))

(defpackage "SC"
  (:use common-lisp)
  (:export make-sc rule def-clause def-action isrch srch assertion
	   sort-entries))

(defpackage "VCR-EXTERNAL"
  (:use common-lisp)
  (:nicknames "VEXT"))

(defpackage "UCPOP"
  (:use common-lisp variable)
  ;;(:import-from excl exit)
  (:import-from cl-user mk)
  (:export *verbose* define reset-domain *version*
	   define reset-domain trace-scr untrace-scr profile 
	   show-profile operator axiom fact problem clause scr))


;; EOF