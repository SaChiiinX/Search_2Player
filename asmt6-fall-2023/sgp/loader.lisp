;;;
;;; This is the Common Lisp implementation of Sensory Graphplan.  Original 
;;; Lisp Graphplan implementation by Mark Peot <peot@rpal.rockwell.com>.
;;; Enhancements by Dave Smith <de2smith@ptolemy.arc.nasa.gov>
;;; Support for Factored Expansion, PDDL
;;; domains, and other optimizations by Dave Smith, Dan Weld
;;; <weld@cs.washington.edu>, and Corin Anderson <corin@cs.washington.edu>.
;;;
;;; Copyright (c) Mark Peot, 1995; University of Washington, 1997, 1998.
;;;
;;; Please send mail to bug-sgp@cs.washington.edu if you download this
;;; code, find bugs in it, or just wish to receive occasional news about
;;; possible new versions. 
;;;

;;; $Id: loader.lisp,v 1.9 1999/05/19 22:14:59 corin Exp $

(in-package :cl-user)

(defpackage "DOMAINS"
  (:use :common-lisp)
  (:export load-domains
	   define def-domain def-problem
	   *domains* *problems* *the-problem* *last-was-problem*
	   domain-name domain-requirements domain-operators 
	   domain-problems domain-types
	   problem-name problem-domain problem-init problem-goal
	   problem-objects
	   situation-objects
	   op-name op-parameters op-precondition op-effect op-variables
	   op-components op-typing
	   comp-precondition comp-effect comp-operator comp-name
	   check-requirements get-domain get-problem process-typed-list
	   oneof uncertain iff observes forall exists))

(defpackage "LOGIC"
  (:use :common-lisp)
  (:import-from domains iff oneof uncertain)
  (:export variable? +false+ plug instance? invert clauses goal-clauses
	   conjuncts inverted? keyword? check-neq check-eq ground?
	   cnf dnf dnf* collect-variables collect-atoms canonicalize-variables
	   eval-f
	   partition-cnf partition partition-literals partition-clauses))

(defpackage "GP"
  (:use :common-lisp :domains :logic)
  ;;(:import-from excl exit)
  (:import-from cl-user mk)
  (:export plan process-quantifier-many))


;; EOF