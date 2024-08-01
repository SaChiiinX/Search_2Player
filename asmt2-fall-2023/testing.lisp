;; ==================================
;;  CMPU-365, Fall 2023
;;  testing.lisp
;; ==================================
;;  Load this file to compile and load all files for this assignment
;;  and run all tests.

(load "asmt-helper.lisp")

(header "Fall 2023 Asmt. 2" "Extended IDS")

;;  Compile-and-load all files

(maker *list-o-files*)

;; ------------------------
;;  TEST EIGHTS
;; ------------------------

;;  Generate random eights problem by doing n randomly chosen moves
;;  from goal position.  Ensures that optimal solution will have at
;;  most n steps.

(defun make-random-eights-xprob (n)
  (make-xproblem :init-state (make-random-eights n)))

;;  *PR* -- a random eights problem with a possibly long optimal solution
;;  *MR* -- the MASTER-RESULTS struct from doing IDA* search on *PR*

(setf *pr* (make-random-eights-xprob 200))
(setf *mr* (xids-astar *pr*))  ;; using MANHATTAN DISTANCE heuristic by default
(print-results *mr* t 0)

;; FANCY-SHOW-EIGHTS-SOLN: concisely shows solution path with 10 puzzles per row
(fancy-show-eights-soln (results-goal-node *mr*))

;; -----------------------
;;  TEST CITIES
;; -----------------------

(format t "Testing the CITIES domain!~%~%~%")
(setf *cpr* (make-xproblem :init-state (make-cities :curr-loc 'arad)))
(setf *cmr* (xids-astar *cpr* :heuristic #'cities-heuristic))
(print-results *cmr* t 0)

;; -----------------------
;;  TEST FIFTEENS
;; -----------------------

(defun make-random-fifteens-xprob (n)
  (make-xproblem :init-state (make-random-fifteens n)))
  
(tester '(setf *px* (make-random-fifteens-xprob 100)))
(setf *mx* (xids-astar *px*))
(print-results *mx* t 0)

(fancy-show-fifteens-soln (results-goal-node *mx*))
