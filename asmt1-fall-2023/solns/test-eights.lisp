;; -----------------------------------------
;;  CMPU-365, Fall 2023
;;  Asmt. 1 -- Testing the Eights Puzzle
;; -----------------------------------------

;;  MAKE-RANDOM-EIGHTS
;; -------------------------------------------
;;  INPUT:  N, a non-negative integer
;;  OUTPUT: An instance of an EIGHTS puzzle that is randomly generated
;;     by doing (or attempting to do) N random moves starting from the
;;     goal position.  This guarantees that the puzzle can be solved in
;;     at most N moves.

(defun make-random-eights (n)
  (let* (;; PUZZ:  a new puzzle starting out in goal state
	 (puzz (make-eights)))
    ;; Then attempt to do N randomly selected moves (some may be illegal)
    (dotimes (i n)
      ;; pick random action and store results of doing that move in TMP-PUZZ
      (let* ((rand-act (nth (random 4) eights-actions))
	     (tmp-puzz (funcall rand-act puzz)))
	;; if it was a legal move, then setf PUZZ to the result
	(when tmp-puzz (setf puzz tmp-puzz))))
    puzz))

;;  MAKE-EIGHTS-PROBLEM
;; ------------------------------------------------
;;  INPUT:  PUZZ, an instance of an EIGHTS struct
;;  OUTPUT:  A SEARCH-PROBLEM instance for solving that eights puzzle

(defun make-eights-problem (puzz)
  ;; return the search problem using that randomly generated puzzle
  (make-search-problem :init-state puzz
		       :actions eights-actions
		       :goal-test-func #'eights-goal?
		       :state-eq-func #'eights-equal?))

;;  DO-EIGHTS-SEARCH
;; ------------------------------------
;;  INPUT:   PUZZ, an instance of an EIGHTS puzzle
;;  OUTPUT:  A RESULTS struct that contains info about what
;;    happened from runding the given search algorithm starting
;;    with initial state PUZZ

(defun do-eights-search (puzz &key (search-func #'depth-first-search) (show? nil))
  (let ((rezzy (funcall search-func (make-eights-problem puzz))))
    (cond
     ;; Case 1:  SHOW the full results (including the solution path)
     (show?
      rezzy)
     ;; Case 2:  Only show stats, not the solution path
     (t
      (format t "RESULTS:  explored ~A nodes in ~A msecs.~%"
	      (results-num-nodes rezzy) (results-elapsed-time rezzy))
      (when (results-goal-node rezzy)
	(format t "Found a solution with ~A steps!~%" 
		(length (build-path (results-goal-node rezzy)))))))))

;;  Wrapper functions for DO-EIGHTS-SEARCH for DFS and BrFS

(defun do-eights-depth (puzz &key (show? nil))
  (do-eights-search puzz :search-func #'depth-first-search :show? show?))

(defun do-eights-breadth (puzz &key (show? nil))
  (do-eights-search puzz :search-func #'breadth-first-search :show? show?))

