;; ========================================
;;   CMPU-365, Fall 2023
;;   Asmt. 1
;;   YOUR NAME HERE!!!
;; ========================================
;;   FILE: gen-search-starter.lisp
;; ========================================
;;   General tree-search algorithm.  Special cases
;;   include breadth-first and depth-first search.

;;  Load the "basic-defns" file that defines the PROBLEM and NODE
;;  data structures and some related low-level functions.

(load "basic-defns" :verbose nil)

;;   CYCLE? 
;; -------------------------------------------------------------
;;  INPUTS:  STATE, a problem state
;;           NODE, a search node
;;           STATE-EQ-FUNC, a function that determines whether
;;             two states are equal
;;  OUTPUT:  T if the given STATE is the same as the state of
;;    the given NODE or the state of any of NODE's ancestors.
;;    Uses STATE-EQ-FUNC to determine whether two states are equal.
;;    Otherwise returns NIL. 
;;    NOTE:  If NODE is NIL it returns NIL.

(defun cycle? (state node state-eq-func)
  
  ;; Replace the following with code that actually WORKS!!
  (format t "The CYCLE? function does not currently work!!~%")
  nil)

;;  MAKE-ROOT-NODE 
;; ---------------------------------------
;;  INPUT:  PROB, a search problem
;;  OUTPUT:  A search NODE that will be the ROOT NODE of a
;;           brand new search tree.

(defun make-root-node (prob)
  
  ;; Replace the following with code that actually WORKS!!
  (format t "The MAKE-ROOT-NODE function does not currently work!!~%")
  nil)

;;  EXPAND 
;; ---------------------------------
;;  INPUTS:  NODE, a search node
;;           ACTS, a list of actions
;;           ST-EQ-FUNC, a function for testing whether two
;;              states are equal
;;  OUTPUT:  A list of child nodes obtained by applying the
;;           given list of actions to NODE's state.  However, it
;;           does *NOT* include child nodes whose states already
;;           appear on the path from the root node to NODE.
;;           (Use CYCLE? to determine this.)

(defun expand (node acts st-eq-func)

  ;; You are encouraged to use LABELS to define an accumulator-based
  ;; recursive helper function, called EXPAND-ACC.  By defining your
  ;; helper function within the body of EXPAND, you will have access
  ;; to NODE, ACTS and ST-EQ-FUNC, should you need them.  Alternatively,
  ;; you could define EXPAND-ACC as a stand-alone helper function.
  ;; In that case, you may need to provide it with additional inputs.

  ;; Replace the following with code that actually works!
  (format t "The EXPAND function does not currently work!!~%")
  nil)

;; --------------------------------------------------------
;;  GEN-SEARCH-GUTS
;; ---------------------------------------------------------------
;;  INPUTS: PROBLEM, a search problem
;;          QUEUE-FN, a queuing function (used to insert newly
;;            created nodes into the search queue).  The queuing
;;            function determines which kind of search we're doing.
;;          REZZY, a RESULTS struct
;;  OUTPUT: The RESULTS struct, with its fields updated to include
;;          info about the search (e.g., num nodes explored; the goal
;;          node, if found; and timing information).
;; ---------------------------------------------------------------
;;  This function performs the indicated search problem using
;;  the search strategy determined by QUEUE-FN.

(defun gen-search-guts (problem queue-fn rezzy)

  (let (;; Some local variables for convenience                                                        
        (gt-func (search-problem-goal-test-func problem))
        (st-eq-func (search-problem-state-eq-func problem))
        (acts (search-problem-actions problem)))
    
    (labels
     ;; GEN-REC:  A RECURSIVE HELPER FUNCTION
     ;;   QUEUE:  Used to hold the "frontier" of as-yet-unexplored" nodes
     ((gen-rec (queue)
	   ;; BODY of GEN-REC function goes here!
	   ;; Replace the following with code that actually works!
	   (format t "The GEN-SEARCH-GUTS function doesn't currently work!~%")
	   nil))

      ;; BODY OF LABELS
     ;;   Call GEN-REC with the initial search queue
      (gen-rec (list (make-root-node problem))))))

