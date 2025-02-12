;; =====================================
;;  CMPU-365, Fall 2023
;;  Extended IDS for different "value" functions
;; =====================================
;;  Functions defined herein:
;;    XEXPAND -- similar to EXPAND from Asmt. 1
;;    XDLS    -- implements depth-limited search
;;    XIDS    -- implements generic iterative-deepening search
;;    XIDS-ASTAR  -- implements IDA* search using XIDS


;;  XEXPAND -- Similar to EXPAND from Asmt. 1
;; -----------------------------------------------------------------
;;  Takes a VALUE-FUNC as an input.  The VALUE function is used to compute
;;    the value of a child-node immediately after the node has been created.
;;    The VALUE-FUNC takes one input: an XNODE.  For plain old IDS, the
;;    VALUE-FUNC will be #'XNODE-DEPTH (the default).  For the iterative-
;;    deepening version of UCS (we'll call it IDS-UCS) the VALUE-FUNC would
;;    be #'XNODE-COST (i.e., the cost so far).  For IDA* (see XIDS-ASTAR
;;    later on) the VALUE-FUNC will be F(n) = G(n) + H(n).  

(defun xexpand (xnode rezzy &key (value-func #'xnode-depth))
  (labels (;; XEXPAND-ACC:  Recursive helper func that walks through the
	   ;;   legal MOVES to create corresponding XNODES.  CHILDREN is
	   ;;   the accumulator of child XNODES.
	   (xexpand-acc (moves children)
	     (cond
	      ;; Case 1:  No more moves/actions to do
	      ((null moves)
	       ;; Increment the num-nodes field of REZZY (num
	       (incf (results-num-nodes-generated rezzy) (length children))
	       ;; Return the list of child nodes
	       children)
	      ;; Case 2:  More moves/actions to do
	      (t
	       (let* ((childPair (do-move (xnode-state xnode) (first moves)))
		      (child (make-xnode :state (first childPair)
					       :parent xnode
					       :action (first moves)
					       :depth (+ (xnode-depth xnode) 1)
					       :cost (+ (xnode-cost xnode) (second childPair)))))  ;; Gets (state, cost)
		 (setf (xnode-value child) (funcall value-func child))
		 (if (xcycle? (first childPair) xnode)
		     (xexpand-acc (rest moves) children)
		   (xexpand-acc (rest moves) (nconc children (list child)))))))))
    (xexpand-acc (fetch-legal-moves (xnode-state xnode)) nil)))

;;  XDLS:  Extended Depth-Limited Search (based on a VALUE-FUNC)
;; ----------------------------------------------------------------------------
;;  INPUTS:  XPROB, an XPROBLEM instance
;;           LIMIT, specifies the cutoff value for the DLS
;;           MASTER-REZZY, a results struct to record nodes generated
;;           VALUE-FUNC, used to compute value of a node
;;  OUTPUT:  One of the following:
;;              An XNODE (i.e., a *GOAL* node)
;;              NIL, search space exhausted without hitting cutoff LIMIT
;;                no point in increasing the cutoff LIMIT
;;              A number = the minimum value of the nodes that were generated
;;                         but not explored (because their value was greater
;;                         than the cutoff LIMIT).  Will be used as LIMIT for
;;                         next iteration.

(defun xdls (xprob limit master-rezzy &key (value-func #'xnode-depth))
  (let (;; The ROOT node
	(root (make-root-xnode xprob)))
    (format t "XDLS with limit = ~A~%" limit)

    ;;  XDLS-REC:  A recursive helper function that processes the
    ;;             sub-tree rooted at its input XNODE
    ;; --------------------------------------------------------------------
    (labels ((xdls-rec (xnode)
	       (cond
		;; Case 1: xnode not within limit
		((> (xnode-value xnode) limit) (xnode-value xnode))
		;; Case 2: xnode is a goal
		  ((state-is-goal? (xnode-state xnode)) 
		   (incf (results-num-nodes-explored master-rezzy))
		   xnode)
	        ;; Case 3: Generate children and recurse
		  (t 
		   (let ((children (sort (xexpand xnode master-rezzy :value-func value-func) #'< :key #'xnode-value))
			 (min-value-unexplored-children 0))
		     (dolist (child children)
		       (let ((child-answer (xdls-rec child)))
			 (when (xnode-p child-answer)
			   (return-from xdls-rec child-answer))
			 (incf (results-num-nodes-explored master-rezzy))
			 (when (not (> min-value-unexplored-children limit))
			   (setf min-value-unexplored-children child-answer)))
		       )
		     min-value-unexplored-children
		     )))))
      ;; Body of LABELS:  Call the XDLS-REC helper function on the ROOT node
      (xdls-rec root))))

;;  XIDS -- Extended Iterative Deepening Search (based on a VALUE-FUNC)
;; -------------------------------------------------------------------------
;;  INPUTS:  XPROB, an XPROBLEM struct
;;           VALUE-FUNC, used to compute the value of each generated node
;;  OUTPUT:  A goal node or NIL.

(defun xids (xprob &key (value-func #'xnode-depth))
  (let* (;; Initialize a RESULTS struct
	 (master-rezzy (make-results :start-time (get-universal-time)))
	 ;; First iteration calls XLDS with a LIMIT of 0
	 ;;   (i.e., will only explore root node)
	 (xdls-answer (xdls xprob 0 master-rezzy :value-func value-func)))
    ;; As long as XDLS hit the cutoff without finding a goal....
    (while (and xdls-answer
		(not (xnode-p xdls-answer)))
      ;; It means that xdls-answer is a number:  MIN-VAL-UNEXPLORED-CHILD!
      ;; Use that number as the cutoff for the next iteration:
      (setf xdls-answer (xdls xprob xdls-answer master-rezzy
			      :value-func value-func)))
    ;; After the WHILE:
    ;; Finish collecting info for the RESULTS struct
    (setf (results-end-time master-rezzy) (get-universal-time))
    (setf (results-elapsed-time master-rezzy)
	  (- (results-end-time master-rezzy)
	     (results-start-time master-rezzy)))
    (setf (results-goal-node master-rezzy) xdls-answer)
    ;; Return the RESULTS struct
    master-rezzy))

;;  XIDS-ASTAR
;; ----------------------------------------------------
;;  INPUTS:  XPROB, an XPROBLEM instance
;;           HEURISTIC, a (domain dependent) heuristic function to
;;             apply to nodes
;;  OUTPUT:  A goal node or NIL.

(defun xids-astar (xprob &key (heuristic #'manhattan-h))
  ;; Call XIDS on XPROB with a VALUE-FUNC that computes F(n) = G(n) + H(n)
  (xids xprob :value-func
	#'(lambda (xnode)
	   (+ (xnode-cost xnode) (funcall heuristic xnode))
	    )))



