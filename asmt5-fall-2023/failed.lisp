;; ===========================================
;;  CMPU-365, Fall 2023
;;  Amst. 5 -- Due Thursday, Nov. 9th at Noon
;;  STNs and friends!
;; ===========================================


;;  STN struct
;; ----------------------------------------

(defstruct (stn (:print-function print-stn))
  ;; NUM-TPS:  The number of timepoints in the network
  (num-tps 0)
  ;; TP-HASH:  A hash-table of (key,value) entries
  ;;   (a) if key is an integer, then value is a symbol naming a timepoint
  ;;   (b) if key is a symbol, then value is the integer index for that timepoint
  (tp-hash (make-hash-table))
  ;; LIST-O-EDGES:  A list of triples, each of the form (FROM WT TO), where
  ;;   FROM and TO are symbols naming timepoints, and WT is a number
  (list-o-edges nil) 
  ;; PREDS:  A vector of hash-tables where the ith hash-table contains (key,value)
  ;;         entries where key=j and value=wt specifies an edge from i to j with weight wt
  (preds nil)
  ;; SUCCS:  A vector of hash-tables where the ith hash-table contains (key,value)
  ;;         entries where key=h and value=wt specifies an edge from h to i with weight wt
  (succs nil))

;;  PRINT-STN
;; -------------------------------
;;  INPUTS: S, an STN struct
;;          STR, a stream (usually T)
;;          DEPTH, ignored
;;  OUTPUT: None
;;  Side Effect:  Pretty-prints the STN

(defun print-stn (s str depth)
  (declare (ignore depth))
  (let ((n (stn-num-tps s)))
    (format str "An STN with ~A tps: " n)
    (dotimes (i n) (format str "~A " (gethash i (stn-tp-hash s))))
    (format str "~%")
    (format str "EDGES: ")
    (dolist (trip (stn-list-o-edges s))
      (format str "~A " trip))
    (format str "~%")))

;;  INIT-STN
;; --------------------------------
;;  INPUT:  TP-NAMES, a list of symbols specifying names for the timepoints
;;          LIST-O-TRIPLES, a list of triples of the form (FROM WT TO)
;;             where FROM and TO are symbols representing timepoints
;;             and TO is a number
;;  OUTPUT:  An STN struct suitable initialized

(defun init-stn (tp-names list-o-triples)
  (let* ((s (make-stn))
	 (tp-hash (stn-tp-hash s))
	 (n (length tp-names))
	 (ctr 0))
    (setf (stn-num-tps s) n)
    ;; Initialize the tp-hash
    (dolist (tp-name tp-names)
      ;; First:  key = ctr, value = tp-name
      (setf (gethash ctr tp-hash) tp-name)
      ;; Second: key = tp-name, value = ctr
      (setf (gethash tp-name tp-hash) ctr)
      (incf ctr))  
    ;; Create the VECTORS of HASH-TABLES for PREDS and SUCCS
    (setf (stn-preds s) (make-array n))
    (setf (stn-succs s) (make-array n))
    (let ((preds (stn-preds s))
	  (succs (stn-succs s)))
      (dotimes (i n)
	(setf (svref preds i) (make-hash-table))
	(setf (svref succs i) (make-hash-table)))
      ;; Walk through the list of edges
      (dolist (trip list-o-triples)
	(let* ((from (first trip))
	       (wt (second trip))
	       (to (third trip))
	       ;; Get the numerical indices associated with FROM and TO
	       (from-indy (gethash from tp-hash))
	       (to-indy (gethash to tp-hash)))
	  (push trip (stn-list-o-edges s))
	  ;; Insert key = from-indy, value = wt into PREDS hash-table for TO-INDY
	  (setf (gethash from-indy (svref preds to-indy)) wt)
	  ;; Insert key = to-indy, value = wt into SUCCS hash-table for FROM-INDY
	  (setf (gethash to-indy (svref succs from-indy)) wt))))
    ;; return the STN
    s))

;;  FW -- Floyd Warshall algorithm
;; ----------------------------------------
;;  INPUT:  S, an STN struct
;;  OUTPUT: DISTY, the distance matrix for S

(defun fw (s)
  (let* ((n (stn-num-tps s))
	 (tp-hash (stn-tp-hash s))
	 (disty (make-array (list n n)))
	 (succs (stn-succs s))
	 (preds (stn-preds s)))
    ;; Init disty using the edges in the STN:  
    (dotimes (i n)
      (maphash #'(lambda (key value) 
		   (setf (aref disty i key) value))
	       (aref succs i)))
    ;; Triple loop:  
    (dotimes (k n)
      (dotimes (i n)
	(when (numberp (aref disty i k))
	  (dotimes (j n)
	    (let ((val (handle-null-1 (aref disty i k) (aref disty k j) (aref disty i j))))
	    (when (numberp val)
	      (setf (aref disty i j) val)))
	  ))))
    ;; Return DISTY
    disty))

(defun handle-null-1 (ik kj ij)
  (cond
   ;; Case 1: If path is nil
   ((or (null ik) (null kj))
    nil)
   ;; Case 2: If 
   ((null ij)
    (+ ik kj))
   ;; Case 3: Both are numbers
   (t
    (if (< (+ ik kj) ij)
	(+ ik kj)
      nil))))

(defun is-soln-for? (soln-veck s)
  (let* ((n (stn-num-tps s))
	 (succs (stn-succs s))
	 (preds (stn-preds s)))
    ;; HINT:  For each timepoint I, use MAPHASH on SUCCS[I] to walk
    ;;        through the edges emanating from I.
    (dotimes (i n)
      (let ((val-i (aref soln-veck i)))
	(maphash #'(lambda (k val-k)
		     (format t "(~A - ~A) <=  ~A~%" (aref soln-veck k) val-i val-k)
		     (when (> (- (aref soln-veck k) val-i) val-k)
		       (return-from is-soln-for? nil))) 
		     (aref succs i)))))
    T)

;;  DECK struct
;; ---------------------------
;;  For dealing "cards" at random from a given deck

(defstruct deck
  num-left
  cards)

;;  INIT-DECK
;; ---------------------------
;;  INPUT:  NUM, the number of cards in the deck
;;  OUTPUT:  A DECK struct that can be used to deal cards from
;;           the set {0,1,...,N-1}

(defun init-deck (num)
  (let ((d (make-deck :num-left num
		      :cards (make-array num))))
    (dotimes (i num)
      (setf (svref (deck-cards d) i) i))
    d))

;;  DEAL!
;; ---------------------------
;;  INPUT:  DEK, a DECK struct
;;  OUTPUT:  A card dealt from DEK
;;  Side Effect:  Destructively modifies DEK by putting the dealt
;;    card into a discard pile.

(defun deal! (dek)
  (let* ((num (deck-num-left dek))
	 (kardz (deck-cards dek))
	 (randy (random num))
	 (card (svref kardz randy)))
    ;; Swap chosen card to "discard pile" at end of vector
    (decf (deck-num-left dek))
    (setf (svref kardz randy) 
      (svref kardz (deck-num-left dek)))
    (setf (svref kardz (deck-num-left dek)) 
      card)
    ;; return the "card"
    card
    ))

;;  GET-RANDY-IN-RANGE
;; ------------------------------------
;;  INPUTS:  LOWER, UPPER, either INTEGERS or NIL
;;  OUTPUT:  A number within the interval [LOWER, UPPER], inclusive.
;;  Note:  If either is NIL, limits range to an arbitrary finite interval.

(defun get-randy-in-range (lower upper)
  (cond
   ;; Case 1:  Both are non-NIL (and hence assumed to be numbers)
   ((and lower upper)
    (+ lower (random (1+ (- upper lower)))))
   ;; Case 2:  LOWER is non-NIL (but UPPER is NIL)
   (lower
    ;; Arbitrarily use upper bound of lower+20
    (+ lower (random 20)))
   ;; Case 3:  UPPER is non-NIL (but LOWER is NIL)
   (upper 
    ;; Arbitrarily use lower bound of upper-20
    (- upper (random 20)))
   ;; Case 4:  Both are NIL
   (t
    ;; Pick any number in [0,20)
    (random 20))))

;;  GEN-SOLN
;; -------------------------------------
;;  INPUT:   S, an STN
;;  OUTPUT:  A vector representing a solution for S
;;  Note:    If S has N timepoints, the vector will have N entries
;;           interpreted as a mapping from timepoint index to timepoint value          

(defun gen-soln (s)
  (let* (;; Use Floyd-Warshall to compute DISTANCE MATRIX
	 (disty (fw s))
	 (n (stn-num-tps s))
	 ;; DEK will be used to "deal out" the next timepoint to execute
	 (dek (init-deck n))
	 ;; LOWERS and UPPERS are vectors of the lower and upper bounds
	 ;; on the timewindows for the N timepoints.  Initially the bounds
	 ;; are all NIL, indicating "no bound".
	 (uppers (make-array n))
	 (lowers (make-array n))
	 ;; SOLN:  a vector to hold the timepoint values
	 (soln (make-array n)))
    ;; MAIN LOOP
    ;;   As long as there are "cards" to deal (i.e., timepoints that are
    ;;   not yet executed) ...
    (while (> (deck-num-left dek) 0)
      (let* ((x (deal! dek))
	     (lbx (aref lowers x))
	     (ubx (aref uppers x))
	     (time nil))
	(when (and (numberp ubx) (numberp lbx) (< ubx lbx))
	  (return-from gen-soln 'Invalid_Bounds))	
	(setf time (get-randy-in-range lbx ubx))
	(setf (aref soln x) time)
	(dotimes (i n)
	    (let ((dist-u (aref disty x i))
		  (dist-l (aref disty i x))
		  (lbi (aref lowers i))
		  (ubi (aref uppers i)))
	      (when (and (numberp dist-l) (or (null lbi) (< lbi (- time dist-l) )))
		(setf (aref lowers i) (- time dist-l)))
	       (when (and (numberp dist-u) (or (null ubi) (> ubi (+ time dist-u))))
		 (setf (aref uppers i) (+ time dist-u)))))))
    ;; end of WHILE
    soln))

;;  RTE
;; -----------------------------------
;;  INPUTS:  S, an STN structs
;;           VERBOSE?, a boolean indicating whether to print out information
;;              along the way
;;  OUTPUT:  A solution vector or a symbol (e.g., FAIL_ENABLEDS_EMPTY) indicating
;;              failure (which can happen if S is not "dispatchable").

(defun rte (s &key (verbose? nil))
  (let* ((n (stn-num-tps s))
	 (preds (stn-preds s))
	 (succs (stn-succs s))
    	 (uppers (make-array n))
	 ;; Arbitrarily set lower bounds to 0
	 (lowers (make-array n :initial-element 0))
         (soln (make-array n))
	 ;; NEG-EDGE-COUNTS:  a vector whose Ith entry holds the number
	 ;;   of negative edges emanating from I and pointing at  
	 ;;   as-yet-unexecuted timepoints
	 (neg-edge-counts (make-array n :initial-element 0))
	 ;; UNEXECUTED-TPS:  Will hold a list of the as-yet-unexecuted timepoints
	 (unexecuted-tps nil)
	 ;; NOW:  current time
	 (now 0)
	 ;; ENABLEDS:  A list of the "enabled" timepoints (i.e., unexecuted 
	 ;;   timepoints  all of whose negative edges point at already executed
	 ;;   timepoints).
	 (enableds nil)
	 ;; COUNTER:  Just counts the number of iterations in the RTE algorithm
	 (counter 0))
   
    (dotimes (i n)
      ;; Puts all timepoints on unexecuted list
      (setf unexecuted-tps (cons i unexecuted-tps))
      ;; Counts Negative outgoing negative edges of each time point
      (maphash #'(lambda (key value)
		   (when (minusp value)
		     (incf (aref neg-edge-counts i))))
	       (aref succs i)) 
      ;; If an edge has 0 negative outgoing edges add to enableds list
      (when (zerop (aref neg-edge-counts i))
	(push i enableds)))
  
    (let ((min-lower nil)
	  (min-upper nil))
      (while (> (length unexecuted-tps) 0)
	(when (null enableds)
	  (return-from rte 'Fail_Enableds_Empty))
	
	;; Finds the lowest upper and lower bounds
	(dolist (ele enableds)
	  (let ((lb (aref lowers ele))
		(ub (aref uppers ele)))
	    (setf min-lower (if (null min-lower) lb (min min-lower lb)))
	    (setf min-upper (cond
			     ((and min-upper ub)
			      (min min-upper ub))
			     ((and min-upper (null ub))
			      min-upper)))))
      
	;; Checks if lowest lower bound is less than current time
	(when (< min-lower now)
	  (setf min-lower now))
	
	;; Sets now to random time point in range
	(setf now (get-randy-in-range min-lower min-upper))
	
	;; If Upper bound is null set to random
	(when (or (null min-upper) (= min-lower min-upper))
	  (setf min-upper (get-randy-in-range min-lower nil)))
	  ;;(setf min-upper (get-randy-in-range min-lower nil)))
	
	(format t "min-upper:~A, min-lower:~A, enableds:~A, " min-upper min-lower enableds)
	
	(when (< min-upper min-lower)
	  (return-from rte 'FAIL_UB_LT_LB))
	
	;; Intializes availables list
	(let ((availables nil))
	  (dolist (ele enableds)
	    (when (<= (aref lowers ele) min-upper)
	      (push ele availables))
	  
	  (let* ((x (nth (random (length availables)) availables))
		 (lb-x (aref lowers x))
		 (ub-x (aref uppers x))
		 (time nil))
	    ;; Set lb-x
	    (when (< lb-x now)
	      (setf lb-x now))
	    ;; Set ub-x
	    (when (or (null ub-x) (> ub-x min-upper))
	      (setf ub-x min-upper))
	    
	    (format t "lb-x: ~A,  ub-x:~A~%"lb-x ub-x)
	    
	    (when (and ub-x lb-x (< ub-x lb-x))
	      (return-from rte 'Invalid_Bounds))
	    
	    (setf time (get-randy-in-range lb-x ub-x))
	    (setf unexecuted-tps (remove x unexecuted-tps))
	    (setf enableds (remove x enableds))
	    (setf (aref soln x) time)
	    (setf now time)
	    
	    ;; Update the upper bounds
	    (maphash #'(lambda (key value)
			 (when (>= value 0)
			   (setf (aref uppers key) (+ now value))))
		     (aref succs x))
	    ;; Update the lower bounds
	    (maphash #'(lambda (key value)
			 (when (< value 0 )
			   (setf (aref lowers key) (- now value))
			   (decf (aref neg-edge-counts key))
			   (when (zerop (aref neg-edge-counts key))
			     (push key enableds))))
		     (aref preds x))))
	(format t "lowers:~A, uppers:~A~%" lowers uppers))))
    ;; return SOLN
    soln))


;;  Be sure to test your FW function on a couple of consistent STNs having 5 timepoints.
;;    (A consistent STN has no negative-length cycles.)
;;  Test GEN-SOLN on your sample consistent STNs ... confirm that GEN-SOLN generates 
;;    solutions by using IS-SOLN-FOR?.  (GEN-SOLN should never fail for consistent STNs.)
;;  Show that RTE may fail on a consistent STN that is not dispatchable.
;;  But also give examples where RTE succeeds on DISPATCHABLE STNs.
;;    (An STN is dispatchable if whenever there is a path from any X to any Y,
;;     there is a shortest path from X to Y that is a "vee-path" (i.e., a path
;;     consisting of zero or more negative edges followed by zero or more
;;     non-negative edges.)

;;  Turn in printouts of your STN.LISP file and INTERACTIONS.
;;  Submit electronically as usual:  submit365 asmt5 asmt5
;;  (assuming your asmt5 directory is called "asmt5")


;; Here's what your interactions might look like:
;; Note the difference between setting the keyword argument verbose? to T vs. NIL (the default).

;; CL-USER(48): s
;; An STN with 3 tps: A B C 
;; EDGES: (B 2 A) (C -3 A) (B 5 C) (A 3 B) 
;; CL-USER(49): (rte s :verbose? t)
;; Starting round 0 with now=0 and enableds=(1 0)
;;   ---> Executing 0 at time 8
;;   .. Updating uppers(1) = 11
;;       -- 2 has become enabled!
;;   .. Updating lowers(2) = 11
;; Starting round 1 with now=8 and enableds=(2 1)
;;   ---> Executing 1 at time 11
;;   .. Updating uppers(0) = 13
;;   .. Updating uppers(2) = 16
;; Starting round 2 with now=11 and enableds=(2)
;;   ---> Executing 2 at time 11
;; #(8 11 11)
;; CL-USER(50): (is-soln-for? #(8 11 11) s)
;; T
;; CL-USER(51): (rte s)
;; #(11 9 14)
;; CL-USER(52): (is-soln-for? #(11 9 14) s)
;; T
;; CL-USER(53):


;; Test 
(defvar tp-names '(A B C))

(defvar edges '((B 2 A)
		(C -3 A)
		(B 5 C)
		(A 3 B)))

(setf stn (init-stn tp-names edges))

;; Test 1
(defvar tp-names1 '(A B C D E))
(defvar edges1 '((A 15 E)
                (B -3 A)
                (B 7 E)
                (C -2 B)
                (D -4 C)
                (D 9 E)
                (E -1 D)))
(setf stn1 (init-stn tp-names1 edges1))

;; Test 2

(defvar tp-names2 '(A B C D E))
(defvar edges2 '((A 2 B)
                 (A 8 E)
                 (B 2 C)
                 (C 4 D)
                 (D 5 E)
                 (D -2 A)
                 (D -1 C)
                 (E 1 A)))

(setf stn2 (init-stn tp-names2 edges2))

;; Test 3

(defvar tp-names3 '(A B C D E))
(defvar edges3 '((A 4 D)
                 (B -1 A)
                 (D -2 B)
                 (D -2 C)
                 (D 4 E)))

(setf stn3 (init-stn tp-names3 edges3))

;; Test 10

(defvar tp-names10 '(Z X1 X2 X3 X4))
(defvar edges10 '((Z 250 X4)
		  (X1 -4 Z)
		  (X1 168 X4)
		  (X2 0 X1)
		  (X3 -120 X2)
		  (X3 7 X4)
		  (X4 0 X3)))

(setf stn10 (init-stn tp-names10 edges10))

;; Test 11
(defvar tp-names11 '(A B C D E))
(defvar edges11 '((B 2 A)
		  (C -3 A)
		  (B 5 C)
		  (A 3 B)
		  (B 6 E)
		  (E 7 D)
		  (D -1 B)
		  (E -2 B)))

(setf stn11 (init-stn tp-names11 edges11))
