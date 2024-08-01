;; ===================================
;;  CMPU-365, Spring 2023
;;  Asmt. 2 -- Sanjae, Michael, Nahom
;;  XFIFTEENS.LISP
;; ===================================

;;  Some useful constants

(defvar goal-15-rows #(0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3))
(defvar goal-15-cols #(0 0 1 2 3 0 1 2 3 0 1 2 3 0 1 2))

;;  FIFTEENS-MOVE struct
;; --------------------------------------------------
;;  Contains sufficient info for the DO-MOVE method to do the move!

(defstruct (fifteens-move (:print-function show))
  drow ;; The change in the blank's row
  dcol ;; The change in the blank's col (only one can be non-zero)
  name ;; A symbol, useful for printing
  )

;;  SHOW method for an FIFTEENS-MOVE struct
;; --------------------------------------------------

(defmethod show ((mv fifteens-move) str depth)
  (declare (ignore depth))
  (format str "Fifteens MOVE: ~A~%" (fifteens-move-name mv)))

;;  FIFTEENS struct  -- Same as for Asmt. 1
;; ---------------------------------------------------

(defstruct (fifteens (:print-function show))
  (locations (make-array '(4 4) 
			 :initial-contents 
			 '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 0))))
  ;; The position of the blank is given by:
  (blank-row 3)
  (blank-col 3))

;;  SHOW method for an FIFTEENS struct
;; ----------------------------------------------------
;;  INPUTS:  GAME, an fifteens tile puzzle
;;           STR, an output stream (probably just T)
;;           DEPTH, ignored
;;  OUTPUT:  None
;;  SIDE EFFECT:  Displays the given game in the interactions window.

(defmethod show ((game fifteens) str depth)
  (declare (ignore depth))
  (let ((locs (fifteens-locations game)))
    (format str "~%")
    ;; Walk through the rows and columns of the puzzle
    (dotimes (row 4)
      (dotimes (col 4)
	;; display the tile at the current location
	(let ((tile (aref locs row col)))
	  (cond
	   ((= tile +blank-num+) (format str " ~A " +blank-symbol+))
	   ((< tile 10) (format str " ~A " tile))
	   (t (format str "~A " tile)))))
	  (format str "~%"))))

;;  FIFTEENS-GOAL-ARRAY -- same as in Asmt. 1

(defvar *fifteens-goal-array* (make-array '(4 4) 
					:initial-contents
					'((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 0))))

;;  FIFTEENS-GOAL -- same as in Asmt. 1

(defvar *fifteens-goal* (make-fifteens :locations *fifteens-goal-array*
				       :blank-row 3
				       :blank-col 3))

;;  For this assignment, each domain must define STATES-ARE-EQUAL? and
;;  STATE-IS-GOAL? methods.  That's why the XPROBLEM struct no longer
;;  contains STATE-EQ-FUNC and GOAL-TEST-FUNC fields.

;;  STATES-ARE-EQUAL? 
;; ---------------------------------
;;  INPUTS:  Two instances of FIFTEENS tile puzzle structs
;;  OUTPUT:  T if they represent the same state.

(defmethod states-are-equal? ((st1 fifteens) (st2 fifteens))
  (equalp (fifteens-locations st1) (fifteens-locations st2)))

;;  STATE-IS-GOAL?
;; --------------------------------------
;;  INPUT:  An FIFTEENS tile-puzzle instance
;;  OUTPUT:  T if that input represents a goal state.

(defmethod state-is-goal? ((st fifteens))
  (states-are-equal? st *fifteens-goal*))


;; =====================================================================
;;  NOTE:  The FETCH-LEGAL-MOVES and DO-MOVE methods are defined in the
;;         body of this LET*.  That way, these methods can make use of
;;         the local variables created by this LET*.
;; =====================================================================

(let* (;; Four FIFTEENS-MOVE instances
       (mv-north (make-fifteens-move :drow -1 :dcol 0 :name 'north))
       (mv-south (make-fifteens-move :drow 1 :dcol 0 :name 'south))
       (mv-east (make-fifteens-move :drow 0 :dcol 1 :name 'east))
       (mv-west (make-fifteens-move :drow 0 :dcol -1 :name 'west))
       ;; A hash table that will provide the legal moves for each position
       (mv-hash (make-hash-table :test #'equal))
       ;; A vector of the 4 FIFTEENS-MOVE structs
       (mv-vector (make-array 4
			      :initial-contents
			      (list mv-north mv-south mv-east mv-west))))
  
  ;; We'll use the following MAPCAR to initialize the MV-HASH hash-table
  (mapcar #'(lambda (pair)
	      ;; PAIR has the form (BPOSN MV-INDICES) where
	      ;;   BPOSN is a pair (e.g., (0 1)) specifying a puzzle position
	      ;;   MV-INDICES is a list of indices specifying which of the
	      ;;     FIFTEENS-MOVE instances in MV-VECTOR are legal for the
	      ;;     position BPOSN.
	      (let ((bposn (first pair))
		    (mv-indices (second pair)))
		;; Set the hash-table entry for BPOSN to the list of
		;; FIFTEENS-MOVE instances that are legal moves if the blank
		;; is at BPOSN.
		(setf (gethash bposn mv-hash)
		  (mapcar #'(lambda (mv-index) (aref mv-vector mv-index))
			  mv-indices))))
	  ;;north 0, south 1, east 2, west 3
	  '(((0 0) (1 2))
	    ((0 1) (1 2 3))
	    ((0 2) (1 2 3)) ;; e.g., Blank at (0 2), so SOUTH, WEST are legal 
	    ((0 3) (1 3))
	    ((1 0) (0 1 2))
	    ((1 1) (0 1 2 3)) ;; e.g., Blank at (1 1), so all moves are legal
	    ((1 2) (0 1 2 3))
	    ((1 3) (0 1 3))
	    ((2 0) (0 1 2))
	    ((2 1) (0 1 2 3))
	    ((2 2) (0 1 2 3))
	    ((2 3) (0 1 3))
	    ((3 0) (0 2))
	    ((3 1) (0 2 3))
	    ((3 2) (0 2 3))
	    ((3 3) (0 3))))
  
  ;;  FETCH-LEGAL-MOVES:  Just look up the legal moves corresponding to
  ;;                      the blank's current position.
  ;; --------------------------------------------------------------------
  
  (defmethod fetch-legal-moves ((state fifteens))
    (let* ((brow (fifteens-blank-row state))
	   (bcol (fifteens-blank-col state))
	   (bposn (list brow bcol)))
      (gethash bposn mv-hash)))
  
  ;;  DO-MOVE
  ;; -------------------------------------------------------------
  ;;  Since we'll only apply this to LEGAL moves, we don't have to worry
  ;;  about returning NIL.
  
  (defmethod do-move ((state fifteens) (mv fifteens-move))
    (let* (;; BROW, BCOL = blank's current position
	   (brow (fifteens-blank-row state))
	   (bcol (fifteens-blank-col state))
	   (curr-locs (fifteens-locations state))
	   ;; NEW-LOCS:  a new array for holding the result of the move
	   (new-locs (make-array '(4 4)))
	   (drow (fifteens-move-drow mv))
	   (dcol (fifteens-move-dcol mv)))
      ;; Copy the array contents from CURR-LOCS to NEW-LOCS
      (dotimes (row 4)
	(dotimes (col 4)
	  (setf (aref new-locs row col) (aref curr-locs row col))))
      ;; Make two changes to NEW-LOCS due to the blank having swapped places
      ;; with another tile.
      (setf (aref new-locs brow bcol) 
	(aref curr-locs (+ brow drow) (+ bcol dcol)))
      (setf (aref new-locs (+ brow drow) (+ bcol dcol)) 
	+blank-num+)
      ;; Return a list of the form (NEW-STATE COST), where cost = 1
      (list (make-fifteens :locations new-locs
			   :blank-row (+ brow drow)
			   :blank-col (+ bcol dcol))
	    1))))
  
;;  MAKE-RANDOM-FIFTEENS --  Same as in Asmt. 1
;; ------------------------------------------------------

(defun make-random-fifteens (n)
  (let (;; PUZZ:  a new puzzle starting out in goal state
	(puzz (make-fifteens)))
    ;; Then attempt to do N randomly selected moves (some may be illegal)
    (dotimes (i n)
      (let* ((moves (fetch-legal-moves puzz))
	     (rand-move (nth (random (length moves)) moves)))
	;; do randomly selected move (don't care about cost)
	(setf puzz (first (do-move puzz rand-move)))))
    puzz))

;;  FANCY-SHOW-FIFTEENS-SOLN
;; ---------------------------------------------

(defun fancy-show-fifteens-soln (goal-node)
  (let* ((nodes (xbuild-path goal-node))
	 (states (mapcar #'xnode-state nodes))
	 (grids (mapcar #'fifteens-locations states))
	 (first10-and-resty (fetch-n-elts 10 grids))
	 (first-ten-grids (first first10-and-resty)))
    (setf grids (second first10-and-resty))
    (while first-ten-grids
      (dotimes (row 4)
	(dolist (gr first-ten-grids)
	  (dotimes (col 4)
	  (cond
	  ((zerop (aref gr row col)) (format t " ~s " +blank-symbol+))
	  ((< (aref gr row col) 10) (format t " ~s " (aref gr row col)))
	  (t
	  (format t "~s " (aref gr row col)))))
	  (format t "  "))
	(format t "~%"))
      (format t "~%")
      (setf first10-and-resty (fetch-n-elts 10 grids))
      (setf first-ten-grids (first first10-and-resty))
      (setf grids (second first10-and-resty)))))


;;  MANHATTAN-DISTANCE
;; ----------------------------------------------
;;  INPUT:  An FIFTEENS struct
;;  OUTPUT:  The total of all the manhattan distances of each tile
;;           to its goal position.

(defmethod manhattan-distance ((state fifteens))
  (let* ((locs (fifteens-locations state))
	 (counter 0))
    ;; For each location (row, col)...
    (dotimes (row 4)
      (dotimes (col 4)
	(let* (;; Fetch the tile at that location
	       (tile (aref locs row col))
	       ;; And fetch the row and column of that tile in
	       ;; the goal array
	       (g-row (svref goal-15-rows tile))
	       (g-col (svref goal-15-cols tile)))
	  ;; Only deal with tiles, not the blank!
	  (when (not (= tile +blank-num+))
	    (incf counter (+ (abs (- g-row row))
			     (abs (- g-col col))))))))
    counter
    ))

