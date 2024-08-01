;; ===================================
;;  CMPU-365, Fall 2023
;;  Asmt. 1 -- YOUR NAME HERE!!
;;  EIGHTS.LISP
;; ===================================
;;  A PARTIAL implementation of the eights tile puzzle

;; The underscore is used when printing out a blank;
;; The number 0 is used as a blank within the eights struct.

(defparameter +blank-symbol+ '_)
(defparameter +blank-num+ 0)

;;  MAKE-GOAL-ARRAY
;; ---------------------------------------------
;;  INPUTS:  None
;;  OUTPUT:  A 3-by-3 array of the tiles in their goal position
;;  Note:    0 represents the blank

(defun make-goal-array ()
  (make-array '(3 3) :initial-contents '((1 2 3) (8 0 4) (7 6 5))))

;;  *EIGHTS-GOAL-ARRAY*
;; ---------------------------------------------
;;  An instance of the goal array

(defparameter *eights-goal-array* (make-goal-array))

;;  The EIGHTS data structure
;; ----------------------------------------------------------------
;;  Each instance represents an instance of the eights tile puzzle

(defstruct (eights (:print-function show-eights))
  ;; LOCATIONS:  A 3-by-3 array showing where the tiles and blank are located
  ;;   Note:  We use the MAKE-GOAL-ARRAY function because we want a *different*
  ;;          instance of the goal array each time.
  (locations (make-goal-array))
  ;; For efficient reference:  the position of the blank is given by:
  (blank-row 1)
  (blank-col 1))

;;  SHOW-EIGHTS
;; ----------------------------------------------------
;;  INPUTS:  GAME, an eights tile puzzle
;;           STR, an output stream (probably just T)
;;           DEPTH, ignored
;;  OUTPUT:  None
;;  SIDE EFFECT:  Displays the given game in the interactions window.

(defun show-eights (game str depth)
  (declare (ignore depth))

  ;; WRITE YOUR CODE HERE!
  ;; It should show the contents of the eights puzzle in the 
  ;; format illustrated below::
  ;;    1 2 3
  ;;    8 _ 4
  ;;    7 6 5
  (format t "SHOW-EIGHTS not yet implemented!\n")
  )

;;  EIGHTS-EQUAL?
;; ---------------------------------------------------
;;  INPUTS:  GAME1, GAME2, two instances of an EIGHTS struct
;;  OUTPUT:  T if the two games have all of their tiles in the
;;              same positions.

(defun eights-equal? (game1 game2)

  ;; WRITE YOUR CODE HERE!
  nil
  )

;;  EIGHTS-GOAL?
;; -------------------------------------------------
;;  INPUT:  GAME, an eights tile puzzle
;;  OUTPUT:  T if the game is equal to the goal state

(defun eights-goal? (game)
  
  ;; WRITE YOUR CODE HERE
  nil;;
  )
;;  ON-BOARD
;; -------------------------------------------------------------
;;  INPUTS:  ROW, COL, integers specifying a location on a tile puzzle
;;  OUTPUT:  T if (ROW,COL) is a legal position (i.e., on the board)

(defun on-board (row col)
  (and (>= row 0)
       (< row 3)
       (>= col 0)
       (< col 3)))

;;  COPY-ARRAY  --  used by DO-BLANK-MOVE
;; -------------------------------------------------------------
;;  INPUTS:  ARRIE, a two-dimensional array<
;;  OUTPUT:  A copy of ARRIE

(defun copy-array (arrie)
  (let (;; NEW-ARRAY:  this will be the copy
	(new-array (make-array (array-dimensions arrie))))
    ;; Walk through the rows and columns of the arrays
    (dotimes (row (array-dimension arrie 0))
      (dotimes (col (array-dimension arrie 1))
	;; Copy corresponding elements...
	(setf (aref new-array row col)
	  (aref arrie row col))))
    ;; Return the NEW-ARRAY
    new-array))

;;  DO-BLANK-MOVE
;; -------------------------------------------------------
;;  INPUTS:  GAME, an eights tile puzzle
;;           ROW-DELTA, COL-DELTA, the direction in which the
;;               blank should move
;;  OUTPUT:  A *new* EIGHTS puzzle that results from moving the
;;           blank in the direction specified by ROW-DELTA and COL-DELTA; 
;;           or NIL if the move would be illegal.

(defun do-blank-move (game row-delta col-delta)
  
  ;; WRITE YOUR CODE HERE!
  (format t "DO-BLANK-MOVE not yet implemented!~%")
  nil
  )

;;  BLANK-NORTH, BLANK-SOUTH, BLANK-EAST, BLANK-WEST
;; -------------------------------------------------------------
;;  INPUT:   GAME, an eights tile puzzle
;;  OUTPUT:  The resulting puzzle (EIGHTS struct) if the move
;;            was legal; otherwise NIL.

(defun blank-north (game) (do-blank-move game -1 0))
(defun blank-south (game) (do-blank-move game 1 0))
(defun blank-west (game) (do-blank-move game 0 -1))
(defun blank-east (game) (do-blank-move game 0 1))

;; The list of actions for the eights puzzle

(defparameter eights-actions 
    (list #'blank-north #'blank-south #'blank-east #'blank-west))

