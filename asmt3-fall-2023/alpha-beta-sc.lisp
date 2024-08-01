;; ====================================
;;  CMPU-365, Fall 2023
;;  Asmt. 3
;;  alpha-beta-sc.lisp
;; ====================================

;;  COMPUTE-MOVE
;; -------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The best move according to MINIMAX with ALPHA-BETA
;;   pruning, using the static eval func, EVAL-FUNC.  Searches to
;;   a depth of CUTOFF-DEPTH.

(defun compute-move (g cutoff-depth)
  (format t "~%COMPUTE-MOVE (cutoff=~A)~%" cutoff-depth)
  (let* ((statty (make-stats))
	 (alpha *neg-inf*)
	 (beta *pos-inf*)
	 (best-move (compute-max g 0 alpha beta statty cutoff-depth))
	 (done-moves (stats-num-moves-done statty))
	 (pruned-moves (- (stats-num-potential-moves statty) done-moves)))
    (format t "   NUM-MOVES-DONE: ~A, NUM-MOVES-PRUNED: ~A~%" 
	    done-moves pruned-moves)
    (format t "   BEST MOVE: ~A~%" best-move)
    best-move))
   

;;  COMPUTE-MAX / COMPUTE-MIN
;; ---------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the current depth in the search
;;           ALPHA, BETA, alpha/beta values for this node in search
;;           STATTY, stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  If CURR-DEPTH is zero, returns best move
;;           Otherwise returns value of this node according
;;           to MINIMAX with ALPHA-BETA pruning.

(defun compute-max (g curr-depth alpha beta statty cutoff-depth)
  (cond
   ;; Case 1: Game Over
   ((game-over? g)
    (+ *loss-value* curr-depth))
   ;; Case 2: Cur-depth >= Cutoff-depth
   ((>= curr-depth cutoff-depth)
    (eval-func g))
   ;; Case 3: Cur-depth < Cutoff-depth
   (t
    (let ((best-move nil)
	  (moves (legal-moves g))
	  (child-val nil))
      (incf (stats-num-potential-moves statty) (length moves))
      ;; Iterates through list of legal moves
      (dolist (mv moves)
	;; Does the move on the current game
	(apply #'do-move! g nil mv)
	(incf (stats-num-moves-done statty))
	(setf child-val (compute-min g (+ curr-depth 1) alpha beta statty cutoff-depth))
	;; Undo move since we just want the value of the move
	(undo-move! g)
	;; Updates alpha if child val is better
	
	(when (> child-val alpha)
	  ;; Updates the returned move if depth = 0
	  (when (= curr-depth 0)
	    (setf best-move mv))
	  (setf alpha child-val)
	  ;; Prunes if beta <= alpha
	  (when (<= beta alpha)
	    (return-from compute-max alpha))))
      ;; Returns alpha val or "best move" if depth = 0
      (if (= curr-depth 0)
	  (progn
	  (format t "   ROOT NODE ALPHA: ~A~%" alpha)
	  best-move)
	  alpha)))
   ))

;;  COMPUTE-MIN
;; -------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the depth of this MIN node
;;           ALPHA, BETA, values received from parent MAX node
;;           STATTY, a stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The value of this MIN node according to rules
;;           of MINIMAX with ALPHA-BETA pruning

(defun compute-min (g curr-depth alpha beta statty cutoff-depth)
  (cond
   ;; Case 1: Game Over
   ((game-over? g)
    (- *win-value* curr-depth))
   ;; Case 2: Cur-depth >= Cutoff-depth
   ((>= curr-depth cutoff-depth)
    (eval-func g))
   ;; Case 3: Cur-depth < Cutoff-depth
   (t 
    (let ((moves (legal-moves g))
	  (child-val nil))
      (incf (stats-num-potential-moves statty) (length moves))
      ;; Iterates though list of legal moves
      (dolist (mv moves)
	;; Does the move on the current game
	(apply #'do-move! g nil mv)
	(incf (stats-num-moves-done statty))
	(setf child-val (compute-max g (+ curr-depth 1) alpha beta statty cutoff-depth))
	;; Undo move since we just want the value of the move
	(undo-move! g)
	;; Updates value of beta if child val is better
	(when (< child-val beta)
	  (setf beta child-val)
	  ;; Prunes if beta <= alpha
	  (when (<= beta alpha)
	    (return-from compute-min beta))))
      beta))
   ))

;;  MY-TEST
;; -------------------------------
;;  Choose One
;;  Test 1 -> 3 Move Checkmate from init game (may be repitive)
;;  Test 2 -> King and Rook Checkmate (Endgame - only those pieces on the board)
;;  Test 3 -> Bishop Fork (Test if it can find a move that can win pieces)

(defun my-test
    ()
  'THIS_FUNCTION_SHOULD_TEST_A_SAMPLE_CHESS_PROBLEM!
  )
