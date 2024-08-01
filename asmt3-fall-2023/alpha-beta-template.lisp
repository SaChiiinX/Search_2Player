;; ====================================
;;  CMPU-365, Fall 2023
;;  Asmt. 3
;;  alpha-beta-template.lisp
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
  (let* ((moves (legal-moves g))
	 ;; ========================================
	 ;;  Currently selects a move at random!
	 ;; ========================================
	 (my-move (nth (random (length moves)) moves)))
    (format t "   My move: ~A~%" my-move)
    ;; return my-move
    my-move))
   

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
  'FIX_ME!
  )

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
  'FIX_ME
  )


;;  MY-TEST
;; -------------------------------
;;  Fill in the contract

(defun my-test
    ()
  'THIS_FUNCTION_SHOULD_TEST_A_SAMPLE_CHESS_PROBLEM!
  )
