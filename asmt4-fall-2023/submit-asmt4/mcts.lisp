;; ========================================
;;  CMPU-365, Fall 2023
;;  Monte Carlo Tree Search -- TEMPLATE!
;; ========================================

;;  MC-NODE struct -- a node in the MCTS tree
;; ----------------------------------------------------------------------------
;;  KEY:          a hash-table key (compact rep'n of current state of game)
;;  WHOSE-TURN:   *BLACK* or *WHITE*
;;  NUM-VISITS:   the number of times this state has been visited
;;  VECK-MOVES:   a VECTOR of the legal moves from this state
;;  VECK-VISITS:  a VECTOR recording the number of times each legal move
;;                   has been visited during MCTS
;;  VECK-SCORES:  a VECTOR recording the average scores for the legal
;;                   moves visited during MCTS

(defstruct mc-node
  key             
  whose-turn      
  (num-visits 0)  
  veck-moves      
  veck-visits     
  veck-scores    
  )

;;  MC-TREE struct -- the MCTS tree
;; -------------------------------------------------------------
;;  HASHY:     a hash-table whose entries are (key,value), where
;;               key = compact repn of state, value = mc-node
;;  ROOT-KEY:  the hash-table key for the root node of the mcts tree

(defstruct mc-tree
  (hashy (make-hash-table :test #'equal))      
  root-key)

;;  GET-ROOT-NODE
;; ------------------------------------------------------
;;  INPUT:   TREE, a MCTS struct
;;  OUTPUT:  The MC-NODE corresponding to the root of the TREE

(defun get-root-node
    (tree)
  (gethash (mc-tree-root-key tree) (mc-tree-hashy tree)))

;;  NEW-MC-TREE
;; ---------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  A new MC tree whose root state is derived
;;           from GAME.

(defun new-mc-tree
    (game)
  (make-mc-tree :root-key (make-hash-key-from-game game)))

;;  INSERT-NEW-NODE
;; -----------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           KEY, a hash-key representing the state of the game
;;  OUTPUT:  The newly created and inserted node
;;  SIDE EFFECT:  Inserts a new node into TREE using KEY.

(defun insert-new-node
    (game tree key)
  (let* ((moves (legal-moves game))
	 (num-moves (length moves))
	 (nodey (make-mc-node 
		 :key key
		 :veck-moves moves
		 :veck-visits (make-array num-moves :initial-element 0)
		 :veck-scores (make-array num-moves :initial-element 0)
		 :whose-turn (whose-turn game))))
    ;; insert nodey into tree
    (setf (gethash key (mc-tree-hashy tree)) nodey)
    ;; return the node
    nodey))

;;  SELECT-MOVE
;; ------------------------------------------
;;  INPUTS:  NODEY, an MC-NODE struct
;;           C, exploitation-exploration constant
;;  OUTPUT:  The INDEX of the selected move into the moves vector, or NIL
;;           if there are no moves.

(defun select-move
    (nodey c)
    (let* ((player (mc-node-whose-turn nodey))
	   (moves (mc-node-veck-moves nodey))
	   (num-moves (length moves)))
  (cond
     ;; No legal moves!
     ((= num-moves 0)
      ;; signal failure
      nil)
     ;; Only one legal move
     ((= num-moves 1)
      ;; return it
      0)
     ;; Two or more moves
     (t
      (let ((best-score nil)
	    (best-index nil)
	    (gr-o-lr nil)
	    (add-o-sub nil) 
	    (visits (mc-node-num-visits nodey))
	    (child-visits (mc-node-veck-visits nodey))
	    (scores (mc-node-veck-scores nodey)))
	;; If first visit node just visit first move
	(when (= visits 0)
	  (return-from select-move 0))
	(cond
	 ;; Case 1 -> Player = Black: Sets functions to > and +
	 ((eq player *black*)
	  (setf gr-o-lr #'>)
	  (setf add-o-sub #'+)
	  (setf best-score *neg-inf*))
	 ;; Case 2 -> Player = White: Sets functions to < and -
	 (t      
	  (setf gr-o-lr #'<)
	  (setf add-o-sub #'-)
	  (setf best-score *pos-inf*)))
	;; Finds the index corresponding to the argmax or argmin of
	;; Q(s,a)  +/-  c*sqrt(log(N(s))/N(s,a))
	(dotimes (i num-moves)
	  ;; Visits each child at least once
	  (when (= 0 (aref child-visits i))
	    (return-from select-move i))
	  (let ((value (funcall add-o-sub (aref scores i) (* c (sqrt (/ (log visits) (aref child-visits i)))))))
	  (when (funcall gr-o-lr value best-score)
	    (setf best-index i)
	    (setf best-score value))))
      best-index)))
  ))

;;  SIM-TREE
;; --------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           C, the exploration/exploitation constant
;;  OUTPUT:  A list of the form (state0 move0 state1 move1 ... statek movek)
;;    where each state_i is represented by a KEY into the hashtable, and each move_i
;;    is an INDEX into the MOVES vector of the node assoc with state_i.
;;    NOTE:  The last statek is the state that was not previously in the tree
;;           but for which a new node will be created, and move_k (the last
;;           move) is selected from the info in that node.

(defun sim-tree
    (game tree c)
  (let (;; KEY-MOVE-ACC:  used to accumulate the list of STATE/KEYS and MOVES/INDICES
	(key-move-acc nil)
	;; HASHY:  the hash-table where KEY represents STATE; and VALUE is the corresponding NODE
	(hashy (mc-tree-hashy tree)))
    ;; Keep going as long as the game is not over...
    (while (not (game-over? game))
      (let* (;; KEY:  Hash key for current state of game
	     (key (make-hash-key-from-game game))
	     ;; NODEY:  The MC-NODE corresponding to KEY (or NIL if not in tree)
	     (nodey (gethash key hashy)))
	;; Case 1:  When key not yet in tree...
	(when (null nodey)
	  (let* ((new-node (insert-new-node game tree key))
		 (moves (mc-node-veck-moves new-node))
		 (move-index (select-move new-node c))
		 (move *pass*))
	    ;; When there is a move update from pass
	    (when (numberp move-index)
	      (setf move (aref moves move-index)))
	    (apply #'do-move! game nil move)
	    (return-from sim-tree (reverse (cons move-index (cons key key-move-acc))))))
	(let* ((moves (mc-node-veck-moves nodey))
	       (move-index (select-move nodey c))
	       (move *pass*))
	  ;; When there is a move update from pass
	  (when (numberp move-index)
	    (setf move (aref moves move-index)))
	  (apply #'do-move! game nil move)
	  (setf key-move-acc (cons move-index (cons key key-move-acc))))))
    ;; After the WHILE... return the reverse accumulated key/move list
    (reverse key-move-acc)))
 
;;  SIM-DEFAULT -- defined for you!
;; ----------------------------------------------
;;  INPUT:   GAME, a game struct
;;  OUTPUT:  The result of following the game's default policy
;;             (domain-dependent method)

(defun sim-default
    (game)
  (default-policy game))

;;  BACKUP
;; ---------------------------------------------------
;;  INPUTS:  HASHY, the hash-table for the MCTS
;;           KEY-MOVE-ACC, the accumulated list of KEYs and MOVEs
;;              from a simulation run
;;           RESULT, the result (from black's perspective) of the 
;;              recently played out simulation
;;  OUTPUT:  doesn't matter
;;  SIDE EFFECT:  Updates the relevant nodes in the MC-TREE/HASHY

(defun backup
    (hashy key-move-acc result)
  (let ((index 0)
	(node nil))
    (dolist (ele key-move-acc)
      (cond
       ;; Case 1: If Key
       ((evenp index)
	(setf node (gethash ele hashy))
	(incf (mc-node-num-visits node)))
       ;; Case 2: If Index
       (t
	(incf (aref (mc-node-veck-visits node) ele))
	(incf (aref (mc-node-veck-scores node) ele)
	      (/ (- result (aref (mc-node-veck-scores node) ele)) (aref (mc-node-veck-visits node) ele)))))
      (incf index)))
  nil
  )

;;  UCT-SEARCH
;; ---------------------------------
;;  INPUTS:  ORIG-GAME, a game struct
;;           NUM-SIMS, a positive integer
;;           C, the exploration/exploitation parameter
;;  OUTPUT:  Best move from that state determined by
;;             doing *NUM-SIMS* simulations of MCTS.

(defparameter *verbose* t) 

(defun uct-search
    (orig-game num-sims c)
  (let* ((tree (new-mc-tree orig-game))
	 (hashy (mc-tree-hashy tree)))
    (dotimes (i num-sims)
      (let* ((cpy-game (copy-game orig-game))
	     (key-move-acc (sim-tree cpy-game tree c))
	     (result (sim-default cpy-game)))
	(backup hashy key-move-acc result)))
    (let* ((root-node (get-root-node tree))
	   (scores (mc-node-veck-scores root-node))
	   (visits (mc-node-veck-visits root-node))
	   (moves (mc-node-veck-moves root-node))
	   (size (length scores))
	   (best-index (select-move root-node 0))
	   (best-move (aref moves best-index)))
      (when (not *verbose*)
	(format t "Best Score: ~A~%" (aref scores best-index))
	(dotimes (i size)
	  (format t "~A, " (aref scores i)))
	(format t "~%Veck Scores: ")
	(dotimes (i size)
	  (format t "~A " (aref visits i))))
      best-move))
)


;;  COMPETE -- defined for you!
;; ------------------------------------------------------------------------------
;;  INPUTS:  BLACK-NUM-SIMS, the number of simulations for each of black's moves
;;           BLACK-C, the exploration/exploitation constant used by black
;;           WHITE-NUM-SIMS, the number of simulations for each of white's moves
;;           WHITE-C, the exploration/exploitation constant used by white
;;  OUTPUT:  Don't care
;;  SIDE EFFECT:  Displays the entire game using UCT-SEARCH to compute best moves
;;    for both players according to the specified parameters.

(defun compete
    (black-num-sims black-c white-num-sims white-c)
  (let ((g (new-othello)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
	(format t "BLACK'S TURN!~%")
	(format t "~A~%" 
		(apply #'do-move! g nil (uct-search g black-num-sims black-c))))
       (t
	(format t "WHITE'S TURN!~%")
	(format t "~A~%"
		(apply #'do-move! g nil (uct-search g white-num-sims white-c))))))))


;;  COMPETE-NO-PRINTING
;; --------------------------------------------------
;;  Same as COMPETE, but only shows the end result

(defun compete-no-printing
    (black-num-sims black-c white-num-sims white-c)
  (let ((g (new-othello)))
    (while (not (game-over? g))
      (cond
       ((eq (whose-turn g) *black*)
	(format t "B ")
	(apply #'do-move! g nil (uct-search g black-num-sims black-c)))
       (t
	(format t "W ")
	(apply #'do-move! g nil (uct-search g white-num-sims white-c)))))
    (format t "~%~A~%" g)))
