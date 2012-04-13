#|***************************************************

                   Tree Class
Description: Defines the Tree class and its methods.


*****************************************************|#


(defun alpha-beta-negasearch ( node depth alpha beta root)


  (setf iteration (+ iteration 1))
  
    
    (if (eq depth 0) 


      ;;then
	(* -1 (eval-state-score (game_state node)))

      ;;else
	(let ((temp-node nil)
	      (move nil))

	    ;; Loop through each move for player
	    (update-all-pieces (game_state node))
	    (update-player-moves (player_turn node))
	    (loop while (hasNext-player-move (player_turn node)) do


		 ;; Get players next move
		 (setf move (getNext-player-move (player_turn node)))
		 
		 ;; Temp-state to perform temp move on
		 (let* ((temp-state (copy-game-state (game_state node)))
			(player (if (eql (allegiance (nth 0 (pieces (player_turn node)))) 1)
				    ;;then
				    (player1 temp-state)
				    ;;else
				    (player2 temp-state))))
		    
		   
		     
		     (move-piece (cond ((eq (car move) 'k)   (nth 0 (pieces player)))
						       ((eq (car move) 'q)   (nth 1 (pieces player)))
						       ((eq (car move) 'r)   (nth 2 (pieces player)))
						       ((eq (car move) 'b)   (nth 3 (pieces player)))
						       ((eq (car move) 'n)   (nth 4 (pieces player)))
						       (t                    (nth 5 (pieces player))))
						 (nth 1 move)
						 (nth 2 move)
						 temp-state)


		   ;; Update 'state_rep' for temp-state
		   (update-state-rep temp-state)


		   

		   ;;; DEBUG ;;;
		   (if (eql print_debug t)
		       (progn
			 (terpri)
			 (print-game-state temp-state)
			 (terpri)
			 (format t "player1: ~d" (count-pieces (player1 temp-state)))
			 (terpri)
			 (format t "player2: ~d" (count-pieces (player2 temp-state)))
			 (terpri)
			 (format t "eval-score: ~d" (eval-state-score temp-state))
			 (terpri)
			 (format t "turn_count: ~d" (turn_count temp-state))))
		   ;;; END DEBUG ;;;


		   ;; Make the new child node with hash name of temp-state 
		   (setf temp-node (make-child-node
			           (hash-game-state temp-state)
				    node
				    temp-state
				    move))


		   
		   ;; Make recursive call of alpha-beta-negaSearch with new child
		   (let ((value (* -1 (alpha-beta-negaSearch 
				  (car (children node)) (- depth 1) (* -1 beta) (* -1 alpha) root))))


		     ;; Setting alpha cutoff and best_move of root if need be.
		     (when (> value alpha)
		       (setf alpha value)
		       (setf (best_node node) (car (children node)))
		       (when (eq (parent temp-node) root)			 
			 (setf (best_move root) move)))
		     (when (eq (parent temp-node) root)
			 (format t "%~d~%" (round (* 100 (/ 
						 (- load-count (length (player_moves (player_turn root))))
						 load-count)))))

		     ;; If best is greater than beta, then prun this node
		     (if (>= alpha beta)
			 (progn
			   (setf cutoffs (+ cutoffs 1))
			   (return))))))
	    
		   alpha)))