#|***************************************************

                   AI Class
Description: Defines the AI class and its methods.


*****************************************************|#

(defvar load-count nil)
(defun ai-move ( game-st )

  (let ((p-index nil))
    
    (setf cutoffs 0) 
    (setf iteration 0)

    (update-all-pieces game-st)
    (update-player-moves (player1 current_game_state)) 
    (update-player-moves (player1 current_game_state))

    (setf root (make-root-node game-st))
    (update-player-moves (player_turn root))
    (setf load-count (length (player_moves (player_turn root))))

    (format t "~% ~% Chisp is thinking:~%")
    
    (alpha-beta-negasearch root 4 -10000000 10000000 root)

    

    (setf p-index (cond
		    ((eq (car (best_move root)) 'k) 0)
		    ((eq (car (best_move root)) 'q) 1)
		    ((eq (car (best_move root)) 'r) 2)
		    ((eq (car (best_move root)) 'b) 3)
		    ((eq (car (best_move root)) 'n) 4)
		    (t 5)))
    

    (move-piece (nth p-index
		     (pieces (if (= (mod (turn_count current_game_state) 2) 0)
				 ;;then
				 (player1 current_game_state)
				 ;;else
				 (player2 current_game_state))))

		(second (best_move root)) (third (best_move root)))

    (terpri)
    (terpri)
    (terpri)
    (print (best-move-combination root))
    (terpri)
    (terpri)
    (terpri)
    ;;(if (eql print_debug t)
    (format t "Iteration: ~d~%" iteration)
    (format t "~%~%Cutoffs:  ~d" cutoffs)))



(defun best-move-combination ( root )
  (best-combination root nil))

(defun best-combination ( node lst )

  (cond 
    ((eq (best_node node) nil)
        (append lst (list (move_from_parent node))))
    (t (best-combination (best_node node) (if (eq lst nil)
					      (append lst (move_from_parent node))
					      (append lst (list (move_from_parent node))))))))
	 

  
