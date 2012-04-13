(update-player-moves (player2 current_game_state))

(print-bb (avail_move (nth 0 (pieces (player1 current_game_state)))))

(update-state-rep current_game_state)

(defun yea (player i)
	   (print-bb (avail_move (nth i (pieces player)))))


(setf this-node (make-instance 'node :game_state current_game_state
			             :player_turn 'player1))


(loop for i from 0 to 5 do 
	      (setf (pos (nth i (pieces (player1 current_game_state)))) (blank-bb))
	      (setf (pos (nth i (pieces (player2 current_game_state)))) (blank-bb)))

(print-game-state current_game_state)

(format t "turn_count: ~d" (turn_count current_game_state))


(ai-move current_game_state)

(setf (pos (nth 4 (pieces (player2 current_game_state)))) (fill-bb '(30)))
(setf (pos (nth 4 (pieces (player1 current_game_state)))) (fill-bb '(36)))
