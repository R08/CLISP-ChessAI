#|*********************************************************

                     Game-util.lisp

*********************************************************|#



#|---------------------------

     FUNCTION DEFINITIONS

----------------------------|#

(defmethod find-piece-at-index ((index integer) (gs game_state))
  "Accept an index, finds the piece at that index, and returns the player and piece reference number at that index."

  (cond ((eq (bit (player_pieces (player1 gs)) index) 1)
      
	 (cond 
	   ((eq (bit (pos (nth 0 (pieces (player1 gs)))) index) 1)
	    (pos (nth 0 (pieces (player1 gs)))))
	   ((eq (bit (pos (nth 1 (pieces (player1 gs)))) index) 1)
	    (pos (nth 1 (pieces (player1 gs)))))
	   ((eq (bit (pos (nth 2 (pieces (player1 gs)))) index) 1)
	    (pos (nth 2 (pieces (player1 gs)))))
	   ((eq (bit (pos (nth 3 (pieces (player1 gs)))) index) 1)
	    (pos (nth 3 (pieces (player1 gs)))))
	   ((eq (bit (pos (nth 4 (pieces (player1 gs)))) index) 1)
	    (pos (nth 4 (pieces (player1 gs)))))
	   ((eq (bit (pos (nth 5 (pieces (player1 gs)))) index) 1)
	    (pos (nth 5 (pieces (player1 gs)))))))

	((eq (bit (player_pieces (player2 gs)) index) 1)
	 (cond 
	   ((eq (bit (pos (nth 0 (pieces (player2 gs)))) index) 1)
	    (pos (nth 0 (pieces (player2 gs)))))
	   ((eq (bit (pos (nth 1 (pieces (player2 gs)))) index) 1)
	    (pos (nth 1 (pieces (player2 gs)))))
	   ((eq (bit (pos (nth 2 (pieces (player2 gs)))) index) 1)
	    (pos (nth 2 (pieces (player2 gs)))))
	   ((eq (bit (pos (nth 3 (pieces (player2 gs)))) index) 1)
	    (pos (nth 3 (pieces (player2 gs)))))
	   ((eq (bit (pos (nth 4 (pieces (player2 gs)))) index) 1)
	    (pos (nth 4 (pieces (player2 gs)))))
	   ((eq (bit (pos (nth 5 (pieces (player2 gs)))) index) 1)
	    (pos (nth 5 (pieces (player2 gs)))))))
	(t  nil)))


(defmethod get-piece-at-index ((index integer) (gs game_state))
  "Accept an index, finds the piece at that index, and returns the player and piece reference number at that index."

  (update-all-pieces gs)
  (cond ((eq (bit (player_pieces (player1 gs)) index) 1)
      
	 (cond 
	   ((eq (bit (pos (nth 0 (pieces (player1 gs)))) index) 1)
	    (nth 0 (pieces (player1 gs))))
	   ((eq (bit (pos (nth 1 (pieces (player1 gs)))) index) 1)
	    (nth 1 (pieces (player1 gs))))
	   ((eq (bit (pos (nth 2 (pieces (player1 gs)))) index) 1)
	    (nth 2 (pieces (player1 gs))))
	   ((eq (bit (pos (nth 3 (pieces (player1 gs)))) index) 1)
	    (nth 3 (pieces (player1 gs))))
	   ((eq (bit (pos (nth 4 (pieces (player1 gs)))) index) 1)
	    (nth 4 (pieces (player1 gs))))
	   ((eq (bit (pos (nth 5 (pieces (player1 gs)))) index) 1)
	    (nth 5 (pieces (player1 gs))))))

	((eq (bit (player_pieces (player2 gs)) index) 1)
	 (cond 
	   ((eq (bit (pos (nth 0 (pieces (player2 gs)))) index) 1)
	    (nth 0 (pieces (player2 gs))))
	   ((eq (bit (pos (nth 1 (pieces (player2 gs)))) index) 1)
	    (nth 1 (pieces (player2 gs))))
	   ((eq (bit (pos (nth 2 (pieces (player2 gs)))) index) 1)
	    (nth 2 (pieces (player2 gs))))
	   ((eq (bit (pos (nth 3 (pieces (player2 gs)))) index) 1)
	    (nth 3 (pieces (player2 gs))))
	   ((eq (bit (pos (nth 4 (pieces (player2 gs)))) index) 1)
	    (nth 4 (pieces (player2 gs))))
	   ((eq (bit (pos (nth 5 (pieces (player2 gs)))) index) 1)
	    (nth 5 (pieces (player2 gs))))))
	(t nil)))




(defun init-game ()
  "Initilizes everything needed for a new game."

  (defvar current_game_state nil)

  (set 'current_game_state (make-instance 'game_state))

  (setf (turn_count current_game_state) 0)
  
  (setf (all_pieces current_game_state) (blank-bb))

  (setf (player1 current_game_state) (new-player1))
  (setf (player2 current_game_state) (new-player2))


  (setf (state_rep current_game_state) (make-array 64 :element-type 'string :initial-element ".."))
  (update-all-pieces current_game_state))




(defun new-game ()
  "Will begin a new game starting with player1's turn (White) and continue 
back and forth between players checking after each move to see if the game 
has been won, at which time it will print the game is over."


  (terprix20)
  (init-game)
  (format t "Chisp: '' ~d'' " (new-game-comments))

  (terpri)
  (terpri)
  (format t "                 Turn count: ~d ~%" (turn_count current_game_state))

  (game-loop))



(defun game-loop ()
    (loop
	    (progn 
	      (terpri)

	      (if (eq (mod (turn_count current_game_state) 2) 0)
		  ;;then
		  (progn
		    (format t "                 Player 1 (white) move: ~%")
		    (format t "                 Move entry format: Wp start_pos end_pos ~%"))
		  ;;else
		  (progn
		    (format t "                 Player 2 (black) move: ~%")
		    (format t "                 Move entry formate: Bp start_pos end_pos ~%")))

	      (if (<= (turn_count current_game_state) 2) (format t "                 Type '/restart-game' to start a new game ~%"))
	      (if (<= (turn_count current_game_state) 2) (format t "                 Type '/help' to display more commands. ~%"))
	      (terpri)
	      (terpri)
	      
	      (print-game-state)
	      (terprix 10)

	      
	      (perform-user-move)
	      
	      (print-game-state)

	      (terprix 20)
	      
              (ai-move current_game_state)

	      (terpri);space      
	      
	      ;;(print-game-state current_game_state)
	      (terpri)
	      (terpri)

	      (terprix20)


	      (format t "                    After ~S number of turns, ~%" (turn_count current_game_state))
	      (let ((percent 
		     (* 100
			(- 1
			   (/
			    (eval-player-score (player1 current_game_state) current_game_state)
			    (eval-player-score (player2 current_game_state) current_game_state))))))
		(if (>= percent 0)
		    (format t "                    Chisp is ahead by ~d% ~%~%" (round (abs (* 100 percent))))
		    (format t "                    Player1 is ahead by ~d% ~%~%" (round (abs (* 100 percent)))))))))

  








(defun new-player1 ()
  "Return an instance of player1, with initial piece pos."
  (make-instance 'player
		 ;; Generate a list containing an object of each piece type, with starting positions for white and store in pieces slot.
		 :pieces (list (make-instance 'king
					      :name 'wK
					      :pos  (copy-seq #*0001000000000000000000000000000000000000000000000000000000000000)
					      :allegiance 1
					      :game_state current_game_state)
			       (make-instance 'queen
					      :name 'wQ
					      :pos  (copy-seq #*0000100000000000000000000000000000000000000000000000000000000000)
					      :allegiance 1
					      :game_state current_game_state)
			       (make-instance 'rook
					      :name 'wR
					      :pos  (copy-seq #*1000000100000000000000000000000000000000000000000000000000000000)
					      :move (blank-bb)
					      :allegiance 1
					      :game_state current_game_state)
			       (make-instance 'bishop
					      :name 'wB
					      :pos  (copy-seq #*0010010000000000000000000000000000000000000000000000000000000000)
					      :allegiance 1
					      :game_state current_game_state)
			       (make-instance 'knight
					      :name 'wN
					      :pos  (copy-seq #*0100001000000000000000000000000000000000000000000000000000000000)
					      :allegiance 1
					      :game_state current_game_state)
			       (make-instance 'pawn 
					      :name 'wP
					      :pos  (copy-seq #*0000000011111111000000000000000000000000000000000000000000000000)
					      :move (copy-seq #*0000000000000000000000000000000000000000000000000000000000000000)
					      :allegiance 1
					      :game_state current_game_state))))



(defun new-player2 ()
  "Return an instance of player2, with initial piece pos."

  (make-instance 'player
		 ;; Generate a list containing an object of each piece type, with starting positions for black and store in pieces slot.
		 :pieces (list (make-instance 'king
					      :name 'bK
					      :pos  (copy-seq #*0000000000000000000000000000000000000000000000000000000000010000)
					      :allegiance 2
					      :game_state current_game_state)
			       (make-instance 'queen
					      :name 'bQ
					      :pos  (copy-seq #*0000000000000000000000000000000000000000000000000000000000001000)
					      :allegiance 2
					      :game_state current_game_state)
			       (make-instance 'rook
					      :name 'bR
					      :pos  (copy-seq #*0000000000000000000000000000000000000000000000000000000010000001)
					      :allegiance 2
					      :game_state current_game_state)
			       (make-instance 'bishop
					      :name 'bB
					      :pos  (copy-seq #*0000000000000000000000000000000000000000000000000000000000100100)
					      :allegiance 2
					      :game_state current_game_state)
			       (make-instance 'knight
					      :name 'bN
					      :pos  (copy-seq #*0000000000000000000000000000000000000000000000000000000001000010)
					      :allegiance 2
					      :game_state current_game_state)
			       (make-instance 'pawn 
					      :name 'bP
					      :pos  (copy-seq #*0000000000000000000000000000000000000000000000001111111100000000)
					      :move (copy-seq #*0000000000000000000000000000000000000000000000000000000000000000)
					      :allegiance 2
					      :game_state current_game_state))))


#|-----------------------------
    Auxillary/Debug Functions
------------------------------|#

(defun reload ()
  "Reloads the game.lisp file and thus recompiles and reloads all the other files."
  (load "./game.lisp"))

  

