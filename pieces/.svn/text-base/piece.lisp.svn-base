#|**************************************************

                  Piece Class
        Description: Defines the Piece Class

****************************************************|#



#|^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Slots: name ~ 2 char string, represents the owners color and
              piece type (eg. wP) for white pawn.

       bb_position ~ 64-bit bit-vector, represents the position
              of the pieces.

       bb_move ~ 64-bit bit-vector, represents where the piece(s)
              can move.

       bb_avail_attack ~ 64-bit bit-vector, represens where the
              piece(s) is 'able' to attack.

       bb_threatening ~ 64-bit bit-vector, represents where the
              oponent has pieces that are under threat from this
              piece(s).

       allegiance ~ integer, represents the owner (1 for white,
              2 for black).

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^|#

(defclass piece ()
  ((name            :accessor name         :initarg :name)
   (bb_position     :accessor pos          :initarg :pos            :initform (copy-seq (blank-bb)))
   (bb_move         :accessor avail_move   :initarg :move           :initform (copy-seq (blank-bb)))
   (bb_avail_attack :accessor avail_attack :initarg :avail_attack   :initform (copy-seq (blank-bb)))
   (bb_threatening  :accessor threatening  :initarg :threatening    :initform (copy-seq (blank-bb)))
   (allegiance      :accessor allegiance   :initarg :allegiance     :initform -1)
   (game_state      :accessor game_state   :initarg :game_state     :initform current_game_state)))





#|------------------------
          Methods        
--------------------------|#

;==;Update-avail-move()
;;       Desc: Updates the avail-move bit-vector for a given piece
;;             to represent all current availible moves for piece.
;;       Overloaded by: King, Queen, Rook, Bishop, knight & Pawn

(defmethod update-avail-move ((piece piece)))



;==;Update-avail-attack()
;;       Desc: Update avail_attack slot for a piece pp.
;;       Note: Avail_attack is the some as Avail_move for
;;             all pieces but the pawn.
;;       Overloaded by: Pawn

(defmethod update-avail-attack ((pp piece))

  (update-avail-move pp)
  (setf (avail_attack pp) (copy-seq (avail_move pp))))



;==;Update-threatening()
;;       Desc: Update threatening slot for a piece pp, to
;;             represent all opposing pieces a piece threatens.
;;       Note: Not the same for the pawn
;;       Overloaded by: Pawn

(defmethod update-threatening ((pp piece))

  (update-avail-attack pp)

  (if (eq (allegiance pp) 1)
      ;;then
      (setf (threatening pp) (bit-and (avail_attack pp) (player_pieces (player1 (game_state pp)))))
      ;;else
      (setf (threatening pp) (bit-and (avail_attack pp) (player_pieces (player2 (game_state pp)))))))


;==;Copy-piece()
;;        Desc: Returns a copy of a given piece

(defmethod copy-piece ((p piece))
  (let ((temp (make-instance (type-of p)
			     :name (name p)
			     :pos (copy-seq (pos p))
			     :move (copy-seq (avail_move p))
			     :threatening (threatening p)
			     :allegiance (allegiance p))))
    temp))
  



(defgeneric move-piece (piece integer integer  &optional game_state))

(defmethod move-piece ((pp piece) (start_index integer) (end_index integer) &optional (gs (game_state pp)))
  "Updates the pos bit-vetor for a given piece at a given index to a new position at a given index if it is a valid location."

  ;;update pieces
  (update-player-pieces (player1 gs))
  (update-player-pieces (player2 gs))
  
  ;; Check if piece is at start_index
  (if (eql (bit (pos pp) start_index) 0)
      (progn
	(terpri)
	(print '*****ERROR*****)
	(terpri)
	(format t "No ~s at index ~D"  (name pp) start_index)
	(terpri)
       ;;return nil to report error
	(return-from move-piece nil))

      ;; Create copy of the piece then fill bb_pos with 0 and set the bit at start_index to 1
      (progn
	(let ((temp (copy-piece pp)))
	  (setf (pos temp) (copy-seq (bit-and (pos pp) (fill-bb (list start_index)))))

	  (update-avail-move temp)
	  (update-avail-attack temp)
	  
	  ;; Check if end_index is vaild for piece
	  (if (eql (bit (avail_move temp) end_index) 0)
	      (progn
		(terpri)
		(print '*****ERROR*****)
		(terpri)
		(format t "Cannot move ~s at ~D to ~D"  (name pp) start_index end_index)
		(terpri)
		  
		;;return nil to report error
		(return-from move-piece nil))))
	      


	      
	      (progn

		;; Prune avail_move to prevent illegal moves that put king in check.
		(let ((temp_gs (copy-game-state (game_state pp)))
		      (player))

		  ;; Set correct player
		  (if (eql (allegiance pp) 1)
		      (setf player (player1 temp_gs))
		      (setf player (player2 temp_gs)))
		  
		  
		  ;; Move for temp
		  (update-all-pieces temp_gs)

		  (remove-piece start_index temp_gs)
		  (remove-piece end_index temp_gs)

		  (setf (pos (nth (piecename-to-index (name pp)) (pieces player)))
			(copy-seq 
			 (bit-ior 
			  (pos (nth (piecename-to-index (name pp)) (pieces player)))
			  (fill-bb (list end_index)))))
		  
		  (update-all-pieces temp_gs)

		  (if (king-check (car (pieces player)) temp_gs)
		      (progn
			(if (eq gs current_game_state)
			    (format t "Error: Cannot put self in check"))
			(return-from move-piece nil))))



		;; Move piece
		(if (eq (bit 
			  (player_pieces (if (= (mod (turn_count gs) 2) 0)
					     ;;Inner if then
					     (player2 gs)
					     ;;Inner if else
					     (player1 gs)))
			                                   end_index) 1)
		    ;;then
		    (remove-piece end_index gs))

		(setf (bit (pos pp) start_index) 0)
		(setf (bit (pos pp) end_index) 1)


	      ;;increase the count for numbers of turns.
	      (setf (turn_count gs) (+ (turn_count gs) 1))
		


	      ;;return true to report no error
	      t))))





(defmethod remove-piece ((index integer) (gs game_state))
  "Removes a piece at a given index"

  (let ((temp_pos (find-piece-at-index index gs)))
	
	(if (not (null temp_pos))
	    (setf (bit temp_pos index)  0))))





;==;Add-piece()
;;         Desc: Adds a piece to the game
;;         Arguments: allegiance, type, index
(defun add-piece ())




(defmethod copy-piece ((p piece))
  "Returns a copy of a given piece."

  (let ((temp_piece (make-instance (type-of p)
				   :name (name p)
				   :pos (copy-seq (pos p))
				   :move (copy-seq (avail_move p))
				   :avail_attack (copy-seq (avail_attack p))
				   :threatening (copy-seq (threatening p))
				   :allegiance (allegiance p)
				   :game_state (game_state p))))

    temp_piece))


(defmethod set-game-state ((p piece) &optional (gs current_game_state))
  "Sets the given game state for a given piece. If no game state is given current_game_state is used."

  (setf (game_state p) gs))



#|---------------------------------

    Perpendicular move generation.
   Used For: Rook & Queen move gen.

-----------------------------------|#

 (defmethod Perpendicular-move-generation ((pp piece))   
	    
	   (progn

	      (let ((temp-avail (blank-bb)))

	    ;;iterate through each index to find all pieces.
	      (loop for i from 0 to 63 do
		
		   (if (eql (bit (pos pp) i) 1)
		    
		       (progn
		    
			 (let ((temp-pos (blank-bb))    ;;temp BB's 
			       (temp-move (blank-bb))
			       (right-move (blank-bb))
			       (left-move (blank-bb))
			       (up-move (blank-bb))
			       (down-move (blank-bb)))

			   (clear-bb temp-pos)
			   (clear-bb temp-move)
			   (clear-bb right-move)
			   (clear-bb left-move)
			   (clear-bb up-move)
			   (clear-bb down-move)
			   
			 ;; BB with only the position'i' piece.
			   (setf (bit temp-pos i) 1) 

			   

		      ;;Generate moves above pp.
			   (setf temp-move (bit-and (up-from-s temp-pos) (all_pieces (game_state pp))))


			   (loop for j from i to 63 do
				
				(if (eql (bit temp-move j) 1)
				    (progn
				      (setf up-move (up-from-s temp-move))
				      (return))))

			   ;;exclusive or BBs
			   (setf up-move (bit-xor (up-from-s temp-pos) up-move))

			   ;;see if piece you may have ran into is your opponents
			   (setf up-move (bit-andc2 up-move (player_pieces 
							        (if
								 (eq (allegiance pp) 1)
								 ;;then
								    (player1 (game_state pp))
								 ;;else
								    (player2 (game_state pp))))))
								           

			   (clear-bb temp-move)


		       ;;Generate moves down from pp.
			   (setf temp-move (bit-and (down-from-s temp-pos) (all_pieces (game_state pp))))

			   (loop for j from i downto 0 do
				
				(if (eql (bit temp-move j) 1)
				    (progn
				      (setf down-move (down-from-s temp-move)) 
				      (return))))

			   (setf down-move (bit-xor down-move (down-from-s temp-pos)))
			   (setf down-move (bit-andc2 down-move (player_pieces 
								 (if
								  (eq (allegiance pp) 1)
								 ;;then
								     (player1 (game_state pp))
								 ;;else
								     (player2 (game_state pp))))))
			   
			   (clear-bb temp-move)
			   


			   ;;Generate moves right from pp.
			   (setf temp-move (bit-and (right-from-s temp-pos) (all_pieces (game_state pp))))

			   (loop for j from i to 63 do
				
				(if (eql (bit temp-move j) 1)
				    (progn
				      (setf right-move (right-from-s temp-move)) 
				      (return))))

			   (setf right-move (bit-xor right-move (right-from-s temp-pos)))
			   (setf right-move (bit-andc2 right-move (player_pieces 
								   (if
								    (eq (allegiance pp) 1)
								 ;;then
								     (player1 (game_state pp))
								 ;;else
								     (player2 (game_state pp))))))

			   (clear-bb temp-move)

			  

			   ;;Generate moves left from pp.
			   (setf temp-move (bit-and (left-from-s temp-pos) (all_pieces (game_state pp))))

			   (loop for j from i downto 0 do
				
				(if (eql (bit temp-move j) 1)
				    (progn
				      (setf left-move (left-from-s temp-move)) 
				      (return))))

			   (setf left-move (bit-xor left-move (left-from-s temp-pos)))

			   (setf left-move (bit-andc2 left-move (player_pieces 
								 (if
								    (eq (allegiance pp) 1)
								 ;;then
								     (player1 (game_state pp))
								 ;;else
								     (player2 (game_state pp))))))

			   (clear-bb temp-move)


			   ;;Combine them all to get all available moves for the pp.		   
			  
			   (setf temp-avail (bit-ior temp-avail down-move))
			   (setf temp-avail (bit-ior temp-avail left-move))
			   (setf temp-avail (bit-ior temp-avail right-move))
			   (setf temp-avail (bit-ior temp-avail up-move))))))
	      
	      ;;return temp-avail moves.
	      (setf (avail_move pp) (copy-seq (bit-ior (avail_move pp) temp-avail)))
	      (clear-bb temp-avail))))
	

