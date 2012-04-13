#|***************************************************

                   Player Class
Description: Defines the Player class and its methods.


*****************************************************|#


(defclass player ()
  ((pieces               :accessor pieces         :initarg :pieces)
   (bb_player-pieces     :accessor player_pieces  :initarg :player_pieces :initform (blank-bb))
   (player_moves         :accessor player_moves   :initarg :player_moves  :initform nil)
   (player_threatening   :accessor player_threatening :initarg :player_threatening :initform (blank-bb))))




#|-------------------------------------------------------
                Method Definitions
---------------------------------------------------------|#


(defmethod update-player-pieces ((p player))
  "Updates the bb_all-player-pieces bit-vector to represent 
which spaces on the chess board contain a piece of a given player."

  (clear-bb (player_pieces p))

  (let ((temp (blank-bb)))
    (clear-bb temp)
    
  ;; For each bit that equals 1 in the pos bit-vector for all pieces, set the 
   ; corresponding bit to 1 in the bb_all-player-pieces bit-vector. |#
    (loop for i from 0 to 5 do
	 (loop for j from 0 to 63 do
	      (if (eql (bit (pos (nth i (pieces p))) j) 1)
		  (setf (bit temp j) 1))))

    (setf (player_pieces p) (bit-ior (player_pieces p) temp))))




(defmethod update-player-threatening (( p player))

  (clear-bb (player_threatening p))
  (loop for i from 0 to 5 do 
       (update-threatening (nth i (pieces p)))
       (setf (player_threatening p) (bit-ior (player_threatening p) (threatening (nth i (pieces p)))))))






(defmethod update-player-moves ((p player))
  "Update the player_moves slot with a list of all the 
availible moves for each piece of player."

  (let ((temp-lst '()))
    
    (loop for i from 0 to 5 do 
	 
	 (loop for j from 0 to 63 do

	    (if (eql (bit (pos (nth i (pieces p))) j) 1) ;; if (index=1 of piece)
		;;then
		  (progn
		    
		    (let ((temp-piece (make-instance
				       (cond
					 ((eq i 0) 'king)
					 ((eq i 1) 'queen)
					 ((eq i 2) 'rook)
					 ((eq i 3) 'bishop)
					 ((eq i 4) 'knight)
					 ((eq i 5) 'pawn)) :pos            (blank-bb)
					                   :move           (blank-bb)
					                   :allegiance     (allegiance (nth 0 (pieces p)))
					                   :avail_attack   (blank-bb)
							   :game_state     (game_state (nth 0 (pieces p)))
					                   :threatening    (blank-bb))))
		      
		      ;; Temp position board with only piece on j on it
		      (setf (bit (pos temp-piece) j) 1)	
		      
		      (if (equal (name temp-piece) 'p)
			  ;;then
			  (update-threatening temp-piece))
								    
		      
		      (update-avail-move temp-piece)

		      (update-avail-move (nth i (pieces p)))
		      
		      (setf (avail_move temp-piece)
			      (bit-and (avail_move (nth i (pieces p))) (avail_move temp-piece))) 
		      

		      (loop for k from 0 to 63 do

			   (if (eql (bit (avail_move temp-piece) k) 1) ;; if (index=1 of piece)
			      ;;then 
			       (setf temp-lst (append  (list (list (name temp-piece) j k)) temp-lst  ))))

		      ;; Setf player_moves to list of all pieces moves, temp-lst
		      (setf (player_moves p) temp-lst))))))))




(defun getNext-player-move ( player )
  "Accepts a player and returns the next move in slot
 player_moves, then removes that move from the list."
 
  (let ((temp-move nil))
    (setf temp-move (car (player_moves player)))
    (setf (player_moves player) (cdr (player_moves player )))
   ;; return temp-move.
    temp-move))




(defun hasNext-player-move (player)
  "Accepts a player and returns the next move in slot player_moves, then removes that move from the list."
  (not (null (player_moves player))))
		      




(defmethod count-pieces ((p player))
  "Returns a list of integers where each element represents the quantity of
 pieces for the type that corresponds with the elements index. (see pieces 
slot in player class)"

  (let ((pieces_count '()))
    (loop for i from 0 to 5 do	 
  
	 (setf pieces_count (append pieces_count (list (reduce #'+ (pos (nth i (pieces p))))))))

    pieces_count))





(defmethod avail-attack ((p player))
  "Returns a 64-bit bit-vector that represents the availible spaces of attack for a player"
  
  (let ((avail-attack (blank-bb)))

    (loop for i from 0 to 5 do

	 (update-avail-attack (nth i (pieces p)))
	 (setf avail-attack (bit-ior avail-attack (avail_attack (nth i (pieces p))))))
 
    avail-attack))



(defmethod copy-pieces ((p player))
  "Returns a newly copied list of a given players pieces"
  
  (let ((temp_pieces))

    (loop for i from 0 to 5 do
	 (setf temp_pieces (append temp_pieces (list (copy-piece (nth i (pieces p)))))))

    temp_pieces))




(defmethod copy-player ((p player))
  (let ((temp_player (make-instance 'player
				    :pieces (copy-pieces p)
				    :player_pieces (copy-seq (player_pieces p))
				    :player_moves (player_moves p)
				    :player_threatening (copy-seq (player_threatening p)))))

    temp_player))



(defmethod set-game-state ((p player) &optional (gs current_game_state))
  "Sets a given game state for all the players pieces, if no game state is given current_game_state is used."

  (loop for i from 0 to 5 do
       (set-game-state (nth i (pieces p)) gs)))




(defmethod checkmate ((p player))
  "Returns t or nil dependeng on whether or not the state is checkmate for the given player."

  (let ((temp_gs)
	(player))

    ;; Set correct player
    (if (eql (allegiance (car p)) 1)
	(setf player (player1 temp_gs))
	(setf player (player2 temp_gs)))


    ;; For all piece types
    (loop for i from 0 to 5 do

	 ;; For each piece
	 (loop for j from 0 to 63 do
	      (if (eql (bit (pos (nth i (pieces player))) j) 1)

		  ;; Get avail moves
		  (loop for k from 0 to 63 do
		       (if (eql (bit (avail_move (nth i (pieces player))) k) 1)
			   
			   ;; Move piece on temp_gs
			   (if (not (move-piece (nth i (pieces player)) j k))
			       nil))))))))
	      
	 
