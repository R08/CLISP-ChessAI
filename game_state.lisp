#|*********************************************************

                     Game_state

*********************************************************|#


(defclass game_state ()
  ;;; Represents whether or not if each space on the board is ocupied by a piece.
  ((all_pieces :accessor all_pieces :initarg :all_pieces :initform (blank-bb))
   
      
   ;;   Current Game-state: an array type, that holds all the pieces for each 
   ;;   player by name (ex. Wp Bk...) according to thier respective positions
   ;;   on the board. 
   (state_rep :accessor state_rep :initarg :game_state :initform (make-array 64 :element-type 'string :initial-element "  "))

   ;; Players
   (player1 :accessor player1 :initarg :player1 :initform (new-player1))
   (player2 :accessor player2 :initarg :player2 :initform (new-player2))
   (turn_count :accessor turn_count :initarg :turn_count :initform 0)))






#|---------------------------

     METHOD DEFINITIONS

----------------------------|#






#|---------------------------
    Modification Methods
----------------------------|#

                        
(defmethod update-all-pieces ((gs game_state))
  "Updates all_pieces bitboard to represent all currently held positions."

  (clear-bb (all_pieces gs))

  (update-player-pieces (player1 gs))
  (update-player-pieces (player2 gs))

  (loop for i from 0 to 63 do
       (setf (all_pieces gs) (copy-seq (bit-ior (player_pieces (player1 gs))  (player_pieces (player2 gs)))))))










(defmethod update-state-rep ((gs game_state)) ;;;;; NAME CHANGE ;;;;;;;
  " For both players iterate through each piece's pos bb and for each bit that == 1 set 
the corresponding index of game-state to the pieces name."

  (update-game-state gs))
(defmethod update-game-state ((gs game_state))

  (update-all-pieces gs)

  ;; Player1 (white)
  (loop for i from 0 to 5 do
       (loop for j from 0 to 63 do
	    (if (eq (bit (pos (nth i (pieces (player1 gs)))) j) 1)
		(setf (aref (state_rep gs) j) (name (nth i (pieces (player1 gs))))))))

  ;; Player2 (black)
  (loop for i from 0 to 5 do
       (loop for j from 0 to 63 do
	    (if (eq (bit (pos (nth i (pieces (player2 gs)))) j) 1)
		(setf (aref (state_rep gs) j) (name (nth i (pieces (player2 gs)))))))))





(defmethod clear-state-rep ((gs game_state)) ;;;;; NAME CHANGE ;;;;;;;
  "Clears the game state representation."
  (clear-game-state gs))
(defmethod clear-game-state ((gs game_state))
  (loop for i from 0 to 63 do
       (setf (aref (state_rep gs) i) "  "))

  ;; Add black squares
  (let ((pattern '("  " "::" "  " "::" "  " "::" "  " "::")))

    (loop for i from 0 to 7 do
	 (loop for j from 0 to 7 do
	      (setf (aref (state_rep gs) (+ j (* i 8))) (copy-seq (nth j pattern))))
	 (setf pattern (reverse pattern)))))





(defmethod set-game-state ((gs game_state) &optional (parent_gs gs))
  "For a given game state set the game state slot for all pieces to a second given game_state.
If no second game state is given use first given game_state."

  (set-game-state (player1 gs) parent_gs)
  (set-game-state (player2 gs) parent_gs)
  t)


(defmethod copy-game-state ((copy-state game_state))
  "Returns a copy of a given game state."

  (let ((temp-state (make-instance 'game_state
				   :all_pieces (copy-seq (all_pieces copy-state))
				   :game_state (copy-seq (state_rep copy-state))
				   :player1 (copy-player (player1 copy-state))
				   :player2 (copy-player (player2 copy-state))
				   :turn_count (turn_count copy-state))))

    (set-game-state temp-state)
    temp-state))







#|------------------------------
      Computation Methods
------------------------------|#

(defmethod checkmate ((gs game_state))
  "Returns t or nil dependeng on whether or not the state is checkmate. If an optional
player is given the method returns t or nil based on if the given player is checkmated (ie. lost the game)"
  
  (or (checkmate (player1 gs)) (checkmate (player2 gs))))



(defmethod hash-game-state ((gs game_state))
  "Returns current game state as a string"

  (clear-game-state gs)
  (update-game-state gs)

  (let ((curState (state_rep gs)))
    (let ((x ""))
      (loop for i from 0 to 63 do
	   (setf x (concatenate 'string x (string (aref curState i)))))
      x)))


(defmethod make-book-name (( gs game_state ))

    (update-game-state gs)
    
    (let ((temp-p    nil)
	  (temp-lst "")
	  (count 0))

      (loop for i from 0 to 7 do
	   (loop for j from 0 to 7 do

		(setf temp-p (get-piece-at-index (+ j (* i 8)) gs))
		(if (eq temp-p nil)
		    ;;then
		    (setf count (+ count 1))
		    ;;else
		    (if (eq count 0)
			  ;;then
			  (setf temp-lst 
				(concatenate 'string temp-lst (cond ((typep temp-p 'pawn)
								     (if (eq (allegiance temp-p) 1) 
									 "P"
									 "p"))
								    ((typep temp-p 'knight)
								     (if (eq (allegiance temp-p) 1) 
									 "N"
									 "n"))
								    ((typep temp-p 'bishop)
								     (if (eq (allegiance temp-p) 1) 
									 "B"
									 "b"))
								    ((typep temp-p 'rook)
								     (if (eq (allegiance temp-p) 1) 
									 "R"
									 "r"))
								    ((typep temp-p 'queen)
								     (if (eq (allegiance temp-p) 1) 
									 "Q"
									 "q"))
								    ((typep temp-p 'king)
								     (if (eq (allegiance temp-p) 1) 
									 "K"
									 "k")))))
			  ;;else
			  (progn
			    (setf temp-lst 
				(concatenate 'string temp-lst (concatenate 'string (write-to-string count) 
									   (cond ((typep temp-p 'pawn)
										  (if (eq (allegiance temp-p) 1) 
										      "P"
										      "p"))
										 ((typep temp-p 'knight)
										  (if (eq (allegiance temp-p) 1) 
										      "N"
										      "n"))
										 ((typep temp-p 'bishop)
										  (if (eq (allegiance temp-p) 1) 
										      "B"
										      "b"))
										 ((typep temp-p 'rook)
										  (if (eq (allegiance temp-p) 1) 
										      "R"
										      "r"))
										 ((typep temp-p 'queen)
										  (if (eq (allegiance temp-p) 1) 
										      "Q"
										      "q"))
										 ((typep temp-p 'king)
										  (if (eq (allegiance temp-p) 1) 
										      "K"
										      "k"))))))
			    (setf count 0)))))
	   (if (eq count 8)
	       (progn
		 (setf temp-lst (concatenate 'string temp-lst "8/"))
		 (setf count 0))
	       (progn
		 (setf temp-lst (concatenate 'string temp-lst (if (not (eq count 0))
								(write-to-string count)) "/"))
		 (setf count 0))))
      (reverse temp-lst)))
	   
				
			  
		



#|------------------------------
         Print Methods
------------------------------|#

(defmethod print-game-state (&optional (gs current_game_state))
  "Print current game state as a board"

  (clear-state-rep gs)
  (update-state-rep gs)

  (terprix 3)

  (format t "~%             A    B    C    D    E    F    G    H")
  (format t "~%          -----------------------------------------")

  (let ((curState (reverse (state_rep gs))))
    (loop for i from 0 to 7 do
	 (let ((x))
	   (loop for j from 0 to 7 do
		(setf x (cons (aref curState (+ j (* i 8))) x)))
	   (format t "~%        ~D | ~{~A | ~} ~D" (- 8 i) x (- 8 i)))
	 (format t "~%          -----------------------------------------")))

  
  (format t "~%             A    B    C    D    E    F    G    H")

  (terprix 3))











#|------------------------------
        State Evaluation
------------------------------|#


(defmethod eval-state-score ((gs game_state))
  "Returns a score based on a given game state object."

  (let ((player1-score 0)
	(player2-score 0))
    
    (setf player1-score (eval-player-score (player1 gs) gs))
    (setf player2-score (eval-player-score (player2 gs) gs))

    (- player1-score player2-score))) 


(defun eval-player-score ( player gs )

  (let ((player-score 0)
	(p 0)	(n 0)	(b 0)	(r 0)	(q 0)	(k 0))
    (progn
      ;; Pawns
      (loop for i from 0 to 63 do 	   
	   (if (= (bit (pos (nth 5 (pieces player))) i) 1)
	       ;;then
	       (progn
		   (setf player-score (+ player-score 
					 (if (eq (allegiance (nth 0 (pieces player))) 1)
					     (nth i PAWN_POSITION_SCORE1)
					     (nth i PAWN_POSITION_SCORE2))))
		  ; (if (defended-by-pawn player i)
		  ;     (setf player-score (+ player-score 15)))
		  ; (if (eq (bit (bit-and (fill-bb (list i)) (
		   (setf p (+ p 1)))))
      ;;Knights
      (loop for i from 0 to 63 do 	   
	   (if (= (bit (pos (nth 4 (pieces player))) i) 1)
	       ;;then
	       (progn
		   (setf player-score (+ player-score 
					 (if (eq (allegiance (nth 0 (pieces player))) 1)
					     (nth i KNIGHT_POSITION_SCORE1)
					     (nth i KNIGHT_POSITION_SCORE2))))
		   (setf n (+ n 1)))))
      ;;Bishops
      (loop for i from 0 to 63 do 	   
	   (if (= (bit (pos (nth 3 (pieces player))) i) 1)
	       ;;then
	       (progn
		   (setf player-score (+ player-score 
					 (if (eq (allegiance (nth 0 (pieces player))) 1)
					     (nth i BISHOP_POSITION_SCORE1)
					     (nth i BISHOP_POSITION_SCORE2))))
		   (setf b (+ b 1)))))
      ;; Rooks
      (loop for i from 0 to 63 do 	   
	   (if (= (bit (pos (nth 2 (pieces player))) i) 1)
	       ;;then
	       (progn
		 (setf r (+ r 1)))))
      ;;Queen
      (loop for i from 0 to 63 do 	   
	   (if (= (bit (pos (nth 1 (pieces player))) i) 1)
	       ;;then
	       (progn
		 (setf q (+ q 1))
		 (return))))
      ;;King of player
      (loop for i from 0 to 63 do 
	   
	   (if (= (bit (pos (nth 0 (pieces player))) i) 1)
	       ;;if endgame
	       (if (< (reduce #'+ (all_pieces gs)) 8)
		   ;;then
		   (progn
		       (setf player-score (+ player-score
					     (if (eq (allegiance (nth 0 (pieces player))) 1)
					     (nth i KING_END_POSITION_SCORE1)
					     (nth i KING_END_POSITION_SCORE2))))
		       (setf k (+ k 1))
		       (return))
		   ;;else
		   (progn
		       (setf player-score (+ player-score 
					     (if (eq (allegiance (nth 0 (pieces player))) 1)
					     (nth i KING_POSITION_SCORE1)
					     (nth i KING_POSITION_SCORE2))))
		       (setf k (+ k 1))
		       (return)))))

      (setf player-score (+ player-score (apply #'+ (mapcar #'* (list k q r b n p) '(35000 900 500 380 300 100))))))
      
      player-score))


  
