#|****************************************

             King Piece Class
            Parent Class: piece

*****************************************|#



(defclass king (piece) 
	((name :initform 'k)) )




#|-----------------------

   King move generation

-------------------------|#


(defmethod update-avail-move ((pk king))

  (clear-bb (avail_move pk))
  (clear-bb (threatening pk))

  ;; Iterate through pos, to find king
  (loop for i from 0 to 63 do
       (if (eql (bit (pos pk) i) 1)

		 ;;; Check boundaries ;;;
	   (progn
	     ;; Down
	     (if (not (<= i 7))
		 (setf (bit (avail_move pk) (- i 8)) 1))
	     
	     ;; Up
	     (if (not (>= i 55))
		 (setf (bit (avail_move pk) (+ i 8)) 1))

	     ;; Right
	     (if (not (eql (mod i 8) 7))
		 (setf (bit (avail_move pk) (+ i 1)) 1))

	     ;; Left
	     (if (not (eql (mod i 8) 0))
		 (setf (bit (avail_move pk) (- i 1)) 1))

	     ;; Up left
	     (if (and (not (>= i 55)) (not (eql (mod i 8) 0)))
		 (setf (bit (avail_move pk) (+ i 7)) 1))
	     
	     ;; Up right
	     (if (and (not (>= i 55)) (not (eql (mod i 8) 7)))
		 (setf (bit (avail_move pk) (+ i 9)) 1))

	     ;; Down left
	     (if (and (not (<= i 7)) (not (eql (mod i 8) 0)))
		 (setf (bit (avail_move pk) (- i 9)) 1))

	     ;; Down right
	     (if (and (not (<= i 7)) (not (eql (mod i 8) 7)))
		 (setf (bit (avail_move pk) (- i 7)) 1))
	     (return))))
	   
  
  (setf (threatening pk) (copy-seq (avail_move pk)))

  ;; Prune avail_move to prevent collision with own pieces.
  (if (eq (allegiance pk) 1)
      ;; Player 1 (white)
	(setf (avail_move pk) (copy-seq (bit-andc2 (avail_move pk) (player_pieces (player1 (game_state pk))))))
      ;; Player 2 (black)
	(setf (avail_move pk) (copy-seq (bit-andc2 (avail_move pk) (player_pieces (player2 (game_state pk))))))))








(defmethod king-check ((king piece) &optional (gs current_game_state))
  "Returns true or nil depending on if a king is in check"
  
  (let ((opponent_avail_attack))

    (if (eql (allegiance king) 1)
	(setf opponent_avail_attack (copy-seq (avail-attack (player2 gs))))
	(setf opponent_avail_attack (copy-seq (avail-attack (player1 gs)))))

    (if (eql (reduce #'+ (bit-and (pos king) opponent_avail_attack)) 0)
	nil
	t)))

