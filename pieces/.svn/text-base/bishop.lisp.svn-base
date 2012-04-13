#|********************************************

            Bishop Piece Class
           Parent Class: piece

*********************************************|#


(defclass bishop (piece) 
	 ((name :initform 'b )) )



#|---------------------------------

    Bishop move generation method

-----------------------------------|#

;==;Update-avail-move()
;;     Desc: updates the avail_move slote of the bishop

(defmethod update-avail-move ((pb bishop))

  (clear-bb (avail_move pb))

  (clear-bb (threatening pb))
  
  (diag-move-generation pb))
  
  




#|--------------------------------------------

        Diagonal move generation
    Used by: Bishop & Queen Move gen.

Decription: accepts a piece pp and generates
             move in up/lower, left and right
             diagonals of pp.  Eliminates spaces 
             held by same color pieces.

----------------------------------------------|#

	 (defmethod  diag-move-generation ((pp piece))  

	   (progn
	      
	      (let ((temp-avail (blank-bb)))

             ;;interate through each index to find all bishops
	      (loop for i from 0 to 63 do
		
		   (if (eql (bit (pos pp) i) 1)
		    
		       (progn
		    
			 (let ((temp-pos (blank-bb))    ;;temp BB's 
			       (temp-move (blank-bb))
			       (upright-move (blank-bb))
			       (upleft-move (blank-bb))
			       (downleft-move (blank-bb))
			       (downright-move (blank-bb)))

			   (clear-bb temp-pos)
			   (clear-bb temp-move)
			   (clear-bb upright-move)
			   (clear-bb upleft-move)
			   (clear-bb downleft-move)
			   (clear-bb downright-move)
			  

			   (setf (bit temp-pos i) 1) ;; BB with only the position 'i' bishop

			   

		      ;;Generate moves lower left of bishop.
			   (setf temp-move (bit-and (lowleft-diag-from-s temp-pos) (all_pieces (game_state pp))))


			   (loop for j from 0 to 63 do
				
				(if (eql (bit temp-move j) 1)
				    (progn
				      (setf downleft-move (lowleft-diag-from-s temp-move))
				      (return))))

			   ;;exclusive 'or' bbs
			   (setf downleft-move (bit-xor (lowleft-diag-from-s temp-pos) downleft-move))

			   ;;update threatening for pb
			   (setf (threatening pp) (bit-ior (threatening pp) downleft-move))

			   ;;logical 'and' bbs to see if peice you may have run into is yours
			   (setf downleft-move (bit-andc2 downleft-move (player_pieces 
							        (if
								    (eq (allegiance pp) 1)
								 ;;then
								     (player1 (game_state pp))
								 ;;else
								     (player2 (game_state pp))))))
								           

			   (clear-bb temp-move)


		       ;;Generate moves lower right of bishop.
			   (setf temp-move (bit-and (lowright-diag-from-s temp-pos) (all_pieces (game_state pp))))

			   (loop for j from 0 to 63 do
				
				(if (eql (bit temp-move j) 1)
				    (progn
				      (setf downright-move (lowright-diag-from-s temp-move)) 
				      (return))))

			   (setf downright-move (bit-xor downright-move (lowright-diag-from-s temp-pos)))

			   (setf (threatening pp) (bit-ior (threatening pp) downright-move))

			   (setf downright-move (bit-andc2 downright-move (player_pieces 
								 (if
								    (eq (allegiance pp) 1)
								 ;;then
								     (player1 (game_state pp))
								 ;;else
								     (player2 (game_state pp))))))
			   
			   (clear-bb temp-move)
			   


			   ;;Generate moves upper right of bishop.
			   (setf temp-move (bit-and (upright-diag-from-s temp-pos) (all_pieces (game_state pp))))

			   (loop for j from 0 to 63 do
				
				(if (eql (bit temp-move j) 1)
				    (progn
				      (setf upright-move (upright-diag-from-s temp-move)) 
				      (return))))

			   (setf upright-move (bit-xor upright-move (upright-diag-from-s temp-pos)))

			   (setf (threatening pp) (bit-ior (threatening pp) upright-move))

			   ;;logical 'and' bbs to see if peice you may have run into is yours
			   (setf downleft-move (bit-andc2 downleft-move (player_pieces 
							        (if
								    (eq (allegiance pp) 1)
								 ;;then
								     (player1 (game_state pp))
								 ;;else
								     (player2 (game_state pp))))))
								           

			   (clear-bb temp-move)


		       
			  			  

			   ;;Generate moves upper left of bishop
			   (setf temp-move (bit-and (upleft-diag-from-s temp-pos) (all_pieces (game_state pp))))

			   (loop for j from 0 to 63 do
				
				(if (eql (bit temp-move j) 1)
				    (progn
				      (setf upleft-move (upleft-diag-from-s temp-move)) 
				      (return))))

			   (setf upleft-move (bit-xor upleft-move (upleft-diag-from-s temp-pos)))

			   (setf (threatening pp) (bit-ior (threatening pp) upleft-move))

			   (setf upleft-move (bit-andc2 upleft-move (player_pieces 
								 (if
								    (eq (allegiance pp) 1)
								 ;;then
								     (player1 (game_state pp))
								 ;;else
								     (player2 (game_state pp))))))

			   (clear-bb temp-move)


			   ;;Combine them all to get all available moves for the pp.		   
			  
			   (setf temp-avail (bit-ior temp-avail downright-move))
			   (setf temp-avail (bit-ior temp-avail upleft-move))
			   (setf temp-avail (bit-ior temp-avail upright-move))
			   (setf temp-avail (bit-ior temp-avail downleft-move))))))

	      ;;return temp-avail moves.
	      (setf (avail_move pp) (copy-seq (bit-ior (avail_move pp) temp-avail)))
	      (clear-bb temp-avail))))
