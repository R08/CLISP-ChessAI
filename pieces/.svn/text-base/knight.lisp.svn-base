#|*******************************************
            Knight Piece Class

********************************************|#

(defclass knight (piece) 
	((name :initform 'n)) )



#|----------------------------------------

     Knight (np) move generation

Description: Accepts a knight piece pn and
             used the 'l' move generation table
             to update avial_move slot.

------------------------------------------|#


(defmethod update-avail-move ((pn knight))

  (clear-bb (avail_move pn))

 ;; update avail_move slot with move gen. bb with indexes your own pieces
 ;; own removed.
  (let ((temp-move (lshape-move-generation-index pn)))
  (setf (threatening pn) (copy-seq temp-move))
  (setf (avail_move pn) (copy-seq 
			      (Bit-andc2 temp-move (player_pieces 
						    (if (eq (allegiance pn) 1)
							;;then
							( player1 (game_state pn))
							;;else
							( player2 (game_state pn)))))))))
  

#|-------------------------------------------

    'L' shape move generation

Description:  list/table of avail moves for 
                the knight at a given index.

*-------------------------------------------|#

(defmethod lshape-move-generation-index ((pp piece))

  (let ((temp-move (copy-seq (blank-bb))))
    
    
    (clear-bb temp-move)

         (loop for i from 0 to 63 do
		
		   (if (eql (bit (pos pp) i) 1)
		       (progn
		    
			 (cond
			   
			   ((eq i 0)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(10 17))))))

			   ((eq i 1)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(16 18 11))))))

			   ((eq i 2)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(19 8 17 12))))))

			   ((eq i 3)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(9 13 18 20))))))
			   
			   ((eq i 4)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(10 19 21 14))))))
			   
			   ((eq i 5)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(11 20 22 15))))))
			   
			   ((eq i 6)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(12 21 23))))))
			   
			   ((eq i 7)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(13 22))))))
			   
			   ((eq i 8)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(25 18 2))))))
			   
			   ((eq i 9)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(24 26 19 3))))))
			   
			   ((eq i 10)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(0 16 25 27 20 4))))))
			   
			   ((eq i 11)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(1 17 26 28 21 5))))))
			   
			   ((eq i 12)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(2 18 27 29 22 6))))))
			   
			   ((eq i 13)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(3 19 28 30 23 7))))))
			   
			   ((eq i 14)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(4 20 29 31))))))
			   
			   ((eq i 15)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(5 21 30))))))
			   
			   ((eq i 16)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(1 10 26 33))))))
			   
			   ((eq i 17)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(0 2  11 27 34 32))))))
			   
			   ((eq i 18)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(1 3 12 28 35 33 24 8))))))
			   
			   ((eq i 19)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(2 4 13 29 36 34 25 9))))))
			   
			   ((eq i 20)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(3 5 14 30 37 35 26 10))))))
			   
			   ((eq i 21)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(4 6 15 31 38 36 27 11))))))
			   
			   ((eq i 22)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(5 7 39 37 28 12))))))
			   
			   ((eq i 23)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(6 13 29 38 ))))))
			   
			   ((eq i 24)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(9 18 34 41))))))
			   
			   ((eq i 25)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(8 10 19 35 42 40))))))
			   
			   ((eq i 26)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(9 11 20 36 43 41 32 16))))))
			   
			   ((eq i 27)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(10 12 21 37 44 42 33 17))))))
			   
			   ((eq i 28)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(11 13 22 38 45 43 34 18))))))
			   
			   ((eq i 29)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(12 14 23 39 46 44 35 19))))))
			   
			   ((eq i 30)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(13 15 47 45 36 20))))))
			   
			   ((eq i 31)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(14 21 37 46))))))
			   
			   ((eq i 32)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(17 26 42 49))))))
			   
			   ((eq i 33)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(16 18 27 43 50 48))))))
			   
			   ((eq i 34)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(24 17 19 28 44 51 49 40))))))
			   
			   ((eq i 35)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(25 18 20 29 45 52 50 41))))))
			   
			   ((eq i 36)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(26 19 21 30 46 53 51 42))))))
			   
			   ((eq i 37)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(27 20 22 31 47 54 52 43))))))
			   
			   ((eq i 38)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(23 21 28 44 53 55))))))
			   
			   ((eq i 39)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(22 29 45 54))))))
			   
			   ((eq i 40)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(25 34 50 57))))))
			   
			   ((eq i 41)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(24 26 35 51 58 56 ))))))
			   
			   ((eq i 42)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(32 48 25 27 36 52 59 57))))))
			   
			   ((eq i 43)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(33 49 26 28 37 53 60 58))))))
			   
			   ((eq i 44)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(34 50 27 29 38 54 61 59))))))
			   
			   ((eq i 45)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(35 51 28 30 39 55 62 60))))))
			   
			   ((eq i 46)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(31 29 36 52 61 63))))))
			   
			   ((eq i 47)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(30 37 53 62))))))
			   
			   ((eq i 48)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(33 42 58))))))
			   
			   ((eq i 49)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(32 34 43 59))))))
			   
			   ((eq i 50)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(33 35 44 60 40 56))))))
			   
			   ((eq i 51)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(34 36 45 61 41 57))))))
			   
			   ((eq i 52)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(35 37 46 62 42 58))))))
			   
			   ((eq i 53)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(36 38 47 63 43 59))))))
			   
			   ((eq i 54)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(37 39 44 60))))))
			   
			   ((eq i 55)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(38 45 61))))))
			   
			   ((eq i 56)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(41 50))))))
			   
			   ((eq i 57)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(40 42 51))))))
			   
			   ((eq i 58)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(41 43 52 48))))))
			 
			   ((eq i 59)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(42 44 53 49))))))
			 
			   ((eq i 60)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(43 45 54 50))))))
			   
			   ((eq i 61)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(44 46 55 51))))))
			 
			   ((eq i 62)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(45 47 52))))))
			 
			   ((eq i 63)
			    (setf temp-move (copy-seq (bit-ior temp-move (fill-bb '(53 46))))))))))
	 
	 temp-move))
  
  


  
  

