#|****************************************************
                    
                       Pawn Class
                   Parent Class: piece

******************************************************|#


(defclass pawn (piece) 
     (( name :initform 'p)))


#|---------------------------------------

                 Functions

---------------------------------------|#


#|--------------------------
    Pawn move Generation
      (non-attacking)
---------------------------|#

;;;Description: Accepts a pawn opbject and sets its
;;              bb_move slot to the possible moves.

(defmethod update-avail-move ((pp pawn)) 
  (cond 

    ;; Player1 (white)
    ((eq (allegiance pp) 1)
     
     (clear-bb (avail_move pp))

     (let ((temp_move (blank-bb)))

       ;; Set temp to valid move pattern
       (loop for i from 0 to 63 do
	    (if (eql (bit (pos pp) i) 1)
		;; If in initial position, move 1 or 2 spaces, else 1
		(if (and (<= i 15) (>= i 8))
		    
		    (progn
		      (setf (bit temp_move (+ i 8)) 1)
		      (setf (bit temp_move (+ i 16)) 1))
		    ;; else: Check boundary and move up 1
		    (if (< i 56)
			(setf (bit temp_move (+ i 8)) 1)))))
       
       ;; Prune temp of moves that cause a collision
       (loop for i from 0 to 63 do
	    (if (eql (bit (bit-and temp_move (all_pieces (game_state pp))) i) 1)
		(progn
		  (setf (bit temp_move i) 0)
		  ;; Prevent jumping
		  (if (and (< i 56) (eql (bit temp_move (+ i 8)) 1))
		      (setf (bit temp_move (+ i 8)) 0)))))
       (update-threatening pp)

       (setf (avail_move pp) (bit-ior temp_move (threatening pp)))))
    



    ;; Player2 (black)
    ((eq (allegiance pp) 2)
     
     (clear-bb (avail_move pp))

     (let ((temp_move (blank-bb)))
       (clear-bb temp_move)

       ;; Set temp to valid move pattern
       (loop for i from 0 to 63 do
	    (if (eql (bit (pos pp) i) 1)
		;; If in initial position, move 1 or 2 spaces, else 1
		(if (and (>= i 47) (<= i 55))
		    
		    (progn
		      (setf (bit temp_move (- i 8)) 1)
		      (setf (bit temp_move (- i 16)) 1))
		    ;; else: Check boundary and move up 1
		    (if (< i 56)
			(setf (bit temp_move (- i 8)) 1)))))
       
       ;; Prune temp of moves that cause a collision
       (loop for i from 0 to 63 do
	    (if (eql (bit (bit-and temp_move (all_pieces (game_state pp))) i) 1)
		(progn
		  (setf (bit temp_move i) 0)
		  ;; Prevent jumping
		  (if (and (> i 7) (eql (bit temp_move (- i 8)) 1))
		      (setf (bit temp_move (- i 8)) 0)))))
       
       (if (not (null (allegiance pp)))
	   ;;then
              (update-threatening pp))

       (setf (avail_move pp) (bit-ior temp_move (threatening pp)))))))






;==;Updates-threatening()
;;     Desc: Itterate through the pos bb, if 1 and index mod 8 == 0, then
;;           bb_threatening at index + 9 = 1. If 1 and index mod 8 == 7, then
;;           bb_threatening at index + 7 = 1. Else bb_threatening at index + 9 and at
;;           index + 7 = 1
(defmethod update-threatening ((p pawn))
  

  (if (eql (allegiance p) 1)
      (progn
	;; Player 1 (white)
	(loop for i from 0 to 63 do
	     (if (eql (bit (pos p) i) 1)

		 ;; If pawn is in A file.
		 (if (eql (mod i 8) 0)
		     (setf (bit (threatening p) (+ i 9)) 1)

		     ;; If pawn is in H file.
		     (if (eql (mod i 8) 7) 
			 (setf (bit (threatening p) (+ i 7)) 1)
			 
			 ;; If pawn is in neither A or H file.
			 (progn
			   (setf (bit (threatening p) (+ i 7)) 1)
			   (setf (bit (threatening p) (+ i 9)) 1))))))
	(setf (threatening p) (bit-and (threatening p) (player_pieces (player2 (game_state p))))))


      ;; Player 2 (black)
      (progn
	(loop for i from 0 to 63 do
	     (if (eql (bit (pos p) i) 1)

		 ;; If pawn is in A file.
		 (if (eql (mod i 8) 0)
		     (setf (bit (threatening p) (- i 7)) 1)

		     ;; If pawn is in H file.
		     (if (eql (mod i 8) 7) 
			 (setf (bit (threatening p) (- i 9)) 1)
			 
			 ;; If pawn is in neither A or H file.
			 (progn
			   (setf (bit (threatening p) (- i 7)) 1)
			   (setf (bit (threatening p) (- i 9)) 1)))))
	     (setf (threatening p) (bit-and (threatening p) (player_pieces (player1 (game_state p))))))))

  t)
	





(defmethod update-avail-attack ((p pawn))
  "Updates a given pawns availible attacks"

  (update-threatening p)
  
  (if (eql (allegiance p) 1)
      ;; Prune the availattack bb, so that only valid and legal attacks remain.
      (setf (avail_attack p) (copy-seq (bit-and (threatening p) (player_pieces (player2 (game_state p))))))

      ;; Prune the availattack bb, so that only valid and legal attacks remain.
      (setf (avail_attack p) (copy-seq (bit-and (threatening p) (player_pieces (player1 (game_state p))))))))
































































