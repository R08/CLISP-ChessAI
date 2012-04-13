#|***********************************************************************

                          Rook Piece Class
                         Parent Class:piece

************************************************************************|#



(defclass rook (piece) 
	((name :initform 'r)) )


#|-------------------------------

            Functions
--------------------------------|#


;;;Accepts a Rook class and assigns the availible moves to
;;   the player's "Rook(s)" piece(s).

(defmethod update-avail-move ((pr rook))
  
  (clear-bb (avail_move pr))
  (clear-bb (threatening pr))
  (Perpendicular-move-generation pr ))






    
