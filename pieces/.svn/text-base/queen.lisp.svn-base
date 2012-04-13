#|******************************************

              Queen Piece Class
             Parent Class: pieces

*******************************************|#


(defclass queen (piece) 
	((name :initform 'q)) )


#|----------------------------------------
              Functions
-----------------------------------------|#

;;;Queen Move Generation
;;
;;Accepts a Queen piece pq and uses the the perpendicular
;; and diagonal move generators and updates its avail move
;; slot.
(defmethod update-avail-move ((pq queen))
  
  (clear-bb (avail_move pq))
  (diag-move-generation pq)
  (perpendicular-move-generation pq))
