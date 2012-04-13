#|***************************************************

                   Node Class
Description: Defines the Node class and its methods.


*****************************************************|#


(defclass node ()      
  ((game_state       :accessor game_state       :initarg :game_state)               ;; Game_state object that the node points to (Ex. Current_game_state).     
   (parent           :accessor parent           :initarg :parent    :initform  nil)  ;; Parent of the node (root == nil)
   (children         :accessor children         :initarg :children  :initform  nil) ;; Children nodes of this node <list> (Ex. 'hashname1 'hashname2 ...)
   (move_from_parent :accessor move_from_parent :initarg :move_from_parent)         ;; The move that differs this node from its parent (Ex. "WP 10 18").
   (player_turn      :accessor player_turn      :initarg :player_turn)             ;; Holds what players turn it is for this game_state (Ex.  player1).
   (best_move        :accessor best_move        :initarg :best_move)
   
   (best_node       :accessor best_node       :initarg :best_node :initform nil)))





#|-------------------------------
            Functions
-------------------------------|#


(defun make-root-node (game-st)
  "Makes a root node for the given game_state."

  (make-instance 'node
		 :game_state          (copy-game-state game-st)
		 :parent               nil
		 :move_from_parent     nil
		 :player_turn         (if (= (mod (turn_count game-st) 2) 0)
					  ;;then
					  (player1 game-st)
					  ;;else
					  (player2 game-st))
		 :best_move           (car (player_moves (player1 game-st )))))





(defun make-child-node (ch-name parent game-st move)
  "Makes a child for 'parent' and returns the new child."

  (setf (children parent) 
	   (append (list 
		    (make-instance 'node
			      :game_state          (setf ch-name (copy-game-state game-st))
			      :parent               parent
			      :move_from_parent     move
			      :player_turn         (if (eql (allegiance (nth 0 (pieces (player_turn parent)))) 1)
						       ;;then
						       (player2 game-st)
						       ;;else
						       (player1 game-st))))
		   (children parent)));;<---appending to this


  (car (children parent)))





(defun moves-from-root (node)
  "Generates the move from the root to this node."

  (moves-from-root-helper node nil))




(defun moves-from-root-helper (node lst)
  "helper to moves-from-root"

  (if (not (null (move_from_parent node)))
	(moves-from-root-helper (parent node) (append lst (list (move_from_parent node))))
	lst))



  