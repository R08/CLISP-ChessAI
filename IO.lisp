#|*************************************************************

                       Input/Output
                           
Description: Defines functions to display and retrieve
             data to and from users.

**************************************************************|#




#|******************************************************************************
                               GENERAL Functions
******************************************************************************|#

(defun index-to-coord ()
  "Convert an index to corresponding standard tournament rules chess coordinate."
)


(defun coord-to-index (coord)
  "Convert a chess coordinate to corresponding index."
  
  (let ((file (subseq coord 0 1))
	(rank (parse-integer (subseq coord 1 2))))

    (setf file (cond
		 ((equal file "A") 0)
		 ((equal file "B") 1)
		 ((equal file "C") 2)
		 ((equal file "D") 3)
		 ((equal file "E") 4)
		 ((equal file "F") 5)
		 ((equal file "G") 6)
		 ((equal file "H") 7)))

    (+ (- (* 8 rank) 8) file)))


(defun piecename-to-index (input)
  "Accepts a list element, input, (ex. Wp Bk...) and returns a number that corrosponds to the piece entered by the user"
  (cond
    ((or (eq input 'bk)
	 (eq input 'wk)
	 (eql input 'k)) 0)
    ((or (eq input 'bq)
	 (eq input 'wq)
	 (eql input 'q)) 1)
    ((or (eq input 'br)
	 (eq input 'wr)
	 (eql input 'r)) 2)
    ((or (eq input 'bb)
	 (eq input 'wb)
	 (eql input 'b)) 3)
    ((or (eq input 'bn)
	 (eq input 'wn)
	 (eql input 'n)) 4)
    ((or (eq input 'bp)
	 (eq input 'wp)
	 (eql input 'p)) 5)
    (t (progn
	 (format t "ERROR: Invalid piece ~s" input)
	 -1))))



(defun parse-move (input)
  "Parses user input (stored as list) to a list in the form of (piecesIndex startIndex endIndex)"

  (let ((result (list (piecename-to-index (car input)))))

    (if (not (typep (second input) 'integer))
	(setf result (append result (list (coord-to-index (string (second input))))))
	(setf result (append result (list (second input)))))

    (if (not (typep (third input) 'integer))
	(setf result (append result (list (coord-to-index (string (third input))))))
	(setf result (append result (list (third input)))))
    
    (print result)

    result))



(defun parse-user-command (input)
  "Takes a string as an argument and executes the corresponding command"

  (let ((command (car input)))

    (cond
      ((eql command '/help) (print-help))                    ;; Prints a list of available commands.
      ((eql command '/restart-game) (new-game))              ;; Starts a new game.
      ((eql command '/quit-game) (quit))                     ;; Quits current game.
      ((eql command '/print-game-state) (print-game-state))) ;; Prints the current game state.

  (get-user-input)))








#|******************************************************************************
                                INPUT Functions
******************************************************************************|#


(defun get-user-input()
  "Promts the user for a movement input and returns a string form of the users input"

  (let ((input nil))
    
    (format t "~%>")
    (setf input (read-line))

    ;; Check for blank input
    (if (equal input "")
	(return-from get-user-input (get-user-input)))


    (let ((stream (make-string-input-stream input))
	  (next-tok nil)
	  (result nil))

      ;;convert user input string to list
      (loop 
	 (setf next-tok (read stream nil 'eos))
	 (if (equal next-tok 'eos)
	     (return)
	     (setf result (append result (list next-tok)))))


	;; If the input begins with a "/" pass the list to parse user command.
	(if (equal (subseq (string (car result)) 0 1) "/")
	    (parse-user-command result)
	    (parse-move result)))))




(defun perform-user-move ()
  "Calls 'get-user-input' to get  a list as users move command and performs the move if posible. 
If not possible throws an error, and intructs the user to enter another input."

  (let ((input nil)
	(piece-reference-num 0))
    
    (setf input (get-user-input))


    ;;;; EXECUTE MOVE ;;;;

    ;;sends input to determine piece # prints error if one occurs
    (setf piece-reference-num (car input))
    
    (if (or (eq piece-reference-num -1) (find (car input) (if (= (mod (turn_count current_game_state) 2) 0)
							      ;;then
							      '(bp bn bb br bq bk)
							      ;;else
							      '(wp wn wb wr wq wk))))
     
     ;;then
     (progn
       (print "ERROR: Move is not for valid piece or correct player")
       (setf input nil)
       (terpri)
       (perform-user-move))
     
     ;;else    
     ;;moves piece or prints error if one occurs
     (if (not (move-piece (nth piece-reference-num
			       (pieces (if (= (mod (turn_count current_game_state) 2) 0)
					   ;;then
					   (player1 current_game_state)
					   ;;else
					   (player2 current_game_state))))
			  (second input) (third input)))
	 ;;then
	 (progn
	   (print "ERROR: Move is not for valid piece or correct player")
	   (terpri)
	   (perform-user-move))))))






#|******************************************************************************
                             OUTPUT Functions
******************************************************************************|#

(defun new-game-comments ()
  "Trash talk/ new game comments, just added for fun."

  (let ((what-to-say (random 5)))
     (cond 
	     ((eq what-to-say 0) "Tis but a flesh wound!!")
	     ((eq what-to-say 1) "Well, thats like.. your opinion... man""YOU SHALL NOT PASSS!!!")
	     ((eq what-to-say 2) "Are you ready for this?")
	     ((eq what-to-say 3) "Well, thats like.. your opinion... man")
	     ((eq what-to-say 4) "If a fat girl falls in the woods.. do the trees laugh?"))))



(defun print-help ()
  "Prints a list of availble commands while in the game loop."

  (let ((commands (list 
		  "/help                - Prints a list of available commands"
		  "/restart-game        - Starts a new game."
		  "/quit-game           - Quits current game and brings user to main menu."
		  "/print-game-state    - Prints the current game state to the console.")))

    (loop for i from 0 to (- (list-length commands) 1) do
	 (print (nth i commands)))))



(defun print-index-bb ()
  "Prints a bit board labled with the indexs from 0 to 64"

  (terpri)
  
  (format t "~%             A    B    C    D    E    F    G    H")
  (format t "~%          -----------------------------------------")

  (loop for i from 0 to 7 do
       (let ((x))
	 (loop for j from 0 to 7 do
	      (setf x (cons (- 63 (+ j (* i 8))) x)))      
	 (format t "~%        ~D | ~{~2,'0d |~^ ~} ~D" (- 8 i) x (- 8 i)))
       (format t "~%          -----------------------------------------"))  
  (format t "~%            A    B    C    D    E    F    G    H")

  (terpri))




(defun terprix (x)
  "Prints x new lines on the console."

  (loop for i from 0 to x do
       (terpri)))

(defun terprix20 ()
  "Prints 20 new lines on the console."
  (loop for i from 0 to 19 do 
       (terpri)))







#|******************************************************************************
                            DEBUG Functions
******************************************************************************|#

(defun print-index-bb ()
  "Prints a bit board labled with the indexs from 0 to 64"
  
  (loop for i from 0 to 7 do
       (let ((x))
	 (loop for j from 0 to 7 do
	      (setf x (cons (- 63 (+ j (* i 8))) x)))      
	 (format t "~{ ~2,'0d~^ ~}~%~%" x))))