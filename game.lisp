#|*************************************************

       Descriptoin: Currently the "MAIN" file

*************************************************|#

#|--------------------------
       Load files
---------------------------|#

(load "./header.lisp")


#|--------------------------
        CONSTANTS 
---------------------------|#

(defvar PAWN_POSITION_SCORE1 
  '(   0   0   0   0   0   0   0   0
       5  10  10 -25 -25  10  10   5
       5  -5 -10   0   0 -10  -5   5       
       0   0   0  25  25   0   0   0
       5   5  10  27  27  10   5   5 
      10  10  20  30  30  20  10  10
      50  50  50  50  50  50  50  50
       0   0   0   0   0   0   0   0  )
  "Bonus or penilty based on position of Pawn")

(defvar PAWN_POSITION_SCORE2 
  '(   0   0   0   0   0   0   0   0
      50  50  50  50  50  50  50  50
      10  10  20  30  30  20  10  10
       5   5  10  27  27  10   5   5
       0   0   0  25  25   0   0   0
       5  -5 -10   0   0 -10  -5   5
       5  10  10 -25 -25  10  10   5
       0   0   0   0   0   0   0   0  )
  "Bonus or penilty based on position of Pawn")

(defvar KNIGHT_POSITION_SCORE1 
  '( -50 -40 -30 -30 -30 -30 -40 -50 
     -40 -20   0   0   0   0 -20 -40
     -30   0  10  15  15  10   0 -30
     -30   5  15  20  20  15   5 -30
     -30   0  15  20  20  15   0 -30
     -30   5  10  15  15  10   5 -30
     -40 -20   0   5   5   0 -20 -40
     -50 -40 -20 -30 -30 -20 -40 -50  )
  "Bonus or penilty based on position of Knight")

(defvar KNIGHT_POSITION_SCORE2 
  '( -50 -40 -30 -30 -30 -30 -40 -50 
     -40 -20   0   0   0   0 -20 -40
     -30   0  10  15  15  10   0 -30
     -30   5  15  20  20  15   5 -30
     -30   0  15  20  20  15   0 -30
     -30   5  10  15  15  10   5 -30
     -40 -20   0   5   5   0 -20 -40
     -50 -40 -20 -30 -30 -20 -40 -50  )
  "Bonus or penilty based on position of Knight")

(defvar BISHOP_POSITION_SCORE1 
  '( -20 -10 -40 -10 -10 -40 -10 -20
     -10   5   0   0   0   0   5 -10
     -10  10  10  10  10  10  10 -10
     -10   0  10  10  10  10   0 -10
     -10   5   5  10  10   5   5 -10
     -10   0   5  10  10   5   0 -10
     -10   0   0   0   0   0   0 -10
     -20 -10 -10 -10 -10 -10 -10 -20 )
  "Bonus or penilty based on position of Bishop")

(defvar BISHOP_POSITION_SCORE2 
  '( -20 -10 -10 -10 -10 -10 -10 -20 
     -10   0   0   0   0   0   0 -10 
     -10   0   5  10  10   5   0 -10 
     -10   5   5  10  10   5   5 -10 
     -10   0  10  10  10  10   0 -10 
     -10  10  10  10  10  10  10 -10 
     -10   5   0   0   0   0   5 -10 
     -20 -10 -40 -10 -10 -40 -10 -20   )
  "Bonus or penilty based on position of Bishop")

(defvar KING_POSITION_SCORE1
  '(  20  30  10   0   0  10  30  20
      20  20   0   0   0   0  20  20
     -10 -20 -20 -20 -20 -20 -20 -10
     -20 -30 -30 -40 -40 -30 -30 -20
     -30 -40 -40 -50 -50 -40 -40 -30
     -30 -40 -40 -50 -50 -40 -40 -30
     -30 -40 -40 -50 -50 -40 -40 -30
     -30 -40 -40 -50 -50 -40 -40 -30 )
  "Bonus or penilty based on position of King")

(defvar KING_POSITION_SCORE2 
  '( -30 -40 -40 -50 -50 -40 -40 -30 
     -30 -40 -40 -50 -50 -40 -40 -30 
     -30 -40 -40 -50 -50 -40 -40 -30 
     -30 -40 -40 -50 -50 -40 -40 -30 
     -20 -30 -30 -40 -40 -30 -30 -20 
     -10 -20 -20 -20 -20 -20 -20 -10 
      20  20   0   0   0   0  20  20 
      20  30  10   0   0  10  30  20  )
  "Bonus or penilty based on position of King")

(defvar KING_END_POSITION_SCORE1 
  '( -50 -30 -30 -30 -30 -30 -30 -50
     -30 -30   0   0   0   0 -30 -30
     -30 -10  20  30  30  20 -10 -30
     -30 -10  30  40  40  30 -10 -30
     -30 -10  30  40  40  30 -10 -30
     -30 -10  20  30  30  20 -10 -30
     -30 -20 -10   0   0 -10 -20 -30
     -50 -40 -30 -20 -20 -30 -40 -50 )
  "Bonus or penilty based on position of King during end-game")

(defvar KING_END_POSITION_SCORE2
  '( -50 -40 -30 -20 -20 -30 -40 -50 
     -30 -20 -10   0   0 -10 -20 -30 
     -30 -10  20  30  30  20 -10 -30 
     -30 -10  30  40  40  30 -10 -30 
     -30 -10  30  40  40  30 -10 -30 
     -30 -10  20  30  30  20 -10 -30 
     -30 -30   0   0   0   0 -30 -30 
     -50 -30 -30 -30 -30 -30 -30 -50 )
  "Bonus or penilty based on position of King during end-game")

#|--------------------------
     GLOBAL VARIABLE
---------------------------|#

(defvar current_game_state nil)
(setf current_game_state (make-instance 'game_state))
(defvar root nil)



;;; DEBUG variables ;;;

;; If nil no debug data is printed. 
(defvar print_debug nil)

(defvar iteration nil)
(setf iteration 0)
(defvar cutoffs nil)
(setf cutoffs 0) 
(defvar best-move nil)



#|------------------------------
	   run\main
-------------------------------|#


(new-game)
