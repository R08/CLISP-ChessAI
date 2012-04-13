#|*****************************************************

                     Header
          Loads all needed source files

******************************************************|#
(defvar current_game_state)


(let ((files '("game_state"

	       "bb-util" 
	       "pieces/piece"
	       

	       "pieces/pawn"
	       "pieces/rook"
	       "pieces/knight"
	       "pieces/bishop"
	       "pieces/queen"
	       "pieces/king"

	       "player"
	       
	       
	       "game-util"
	       "IO"
	       "node"
	       "tree"
	       "ai"
	       )))

#|
  (mapcar #'(lambda (s)
	      (merge-pathnames "src/" s))
	  files)
  |#

  ;; Compile files
  (mapcar #'(lambda (s) 
	      (compile-file s :output-file (merge-pathnames "bin/")))
	  files)
  

  ;; Load files
  (mapcar #'load files))
