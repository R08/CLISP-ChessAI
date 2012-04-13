#|*****************************************************************

        Description: Defines methods and functions to modify
 and preform calculatoins on "bit boards" (64-bit bit-vector).

    Notes: Need to decide if all of these functions/methods
           should be keept as is or changed to a method/function 
           respectivly.

******************************************************************|#




#|--------------------------------------------
               Modification
--------------------------------------------|#



(defmethod clear-bb ((bb bit-vector))
  "Initilizes a 64-bit bit-vector to 0"

  (loop for i from 0 to 63 do
       (setf (bit bb i) 0)))


(defun blank-bb ()
  "Returns a 64-bit bit-vector, of all 0"

  (let ((temp (copy-seq #*0000000000000000000000000000000000000000000000000000000000000000)))
    (clear-bb temp)
    temp))



#|----------------------------------------------
                  Output
----------------------------------------------|#



(defun print-bb (bb)
  "Print a 64-bit bit-vector as a board."

  (terpri)
  (let ((b (reverse bb)))
    
    (loop for i from 0 to 7 do
	 (let ((x))
	   (loop for j from 0 to 7 do
		(setf x (cons (aref b (+ j (* i 8))) x )))
	   (print x))))
  (terpri))




(defun fill-bb (index)
  "Returns a 64-bit bit-vector with the given indexes = 1, and the rest = 0"

  (let ((temp (copy-seq (blank-bb))))
    (loop 
       (if (and (listp index) (integerp (car index)))
	   (progn
	     (setf (bit temp (car index)) 1)
	     (setf index (cdr index)))
	   (progn
	     (if (not (eql (car index) nil))
		 (progn
		   (print 'FILL-BB__INVALID_INPUT)
		   (car index)))
		 
	     (return temp))))))
	


         
#|---------------------------------------------------

               ;;;;; Calculation ;;;;;

        Arguments:  bit-vector
        Returns:    These methods return a 64-bit
                    bit-vector.

-----------------------------------------------------|#


(Defmethod up-from-s ((bb bit-vector))
  "Accepts a bit-vector and returns a bit-vector with all elements 'above' 
the inital true elements set to true."
  
  (let ((Up-bb (blank-bb)))
    
    (loop for i from 0 to 63 do 
	 
	   (if (eq (bit bb i) 1)

	       (progn
		 (let ((j (+ i 8)))
		 (loop while (< j 64) do
		      (setf (bit Up-BB j) 1)
		      (setf j (+ j 8)))))))
    Up-BB))
    


(defmethod down-from-s ((bb bit-vector))
  "Accepts a bit-vector and returns a bit-vector with all elements 'below' 
the inital true elements set to true."

  (let ((down-bb (blank-bb)))

    (loop for i from 0 to 63 do 

	 (let ((j (- i 8)))
	 
	   (if (eq (bit bb i) 1)

	       (progn 
		 (loop while (>= j 0) do
		      (setf (bit down-BB j) 1)
		      (setf j (- j 8)))))))
    down-BB))




(Defmethod left-from-s ((bb bit-vector))
  "Accepts a bit-vector and returns a bit-vector with all elements 'left'
of the inital true elements set to true."

  (let ((left-bb (blank-bb)))
    

    (loop for i from 0 to 63 do	 
	 
	   (if (eq (bit bb i) 1)

	       (progn 
		 (let ((k (mod i 8))(j (- i 1)))
		 (loop while (>= j (- i k)) do
		      (setf (bit left-BB j) 1)
		      (setf j (- j 1)))))))
    left-BB))



(Defmethod right-from-s ((bb bit-vector))
  "Accepts a bit-vector and returns a bit-vector with all elements 'right'
 of the inital true elements set to true."

  (let ((right-bb (blank-bb)))

    (loop for i from 0 to 63 do	 
	 
	   (if (eq (bit bb i) 1)

	       (progn 
		 (let ((k (+ (mod i 8) 1)) (j (+ i 1)))
		 (loop while (<= j (+ i (- 8 k))) do
		      (setf (bit right-BB j) 1)
		      (setf j (+ j 1)))))))
    right-BB))




(Defmethod upleft-diag-from-s ((bb bit-vector))
  "Accepts a bit-vector and returns a bit-vector with all elements 'In the upper left diagonals'
of the inital true elements set to true."

  (let ((uldiag-bb (blank-bb)))

    (loop for i from 0 to 63 do	 
	 
	   (if (eq (bit bb i) 1)

	       (progn 
		 (let ((j (+ i 7)) (k (- (mod i 8) 1)))
		 (loop while (and (< j 64) (>= j 0) (>= k 0)) do
		      (setf (bit uldiag-BB j) 1)
		      (setf j (+ j 7))
		      (setf k (- k 1)))))))
    uldiag-BB))





(Defmethod lowleft-diag-from-s ((bb bit-vector))
  "Accepts a bit-vector and returns a bit-vector with all elements 'In the lower left diagonals'
;; of the inital true elements set to true."

  (let ((lldiag-bb (blank-bb)))

    (loop for i from 0 to 63 do	 
	 
	   (if (eq (bit bb i) 1)

	       (progn 
		 (let ((j (- i 9)) (k (- (mod i 8) 1)))
		 (loop while (and (< j 64) (>= j 0) (>= k 0)) do
		      (setf (bit lldiag-BB j) 1)
		      (setf j (- j 9))
		      (setf k (- k 1)))))))
    lldiag-BB))




(Defmethod lowright-diag-from-s ((bb bit-vector))
  "Accepts a bit-vector and returns a bit-vector with all elements 'In the lower right diagonals'
;; of the inital true elements set to true."

  (let ((lrdiag-bb (blank-bb)))

    (loop for i from 0 to 63 do	 
	 
	   (if (eq (bit bb i) 1)

	       (progn 
		 (let ((j (- i 7)) (k (+ (mod i 8) 1)))
		 (loop while (and (< j 64) (>= j 0) (< k 8)) do
		      (setf (bit lrdiag-BB j) 1)
		      (setf j (- j 7))
		      (setf k (+ k 1)))))))
    lrdiag-BB))




(Defmethod upright-diag-from-s ((bb bit-vector))
  "Accepts a bit-vector and returns a bit-vector with all elements 'In the lower right diagonals'
;; of the inital true elements set to true."

  (let ((urdiag-bb (blank-bb)))

    (loop for i from 0 to 63 do	 
	 
	   (if (eq (bit bb i) 1)

	       (progn 
		 (let ((j (+ i 9)) (k (+ (mod i 8) 1)))
		 (loop while (and (< j 64) (>= j 0) (< k 8)) do
		      (setf (bit urdiag-BB j) 1)
		      (setf j (+ j 9))
		      (setf k (+ k 1)))))))
    urdiag-BB))




       