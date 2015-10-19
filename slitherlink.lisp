(defvar usermove)
(defvar degree)
(defvar i)
(defvar nrow)
(defvar ncol)
(defvar numlines)
(defvar nlines)
(defvar flag1)
(defvar flag2)
(defvar flag3)
(defvar listt)
(defvar x-start)
(defvar y-start)
(defvar total_time)
(defvar data)



(defun slither()
	(format t "Welcome to slitherlink.~%")
	(slither-loop (read-board (query-board))))

(defun query-board ()
  (format t "Which game do you want to play?~&")
  (read-line *query-io*))
	
(defun slither-loop (board)	 
	 (print-board board)	 
	(loop for move = (query-move)		
		
		do (when (string-equal "solve" (string-trim " " move))               
               (solve board)
               (return-from slither-loop))   	
	
		do (setf usermove (parse-move move))
		 	;(print usermove)
			
			(place-move usermove board)
			(print-board board)
			;(print degree)
		(if  (detect-loop degree board) 
			(progn 
			(format t "You Won!~&")
			(return)))
			))

(defun query-move()
  (format *query-io* "Your move? :~&")
  (read-line *query-io*))
  

			  
; converts the users move into a list			   "1 1 t"  to (1 1 t)
(defun parse-move (move)
	(setf listt (with-input-from-string (s move :index i :start 0 :end 5 )
              (list (read s)  (read s) (character(read s))))
			  ))	  
				
			  
			  
			  
(defun place-move (triplet board)
    ;(print triplet)
  (let ((x (1+ (* 2 (1- (car triplet)))))
        (y (1+ (* 2 (1- (cadr triplet)))))
        (tlrb (char-downcase (caddr triplet))))
    
    (cond ((char= #\t tlrb)
           (if (char= (aref board (1- x) y) #\Space)
               (progn
			   (setf (aref board (1- x) y) #\-)
			    (inc-degree (1- x) (1- y))
				(inc-degree (1- x) (1+ y))
			    )
               (progn
			   (setf (aref board (1- x) y) #\Space)
			   (dec-degree (1- x) (1- y))
			   (dec-degree (1- x) (1+ y))
			   )))
          ((char= #\b tlrb)
           (if (char= (aref board (1+ x) y) #\Space)
               (progn
			   (setf (aref board (1+ x) y) #\-)
			    (inc-degree (1+ x) (1- y))
				(inc-degree (1+ x) (1+ y))
			    )
               (progn
			   (setf (aref board (1+ x) y) #\Space)
			   (dec-degree (1+ x) (1- y))
			   (dec-degree (1+ x) (1+ y))
			   )))
          ((char= #\l tlrb)
           (if (char= (aref board x (1- y)) #\Space)
               (progn
			   (setf (aref board x (1- y)) #\|)
			    (inc-degree (1- x) (1- y))
				(inc-degree (1+ x) (1- y))
			    )
               (progn
			   (setf (aref board x (1- y)) #\Space)
			   (dec-degree (1- x) (1- y))
			   (dec-degree (1+ x) (1- y))
			   )))
          ((char= #\r tlrb)
           (if (char= (aref board x (1+ y)) #\Space)
               (progn
			   (setf (aref board x (1+ y)) #\|)
			    (inc-degree (1- x) (1+ y))
				(inc-degree (1+ x) (1+ y))
			    )
               (progn
			   (setf (aref board x (1+ y)) #\Space)
			   (dec-degree (1- x) (1+ y))
			   (dec-degree (1+ x) (1+ y))
			   ))))))
			  
	
(defun read-board (pathname)
  (parse-board (read-file pathname)))


(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
       while line
       collecting line into lines
       finally (return lines))))





(defun parse-board (strings)
  
  (setq ncol (length (car strings)))

  (setq nrow (length strings))
  (setq degree (make-array (list ncol nrow)
								))
  
    
  (let ((board (make-array(list ncol nrow)
                                ))) ;;;; rows
		
								
		(loop :for row  :from 0 :to (1- (array-dimension board 0))  ; since index starts from 0
           :for row-string = nil
		   :do (setf row-string (elt strings row))
		       ;( print row-string)
           :do (loop :for column :from 0 :to (1- (array-dimension board 1))						
				 :do (setf (aref board row column) (elt row-string column))
								
								)) 
								board))
								


(defun print-board (board)
  (format t "~&") 
  (loop :for row :from 0 :to (array-dimension board 0)
     :do (loop :for column :from 0 :to (array-dimension board 1)
            :do (cond
                  ((= row 0) 
                   (cond ((= 0 column)
                          (format t "     "))
                         ((oddp column) ;non numbered columns
                          (format t " "))
                         ((evenp column) ; numbered columns
                          (format t "~3D  " (/ column 2)))))
                  ((= column 0) 
                   (cond ((oddp row)
                          (format t "     "))
                         ((evenp row)
                          (format t "~3D  " (/ row 2)))))
                  ((AND (> row 0) (> column 0))
                   (format t "~3A" (aref board (1- row) (1- column))))))
     :do (format t "~2&")))


(defun is-vertex? (x y)
    (and (evenp x) (evenp y)))	 
	
	
(defun inc-degree (x y)
  (setf  (aref degree x y) (incf (aref degree x y))))
  
 (defun dec-degree (x y)
	(setf (aref degree x y) (decf (aref degree x y))))

	
;to detect the end of game 
(defun detect-loop (degree board)
    (check-degree degree)
	
	;(print flag1)
	
	(if (equalp flag1 1)
	    (progn
		(format t "continue playing~&")
		)
		(progn
		(check-node board)
		;(print flag2)
		 (if (equalp flag2 5)
				(format t "wrong moves~&")
		        (progn
				(format t "loop complete~&")
		        (return-from detect-loop t)))
		
	    )))

;when the loop completes all vertices should have degree of 0 or 2, until this condition matches the flag is set to 1	  
(defun check-degree	(degree)
(setf flag1 0)		 	   
  (loop :for row :from 0 :to (1- nrow)
		:do (loop :for column  :from 0 :to (1- ncol)
             :do (when (not (vertex-degree row column)) (progn (setf flag1 1) (return flag1)))
					)))
				   
(defun vertex-degree (row column)
  (or (= (aref degree row column) 0) (= (aref degree row column) 2)))

(defun is-number? (board x y)
    ;(print (aref board x y))
    (and (numberp (digit-char-p(aref board x y))) (and (oddp x) (oddp y))))	 
	
;; checks if an element in the board array is a number and sets flag2 to 5 if the lines surrounding the number exactly matches the number
(defun check-node (board)
 (setf flag2 0)
	(loop :for row :from 0 :to (1- nrow)
		:do (loop :for column  :from 0 :to (1- ncol)		      
			  :do (if (is-number? board row column)						
				          (progn						  
						    ;(print row)
							;(print column)
							
							(check-lines board row column)
							;(print numlines)
							:do (when  (not(equalp numlines (digit-char-p(aref board row column))))
						              
									   (progn
									   ;(print row)
									   ;(print column)
									   ;(print numlines)
									   ;(print (aref board row column))									   
									   (setf flag2 5)
									   ;(print flag2)
									   (return-from check-node flag2)))
							)))))
			
;; checks the number of lines around a number in the board			
(defun check-lines (board x y)
	
	(setf numlines 0)
    (if (char= (aref board (1- x) y) #\-)
			(setf numlines (1+ numlines)))
	(if (char= (aref board x (1- y)) #\|) 
			(setf numlines (1+ numlines)))
	(if (char= (aref board (1+ x) y) #\-)
			(setf numlines (1+ numlines)))
	(if (char= (aref board x (1+ y)) #\|)
			(setf numlines (1+ numlines)))
			 
			  )
	
			
			

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun solve(board)
(let ((moves nil))
(loop for move = (query)		
 do (when (string-equal "apply-hints" (string-trim " " move))
               
               (mark-possible-hints board)
			   (print-board board)
               )
do (when (string-equal "solve-anyway" (string-trim " " move))
(let ((time_start (get-universal-time))               
               )
(find-start-point board)
;(print x-start)
;(print y-start)
(mark-possible-hints board)
(depth-first-search board (1- x-start) (1- y-start) moves)
(let ((time_end (get-universal-time)))
(print-board board)
(setf total_time (- time_end time_start))
(format t "Time taken to solve: ~D seconds." total_time)

(return-from solve)

))))))

(defun query()
  (format t "apply-hints or solve-anyway?~&")
  (format *query-io* "What do you choose? :~&")
  (read-line *query-io*))

(defun mark-possible-hints (board)
	(loop for x  :from 1 :to (1- (array-dimension board 0)) by 2
     :do (loop :for y  from 1 to (1- (array-dimension board 1)) by 2
	  :do (if (numberp (digit-char-p(aref board x y)))
				(progn (setf data (digit-char-p(aref board x y)))
				       
				(case data 
				  (0 (hint-0 board x y))
				  ;(1 (hint-1 board x y))
				  (2 (hint-2 board x y))
				  (3 (hint-3 board x y)))
					))))
					board)



					
(defun is-corner? (board x y)
  (or (and (= x 1) (= y 1))
      (and (= x 1) (= y (- (array-dimension board 0) 2)))
      (and (= x (- (array-dimension board 0) 2)) (= y 1))
      (and (= x (- (array-dimension board 0) 2)) (= y (- (array-dimension board 1) 2))))
	  )

	  
(defun valid-x (board x)  
  (if (AND (>= x 0)
           (<= x (1- (array-dimension board 0))))
      t nil))

(defun valid-y (board y)  
  (if (AND (>= y 0)
           (<= y (1- (array-dimension board 1))))
      t nil))

(defun valid-position (board x y)  
  (if (AND (valid-x board x)
           (valid-y board y))
      t))
	  


;;; hint-0	  
(defun hint-0 (board x y)
					

    (setf (aref board x (1+ y)) #\X
		  (aref board x (1- y)) #\X
		  (aref board (1+ x) y) #\X
		  (aref board (1- x) y) #\X)
				
    ;"below"
    (let ((x (+ x 2))
          (y y))
		  
      (if (valid-position board x y)
          (progn 
		  (if (numberp (digit-char-p(aref board x y)))
		       (progn
		       (let ((data (digit-char-p(aref board x y))))            
			    
                    (case data
                    (3 (progn
                       (setf (aref board (1+ x) y) #\-
                             (aref board x (1+ y)) #\|
                             (aref board x (1- y)) #\|)
							 ))
                     (2 t)
                     (1 t)
                     (0 t))))))))

		  
    ;"right"
    (let ((x x)
          (y (+ y 2)))
		  ;(print 5)
      (if (valid-position board x y)
          (progn 
		  (if (numberp (digit-char-p(aref board x y)))
		       (progn
		       (let ((data (digit-char-p(aref board x y))))            
			    
                    (case data
                    (3 (progn
                       (setf (aref board (1+ x) y) #\-
                             (aref board (1- x) y) #\-
                             (aref board x (1+ y)) #\|)
							 ))
                    (2 t)
                    (1 t)
                    (0 t))))))))
   
	
	;"up"
    
  (let ((x (- x 2))
          (y y))
		  ;(print 5)
      (if (valid-position board x y)
          (progn 
		  (if (numberp (digit-char-p(aref board x y)))
		       (progn
		       (let ((data (digit-char-p(aref board x y))))            
			    
                    (case data
                    (3 (progn
                       (setf (aref board (1- x) y) #\-
                             (aref board x (1+ y)) #\|
                             (aref board x (1- y)) #\|)
							 ))
                    (2 t)
                    (1 t)
                    (0 t))))))))
				  
				  
				  
   ;"left"
   (let ((x x)
          (y (- y 2)))
		  ;(print 5)
      (if (valid-position board x y)
          (progn 
		  (if (numberp (digit-char-p(aref board x y)))
		       (progn
		       (let ((data (digit-char-p(aref board x y))))            
			    
                    (case data
                    (3 (progn
                       (setf (aref board (1+ x) y) #\-
                             (aref board (1- x) y) #\-
                             (aref board x (1- y)) #\|)
							 ))
                    (2 t)
                    (1 t)
                    (0 t))))))))		
						
						
						
;diagonal bottom right
(let ((x (+ x 2))
          (y (+ y 2)))
		  ;(print 5)
      (if (valid-position board x y)
          (progn 
		  (if (numberp (digit-char-p(aref board x y)))
		       (progn
		       (let ((data (digit-char-p(aref board x y))))            
			    
                    (case data
                    (3 (progn
                       (setf (aref board (1- x) y) #\-
                             (aref board x (1- y)) #\|)
							 ))
                    (2 t)
                    (1 t)
                    (0 t))))))))

;diagonal top right
(let ((x (- x 2))
          (y (+ y 2)))
		  ;(print 5)
      (if (valid-position board x y)
          (progn 
		  (if (numberp (digit-char-p(aref board x y)))
		       (progn
		       (let ((data (digit-char-p(aref board x y))))            
			    
                    (case data
                    (3 (progn
                       (setf (aref board (1+ x) y) #\-
                             (aref board x (1- y)) #\|)
							 ))
                    (2 t)
                    (1 t)
                    (0 t))))))))	

;diagonal bottom left
(let ((x (+ x 2))
          (y (- y 2)))
		  ;(print 5)
      (if (valid-position board x y)
          (progn 
		  (if (numberp (digit-char-p(aref board x y)))
		       (progn
		       (let ((data (digit-char-p(aref board x y))))            
			    
                    (case data
                    (3 (progn
                       (setf (aref board (1- x) y) #\-
                             (aref board x (1+ y)) #\|)
							 ))
                    (2 t)
                    (1 t)
                    (0 t))))))))

;diagonal top left
(let ((x (- x 2))
          (y (- y 2)))
		  ;(print 5)
      (if (valid-position board x y)
          (progn 
		  (if (numberp (digit-char-p(aref board x y)))
		       (progn
		       (let ((data (digit-char-p(aref board x y))))            
			    
                    (case data
                    (3 (progn
                       (setf (aref board (1+ x) y) #\-
                             (aref board x (1+ y)) #\|)
							 ))
                    (2 t)
                    (1 t)
                    (0 t))))))))					
						)


;;;hint-3					
(defun hint-3 (board x y)


(if (is-corner? board x y)
        (cond ((and (= x 1)
                    (= y 1))
               (setf (aref board x (- y 1)) #\|
                     (aref board (- x 1) y) #\-))
              ((and (= x 1)
                    (= y (- (array-dimension board 1) 2)))
               (setf (aref board (- x 1) y) #\-
                     (aref board x (+ y 1)) #\|))
              ((and (= x (- (array-dimension board 0) 2))
                    (= y 1))
               (setf (aref board x (- y 1)) #\|
                     (aref board (+ x 1) y) #\-))
              ((and (= x (- (array-dimension board 0) 2))
                    (= y (- (array-dimension board 1) 2)))
               (setf (aref board x (+ y 1)) #\|
                     (aref board (+ x 1) y) #\-))))

 ;right
 (let ((x x)
          (y (+ y 2)))
		  
      (if (valid-position board x y)
          (progn 
		  (if (numberp (digit-char-p(aref board x y)))
		       (progn
		       (let ((data (digit-char-p(aref board x y))))            
			    
                    (case data
                    (3 (progn 
                       (setf (aref board x (1+ y)) #\|
                             (aref board x (1- y)) #\|
							 (aref board x (- y 3)) #\|)
							 ))
                    (2 t)
                    (1 t)
                    (0 t))))))))
 ;bottom
(let ((x (+ x 2))
          (y y))
		  
      (if (valid-position board x y)
          (progn 
		  (if (numberp (digit-char-p(aref board x y)))
		       (progn
		       (let ((data (digit-char-p(aref board x y))))            
			    
                    (case data
                    (3 (progn
                       (setf (aref board (1+ x) y) #\-
                             (aref board (1- x) y) #\-
							 (aref board (- x 3) y) #\-)
							 ))
                    (2 t)
                    (1 t)
                    (0 t)))))))) 

;left
 (let ((x x)
          (y (- y 2)))
		  
      (if (valid-position board x y)
          (progn 
		  (if (numberp (digit-char-p(aref board x y)))
		       (progn
		       (let ((data (digit-char-p(aref board x y))))            
			    
                    (case data
                    (3 (progn
                       (setf (aref board x (1+ y)) #\|
                             (aref board x (1- y)) #\|
							 (aref board x (+ y 3)) #\|)
							 ))
                    (2 t)
                    (1 t)
                    (0 t))))))))
;top					
(let ((x (- x 2))
          (y y))
		  
      (if (valid-position board x y)
          (progn 
		  (if (numberp (digit-char-p(aref board x y)))
		       (progn
		       (let ((data (digit-char-p(aref board x y))))            
			    
                    (case data
                    (3 (progn
                       (setf (aref board (1+ x) y) #\-
                             (aref board (1- x) y) #\-
							 (aref board (+ x 3) y) #\-)
							 ))
                    (2 t)
                    (1 t)
                    (0 t))))))))			
)

;;hint-2
(defun hint-2 (board x y)
      (if (is-corner? board x y)
         (cond ((and (= x 1)
                    (= y 1))
               (setf (aref board (- x 1) (+ y 2)) #\-
                     (aref board (+ x 2) (- y 1)) #\|))
              ((and (= x 1)
                    (= y (- (array-dimension board 1) 2)))
               (setf (aref board (- x 1) (- y 2)) #\-
                     (aref board (+ x 2) (+ y 1)) #\|))
              ((and (= x (- (array-dimension board 0) 2))
                    (= y 1))
               (setf (aref board (+ x 1) (+ y 2)) #\-
                     (aref board (- x 2) (- y 1)) #\|))
              ((and (= x (- (array-dimension board 0) 2))
                    (= y (- (array-dimension board 1) 2)))
               (setf (aref board (- x 2) (+ y 1)) #\|
                     (aref board (+ x 1) (- y 2)) #\-)))
				   
    ))



;;;;;;;;
;;after the hints are applied find a starting location for depth-first-search:	
;;;;;;;;

(defun find-start-point(board)
                                        
    (loop for x  from 1 to (- (array-dimension board 0) 2) by 2
       :do (loop for y  from 1 to (- (array-dimension board 1) 2) by 2
              :do (if (equalp (digit-char-p(aref board x y)) 3)
				(progn (setf x-start x)
				       (setf y-start y)					   
				       (return-from find-start-point))))
		
		))
		
		
(defun depth-first-search (board x y moves
            &optional (depth 0) (checked-edges (make-array (array-dimensions board) :initial-element nil)))
	;(print 5555)
	;(print x)
	;(print y)
	;(print 5555)
	(if (> depth (1- (* (array-dimension board 0) (array-dimension board 1))))
      (progn        
        (return-from depth-first-search))
      (progn
        (degree-count board)
        (check-node board)
            (if (and (equalp  flag2 0) (equalp flag3 0))
              (progn (print flag2)
					 (print flag3)
					 (format t "~&Solved!! ~&")			         
					 (list-moves (nreverse moves))
					 
                   (return-from depth-first-search board)))
        (progn
          ;; (print-board board)
          (e-count board x y)
			
            (if (< nlines 3)
                (progn
                    (if (= 2 nlines) 
                        (progn 
						
						
                          ;;;t;;Down;;;;;
                          (let ((x (1+ x))
                                (y y))
								
                            (if (and (valid-x board x)
                                     (not (is-empty?? board x y)))
                                
                                (if (and (not (aref checked-edges x y))
                                         (can-proceed? board x y))
                                    (progn
									  
                                      (setf (aref checked-edges x y) t) 
                                      (push (list x y) moves)
									  ;(format t "tdown~&")
									  ;(list-moves moves)
                                      (if (depth-first-search board (1+ x) y moves (1+ depth) checked-edges)
                                          (return-from depth-first-search t)
                                          (progn 
                                            (pop moves)
											;(list-moves moves)
                                            (setf (aref checked-edges x y) nil)))))))

                          ;;;t;;right;;;;
                          (let ((x x)
                                (y (1+ y)))
                            (if (and (valid-y board y)
                                     (not (is-empty?? board x y)))

                                        
                                (if (and (not (aref checked-edges x y))
                                         (can-proceed? board x y))
                                    (progn
                                      (setf (aref checked-edges x y) t) 
                                      (push (list x y) moves)
									  ;(format t "tright~&")
									  ;(list-moves moves)									  
                                      (if (depth-first-search board x (1+ y) moves (1+ depth) checked-edges)
                                          (return-from depth-first-search t)
                                          (progn 
                                            (pop moves)
											;(list-moves moves)
                                            (setf (aref checked-edges x y) nil)))))))
                          ;;t;;up;;;
                          (let ((x (1- x))
                                (y y))
                            (if (and (valid-x board x)
                                     (not (is-empty?? board x y)))
                                        
                                (if (and (not (aref checked-edges x y))
                                         (can-proceed? board x y))
                                    (progn
                                      (setf (aref checked-edges x y) t) 
                                      (push (list x y) moves)
									  ;(format t "tup~&")
									  ;(list-moves moves)
                                      (if (depth-first-search board (1- x) y moves (1+ depth) checked-edges)
                                          (return-from depth-first-search t)
                                          (progn 
                                            (pop moves)
											;(list-moves moves)
                                            (setf (aref checked-edges x y) nil)))))))
                          ;;t;;left;;; 
                          (let ((x x)
                                (y (1- y)))
                            (if (and (valid-y board y)
                                     (not (is-empty?? board x y)))
                                        
                                (if (and (not (aref checked-edges x y))
                                         (can-proceed? board x y))
                                    (progn
                                      (setf (aref checked-edges x y) t)
                                      (push (list x y) moves)
									  ;(format t "tleft~&")
									  ;(list-moves moves)
                                      (if (depth-first-search board x (1- y) moves (1+ depth) checked-edges)
                                          (return-from depth-first-search t)
                                          (progn 
                                            (pop moves)
											;(list-moves moves)
                                            (setf (aref checked-edges x y) nil)))))))
                          )

                        (progn
         
                          ;;;;;down
                          (let ((x (1+ x))
                                (y y))
                            
                            (if (valid-x board x)
                                (if
                                 (is-empty?? board x y)
                                 (progn 
                                   (setf (aref board x y) #\|
                                         (aref checked-edges x y) t)
                                   
                                   (if (can-proceed? board x y)
                                       (progn
                                         (push (list x y) moves)
										 ;(format t "bdown1~&")
										 ;(list-moves moves)
                                         (if (depth-first-search board (1+ x) y moves (1+ depth) checked-edges)
                                             (return-from depth-first-search t)
                                             (progn
                                               (pop moves)
											   ;(list-moves moves)
                                               (setf (aref board x y) #\Space
                                                     (aref checked-edges x y) nil))))
                                       (setf (aref board x y) #\Space
                                             (aref checked-edges x y) nil)))

                                 
                                 (if (and (not (aref checked-edges x y))
                                          (can-proceed? board x y))
                                     (progn
                                       (setf (aref checked-edges x y) t) 
                                       (push (list x y) moves)
									   ;(format t "bdown2~&")
									   ;(list-moves moves)
                                       (if (depth-first-search board (1+ x) y moves (1+ depth) checked-edges)
                                           (return-from depth-first-search t)
                                           (progn 
                                             (pop moves)
											 ;(list-moves moves)
                                             (setf (aref checked-edges x y) nil))))))))

                          ;;;right;;
                          (let ((x x)
                                (y (1+ y)))
                           
                            (if  (valid-y board y)
                                 (if (is-empty?? board x y)
                                     (progn                                     
                                       (setf (aref board x y) #\-
                                             (aref checked-edges x y) t)
                                       (if (can-proceed? board x y)
                                           (progn
                                             (push (list x y) moves)
											 ;(format t "bright1~&")
											 ;(list-moves moves)
                                             (if (depth-first-search board x (1+ y) moves (1+ depth) checked-edges)
                                                 (return-from depth-first-search t)
                                                 (progn
                                                   (pop moves)
												   ;(list-moves moves)
                                                   (setf (aref board x y) #\Space
                                                         (aref checked-edges x y) nil))))
                                           (setf (aref board x y) #\Space
                                                 (aref checked-edges x y) nil)))
                                     
                                     (if (and (not (aref checked-edges x y))
                                              (can-proceed? board x y))
                                         (progn
                                           (setf (aref checked-edges x y) t) 
                                           (push (list x y) moves) 
										   ;(format t "bright2~&")
										   ;(list-moves moves)
                                           (if (depth-first-search board x (1+ y)  moves (1+ depth) checked-edges)
                                               (return-from depth-first-search t)
                                               (progn 
                                                 (pop moves)
												 ;(list-moves moves)
                                                 (setf (aref checked-edges x y) nil))))))))

                          ;; up;;
                          (let ((x (1- x))
                                (y y))
                            
                            (if (valid-x board x)
                                (if (is-empty?? board x y)
                                    (progn                                      
                                      (setf (aref board x y) #\|
                                            (aref checked-edges x y) t)
                                      (if (can-proceed? board x y)
                                          (progn
                                            (push (list x y) moves)
											;(format t "bup1~&")
											;(list-moves moves)
                                            (if (depth-first-search board (1- x) y moves (1+ depth) checked-edges)
                                                (return-from depth-first-search t)
                                                (progn
                                                  (pop moves)
												  ;(list-moves moves)
                                                  (setf (aref board x y) #\Space
                                                        (aref checked-edges x y) nil))))
                                          (setf (aref board x y) #\Space
                                                (aref checked-edges x y) nil)))
                                    
                                    (if (and (not (aref checked-edges x y))
                                             (can-proceed? board x y))
                                        (progn
                                          (setf (aref checked-edges x y) t)
                                          (push (list x y) moves)
										  ;(format t "bup2~&")
										  ;(list-moves moves)
                                          (if (depth-first-search board (1- x) y moves (1+ depth) checked-edges)
                                              (return-from depth-first-search t)
                                              (progn 
                                                (pop moves)
												;(list-moves moves)
                                                (setf (aref checked-edges x y) nil))))))))

                          ;;;;left;;;
                          (let ((x x)
                                (y (1- y)))
                            
                            (if (valid-y board y)
                                (if (is-empty?? board x y)
                                    (progn                                      
                                      (setf (aref board x y) #\-
                                            (aref checked-edges x y) t)
                                      (if (can-proceed? board x y)
                                          (progn
                                            (push (list x y) moves)
											;(format t "bleft1~&")
											;(list-moves moves)
                                            (if (depth-first-search board x (1- y) moves (1+ depth) checked-edges)
                                                (return-from depth-first-search t)
                                                (progn
                                                  (pop moves)
												  ;(list-moves moves)
                                                  (setf (aref board x y) #\Space
                                                        (aref checked-edges x y) nil))))
                                          (setf (aref board x y) #\Space
                                                (aref checked-edges x y) nil)))
                                   
                                    (if (and (not (aref checked-edges x y))
                                             (can-proceed? board x y))
                                        (progn
                                          (setf (aref checked-edges x y) t)
                                          (push (list x y) moves)
										  ;(format t "bleft2~&")
										  ;(list-moves moves)
                                          (if (depth-first-search board x (1+ y) moves (1+ depth) checked-edges)
                                              (return-from depth-first-search t)
                                              (progn ; else this is not on the solution path
                                                (pop moves)
												;(list-moves moves)
                                                (setf (aref checked-edges x y) nil))))))))
                         
                          nil))))))))




  
(defun is-empty?? (board x y)
 (let ((temp (aref board x y)))      
      (case temp
      ((#\| #\- #\x) nil)
      ((#\Space) t)))) 
			

(defun e-count (board x y)
	
	(setf nlines 0)
    (if (and (valid-position board (1- x) y)(char= (aref board (1- x) y) #\|) )
			(setf nlines (1+ nlines)))
	(if (and (valid-position board x (1- y))(char= (aref board x (1- y)) #\-) )
			(setf nlines (1+ nlines)))
	(if (and (valid-position board(1+ x) y)(char= (aref board (1+ x) y) #\|) )
			(setf nlines (1+ nlines)))
	(if (and (valid-position board x (1+ y))(char= (aref board x (1+ y)) #\-) )
			(setf nlines (1+ nlines)))
		 
			  )
(defun can-proceed? (board x y)
  (let ((edge (aref board x y)))
    (case edge
      (#\| 
       (progn
        (AND
		  (let ((y (1- y))) 
            (if (valid-y board y)
                (if (numberp (aref board x y))
                    (>= (aref board x y) (e-count board x y))
                    t)
                t))
          (let ((y (1+ y))) 
            (if (valid-y board y)
                (if (numberp (aref board x y))
                    (>= (aref board x y) (e-count board x y))
                    t)
                t)))))
      (#\- 
       (progn
         (AND
          (let ((x (1- x))) 
            (if (valid-x board x)
                (if (numberp (aref board x y))
                    (>= (aref board x y) (e-count board x y))
                    t)
                t))
          (let ((x (1+ x))) 
            (if (valid-x board x)
                (if (numberp (aref board x y))
                    (>= (aref board x y) (e-count board x y))
                    t)
                t))))))))
			  
(defun degree-count (board)
(setf flag3 0)		 	   
  (loop :for row :from 0 :to (1- nrow)
		:do (loop :for column  :from 0 :to (1- ncol)
             :do (if (is-vertex? row column)
				 (progn 
				 (e-count board row column)
				 :do (when (not (or (= nlines 0) (= nlines 2))) (progn (setf flag3 7) (return flag3))))))))
				  



			   
(defun list-moves (moves)
  (format t "~&The moves in sequence were: ~&")
  (loop for move in moves
     :do (format t "~& ~A ~&" move)))
			   
