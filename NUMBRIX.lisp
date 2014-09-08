

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; CREATE AND COPY BOARD 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (DEFUN COPY-BRD(BRD)
	(COND ((NULL BRD) NIL)
	
	(T (CONS (MAKE_SUBLIST (CAR BRD)) (COPY-BRD (CDR BRD))) ) )
  )
  
  (DEFUN MAKE_SUBLIST(ROW)
	(COND ((NULL ROW) NIL)
		(T (CONS (CAR ROW) (MAKE_SUBLIST (CDR ROW))) ) )
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; PRINT THE INITIAL BOARD   
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (DEFUN PBOARD(BRD N)
	(COND ((NULL BRD) (TERPRI)
		(COND ((<= N 3) (BOARDDOUBLE N) )
			(T (BOARDTRIPLE N)) ) )
	(T (TERPRI)
		(COND ((<= N 3) (BOARDDOUBLE N) )
			(T (BOARDTRIPLE N)) )
				(TERPRI)(PROW (CAR BRD) N)
					(PBOARD (CDR BRD) N)) ) ) 
				
  (DEFUN BOARDDOUBLE(N)
	(COND ((EQUAL N 0) (PRINC "+"))
		(T (PRINC "+--")
		 (BOARDDOUBLE (1- N)))))
  (DEFUN BOARDTRIPLE(N)
	(COND ((EQUAL N 0) (PRINC "+"))
		(T (PRINC "+---")
		
	(SETQ N (1- N)) (BOARDTRIPLE N))))
  (DEFUN PROW(ROW N)
	(COND ((<= N 3) (PROW1 ROW))
	(T (PROW2 ROW)))
   )
  (DEFUN PROW1(ROW)
	(COND ((NULL ROW)(PRINC "| "))
		(T (PRINC "| ")
			(PRINC (CAR ROW))
				(PROW1 (CDR ROW)) ) ) 
    )
  
  (DEFUN PROW2(ROW)
	(COND ((NULL ROW)(PRINC "| "))
		(T (PRINC "| ")
			(SETQ NUMB (CAR ROW))
			(SETF DIGIT NUMB)
			(PRINC NUMB)
			(SETQ COUNT 0) 
			(COND ((EQUAL DIGIT '--) NIL)
              		(T (LOOP 
                    		(SETQ DIGIT (FLOOR (FLOAT (/ DIGIT 10))))
                    		(SETQ COUNT (1+ COUNT))
                    		(WHEN (EQUAL DIGIT 0) (RETURN T))
			    )
			    (COND ((EQUAL COUNT 1) (PRINC " ")))))
			(PROW2 (CDR ROW)))))
    
;;(PBOARD BRD N)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;HUMAN PLAYS
;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN HUMAN()
 (PRINC "PLAYING IN AUTOMATIC PLAY MODE")
 (PRINC "ENTER YOUR BOARD CHOICES FROM 1-17: ")
  (SETQ CHOICE (READ))
   (COND 
   ((EQUAL CHOICE 1)
   (SETQ N 5)       
   (SETQ BRD (LIST '(1 -- -- -- 9) 
                  '(-- -- 6 -- --) 
                  '(-- 18 -- 14 --)
                  '(-- -- 16 -- --)
                  '(21 -- -- -- 25)
                   ))
   (SETQ BRD1 (COPY-BRD BRD))
        (SETQ F (LIST '(1 0 0 0 1) '(0 0 1 0 0)
                      '(0 1 0 1 0) '(0 0 1 0 0)
                      '(1 0 0 0 1))))
   
   ((EQUAL CHOICE 2)
    (SETQ N 6)
    (SETQ BRD (LIST '(3 4 9 10 15 16)
                  '(2 -- -- -- -- 17)
                  '(1 -- -- -- -- 18)
                  '(36 -- -- -- -- 19)
                  '(31 -- -- -- -- 24)
                  '(30 29 28 27 26 25)
                    ))
    (SETQ BRD1 (COPY-BRD BRD))
    (SETQ F (LIST '(1 1 1 1 1 1) '(1 0 0 0 0 1)
                  '(1 0 0 0 0 1) '(1 0 0 0 0 1)
                  '(1 0 0 0 0 1) '(1 1 1 1 1 1))))
   
   ((EQUAL CHOICE 3)
    (SETQ N 7)
    (SETQ BRD (LIST '(1 -- 9 -- 25 -- 49)
                  '(-- 3 -- 11 -- 27 --)
                  '(5 -- -- -- -- -- 49)
                  '(-- 15 -- -- -- 29 --)
                  '(17 -- -- -- -- -- 49)
                  '(-- 35 -- 33 -- 31 --)
                  '(37 -- 39 -- 41 -- 43)
                    ))
    (SETQ BRD1 (COPY-BRD BRD))
             (SETQ F (LIST '(1 0 1 0 1 0 1) '(0 1 0 1 0 1 0)
                      '(1 0 0 0 0 0 1) '(0 1 0 0 0 1 0)
                      '(1 0 0 0 0 0 1) '(0 1 0 1 0 1 0)
                      '(1 0 1 0 1 0 1))))
    
  ((EQUAL CHOICE 4)
    (SETQ N 8)
    (SETQ BRD (LIST '(54 -- 52 -- -- 37 -- 35)
                  '(-- -- -- -- -- -- -- --)
                  '(58 -- 48 -- -- 41 -- 31)
                  '(-- -- -- 46 -- -- -- --) 
                  '(-- -- -- -- 16 -- -- --)
                  '(63 -- 5 -- -- 18 -- 26) 
                  '(-- -- -- -- -- -- -- --) 
                  '(1 -- 11 -- -- 20 -- 22)))
   (SETQ BRD1 (COPY-BRD BRD))
            (SETQ F (LIST '(1 0 1 0 0 1 0 1) '(0 0 0 0 0 0 0 0)
                      '(1 0 1 0 0 1 0 1) '(0 0 0 1 0 0 0 0)
                      '(0 0 0 0 1 0 0 0) '(1 0 1 0 0 1 0 1)
                      '(0 0 0 0 0 0 0 0) '(1 0 1 0 0 1 0 1))))
   
   ((EQUAL CHOICE 5)
    (SETQ N 5)
    (SETQ BRD (LIST '(1 -- -- -- 9) 
                  '(-- -- -- -- --) 
                  '(-- -- 21 -- --)
                  '(-- -- -- -- --)
                  '(25 -- -- -- 15)
                  ))
    (SETQ BRD1 (COPY-BRD BRD))
             (SETQ F (LIST '(1 0 0 0 1) '(0 0 0 0 0)
                           '(0 0 1 0 0) '(0 0 0 0 0)
                           '(1 0 0 0 1))))
    
  ((EQUAL CHOICE 6)
    (SETQ N 5)
    (SETQ BRD (LIST '(25 -- -- -- 13) 
                  '(-- -- -- -- --) 
                  '(-- -- 1 -- --)
                  '(-- -- -- -- --)
                  '(21 -- -- -- 17)
                  ))
   (SETQ BRD1 (COPY-BRD BRD))
            (SETQ F (LIST '(1 0 0 0 1) '(0 0 0 0 0)
                          '(0 0 1 0 0) '(0 0 0 0 0)
                          '(1 0 0 0 1))))
     
  ((EQUAL CHOICE 7)
    (SETQ N 7)
    (SETQ BRD (LIST '(49 -- -- -- -- -- 31)
                  '(-- 9 -- 3 -- -- --)
                  '(-- -- -- -- -- -- --)
                  '(-- -- -- 5 -- -- --)
                  '(-- -- -- -- 17 -- --)
                  '(-- -- -- -- -- 25 --)
                  '(-- -- -- -- -- -- 37)
                  ))
   (SETQ BRD1 (COPY-BRD BRD))
           (SETQ F (LIST '(1 0 0 0 0 0 1) '(0 1 0 1 0 0 0)
                         '(0 0 0 0 0 0 0) '(0 0 0 1 0 0 0)
                         '(0 0 0 0 1 0 0) '(0 0 0 0 0 1 0)
                         '(0 0 0 0 0 0 1))))
      
  ((EQUAL CHOICE 8)
    (SETQ N 8)
    (SETQ BRD (LIST '(1 -- -- -- -- -- -- 64)
                  '(-- 7 -- -- -- -- 38 --)
                  '(-- -- -- 14 19 -- -- --)
                  '(-- -- -- -- -- -- -- --) 
                  '(-- -- -- -- -- -- -- --)
                  '(-- -- -- 29 30 -- -- --) 
                  '(-- 48 -- -- -- -- 43 --) 
                  '(50 -- -- -- -- -- -- 57)))
   (SETQ BRD1 (COPY-BRD BRD))
            (SETQ F (LIST '(1 0 0 0 0 0 0 1) '(0 1 0 0 0 0 1 0)
                          '(0 0 0 1 1 0 0 0) '(0 0 0 0 0 0 0 0)
                          '(0 0 0 0 0 0 0 0) '(0 0 0 1 1 0 0 0)
                          '(0 1 0 0 0 0 1 0) '(1 0 0 0 0 0 0 1))))
   
   ((EQUAL CHOICE 9)
    (SETQ N 9)
    (SETQ BRD (LIST '(-- -- -- -- -- -- -- -- --)
                  '(-- 75 -- 9 -- 3 -- 43 --)
                  '(-- -- 79 -- 1 -- 15 -- --)
                  '(-- 77 -- -- -- -- -- 41 --) 
                  '(-- -- 21 -- -- -- 17 -- --)
                  '(-- 67 -- -- -- -- -- 39 --) 
                  '(-- -- 31 -- 29 -- 27 -- --) 
                  '(-- 63 -- 33 -- 35 -- 37 --)
                  '(-- -- -- -- -- -- -- -- --)))
    (SETQ BRD1 (COPY-BRD BRD))
             (SETQ F (LIST '(0 0 0 0 0 0 0 0 0) '(0 1 0 1 0 1 0 1 0)
                           '(0 0 1 0 1 0 1 0 0) '(0 1 0 0 0 0 0 1 0)
                           '(0 0 1 0 0 0 1 0 0) '(0 1 0 0 0 0 0 1 0)
                           '(0 0 1 0 1 0 1 0 0) '(0 1 0 1 0 1 0 1 0)
                           '(0 0 0 0 0 0 0 0 0))))
    
  ((EQUAL CHOICE 10)
    (SETQ N 9)
    (SETQ BRD (LIST '(73 -- 81 -- 11 -- 13 -- 45)
                  '(-- -- -- -- -- -- -- -- --)
                  '(-- -- -- -- 1 -- -- -- --)
                  '(-- -- -- -- -- -- -- -- --) 
                  '(-- 68 -- -- 19 -- -- 40 --)
                  '(-- -- -- -- -- -- 26 -- --) 
                  '(-- -- -- -- 29 -- -- -- --) 
                  '(-- -- 32 -- -- -- 36 -- --)
                  '(61 -- -- -- -- -- -- -- 53)))
   (SETQ BRD1 (COPY-BRD BRD))
            (SETQ F (LIST '(1 0 1 0 1 0 1 0 1) '(0 0 0 0 0 0 0 0 0)
                      '(0 0 0 0 1 0 0 0 0) '(0 0 0 0 0 0 0 0 0)
                      '(0 1 0 0 1 0 0 1 0) '(0 0 0 0 0 0 1 0 0)
                      '(0 0 0 0 1 0 0 0 0) '(0 0 1 0 0 0 1 0 0)
                      '(1 0 0 0 0 0 0 0 1))))
   
  ((EQUAL CHOICE 11)
    (SETQ N 10)
    (SETQ BRD (LIST '(81 -- 79 -- -- -- -- 74 -- 72)
                  '(-- -- -- 15 -- -- 18 -- -- --)
                  '(-- -- -- -- -- -- -- -- -- --)
                  '(-- -- -- -- -- -- -- -- -- --) 
                  '(-- -- 91 -- 1 -- -- -- -- 64)
                  '(-- -- 32 -- -- 9 -- -- -- --) 
                  '(-- -- -- -- -- -- 25 -- -- --) 
                  '(-- -- -- -- -- -- -- 47 -- --)
                  '(-- -- -- -- -- -- -- -- 53 --)
                  '(98 -- 38 -- -- -- -- -- -- 59)))
   (SETQ BRD1 (COPY-BRD BRD))
            (SETQ F (LIST '(1 0 1 0 0 0 0 1 0 1) '(0 0 0 1 0 0 1 0 0 0)
                          '(0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 0)
                          '(0 0 1 0 1 0 0 0 0 1) '(0 0 1 0 0 1 0 0 0 0)
                          '(0 0 0 0 0 0 1 0 0 0) '(0 0 0 0 0 0 0 1 0 0)
                          '(0 0 0 0 0 0 0 0 1 0) '(1 0 1 0 0 0 0 0 0 1))))
   
  ((EQUAL CHOICE 12)
    (SETQ N 10)
    (SETQ BRD (LIST '(68 -- -- -- -- -- -- -- -- 77)
                  '(-- 50 -- -- -- -- -- -- -- --)
                  '(-- -- 20 -- -- -- -- 29 -- --)
                  '(-- -- -- 22 -- -- -- -- -- --) 
                  '(-- -- -- -- 10 -- -- -- -- --)
                  '(-- -- -- -- 1 6 -- -- -- --) 
                  '(-- -- -- -- -- -- 34 -- -- --) 
                  '(-- -- 15 -- -- -- -- 36 -- --)
                  '(-- -- -- -- -- -- -- -- 86 --)
                  '(59 -- -- -- -- -- -- -- -- 100)))
   (SETQ BRD1 (COPY-BRD BRD))
            (SETQ F (LIST '(1 0 0 0 0 0 0 0 0 1) '(0 1 0 0 0 0 0 0 0 0)
                          '(0 0 1 0 0 0 0 1 0 0) '(0 0 0 1 0 0 0 0 0 0)
                          '(0 0 0 0 1 0 0 0 0 0) '(0 0 0 0 1 1 0 0 0 0)
                          '(0 0 0 0 0 0 1 0 0 0) '(0 0 1 0 0 0 0 1 0 0)
                          '(0 0 0 0 0 0 0 0 1 0) '(1 0 0 0 0 0 0 0 0 1))))
   
  ((EQUAL CHOICE 13)
    (SETQ N 11)
    (SETQ BRD (LIST '(1 4 5 6 7 8 21 22 41 42 43)
                  '(2 -- -- -- -- -- -- -- -- -- 44)
                  '(119 -- 13 -- -- -- -- -- -- -- 47)
                  '(120 -- -- -- 104 -- -- -- -- -- 48) 
                  '(121 -- -- -- -- -- -- -- -- -- 51)
                  '(112 -- -- -- -- -- -- -- -- -- 52) 
                  '(111 -- -- -- -- -- -- -- 35 -- 55) 
                  '(96 -- -- 99 -- 83 -- -- -- -- 56)
                  '(95 -- -- -- -- -- -- -- -- -- 57)
                  '(92 -- -- -- -- -- -- -- -- -- 58)
                  '(91 90 89 78 77 76 75 74 73 60 59)))
   (SETQ BRD1 (COPY-BRD BRD))
            (SETQ F (LIST '(1 1 1 1 1 1 1 1 1 1 1) '(1 0 0 0 0 0 0 0 0 0 1)
                          '(1 0 1 0 0 0 0 0 0 0 1) '(1 0 0 0 1 0 0 0 0 0 1)
                          '(1 0 0 0 0 0 0 0 0 0 1) '(1 0 0 0 0 0 0 0 0 0 1)
                          '(1 0 0 0 0 0 0 0 1 0 1) '(1 0 0 1 0 1 0 0 0 0 1)
                          '(1 0 0 0 0 0 0 0 0 0 1) '(1 0 0 0 0 0 0 0 0 0 1)
                          '(1 1 1 1 1 1 1 1 1 1 1))))
   
  ((EQUAL CHOICE 14)
    (SETQ N 13)
    (SETQ BRD (LIST '(17 -- 19 -- 21 -- 45 -- 55 -- 97 -- 161)
                  '(-- 5 -- 7 -- 43 -- 53 -- 95 -- 163 --)
                  '(15 -- -- -- -- -- -- -- -- -- -- -- 159)
                  '(-- 3 -- -- -- -- -- -- -- -- -- 165 --)
                  '(13 -- -- -- -- -- -- -- -- -- -- -- 157) 
                  '(-- 29 -- -- -- -- -- 61 -- -- -- 167 --)
                  '(31 -- -- -- -- -- -- -- -- -- -- -- 155) 
                  '(-- 69 -- -- -- -- -- -- -- -- -- 169 --) 
                  '(71 -- -- -- -- -- -- -- -- -- -- -- 153)
                  '(-- 73 -- -- -- -- -- 85 -- -- -- 149 --)
                  '(117 -- -- -- -- -- -- -- -- -- -- -- 147)
                  '(-- 121 -- 125 -- 129 -- 133 -- 137 -- 145 --)
                  '(119 -- 123 -- 127 -- 131 -- 135 -- 141 -- 143)))
   (SETQ BRD1 (COPY-BRD BRD))
            (SETQ F (LIST '(1 0 1 0 1 0 1 0 1 0 1 0 1) '(0 1 0 1 0 1 0 1 0 1 0 1 0)
                          '(1 0 0 0 0 0 0 0 0 0 0 0 1) '(0 1 0 0 0 0 0 0 0 0 0 1 0)
                          '(1 0 0 0 0 0 0 0 0 0 0 0 1) '(0 1 0 0 0 0 0 1 0 0 0 1 0)
                          '(1 0 0 0 0 0 0 0 0 0 0 0 1) '(0 1 0 0 0 0 0 0 0 0 0 1 0)
                          '(1 0 0 0 0 0 0 0 0 0 0 0 1) '(0 1 0 0 0 0 0 1 0 0 0 1 0)
                          '(1 0 0 0 0 0 0 0 0 0 0 0 1) '(0 1 0 1 0 1 0 1 0 1 0 1 0) 
                          '(1 0 1 0 1 0 1 0 1 0 1 0 1))))

   
  ((EQUAL CHOICE 15)
    (SETQ N 15)
    (SETQ BRD (LIST '(41 -- -- -- -- -- -- -- -- -- -- -- -- -- 221)
                  '(-- 43 -- -- -- -- -- -- -- -- -- -- -- 77 --)
                  '(-- -- 197 -- -- -- -- -- -- -- -- -- -- -- --)
                  '(-- -- -- 89 -- -- -- -- -- -- -- -- -- -- --)
                  '(-- -- -- -- 159 -- -- -- -- -- 143 -- -- -- --)
                  '(-- -- -- -- -- 119 -- -- -- -- -- -- -- -- --)
                  '(-- -- -- -- -- -- 137 -- -- -- -- -- -- -- --) 
                  '(-- -- -- -- -- -- -- 135 -- -- -- -- -- -- --)
                  '(-- -- -- -- -- -- 123 -- 125 -- -- -- -- -- --) 
                  '(-- -- -- -- -- -- -- -- -- 149 -- -- -- -- --) 
                  '(-- -- -- -- 97 -- -- -- -- -- 103 -- -- -- --)
                  '(-- -- -- -- -- -- -- -- -- -- -- 179 -- -- --)
                  '(-- -- -- -- -- -- -- -- -- -- -- -- 65 -- --)
                  '(-- -- -- -- -- -- -- -- -- -- -- -- -- 1 --)
                  '(27 -- -- -- -- -- -- -- -- -- -- -- -- -- 225)))
   (SETQ BRD1 (COPY-BRD BRD))
            (SETQ F (LIST '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 1) '(0 1 0 0 0 0 0 0 0 0 0 0 0 1 0)
                          '(0 0 1 0 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)
                          '(0 0 0 0 1 0 0 0 0 0 1 0 0 0 0) '(0 0 0 0 0 1 0 0 0 0 0 0 0 0 0)
                          '(0 0 0 0 0 0 1 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 1 0 0 0 0 0 0 0)
                          '(0 0 0 0 0 0 1 0 1 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 1 0 0 0 0 0)
                          '(0 0 0 0 1 0 0 0 0 0 1 0 0 0 0) '(0 0 0 0 0 0 0 0 0 0 0 1 0 0 0) 
                          '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0) '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)
                          '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 1))))


   
  ((EQUAL CHOICE 16)
    (SETQ N 9)
    (SETQ BRD (LIST '(43 -- 41 -- 39 -- 33 -- 27)
                  '(-- -- -- -- -- -- -- -- --)
                  '(51 -- -- -- -- -- -- -- 25)
                  '(-- -- -- -- -- -- -- -- --) 
                  '(53 -- -- -- -- -- -- -- 23)
                  '(-- -- -- -- -- -- -- -- --) 
                  '(55 -- -- -- -- -- -- -- 21) 
                  '(-- -- -- -- -- -- -- -- --)
                  '(57 -- 81 -- 71 -- 7 -- 9)))
   (SETQ BRD1 (COPY-BRD BRD))
        (SETQ F (LIST '(1 0 1 0 1 0 1 0 1) '(0 0 0 0 0 0 0 0 0)
                      '(1 0 0 0 0 0 0 0 1) '(0 0 0 0 0 0 0 0 0)
                      '(1 0 0 0 0 0 0 0 1) '(0 0 0 0 0 0 0 0 0)
                      '(1 0 0 0 0 0 0 0 1) '(0 0 0 0 0 0 0 0 0)
                      '(1 0 1 0 1 0 1 0 1))))
   
   
  ((EQUAL CHOICE 17)
    (SETQ N 9)
    (SETQ BRD (LIST '(1 -- 3 -- 19 -- 23 -- 25)
                  '(-- -- -- -- -- -- -- -- --)
                  '(7 -- -- -- -- -- -- -- 27)
                  '(-- -- -- -- -- -- -- -- --) 
                  '(75 -- -- -- -- -- -- -- 35)
                  '(-- -- -- -- -- -- -- -- --) 
                  '(81 -- -- -- -- -- -- -- 45) 
                  '(-- -- -- -- -- -- -- -- --)
                  '(61 -- 59 -- 57 -- 48 -- 47)))
   (SETQ BRD1 (COPY-BRD BRD))
   (SETQ F (LIST '(1 0 1 0 1 0 1 0 1) '(0 0 0 0 0 0 0 0 0)
                      '(1 0 0 0 0 0 0 0 1) '(0 0 0 0 0 0 0 0 0)
                      '(1 0 0 0 0 0 0 0 1) '(0 0 0 0 0 0 0 0 0)
                      '(1 0 0 0 0 0 0 0 1) '(0 0 0 0 0 0 0 0 0)
                      '(1 0 1 0 1 0 1 0 1))))

            
   (T (PRINC "INVALID OPTION")))  
  
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; INSERT THE INPUT INTO THE BOARD   
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (DEFUN INSERT-BRD (POSX POSY BRD1 VALUE)
    (SETF (NTH  (- POSY 1) (NTH (- N POSX) BRD1))
      VALUE)
    
    )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; CHECK FOR VALIDATION  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  (DEFUN CHECK-VALUE (VALUE N)
   (COND ((EQUAL N 3)
           (COND ((>= VALUE 10) (PRINC "THE ENTERED VALUE IS NOT ALLOWED") (TAKE-INPUT N))
           ((<= VALUE 0) (PRINC "THE ENTERED VALUE IS NOT ALLOWED") (TAKE-INPUT N))
           (T (CONTINUE))))
    
    (T (COND ((>= VALUE 64) (PRINC "THE ENTERED VALUE IS NOT ALLOWED") (TAKE-INPUT N))
             ((<= VALUE 0) (PRINC "THE ENTERED VALUE IS NOT ALLOWED") (TAKE-INPUT N))
             (T (CONTINUE))))
         )
  
    )
  
  (DEFUN CHECK-ROW-VALUE (POSX N)
      (COND ((>= POSX (+ N 1)) (PRINC "THE ENTERED VALUE FOR ROW IS NOT ALLOWED") (TAKE-INPUT N))
            ((<= POSX 0) (PRINC "THE ENTERED VALUE FOR ROW IS NOT ALLOWED") (TAKE-INPUT N))
            (T (CONTINUE)))
           
    )
  
  (DEFUN CHECK-COL-VALUE (POSY N)
    (COND ((>= POSY (+ N 1)) (PRINC "THE ENTERED VALUE FOR COLUMN IS NOT ALLOWED") (TAKE-INPUT N))
          ((<= POSY 0) (PRINC "THE ENTERED VALUE FOR COLUMN IS NOT ALLOWED") (TAKE-INPUT N))
          (T (CONTINUE)))
    )
  
  (DEFUN OVERWRITE (POSX POSY F N)
    (SETQ TEMP (NTH  (- POSY 1) (NTH (- N POSX) F))) 
    (COND ((EQUAL TEMP '1) 
          (PRINC "CANNOT INSERT IN THIS POSITION") (TAKE-INPUT N))
          (T (CONTINUE)))
    
          
      )  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; CHECK IF THE BOARD IS COMPLETE   
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (DEFUN EXTRACT-ROW (BRD1 N) 
    (COND ((NULL BRD1) NIL)
           (T (CHECK-COMPLETE (CAR BRD1) BRD1 N) (EXTRACT-ROW (CDR BRD1) N)))
   
    
    )
  
  (DEFUN CHECK-COMPLETE (ROW BRD1 N)
    (COND ((NULL ROW) (EXTRACT-ROW (CDR BRD1) N))
          ((NUMBERP (CAR ROW)) (CHECK-COMPLETE (CDR ROW) BRD1 N))
          (T (EQUAL (CAR ROW) '-) (TAKE-INPUT N)))
     
    )
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; EVALUATE WIN OR LOSS  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
  ;;;;;;;RETRIEVE SQUARE VALUE;;;;;;;;;;
  
  (DEFUN SQR (ROW COL BRD1)
      (NTH (1- COL) (NTH (1- ROW) BRD1))  
    )
  
  ;;;;;;;CHECK FOR CORNER SQUARES;;;;;;;;;;
  
  (DEFUN CHECK-IF-ONE-OR-N (N PAR)
    (COND ((EQUAL (OR (EQUAL PAR 1) (EQUAL PAR (* N N))) T) (RETURN-FROM CHECK-IF-ONE-OR-N T))
          (T (RETURN-FROM CHECK-IF-ONE-OR-N NIL)))
    )

  
  
 (DEFUN CHECK-FOR-FIRST-SQR (N BRD1)
    
    (SETQ CFRC-1 (+ (SQR 1 1 BRD1) 1))
    (SETQ CFRC-2 (- (SQR 1 1 BRD1) 1))
    (SETQ CFRC-3 (SQR 1 2 BRD1)) 
    (SETQ CFRC-4 (SQR 2 1 BRD1))
    (SETQ CFRC (CHECK-IF-ONE-OR-N N (SQR 1 1 BRD1)))
    
    (COND 
     ((EQUAL CFRC NIL)
           (COND
               ((EQUAL (AND (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                            (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2))) T)
                            (RETURN-FROM CHECK-FOR-FIRST-SQR T))
               (T (RETURN-FROM CHECK-FOR-FIRST-SQR NIL))
            )) 
          
     ((EQUAL CFRC T)
           (COND
               ((EQUAL (OR (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                           (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2))) T)
                           (RETURN-FROM CHECK-FOR-FIRST-SQR T))
               (T (RETURN-FROM CHECK-FOR-FIRST-SQR NIL))))
                    
     (T (RETURN-FROM CHECK-FOR-FIRST-SQR NIL)))
    
    )
  
  (DEFUN CHECK-FOR-RIGHT-CORNER-SQR (N BRD1)
    
    (SETQ CFRC-1 (+ (SQR 1 N BRD1) 1))
    (SETQ CFRC-2 (- (SQR 1 N BRD1) 1))
    (SETQ CFRC-3 (SQR 1 (- N 1) BRD1)) 
    (SETQ CFRC-4 (SQR 2 N BRD1))
    (SETQ CFRC (CHECK-IF-ONE-OR-N N (SQR 1 N BRD1)))
    
    (COND 
     ((EQUAL CFRC NIL)
           (COND
               ((EQUAL (AND (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                            (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2))) T)
                            (RETURN-FROM CHECK-FOR-RIGHT-CORNER-SQR T))
               (T (RETURN-FROM CHECK-FOR-RIGHT-CORNER-SQR NIL))
            )) 
          
     ((EQUAL CFRC T)
           (COND
               ((EQUAL (OR (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                           (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2))) T)
                           (RETURN-FROM CHECK-FOR-RIGHT-CORNER-SQR T))
               (T (RETURN-FROM CHECK-FOR-RIGHT-CORNER-SQR NIL))))
                    
     (T (RETURN-FROM CHECK-FOR-RIGHT-CORNER-SQR NIL)))
    
    )
  
    (DEFUN CHECK-FOR-LEFT-BOTTOM-CORNER-SQR (N BRD1)
    
    (SETQ CFRC-1 (+ (SQR N 1 BRD1) 1))
    (SETQ CFRC-2 (- (SQR N 1 BRD1) 1))
    (SETQ CFRC-3 (SQR (1- N) 1 BRD1)) 
    (SETQ CFRC-4 (SQR N 2 BRD1))
    (SETQ CFRC (CHECK-IF-ONE-OR-N N (SQR N 1 BRD1)))
    
    (COND 
     ((EQUAL CFRC NIL)
           (COND
               ((EQUAL (AND (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                            (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2))) T)
                            (RETURN-FROM CHECK-FOR-LEFT-BOTTOM-CORNER-SQR T))
               (T (RETURN-FROM CHECK-FOR-LEFT-BOTTOM-CORNER-SQR NIL))
            )) 
          
     ((EQUAL CFRC T)
           (COND
               ((EQUAL (OR (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                           (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2))) T)
                           (RETURN-FROM CHECK-FOR-LEFT-BOTTOM-CORNER-SQR T))
               (T (RETURN-FROM CHECK-FOR-LEFT-BOTTOM-CORNER-SQR NIL))))
                    
     (T (RETURN-FROM CHECK-FOR-LEFT-BOTTOM-CORNER-SQR NIL)))
    
    )
  
    (DEFUN CHECK-FOR-RIGHT-BOTTOM-CORNER-SQR (N BRD1)
    
    (SETQ CFRC-1 (+ (SQR N N BRD1) 1))
    (SETQ CFRC-2 (- (SQR N N BRD1) 1))
    (SETQ CFRC-3 (SQR (1- N) N BRD1)) 
    (SETQ CFRC-4 (SQR N (1- N) BRD1))
    (SETQ CFRC (CHECK-IF-ONE-OR-N N (SQR N N BRD1)))
    
    (COND 
     ((EQUAL CFRC NIL)
           (COND
               ((EQUAL (AND (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                            (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2))) T)
                            (RETURN-FROM CHECK-FOR-RIGHT-BOTTOM-CORNER-SQR T))
               (T (RETURN-FROM CHECK-FOR-RIGHT-BOTTOM-CORNER-SQR NIL))
            )) 
          
     ((EQUAL CFRC T)
           (COND
               ((EQUAL (OR (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                           (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2))) T)
                           (RETURN-FROM CHECK-FOR-RIGHT-BOTTOM-CORNER-SQR T))
               (T (RETURN-FROM CHECK-FOR-RIGHT-BOTTOM-CORNER-SQR NIL))))
                    
     (T (RETURN-FROM CHECK-FOR-RIGHT-BOTTOM-CORNER-SQR NIL)))
    
    )
  
   ;;;;;;;CHECK FOR BOUNDARY SQUARES;;;;;;;;;;
  
  (DEFUN CHECK-TOP-ROW (N BRD1)
    (LOOP FOR X FROM 2 TO (- N 1) DO
    
    (SETQ CFRC-1 (+ (SQR 1 X BRD1) 1))
    (SETQ CFRC-2 (- (SQR 1 X BRD1) 1))
    (SETQ CFRC-3 (SQR 1 (- X 1) BRD1)) 
    (SETQ CFRC-4 (SQR 2 X BRD1))
    (SETQ CFRC-5 (SQR 1 (+ X 1) BRD1))
    (SETQ CFRC (CHECK-IF-ONE-OR-N N (SQR 1 X BRD1)))
          
    
    (COND 
     ((EQUAL CFRC NIL)
           (COND
               ((EQUAL (OR (OR (AND (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                    (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))
                       
                               (AND (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                    (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))))
                       
                               (AND (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))
                                    (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))) T)
                              
                            (SETQ CHECK T))
               (T (SETQ CHECK NIL)))
      
            (COND ((EQUAL CHECK T) (CONTINUE))
                   (T (RETURN-FROM CHECK-TOP-ROW NIL)))
            ) 
          
     ((EQUAL CFRC T)
           (COND
               ((EQUAL (OR (OR (OR (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                   (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))
                       
                               (OR (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                   (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))))
                       
                               (OR (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))
                                   (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))) T)
                       
                       (SETQ CHECK T))
            
            (T (SETQ CHECK NIL)))
      
      (COND ((EQUAL CHECK T) (CONTINUE))
                (T (RETURN-FROM CHECK-TOP-ROW NIL) (PRINT "T")))
      
      )))
    
    (COND ((EQUAL CHECK T) (RETURN-FROM CHECK-TOP-ROW T))
          (T (RETURN-FROM CHECK-TOP-ROW NIL)))
    
    )
  
  (DEFUN CHECK-BOTTOM-ROW (N BRD1)
    (LOOP FOR X FROM 2 TO (- N 1) DO
    
    (SETQ CFRC-1 (+ (SQR N X BRD1) 1))
    (SETQ CFRC-2 (- (SQR N X BRD1) 1))
    (SETQ CFRC-3 (SQR N (- X 1) BRD1)) 
    (SETQ CFRC-4 (SQR (- N 1) X BRD1))
    (SETQ CFRC-5 (SQR N (+ X 1) BRD1))
    (SETQ CFRC (CHECK-IF-ONE-OR-N N (SQR N X BRD1)))
          
    
    (COND 
     ((EQUAL CFRC NIL)
           (COND
               ((EQUAL (OR (OR (AND (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                    (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))
                       
                               (AND (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                    (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))))
                       
                               (AND (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))
                                    (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))) T)
                              
                            (SETQ CHECK T))
               (T (SETQ CHECK NIL)))
      
            (COND ((EQUAL CHECK T) (CONTINUE))
                   (T (RETURN-FROM CHECK-BOTTOM-ROW NIL)))
            ) 
          
     ((EQUAL CFRC T)
           (COND
               ((EQUAL (OR (OR (OR (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                   (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))
                       
                               (OR (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                   (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))))
                       
                               (OR (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))
                                   (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))) T)
                       
                       (SETQ CHECK T))
            
            (T (SETQ CHECK NIL)))
      
      (COND ((EQUAL CHECK T) (CONTINUE))
                (T (RETURN-FROM CHECK-BOTTOM-ROW NIL)))
      
      )))
    
    (COND ((EQUAL CHECK T) (RETURN-FROM CHECK-BOTTOM-ROW T))
          (T (RETURN-FROM CHECK-BOTTOM-ROW NIL)))
    
    )

  (DEFUN CHECK-LEFTMOST-ROW (N BRD1)
    (LOOP FOR X FROM 2 TO (- N 1) DO
    
    (SETQ CFRC-1 (+ (SQR X 1 BRD1) 1))
    (SETQ CFRC-2 (- (SQR X 1 BRD1) 1))
    (SETQ CFRC-3 (SQR (- X 1) 1 BRD1)) 
    (SETQ CFRC-4 (SQR X 2 BRD1))
    (SETQ CFRC-5 (SQR (+ X 1) 1 BRD1))
    (SETQ CFRC (CHECK-IF-ONE-OR-N N (SQR X 1 BRD1)))
          
    
    (COND 
     ((EQUAL CFRC NIL)
           (COND
               ((EQUAL (OR (OR (AND (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                    (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))
                       
                               (AND (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                    (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))))
                       
                               (AND (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))
                                    (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))) T)
                              
                            (SETQ CHECK T))
               (T (SETQ CHECK NIL)))
      
            (COND ((EQUAL CHECK T) (CONTINUE))
                   (T (RETURN-FROM CHECK-LEFTMOST-ROW NIL)))
            ) 
          
     ((EQUAL CFRC T)
           (COND
               ((EQUAL (OR (OR (OR (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                   (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))
                       
                               (OR (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                   (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))))
                       
                               (OR (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))
                                   (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))) T)
                       
                       (SETQ CHECK T))
            
            (T (SETQ CHECK NIL)))
      
      (COND ((EQUAL CHECK T) (CONTINUE))
                (T (RETURN-FROM CHECK-LEFTMOST-ROW NIL)))
      
      )))
    
    (COND ((EQUAL CHECK T) (RETURN-FROM CHECK-LEFTMOST-ROW T))
          (T (RETURN-FROM CHECK-LEFTMOST-ROW NIL)))
    
    )
  
    (DEFUN CHECK-RIGHTMOST-ROW (N BRD1)
    (LOOP FOR X FROM 2 TO (- N 1) DO
    
    (SETQ CFRC-1 (+ (SQR X N BRD1) 1))
    (SETQ CFRC-2 (- (SQR X N BRD1) 1))
    (SETQ CFRC-3 (SQR (- X 1) N BRD1)) 
    (SETQ CFRC-4 (SQR X (- N 1) BRD1))
    (SETQ CFRC-5 (SQR (+ X 1) N BRD1))
    (SETQ CFRC (CHECK-IF-ONE-OR-N N (SQR X N BRD1)))
          
    
    (COND 
     ((EQUAL CFRC NIL)
           (COND
               ((EQUAL (OR (OR (AND (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                    (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))
                       
                               (AND (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                    (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))))
                       
                               (AND (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))
                                    (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))) T)
                              
                            (SETQ CHECK T))
               (T (SETQ CHECK NIL)))
      
            (COND ((EQUAL CHECK T) (CONTINUE))
                   (T (RETURN-FROM CHECK-RIGHTMOST-ROW NIL)))
            ) 
          
     ((EQUAL CFRC T)
           (COND
               ((EQUAL (OR (OR (OR (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                   (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))
                       
                               (OR (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                   (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))))
                       
                               (OR (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))
                                   (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))) T)
                       
                       (SETQ CHECK T))
            
            (T (SETQ CHECK NIL)))
      
      (COND ((EQUAL CHECK T) (CONTINUE))
                (T (RETURN-FROM CHECK-RIGHTMOST-ROW NIL)))
      
      )))
    
    (COND ((EQUAL CHECK T) (RETURN-FROM CHECK-RIGHTMOST-ROW T))
          (T (RETURN-FROM CHECK-RIGHTMOST-ROW NIL)))
    
    )


  ;;;;;;;CHECK FOR LEFTOVER SQUARES;;;;;;;;;;
  
  (DEFUN CHECK-CENTREMOST-SQR (N BRD1)
    (LOOP FOR Y FROM 2 TO (- N 1) DO
    (LOOP FOR X FROM 2 TO (- N 1) DO
    
    (SETQ CFRC-1 (+ (SQR Y X BRD1) 1))
          
    (SETQ CFRC-2 (- (SQR Y X BRD1) 1))
    (SETQ CFRC-3 (SQR Y (- X 1) BRD1)) 
    (SETQ CFRC-4 (SQR (- Y 1) X BRD1))      
    (SETQ CFRC-5 (SQR Y (+ X 1) BRD1))
    (SETQ CFRC-6 (SQR (+ Y 1) X BRD1))
    (SETQ CFRC (CHECK-IF-ONE-OR-N N (SQR Y X BRD1)))
         
    
    (COND 
     ((EQUAL CFRC NIL)
           (COND
               ((EQUAL (OR (OR (AND (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                (OR (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2))
                                (OR (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))
                                    (OR (EQUAL CFRC-6 CFRC-1) (EQUAL CFRC-6 CFRC-2)))))
                                        
                       
                               (AND (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2))
                                (OR (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))
                                    (OR (EQUAL CFRC-6 CFRC-1) (EQUAL CFRC-6 CFRC-2)))))
                                                  
                               (AND (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))
                                    (OR (EQUAL CFRC-6 CFRC-1) (EQUAL CFRC-6 CFRC-2)))) T)
                              
                (SETQ CHECK T))            
               (T (SETQ CHECK NIL)))
      
            (COND ((EQUAL CHECK T) (CONTINUE))
                   (T (RETURN-FROM CHECK-CENTREMOST-SQR NIL)))
            ) 
          
     ((EQUAL CFRC T)
           (COND
               ((EQUAL (OR (OR (OR (OR (OR (OR (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                   (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2)))
                       
                               (OR (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                   (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))))
                       
                               (OR (OR (EQUAL CFRC-3 CFRC-1) (EQUAL CFRC-3 CFRC-2))
                                   (OR (EQUAL CFRC-6 CFRC-1) (EQUAL CFRC-6 CFRC-2))))
                           
                               (OR (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2))
                                   (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))))
                           
                               (OR (OR (EQUAL CFRC-4 CFRC-1) (EQUAL CFRC-4 CFRC-2))
                                   (OR (EQUAL CFRC-6 CFRC-1) (EQUAL CFRC-6 CFRC-2))))
                           
                               (OR (OR (EQUAL CFRC-5 CFRC-1) (EQUAL CFRC-5 CFRC-2))
                                   (OR (EQUAL CFRC-6 CFRC-1) (EQUAL CFRC-6 CFRC-2)))) T)
                       
                       (SETQ CHECK T))
            
            (T (SETQ CHECK NIL)))
      
      (COND ((EQUAL CHECK T) (CONTINUE))
                (T (RETURN-FROM CHECK-CENTREMOST-SQR NIL)))
      
      ) 
          
      (COND ((EQUAL CHECK T) (CONTINUE))
                (T (RETURN-FROM CHECK-CENTREMOST-SQR NIL)))
     
     
          
          )))
    
    (COND ((EQUAL CHECK T) (RETURN-FROM CHECK-CENTREMOST-SQR T))
          (T (RETURN-FROM CHECK-CENTREMOST-SQR NIL)))
    
    )
   

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; TAKE INPUT FROM USER IN X, Y, VALUE FORMAT   
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  

  (DEFUN TAKE-INPUT(N)

    (TERPRI)
    (PRINC "ENTER THE NUMBER TO BE INSERTED IN X,Y,VALUE FORMAT : ")
    (SETQ POSX (READ) POSY (READ) VALUE (READ))
        
    (CHECK-VALUE VALUE N)
    (CHECK-ROW-VALUE POSX N)
    (CHECK-COL-VALUE POSY N)
    (OVERWRITE POSX POSY F N)
   
    (INSERT-BRD POSX POSY BRD1 VALUE)
    (PBOARD BRD1 N)
    
    (EXTRACT-ROW BRD1 N)
    
    (SETQ A (CHECK-FOR-FIRST-SQR N BRD1))
    (SETQ B (CHECK-FOR-RIGHT-CORNER-SQR N BRD1))
    (SETQ C (CHECK-FOR-LEFT-BOTTOM-CORNER-SQR N BRD1))
    (SETQ D (CHECK-FOR-RIGHT-BOTTOM-CORNER-SQR N BRD1))
    (SETQ E (CHECK-TOP-ROW N BRD1))
    (SETQ F (CHECK-BOTTOM-ROW N BRD1))
    (SETQ G (CHECK-LEFTMOST-ROW N BRD1))
    (SETQ H (CHECK-RIGHTMOST-ROW N BRD1))
    (SETQ I (CHECK-CENTREMOST-SQR N BRD1))
    
    (SETQ OPT-1 (AND (AND A B) (AND C D)))
    (SETQ OPT-2 (AND (AND E F) (AND G H)))
    (SETQ OPT-3 (AND (AND OPT-1 OPT-2) I))
    
    (DEFUN DECLARE-RESULT()
      (COND ((EQUAL OPT-3 T) 
             (PRINC "YOU WIN") (TERPRI)
             (PRINC "DO YOU WANT TO PLAY ANOTHER GAME? ENTER Y/N: ")
             (SETQ INPUT (READ))
             (COND ((EQUAL INPUT 'Y) (NUMBRIX))
                   (T (PRINC "GAME OVER: EXIT")
                      (RETURN-FROM DECLARE-RESULT NIL))))

             
          (T (PRINC "YOU LOSE") (TERPRI) 
             (PRINC "DO YOU WANT TO PLAY ANOTHER GAME? ENTER Y/N: ") 
               (SETQ INPUT (READ))
                     (COND ((EQUAL INPUT 'Y) (NUMBRIX))
                           (T (PRINC "GAME OVER: EXIT") 
                              (RETURN-FROM DECLARE-RESULT NIL))))
            )

      )
    
    (DECLARE-RESULT)
    (RETURN-FROM NUMBRIX NIL)
        
    )
      
  (TAKE-INPUT N)
 

  )

(DEFUN COMPUTER()
  (PRINC "PLAYING IN AUTOMATIC PLAY MODE")
  (SETQ REPLAY T)
  (LOOP WHILE (EQUAL REPLAY T) DO
        (TERPRI) (PRINT "ENTER YOUR BOARD CHOICES FROM 1-17: ")
        (SETQ CHOICE (READ))
(COND 
   ((EQUAL CHOICE 1)
   (SETQ N 5)       
   (SETQ BRD (LIST '(1 -- -- -- 9) 
                  '(-- -- 6 -- --) 
                  '(-- 18 -- 14 --)
                  '(-- -- 16 -- --)
                  '(21 -- -- -- 25)
                   ))
   (SETQ BRD1 (COPY-BRD BRD))
   (PLAY BRD1))
    
   
   ((EQUAL CHOICE 2)
    (SETQ N 6)
    (SETQ BRD (LIST '(3 4 9 10 15 16)
                  '(2 -- -- -- -- 17)
                  '(1 -- -- -- -- 18)
                  '(36 -- -- -- -- 19)
                  '(31 -- -- -- -- 24)
                  '(30 29 28 27 26 25)
                    ))
    (SETQ BRD1 (COPY-BRD BRD))
    (PLAY BRD1))
   
   ((EQUAL CHOICE 3)
    (SETQ N 7)
    (SETQ BRD (LIST '(1 -- 9 -- 25 -- 49)
                  '(-- 3 -- 11 -- 27 --)
                  '(5 -- -- -- -- -- 49)
                  '(-- 15 -- -- -- 29 --)
                  '(17 -- -- -- -- -- 49)
                  '(-- 35 -- 33 -- 31 --)
                  '(37 -- 39 -- 41 -- 43)
                    ))
    (SETQ BRD1 (COPY-BRD BRD))
             (PLAY BRD1))
    
  ((EQUAL CHOICE 4)
    (SETQ N 8)
    (SETQ BRD (LIST '(54 -- 52 -- -- 37 -- 35)
                  '(-- -- -- -- -- -- -- --)
                  '(58 -- 48 -- -- 41 -- 31)
                  '(-- -- -- 46 -- -- -- --) 
                  '(-- -- -- -- 16 -- -- --)
                  '(63 -- 5 -- -- 18 -- 26) 
                  '(-- -- -- -- -- -- -- --) 
                  '(1 -- 11 -- -- 20 -- 22)))
   (SETQ BRD1 (COPY-BRD BRD))
            (PLAY BRD1))
   
   ((EQUAL CHOICE 5)
    (SETQ N 5)
    (SETQ BRD (LIST '(1 -- -- -- 9) 
                  '(-- -- -- -- --) 
                  '(-- -- 21 -- --)
                  '(-- -- -- -- --)
                  '(25 -- -- -- 15)
                  ))
    (SETQ BRD1 (COPY-BRD BRD))
             (PLAY BRD1))
    
  ((EQUAL CHOICE 6)
    (SETQ N 5)
    (SETQ BRD (LIST '(25 -- -- -- 13) 
                  '(-- -- -- -- --) 
                  '(-- -- 1 -- --)
                  '(-- -- -- -- --)
                  '(21 -- -- -- 17)
                  ))
   (SETQ BRD1 (COPY-BRD BRD))
            (PLAY BRD1))
     
  ((EQUAL CHOICE 7)
    (SETQ N 7)
    (SETQ BRD (LIST '(49 -- -- -- -- -- 31)
                  '(-- 9 -- 3 -- -- --)
                  '(-- -- -- -- -- -- --)
                  '(-- -- -- 5 -- -- --)
                  '(-- -- -- -- 17 -- --)
                  '(-- -- -- -- -- 25 --)
                  '(-- -- -- -- -- -- 37)
                  ))
   (SETQ BRD1 (COPY-BRD BRD))
           (PLAY BRD1))
      
  ((EQUAL CHOICE 8)
    (SETQ N 8)
    (SETQ BRD (LIST '(1 -- -- -- -- -- -- 64)
                  '(-- 7 -- -- -- -- 38 --)
                  '(-- -- -- 14 19 -- -- --)
                  '(-- -- -- -- -- -- -- --) 
                  '(-- -- -- -- -- -- -- --)
                  '(-- -- -- 29 30 -- -- --) 
                  '(-- 48 -- -- -- -- 43 --) 
                  '(50 -- -- -- -- -- -- 57)))
   (SETQ BRD1 (COPY-BRD BRD))
            (PLAY BRD1))
   
   ((EQUAL CHOICE 9)
    (SETQ N 9)
    (SETQ BRD (LIST '(-- -- -- -- -- -- -- -- --)
                  '(-- 75 -- 9 -- 3 -- 43 --)
                  '(-- -- 79 -- 1 -- 15 -- --)
                  '(-- 77 -- -- -- -- -- 41 --) 
                  '(-- -- 21 -- -- -- 17 -- --)
                  '(-- 67 -- -- -- -- -- 39 --) 
                  '(-- -- 31 -- 29 -- 27 -- --) 
                  '(-- 63 -- 33 -- 35 -- 37 --)
                  '(-- -- -- -- -- -- -- -- --)))
    (SETQ BRD1 (COPY-BRD BRD))
             (PLAY BRD1))
    
  ((EQUAL CHOICE 10)
    (SETQ N 9)
    (SETQ BRD (LIST '(73 -- 81 -- 11 -- 13 -- 45)
                  '(-- -- -- -- -- -- -- -- --)
                  '(-- -- -- -- 1 -- -- -- --)
                  '(-- -- -- -- -- -- -- -- --) 
                  '(-- 68 -- -- 19 -- -- 40 --)
                  '(-- -- -- -- -- -- 26 -- --) 
                  '(-- -- -- -- 29 -- -- -- --) 
                  '(-- -- 32 -- -- -- 36 -- --)
                  '(61 -- -- -- -- -- -- -- 53)))
   (SETQ BRD1 (COPY-BRD BRD))
            (PLAY BRD1))
   
  ((EQUAL CHOICE 11)
    (SETQ N 10)
    (SETQ BRD (LIST '(81 -- 79 -- -- -- -- 74 -- 72)
                  '(-- -- -- 15 -- -- 18 -- -- --)
                  '(-- -- -- -- -- -- -- -- -- --)
                  '(-- -- -- -- -- -- -- -- -- --) 
                  '(-- -- 91 -- 1 -- -- -- -- 64)
                  '(-- -- 32 -- -- 9 -- -- -- --) 
                  '(-- -- -- -- -- -- 25 -- -- --) 
                  '(-- -- -- -- -- -- -- 47 -- --)
                  '(-- -- -- -- -- -- -- -- 53 --)
                  '(98 -- 38 -- -- -- -- -- -- 59)))
   (SETQ BRD1 (COPY-BRD BRD))
            (PLAY BRD1))
   
  ((EQUAL CHOICE 12)
    (SETQ N 10)
    (SETQ BRD (LIST '(68 -- -- -- -- -- -- -- -- 77)
                  '(-- 50 -- -- -- -- -- -- -- --)
                  '(-- -- 20 -- -- -- -- 29 -- --)
                  '(-- -- -- 22 -- -- -- -- -- --) 
                  '(-- -- -- -- 10 -- -- -- -- --)
                  '(-- -- -- -- 1 6 -- -- -- --) 
                  '(-- -- -- -- -- -- 34 -- -- --) 
                  '(-- -- 15 -- -- -- -- 36 -- --)
                  '(-- -- -- -- -- -- -- -- 86 --)
                  '(59 -- -- -- -- -- -- -- -- 100)))
   (SETQ BRD1 (COPY-BRD BRD))
            (PLAY BRD1))
   
  ((EQUAL CHOICE 13)
    (SETQ N 11)
    (SETQ BRD (LIST '(1 4 5 6 7 8 21 22 41 42 43)
                  '(2 -- -- -- -- -- -- -- -- -- 44)
                  '(119 -- 13 -- -- -- -- -- -- -- 47)
                  '(120 -- -- -- 104 -- -- -- -- -- 48) 
                  '(121 -- -- -- -- -- -- -- -- -- 51)
                  '(112 -- -- -- -- -- -- -- -- -- 52) 
                  '(111 -- -- -- -- -- -- -- 35 -- 55) 
                  '(96 -- -- 99 -- 83 -- -- -- -- 56)
                  '(95 -- -- -- -- -- -- -- -- -- 57)
                  '(92 -- -- -- -- -- -- -- -- -- 58)
                  '(91 90 89 78 77 76 75 74 73 60 59)))
   (SETQ BRD1 (COPY-BRD BRD))
            (PLAY BRD1))
   
  ((EQUAL CHOICE 14)
    (SETQ N 13)
    (SETQ BRD (LIST '(17 -- 19 -- 21 -- 45 -- 55 -- 97 -- 161)
                  '(-- 5 -- 7 -- 43 -- 53 -- 95 -- 163 --)
                  '(15 -- -- -- -- -- -- -- -- -- -- -- 159)
                  '(-- 3 -- -- -- -- -- -- -- -- -- 165 --)
                  '(13 -- -- -- -- -- -- -- -- -- -- -- 157) 
                  '(-- 29 -- -- -- -- -- 61 -- -- -- 167 --)
                  '(31 -- -- -- -- -- -- -- -- -- -- -- 155) 
                  '(-- 69 -- -- -- -- -- -- -- -- -- 169 --) 
                  '(71 -- -- -- -- -- -- -- -- -- -- -- 153)
                  '(-- 73 -- -- -- -- -- 85 -- -- -- 149 --)
                  '(117 -- -- -- -- -- -- -- -- -- -- -- 147)
                  '(-- 121 -- 125 -- 129 -- 133 -- 137 -- 145 --)
                  '(119 -- 123 -- 127 -- 131 -- 135 -- 141 -- 143)))
   (SETQ BRD1 (COPY-BRD BRD))
            (PLAY BRD1))

   
  ((EQUAL CHOICE 15)
    (SETQ N 15)
    (SETQ BRD (LIST '(41 -- -- -- -- -- -- -- -- -- -- -- -- -- 221)
                  '(-- 43 -- -- -- -- -- -- -- -- -- -- -- 77 --)
                  '(-- -- 197 -- -- -- -- -- -- -- -- -- -- -- --)
                  '(-- -- -- 89 -- -- -- -- -- -- -- -- -- -- --)
                  '(-- -- -- -- 159 -- -- -- -- -- 143 -- -- -- --)
                  '(-- -- -- -- -- 119 -- -- -- -- -- -- -- -- --)
                  '(-- -- -- -- -- -- 137 -- -- -- -- -- -- -- --) 
                  '(-- -- -- -- -- -- -- 135 -- -- -- -- -- -- --)
                  '(-- -- -- -- -- -- 123 -- 125 -- -- -- -- -- --) 
                  '(-- -- -- -- -- -- -- -- -- 149 -- -- -- -- --) 
                  '(-- -- -- -- 97 -- -- -- -- -- 103 -- -- -- --)
                  '(-- -- -- -- -- -- -- -- -- -- -- 179 -- -- --)
                  '(-- -- -- -- -- -- -- -- -- -- -- -- 65 -- --)
                  '(-- -- -- -- -- -- -- -- -- -- -- -- -- 1 --)
                  '(27 -- -- -- -- -- -- -- -- -- -- -- -- -- 225)))
   (SETQ BRD1 (COPY-BRD BRD))
            (PLAY BRD1))


   
  ((EQUAL CHOICE 16)
    (SETQ N 9)
    (SETQ BRD (LIST '(43 -- 41 -- 39 -- 33 -- 27)
                  '(-- -- -- -- -- -- -- -- --)
                  '(51 -- -- -- -- -- -- -- 25)
                  '(-- -- -- -- -- -- -- -- --) 
                  '(53 -- -- -- -- -- -- -- 23)
                  '(-- -- -- -- -- -- -- -- --) 
                  '(55 -- -- -- -- -- -- -- 21) 
                  '(-- -- -- -- -- -- -- -- --)
                  '(57 -- 81 -- 71 -- 7 -- 9)))
   (SETQ BRD1 (COPY-BRD BRD))
        (PLAY BRD1))
   
   
  ((EQUAL CHOICE 17)
    (SETQ N 9)
    (SETQ BRD (LIST '(1 -- 3 -- 19 -- 23 -- 25)
                  '(-- -- -- -- -- -- -- -- --)
                  '(7 -- -- -- -- -- -- -- 27)
                  '(-- -- -- -- -- -- -- -- --) 
                  '(75 -- -- -- -- -- -- -- 35)
                  '(-- -- -- -- -- -- -- -- --) 
                  '(81 -- -- -- -- -- -- -- 45) 
                  '(-- -- -- -- -- -- -- -- --)
                  '(61 -- 59 -- 57 -- 48 -- 47)))
   (SETQ BRD1 (COPY-BRD BRD))
   (PLAY BRD1))

            
   (T (PRINC "INVALID OPTION") (TERPRI)))  
 
   (PRINC "DO YOU WANT TO PLAY AGAIN (Y/N)?: ")
   (SETQ AGAIN (READ))
   (COND ((EQUAL AGAIN 'Y) (SETQ REPLAY T))
              (T (SETQ REPLAY NIL)))
  )
  
  (TERPRI) (PRINC "EXITING AUTOMATIC PLAY")
)

(DEFUN PLAY (BRD1)
  
  (SETQ BRD2 BRD1)
  (PBOARD BRD2 N)
  (INIT BRD2 N)
  (SETQ R 1)
  (SETQ C 1)
  (SETQ A (GET-INTERNAL-REAL-TIME))
  (LOOP FOR R FROM 1 TO N DO
        (LOOP FOR C FROM 1 TO N DO
              (COND ((EQUAL (SOLVE  R C 1 BRD2 AVAILABLE) T) (TERPRI) (PRINC "SOLVED!"))
                    (T (CONTINUE)))))
  (SETQ B (GET-INTERNAL-REAL-TIME))
  (SETQ C INTERNAL-TIME-UNITS-PER-SECOND)
  (SETQ TOTAL (/ (FLOAT (/ (- B A) C)) 60))
  (TERPRI)
  (PRINC "SOLUTION IS")
  (TERPRI)
  (PBOARD BRD2 N)
  (TERPRI)
  (FORMAT T "TOTAL TIME TAKEN: ~A" TOTAL)
  (TERPRI)
  )

(DEFUN SOLVE (R C NUMBER BRD AVAILABLE)
  
  (LET* ((FLAG 0))
  (SETQ N1 (* N N))
  (COND ((> NUMBER (* N N)) (RETURN-FROM SOLVE T))
        (T (CONTINUE)))
    (COND ((EQUAL (SQR1 NUMBER AVAILABLE N1) 0) 
           (COND ((EQUAL (SQR R C BRD N) NUMBER) (CONTINUE))
                  (T (RETURN-FROM SOLVE NIL))))
        (T (COND ((EQUAL (SQR R C BRD N) '--)(CONTINUE))
                 (T (RETURN-FROM SOLVE NIL))) 
           (PLACE1 R C BRD NUMBER N) 
           (TERPRI)(FORMAT T " ROW: ~A COL: ~A VALUE: ~A" R C NUMBER)           
           (PLACE NUMBER AVAILABLE 0 N1)
           (SETQ FLAG 1)))
  (COND ((< C N) (COND ((EQUAL (SOLVE R (+ C 1) (+ NUMBER 1) BRD AVAILABLE) T) (RETURN-FROM SOLVE T))
                       (T (CONTINUE))))
        (T (CONTINUE)))
  (COND ((> C 1) (COND ((EQUAL (SOLVE R (- C 1) (+ NUMBER 1) BRD AVAILABLE) T) (RETURN-FROM SOLVE T))
                       (T (CONTINUE))))
        (T (CONTINUE)))
  (COND ((< R N) (COND ((EQUAL (SOLVE (+ R 1) C (+ NUMBER 1) BRD AVAILABLE) T) (RETURN-FROM SOLVE T))
                       (T (CONTINUE))))
        (T (CONTINUE)))
  (COND ((> R 1) (COND ((EQUAL (SOLVE (- R 1) C (+ NUMBER 1) BRD AVAILABLE) T) (RETURN-FROM SOLVE T))
                       (T (CONTINUE))))
        (T (CONTINUE)))
  (COND ((/= FLAG 0) (PLACE1 R C BRD '-- N) (PLACE NUMBER AVAILABLE 1 N1))
        (T (CONTINUE)))
   (RETURN-FROM SOLVE NIL))
)
  
(DEFUN INIT (BRD2 N)
  
  (SETQ AVAILABLE '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
                    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
                    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
                    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
                    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
                    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
                    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
                    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
                    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
                    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
                    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
                    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
  (SETQ N1 (* N N))
  (SETQ K 1)
  (LOOP FOR K FROM 1 TO N1 DO
        (PLACE K AVAILABLE 1 N1)
       )
  (SETQ R 1)
  (SETQ C 1)
  (LOOP FOR C FROM 1 TO N DO
        (LOOP FOR R FROM 1 TO N DO
              (COND((EQUAL (SQR R C BRD N) '--) (CONTINUE))
                    ( T (PLACE (SQR R C BRD N) AVAILABLE 0 N1)))))
  
  )

(DEFUN SQR(POSX POSY BRD2 N)
   (NTH (- POSY 1) (NTH (- N POSX) BRD2)))


(DEFUN SQR1 (POSY BRD2 N1)
  (NTH (- POSY 1)  BRD2))

(DEFUN PLACE1 (POSX POSY BRD2 VAL N)
  (SETF (NTH (- POSY 1) (NTH (- N POSX) BRD2)) VAL))


(DEFUN PLACE (POSY AVAILABLE VAL N1)

  (SETF (NTH (- POSY 1) AVAILABLE) VAL)
  )

(DEFUN NUMBRIX() 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; print the rules and menu/options   
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (PRINT "WELCOME TO NUMBRIX")(TERPRI)
  (PRINT "INSTRUCTIONS") (TERPRI)
  (PRINT "This game is played on a square grid.") (TERPRI)
  (PRINT "To refer to positions on the grid") (TERPRI)
  (PRINT "we use the following numbering convention:") (TERPRI)
  (PRINT "1.Each position is referred to by two numbers,") (TERPRI)
  (PRINT "its row and its column.") (TERPRI)
  (PRINT "2.The bottom-most left corner of the grid is position (1,1),") (TERPRI)
  (PRINT "the top-most left corner of the grid is (N,1) where N is") (TERPRI)
  (PRINT "the number of rows/columns in the square grid and so on.") (TERPRI)
  (PRINT "3.The input is in the form of [position-row, position-column, value].") (TERPRI)
  
  (SETQ REPLAY T)
  (LOOP WHILE (EQUAL REPLAY T) DO
        (PRINC "SELECT BETWEEN HUMAN (ENTER 1) OR COMPUTER (ENTER 2) PLAYER: ")
        (SETQ CHOICE (READ))
        (COND ((= CHOICE 1) (HUMAN))
              ((= CHOICE 2)(COMPUTER))
              (T (TERPRI)(PRINC "YOU MADE THE WRONG CHOICE, DO YOU WANT TO PLAY AGAIN? (Y/N): "))) 
        (TERPRI)(PRINC "DO YOU WANT TO PLAY AGAIN (Y/N)?: ")
        (SETQ AGAIN (READ))
        (COND ((EQUAL AGAIN 'Y) (SETQ PLAY-AGAIN T))
              (T (SETQ PLAY-AGAIN NIL)))
        )
  )
  
  
(NUMBRIX)
