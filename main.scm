;Binomial function
(define (C N K)
  (if (= K 0)
  1
    (if (= K N)
    1
    (+ (C (- N 1) K) (C (- N 1)(- K 1)))
    )
  )
)

;Mod function
(define (mod N M)
  (abs(- N(* M(truncate(/ N M)))))
)

;Binary to decimal function
(define (modFunc A B)
  (mod A B)
)
(define (divFunc A B)
  (div A B)
)
(define (binaryToDecimal A)
  (if (= A 0)
    0
    (+ (modFunc A 10)(* 2(binaryToDecimal(divFunc A 10))))
  )
)

;Add binary list function
(define (addBinary binaryList)
  (if(null? binaryList)
    0
    (+ (binaryToDecimal(car binaryList))(addBinary(cdr binaryList)))
  )
)

;Min list function
(define (min list)
    (cond ((null? (cdr list)) (car list))
          ((< (car list) (min (cdr list))) (car list))
          (else (min (cdr list)))
    ) 
)

;Remove function
(define remov
  (lambda (num list)
    (cond
     ((equal? num (car list)) (cdr list))
     (else (cons (car list) (remov num (cdr list))))
    )
  )
)

;Selection sort function
(define (remove A B)
  (cond ( (null? A) '() )           
    ( (= (car A) B) (cdr A))
    (else 
      (cons (car A)(remove (cdr A) B))
    )
  )
)
(define (minimum A B)
  (cond ( (null? A) B)
    ( (< (car A) B) (minimum (cdr A)(car A)))
    (else 
      (minimum (cdr A) B )
    )
  )
)
(define (selectionSort list) 
  (cond ( (null? list) '() )
    (else 
      (cons (minimum list (car list))
        (selectionSort (remove list (minimum list (car list))))
      )
    )
  )
)
