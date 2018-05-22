;; Nicholas Keen
;; p004 SchemerI
;; 2/20/2018
;; Programming Languages

;; Checks if the first number passed is in the range of the next 
;; two numbers passed
(define in-range?
    (lambda (tst low high)
        (cond
            ;; if the first number is equal to low or between low and high
            ;; then return true, else return false
            ((> tst low)(< tst high))
            ((= tst low) #t)
            ((< tst low) #f)
        )
    )
)

;; Sets a range of numbers and lets you guess a number randomly 
;; chosen from that range
(define guess-my-number
    (lambda (low high)
        ;; done condition
        (define done 0)
        ;; error message
        (define error "No numbers in given range.")
        ;; range doesn't make sense, send error and set our done condition
        (if (or (= high low) (> low high))
            (begin(set! done (+ done 1))(display error)))
        (define rand 0)
        ;; we're not done, so let's pick a random number in the range
        ;; and begin recursion!
        (if (= done 0)
            (begin (set! rand (+ low(random (- high low))))
            ;; send info to helper method
            (helper low high 0 1 done rand))#f)
    )
)

        
(define helper
    (lambda (low high guess guess-num done answer)
        ;; prompt user for a number
        (display "Guess my number: ")
        ;; read input
        (define guess (read))
        ;; define out of bounds guess message
        (define bound 
            (string-append (number->string guess) " is out of range from "
            (number->string low) " to " (number->string high))
        )
        ;; define correct answer message
        (define correct (string-append "You guessed my number in "
                        (number->string guess-num) " tries."))
        ;; guess is out of bounds, display out of bounds message
        (if (< guess low)
            (begin(display bound)(newline)))
        (if (or (= guess high) (> guess high)) 
            (begin(display bound)(newline)))
        ;; correct! display correct answer message, set done condition
        (if (= guess answer)
            (begin(set! done (+ done 1))(display correct)(newline)))
        ;; guess was too low
        (if (and (< guess answer) (> guess (- low 1)))
            (begin(display "Too low!")(newline)))
        ;; guess was too high
        (if (and (> guess answer) (< guess high))
            (begin(display "Too high!")(newline)))
        ;; check done condition, if done quit, otherwise recursion
        ;; until user guesses correctly
        (if (= done 0)
            (helper low high guess (+ guess-num 1) done answer)#t)
    )
)