#lang racket
(provide (prefix-out human: player))
(require "play.rkt")
(require "board.rkt")

(define (user-input prompt-message
                    [default #f]
                    #:validator [valid? (lambda (val) #t)]
                    #:validation-error [err-msg "Invalid value"]
                    #:parser [parse identity])
  (define (prompt)
    (display (string-append prompt-message "\n"))
    (let* ([raw-input (begin (display "? ") (read-line))]
           [attempt (if (equal? raw-input "") default raw-input)])
      (cond [(or (eof-object? raw-input)
                 (quit-command? raw-input)) (raise 'quit-game #t)]
            [(valid? attempt) (parse attempt)]
            [else (begin
                    (display err-msg)
                    (newline)
                    (prompt))])))
  (prompt))

(define next-play user-input)

(define/contract (quit-command? user-input)
  (-> string? boolean?)
  (let ([command (string-downcase user-input)])
    (cond [(equal? "quit" command) #t]
          [(equal? "exit" command) #t]
          [(equal? "bye" command) #t]
          [(equal? (string-ref command 0) #\q) #t]
          [else #f])))

(define/contract (player board self opponent)
  (-> board? symbol? symbol? board?)
  (let ([user-play (next-play (format "\nYour move, ~a. Name your play." self)
                              #:validator valid-play?
                              #:validation-error "Sorry, what was that?\nPlease specify a row column play, (eg: a1) or quit to exit.\n"
                              #:parser (lambda (s) (string->play s self)))])
    (if (playable? board user-play)
        (play board user-play)
        (begin
          (display "Can't play there...\n")
          (player board self opponent)))))