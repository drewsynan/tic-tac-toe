#lang racket
(require data/collection)
(require "ai.rkt"
         "human.rkt"
         "play.rkt"
         "board.rkt")

(provide new-game
         human-computer-game
         human-human-game
         computer-computer-game)

(define position/c (sequenceof play?))

(define/contract (three-in-a-row? position)
  (-> position/c (or/c symbol? #f))
  (let* ([first-square (first position)]
         [symbol (play-symbol first-square)]
         [other-symbols (filter (lambda (square)
                                  (not (equal? (play-symbol square) symbol)))
                                position)])
    (cond
      [(blank-play? first-square) #f]
      [(empty? other-symbols) symbol]
      [else #f])))

(define/contract (game-won? board)
  (-> board? (or/c symbol? #f))
  (let ([winners (filter identity
                         (map three-in-a-row?
                              (board-positions board)))])
    (if (empty? winners)
        #f
        (first winners))))

(define/contract (game-tied? board)
  (-> board? boolean?)
  (empty?
   (filter blank-play?
           (get-all-plays board))))

(define (quit-game? exn)
  (eq? 'quit-game exn))

(define (game-over? exn)
  (eq? 'game-over exn))

(define/contract (new-game [player1 human:player] [symbol1 'x]
                  [player2 ai:player] [symbol2 'o]
                  [game (make-board)]
                  [current-player symbol1]
                  #:show-splash [show-splash #t])
  (->i ()
       ([player1 (-> board? symbol? symbol? board?)]
        [player1-symbol symbol?]
        [player2 (-> board? symbol? symbol? board?)]
        [player2-symbol (player1-symbol) (and/c symbol? (not/c player1-symbol))]
        [game board?]
        [current-player symbol?]
        #:show-splash [show boolean?])
       [result any/c])
  (if show-splash
      (begin
        (display "          T  I  C\n          T  A  C\n          T  O  E\n")
        (display "\n"))
      (void))
  (define (recur game current-player)
    (let ([player-func (if (equal? current-player symbol1) player1 player2)]
          [player-symbol (if (equal? current-player symbol1) symbol1 symbol2)]
          [opponent-symbol (if (equal? current-player symbol1) symbol2 symbol1)])
      (cond
        [(game-won? game) => (lambda (winner)
                               (display (board->string game))
                               (display (format "\n\n~a wins!\n" winner))
                               (raise 'game-over))]
        [(game-tied? game) (begin (display (board->string game))
                                  (display "\n\nIt's a tie...\n")
                                  (raise 'game-over))]
        [else (begin (display (board->string game))
                     (newline)
                     (recur (player-func game player-symbol opponent-symbol) opponent-symbol))])))
  (with-handlers ([quit-game? (lambda (_) (display "\nBye!\n"))]
                  [exn:break? (lambda (_) (display "\nBye!\n"))]
                  [game-over? (lambda (_) (if (confirm "Play again?")
                                              (new-game player1 symbol1 player2 symbol2 #:show-splash #f)
                                              (display "Bye!\n")))])
    (recur game current-player)))

(define (confirm prompt-message)
  (display prompt-message)
  (display "\n")
  (let ([user-input (begin (display "? ") (read-line))])
    (cond
      [(eof-object? user-input) #f]
      [(equal? "" user-input) (confirm prompt-message)]
      [(equal? "yes" (string-downcase user-input)) #t]
      [(equal? "yep" (string-downcase user-input)) #t]
      [(equal? "yea" (string-downcase user-input)) #t]
      [(equal? "yeah" (string-downcase user-input)) #t]
      [(equal? "sure" (string-downcase user-input)) #t]
      [(equal? "y" (string-downcase user-input)) #t]
      [(equal? "no" (string-downcase user-input)) #f]
      [(equal? "nope" (string-downcase user-input)) #f]
      [(equal? "naw" (string-downcase user-input)) #f]
      [(equal? "nah" (string-downcase user-input)) #f]
      [(equal? "no thanks" (string-downcase user-input)) #f]
      [(equal? "n" (string-downcase user-input)) #f]
      [else (confirm prompt-message)])))

(define (human-computer-game)
  (new-game human:player 'X
            ai:intermediate 'O))

(define (computer-computer-game)
  (new-game ai:novice 'X
            ai:novice 'O))

(define (human-human-game)
  (new-game human:player 'X
            human:player 'O))