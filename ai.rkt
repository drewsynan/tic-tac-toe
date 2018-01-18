#lang racket
(require data/collection)
(require "play.rkt")
(require "board.rkt")

(provide (prefix-out ai: player)
         (prefix-out ai: expert)
         (prefix-out ai: intermediate)
         (prefix-out ai: novice))

(define position/c (sequenceof play?))

;; crawley and seigler 1993
;; doi 10.1207/s15516709cog1704_3
(define/contract (two-pieces-and-blank? position symbol)
  (-> position/c symbol? boolean?)
  (let ([remaining (filter (lambda (square)
                             (not (equal? (play-symbol square) symbol)))
                           position)])
    (cond
      [(empty? remaining) #f]
      [(not (= 1 (length remaining))) #f]
      [(blank-play? (first remaining)) #t]
      [else #f])))
    
(define/contract (one-piece-and-blank? position symbol)
  (-> position/c symbol? boolean?)
  (let ([non-blank (filter (lambda (square)
                             (not (blank-play? square)))
                           position)])
    (cond
      [(empty? non-blank) #f]
      [(not (= 1 (length non-blank))) #f]
      [(equal? symbol (play-symbol (first non-blank))) #t]
      [else #f])))

(define/contract (intersection pos1 pos2)
  (-> position/c position/c (or/c play? #f))
  (let ([matching (filter (lambda (square)
                            (not (empty? (filter (lambda (other-square)
                                                   (and (equal? (play-row square) (play-row other-square))
                                                        (equal? (play-col square) (play-col other-square))))
                                                 pos2))))
                          pos1)])
    (if (empty? matching)
        #f
        (first matching))))

;; intersect positions
;; block is (win board 'opponent)
(define/contract (win? board symbol)
  (-> board? symbol? (or/c (or/c (sequenceof play?) null?) #f))
  (let* ([wins (filter (lambda (position)
                         (two-pieces-and-blank? position symbol))
                       (board-positions board))]
         [winning-plays (remove-duplicates (sequence->list (map (lambda (position)
                                                                  (first (filter blank-play? position)))
                                                                wins)))])
    (if (empty? winning-plays)
        #f
        winning-plays)))

;; block fork is (fork board 'opponent)
(define/contract (fork? board symbol)
  (-> board? symbol? (or/c (or/c (sequenceof play?) null?) #f))
  (let* ([candidates (filter (lambda (position)
                               (one-piece-and-blank? position symbol))
                             (board-positions board))]
         [empty-intersection? (lambda (pos1 pos2)
                                (cond
                                  [(equal? pos1 pos2) #f]
                                  [(intersection pos1 pos2) => blank-play?]
                                  [else #f]))]
         [forks (foldl (lambda (acc tuple)
                         (let ([p1 (first tuple)]
                               [p2 (second tuple)])
                           (cond
                             [(empty-intersection? p1 p2) (conj acc (intersection p1 p2))]
                             [else acc])))
                       '()
                       (cartesian-product candidates candidates))])
    (if (empty? forks)
        #f
        (remove-duplicates forks))))

(define/contract (corners board)
  (-> board? (sequenceof play?))
  (let ([rows (board-num-rows board)]
        [cols (board-num-cols board)])
    (list (get-play board 1 1)
          (get-play board 1 cols)
          (get-play board rows 1)
          (get-play board rows cols))))

(define/contract (corner? board play)
  (-> board? play? boolean?)
  (member play (corners board)))

(define/contract (empty-corner? board)
  (-> board? (or/c (sequenceof play?) #f))
  (let ([empties (filter blank-play? (corners board))])
    (if (empty? empties)
        #f
        empties)))

(define/contract (empty-opposite-corner? board opponent)
  (-> board? symbol? (or/c (sequenceof play?) #f))
  (let* ([board-corners (corners board)]
         [opponent-corners (filter (lambda (corner)
                                     (equal? (play-symbol corner) opponent))
                                   board-corners)]
         [blanks (filter blank-play? board-corners)])
    (cond
      [(empty? opponent-corners) #f]
      [(empty? blanks) #f]
      [else blanks])))
        
(define/contract (empty-side? board)
  (-> board? (or/c (sequenceof play?) null?))
  (let ([rows (board-num-rows board)]
        [cols (board-num-cols board)])
    (filter (lambda (play)
              (cond
                [(not (blank-play? play)) #f]
                [(corner? board play) #f]
                [(= 1 (play-row play)) #t]
                [(= rows (play-row play)) #t]
                [(= 1 (play-col play)) #t]
                [(= cols (play-col play)) #t]
                [else #f]))
            (get-all-plays board))))

(define-syntax (stochastic stx)
  (syntax-case stx ()
    ((_ p form)
     #'(let ([val form]
             [mistaken? (lambda () (> (random) (exact->inexact p)))])
         (if (and val (not (mistaken?)))
             val
             #f)))))

(define-syntax-rule (stochastic2 p form)
  (let ([val form]
        [mistaken? (lambda () (> (random) (exact->inexact p)))])
    (if (and val (not (mistaken?))) val #f)))

(define/contract (next-play board self opponent [correct-prob 1.0])
  (->* (board? symbol? symbol?) (real?) play?) 
  (let ([center (get-play board 2 2)]
        [play-strategy (lambda (plays)
                         (let ([play (first plays)])
                           (make-play (play-row play)
                                      (play-col play)
                                      self)))]
        [play-center (make-play 2 2 self)]
        [p correct-prob])
    (cond
      ;; rules that are always tested ensure that the computer
      ;; doesn't pass on a blank square, even if all the rest of the
      ;; possible plays are ignored
      [(stochastic p (win? board self)) => play-strategy]
      [(stochastic p (win? board opponent)) => play-strategy]
      [(stochastic p (fork? board self)) => play-strategy]
      [(stochastic p (fork? board opponent)) => play-strategy]
      [(stochastic 1 (blank-play? center)) play-center]
      [(stochastic p (empty-opposite-corner? board opponent)) => play-strategy]
      [(stochastic 1 (empty-corner? board)) => play-strategy]
      [(stochastic 1 (empty-side? board)) => play-strategy]
      [else #f])))

(define/contract (player board self opponent [p 1.0])
  (->* (board? symbol? symbol?) (real?) board?)
  (let ([computer-play (next-play board self opponent p)])
    (display (format "\n~a played ~a.\n" self (play->string computer-play)))
    (play board computer-play)))

(define/contract (expert board self opponent)
  (-> board? symbol? symbol? board?)
  (player board self opponent 1.0))

(define/contract (intermediate board self opponent)
  (-> board? symbol? symbol? board?)
  (player board self opponent 0.6))

(define/contract (novice board self opponent)
  (-> board? symbol? symbol? board?)
  (player board self opponent 0.33))