#lang racket
(require data/collection)
(require data/pvector)

(require "play.rkt")

(provide make-board
         board?
         board->string
         board-rows
         board-cols
         board-diags
         board-positions
         board-num-rows
         board-num-cols
         get-play
         set-play
         get-all-plays
         playable?
         play)


(define/contract (board? b)
  (-> any/c boolean?)
  (and (hash? b)
       (subset? (set "num-rows" "num-cols" "vec")
                (list->set (hash-keys b)))))

(define/contract (make-board [num-rows 3]
                             [num-cols 3]
                             [content (make-pvector (* num-rows num-cols) null)])
  (->* () (number? number? sequence?) board?)
  (hash "num-rows" num-rows
        "num-cols" num-cols
        "vec" content))

(define/contract (board-num-cols board)
  (-> board? number?)
  (hash-ref board "num-cols"))

(define/contract (board-num-rows board)
  (-> board? number?)
  (hash-ref board "num-rows"))

(define/contract (board-vec board)
  (-> board? (sequenceof (or/c symbol? null?)))
  (hash-ref board "vec"))

(define/contract (board->string board)
  (-> board? string?)
  (let ([squares (sequence->list (map (lambda (square)
                                        (if (blank-play? square)
                                            " "
                                            (play-symbol square)))
                                      (get-all-plays board)))]
        [template #<<board
       1     2     3  
a      ~a  |  ~a  |  ~a  
     -----+-----+-----
b      ~a  |  ~a  |  ~a  
     -----+-----+-----
c      ~a  |  ~a  |  ~a  
board
                  ])
    (apply format template squares)))

(define/contract (get-all-plays board)
  (-> board? (sequenceof play?))
  (let* ([rows (board-num-rows board)]
         [cols (board-num-cols board)]
         [vec (board-vec board)]
         [idx->play (lambda (idx)
                      (let ([col (add1 (remainder idx cols))]
                            [row (add1 (inexact->exact (floor (/ idx rows))))]
                            [symbol (nth vec idx)])
                        (make-play row col symbol)))])
    (map idx->play (range 0 (* rows cols)))))                

(define/contract (get-play board row col)
  (-> board? number? number? (or/c play? #f))
  (let* ([rows (board-num-rows board)]
         [cols (board-num-cols board)]
         [idx (+ (sub1 col)
                 (* (sub1 row) cols))]
         [vec (board-vec board)])
    (cond
      [(or (> row rows)
           (> 1 row)) #f]
      [(or (> col cols)
           (> 1 col)) #f]
      [else (make-play row
                       col
                       (nth vec idx))])))

(define/contract (set-play board row col val)
  (-> board? number? number? any/c board?)
  (let* ([rows (board-num-rows board)]
         [cols (board-num-cols board)]
         [idx (+ (sub1 col)
                 (* (sub1 row) cols))]
         [vec (board-vec board)])
    (make-board rows
                cols
                (set-nth vec idx val))))

(define/contract (board-rows board)
  (-> board? (sequenceof sequence?))
  (chunk (board-num-cols board) (sort (sequence->list (get-all-plays board))
                                      <
                                      #:key play-row)))

(define/contract (board-cols board)
  (-> board? (sequenceof sequence?))
  (chunk (board-num-cols board) (sort (sequence->list (get-all-plays board))
                                      <
                                      #:key play-col)))

(define/contract (board-diags board)
  (-> board? (sequenceof sequence?))
  (let* ([fwd (range 1 (add1 (board-num-cols board)))]
         [rev (reverse fwd)])
    (map (lambda (rows cols)
           (map (lambda (row col)
                  (get-play board row col))
                rows cols))
         (list fwd fwd)
         (list fwd rev))))

(define/contract (board-positions board)
  (-> board? (sequenceof sequence?))
  (append (board-rows board)
          (board-cols board)
          (board-diags board)))

(define/contract (playable? board the-play)
  (-> board? play? boolean?)
  (blank-play? (get-play board
                         (play-row the-play)
                         (play-col the-play))))

(define/contract (play board the-play)
  (-> board? play? board?)
  (if (playable? board the-play)
      (set-play board
                (play-row the-play)
                (play-col the-play)
                (play-symbol the-play))
      #f))