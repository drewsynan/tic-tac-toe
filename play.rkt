#lang racket
(provide valid-play?
         blank-play?
         make-play
         play?
         play-symbol
         play-row
         play-col
         string->play
         play->string)

(define/contract (valid-play? str)
  (-> string? boolean?)
  (not (equal? #f (string->play str))))

(define (play? p)
  (and (hash? p)
       (subset? (set "row" "col" "symbol") (list->set (hash-keys p)))
       (number? (hash-ref p "row"))
       (number? (hash-ref p "col"))
       ((lambda (x) (or (symbol? x) (null? x))) (hash-ref p "symbol"))))

(define/contract (make-play [row 1] [col 1] [symbol null])
  (->* () (number? number? (or/c symbol? null?)) play?)
  (hash "row" row
        "col" col
        "symbol" symbol))

(define/contract (play-row play)
  (-> play? number?)
  (hash-ref play "row"))

(define/contract (play-col play)
  (-> play? number?)
  (hash-ref play "col"))

(define/contract (play-symbol play)
  (-> play? (or/c symbol? null?))
  (hash-ref play "symbol"))

(define/contract (blank-play? play)
  (-> play? boolean?)
  (equal? (play-symbol play) null))

(define/contract (play->string p)
  (-> play? string?)
  (let* ([row (play-row p)]
         [row-string (string (integer->char (+ (- row 1)
                                               (char->integer #\a))))]
         [col (play-col p)]
         [col-string (number->string col)])
    (string-append row-string col-string)))
    

(define/contract (string->play string [player-symbol 'x])
  (->* (string?) ((or/c symbol? null?)) (or/c play? #f))
  (let* ([lc (string-downcase string)]
         [char->idx (lambda (char-string)
                      (+ 1 (- (char->integer (string-ref char-string 0))
                              (char->integer #\a))))]
         [num->idx (lambda (num-string)
                     (string->number num-string))]
         [chars "[abc]{1}"]
         [nums "[123]{1}"]
         [skeleton "^\\s*(~a)\\s*(~a)\\s*$"]
         [alpha-first (pregexp (format skeleton chars nums))]
         [num-first (pregexp (format skeleton nums chars))])
    (cond
      [(regexp-match alpha-first lc) => (lambda (match)
                                          (let ([alpha (second match)]
                                                [num (third match)])
                                            (make-play
                                             (char->idx alpha)
                                             (num->idx num)
                                             player-symbol)))]
      [(regexp-match num-first lc) => (lambda (match)
                                        (let ([alpha (third match)]
                                              [num (second match)])
                                          (make-play
                                           (char->idx alpha)
                                           (num->idx num)
                                           player-symbol)))]
      [else #f])))
