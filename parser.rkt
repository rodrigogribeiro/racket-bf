#lang racket

(provide parse-expr)

(define (parse-expr src in)
  (define-values (line column position) (port-next-location in))
  (define next-char (read-char in))
 
  ;; decorate: s-expression number -> syntax
  ;; Wrap the s-expression with source location.
  (define (decorate sexp span)
    (datum->syntax #f sexp (list src line column position span)))
 
  (cond
    [(eof-object? next-char) eof]
    [else
     (case next-char
       [(#\<) (decorate '(less-than) 1)]
       [(#\>) (decorate '(greater-than) 1)]
       [(#\+) (decorate '(plus) 1)]
       [(#\-) (decorate '(minus) 1)]
       [(#\,) (decorate '(comma) 1)]
       [(#\.) (decorate '(period) 1)]
       [(#\[)
        (define elements (parse-exprs src in))
        (define-values (l c tail-position)
          (port-next-location in))
        (decorate `(brackets ,@elements)
                  (- tail-position position))]
       [else
        (parse-expr src in)])]))

(define (parse-exprs source-name in)
  (define peeked-char (peek-char in))
  (cond
    [(eof-object? peeked-char)
     (error 'parse-exprs "Expected ], but read eof")]
    [(char=? peeked-char #\])
     (read-char in)
     empty]
    [(member peeked-char (list #\< #\> #\+ #\- #\, #\. #\[))
     (cons (parse-expr source-name in)
           (parse-exprs source-name in))]
    [else
     (read-char in)
     (parse-exprs source-name in)]))

