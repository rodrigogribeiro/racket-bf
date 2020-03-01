#lang racket

(require rackunit rackunit/text-ui
         "semantics.rkt")

; test suite for the semantics

(define semantics-tests
  (test-suite "Tests for semantics.rkt"
     (test-case "increment/decrement-byte test"
       (let ([s (new-state)])
         (increment-byte s)
         (check-equal? 1 (vector-ref (state-data s) 0))
         (increment-byte s)
         (check-equal? 2 (vector-ref (state-data s) 0))
         (decrement-byte s)
         (check-equal? 1 (vector-ref (state-data s) 0))))
     (test-case "Pointer movement test"
       (let ([s (new-state)])
         (increment-ptr s)
         (increment-byte s)
         (check-equal? 0 (vector-ref (state-data s) 0))
         (check-equal? 1 (vector-ref (state-data s) 1))
         (decrement-ptr s)
         (increment-byte s)
         (check-equal? 1 (vector-ref (state-data s) 0))
         (check-equal? 1 (vector-ref (state-data s) 1))))
     (test-case "Standard input test"
       (let ([s (new-state)])
         (parameterize ([current-input-port
                         (open-input-bytes (bytes 3 1 4))])
           (read-byte-from-stdin s)
           (increment-ptr s)
           (read-byte-from-stdin s)
           (increment-ptr s)
           (read-byte-from-stdin s))
         (check-equal? 3 (vector-ref (state-data s) 0))
         (check-equal? 1 (vector-ref (state-data s) 1))
         (check-equal? 4 (vector-ref (state-data s) 2))))
     (test-case "Standard output test"
       (let ([s (new-state)])
         (set-state-data! s (vector 80 76 84))
         (let ([simulated-stdout (open-output-string)])
           (parameterize ([current-output-port simulated-stdout])
             (write-byte-to-stdout s)
             (increment-ptr s)
             (write-byte-to-stdout s)
             (increment-ptr s)
             (write-byte-to-stdout s))
           (check-equal? "PLT" (get-output-string simulated-stdout)))))
     (test-case "Loop test"
       (let ([s (new-state)])
         (set-state-data! s (vector 0 104 101 108 112 109 101 105
                                    109 109 101 108 116 105 110 103))
         (set-state-ptr! s 15)
         ;; [ [-] < ]
         (loop s
               (loop s (decrement-byte s))
               (decrement-ptr s))
         (check-equal? 0 (state-ptr s))
         (check-equal? (make-vector 16 0) (state-data s))))))
