#lang at-exp racket

(provide (contract-out
          [make-process-queue
           ({(and/c natural? (>/c 0))}
            {any/c
             #:empty? (-> boolean?)
             #:enq (unconstrained-domain-> process-queue?)
             #:wait (unconstrained-domain-> process-queue?)
             #:active-count (-> process-queue? natural?)
             #:waiting-count (-> process-queue? natural?)
             #:get-data (-> process-queue? any/c)
             #:set-data (-> process-queue? any/c process-queue?)
             #:kill-older-than any/c}
            . ->* .
            process-queue?)]
          [make-recording-process-queue
           ({(and/c natural? (>/c 0))
             #:record-in (and/c hash? (not/c immutable?))}
            {any/c
             #:kill-older-than any/c}
            . ->* .
            process-queue?)])
         (all-from-out "private/interface.rkt"))

(require "private/interface.rkt"
         (submod "private/interface.rkt"
                 internal)
         "private/generic-functional-process-queue.rkt")

(define (make-process-queue process-limit
                            [data-init #f]
                            #:empty? [empty? (λ _ #f)]
                            #:enq [enq (λ (q . _) q)]
                            #:wait [wait (λ (q) q)]
                            #:active-count [active-count (λ (q) 1)]
                            #:waiting-count [waiting-count (λ (q) 1)]
                            #:get-data [get-data process-queue-data]
                            #:set-data [set-data (λ (q new)
                                                   (struct-copy process-queue q
                                                                [data new]))]
                            #:kill-older-than [ignored #f])
  (process-queue empty?
                 enq
                 wait
                 active-count
                 waiting-count
                 get-data
                 set-data

                 data-init))

(define (make-recording-process-queue process-limit
                                      #:record-in h
                                      [data-init #f]
                                      #:kill-older-than [ignored #f])
  (for ([k (in-list '(empty? create enq wait active-count waiting-count))])
    (hash-set! h k 0))
  (define (add-call! name)
    (hash-update! h name add1 0))
  (make-process-queue process-limit
                      data-init
                      #:empty? (λ (q) (add-call! 'empty?) #f)
                      #:enq (λ (q v . _) (add-call! 'enq) q)
                      #:wait (λ (q) (add-call! 'wait) q)
                      #:active-count (λ (q) (add-call! 'active-count) 1)
                      #:waiting-count (λ (q) (add-call! 'waiting-count) 1)))

(module+ test
  (require ruinit)

  (test-begin
    #:name mock
    (ignore (define b (box #f))
            (define mq (make-process-queue 5
                                           #:enq (λ (q . _)
                                                   (set-box! b #t)
                                                   q))))
    (not (unbox b))
    (ignore (process-queue-enqueue mq void))
    (unbox b))

  (test-begin
    #:name recording-mock
    (ignore (define h (make-hash))
            (define mq (make-recording-process-queue 5
                                                     #:record-in h)))
    (test-match (hash->list h)
                (list (cons _ 0)
                      ___))
    (process-queue-enqueue mq void)
    (test-match (hash->list h)
                (list-no-order (cons 'enq 1)
                               (cons _ 0)
                               ___)))

  (test-begin
    #:name mock-test
    (ignore
     (define call-hash (make-hash))
     (define mock-q
       (make-process-queue
        1
        #:empty? (λ _
                   (hash-set! call-hash 'empty? #t)
                   #f)
        #:enq (λ (q . _)
                (hash-set! call-hash 'enq #t)
                q)
        #:wait (λ (q)
                 (hash-set! call-hash 'wait #t)
                 q)
        #:active-count (λ _
                         (hash-set! call-hash 'active-count #t)
                         42)
        #:waiting-count (λ _
                          (hash-set! call-hash 'waiting-count #t)
                          24))))
    (begin (process-queue-empty? mock-q)
           (hash-ref call-hash 'empty? #f))
    (begin (process-queue-enqueue mock-q (thunk 42))
           (hash-ref call-hash 'enq #f))
    (begin (process-queue-wait mock-q)
           (hash-ref call-hash 'wait #f))
    (test-= (process-queue-active-count mock-q)
            42)
    (hash-ref call-hash 'active-count #f)
    (test-= (process-queue-waiting-count mock-q)
            24)
    (hash-ref call-hash 'waiting-count #f)
    (test-= (process-queue-get-data (process-queue-set-data mock-q 2))
            2)))
