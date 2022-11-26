#lang at-exp racket

(define queue? any/c)
(provide (contract-out
          [make-generic-imperative-process-queue
           ({(and/c natural? (>/c 0))
             (-> queue?)
             (-> queue? any/c any/c void?)
             (-> queue? any/c)
             (-> queue? natural?)}
            {#:data any/c
             #:kill-older-than (or/c positive-integer? #f)}
            . ->* .
            (and/c process-queue?
                   process-queue-empty?))]))

(require "interface.rkt"
         (submod "interface.rkt" internal)
         (prefix-in gen: "generic-functional-process-queue.rkt"))

(define (make-generic-imperative-process-queue process-limit
                                               make-queue ; -> queue?
                                               enqueue! ; queue? any/c any/c -> void?
                                               dequeue! ; queue? -> any/c
                                               queue-length ; queue? -> natural?
                                               #:data [data-init #f]
                                               ;; These timeouts are enforced on a best-effort basis.
                                               ;; I.e. whenever we "notice" a process that has exceeded its timeout,
                                               ;; we kill it. But no guarantees about how quickly we will notice.
                                               #:kill-older-than [proc-timeout-secs #f])
  (gen:generic-functional-process-queue
   imperative-process-queue-empty?
   (λ (q v [extra #f])
     (gen:enq-process q v extra)
     q)
   (λ (q)
     (gen:wait q)
     q)
   (compose1 set-count gen:generic-functional-process-queue-active)
   (compose1 queue-length gen:generic-functional-process-queue-waiting)
   imperative-process-queue-get-mutable-data
   imperative-process-queue-set-mutable-data!

   (box data-init)

   process-limit
   (mutable-set)
   0 ; ignored
   (make-queue)
   0 ; ignored

   proc-timeout-secs

   (λ (q v extra)
     (enqueue! q v extra)
     q)
   (λ (q)
     (list q
           (dequeue! q)))
   (λ (el set)
     (set-add! set el)
     set)
   (λ (select set)
     (define unselected (mutable-set))
     (for ([el (in-list (set->list set))]
           #:unless (select el))
       (set-remove! set el)
       (set-add! unselected el))
     (values set unselected))))

(define (imperative-process-queue-get-mutable-data q)
  (unbox (process-queue-data q)))

(define (imperative-process-queue-set-mutable-data! q v)
  (set-box! (process-queue-data q) v)
  q)

(define (imperative-process-queue-empty? q)
  (and (zero? (process-queue-active-count q))
       (zero? (process-queue-waiting-count q))))

(module+ test
  (require "test-common.rkt")

  (test-imperative-process-queue-basics (λ (pl #:kill-older-than [kot #f])
                                          (make-generic-imperative-process-queue
                                           pl
                                           (λ () (box empty))
                                           (λ (q v ignored)
                                             (set-box! q (append (unbox q) (list v))))
                                           (λ (q)
                                             (define l (unbox q))
                                             (define v (first l))
                                             (set-box! q (rest l))
                                             v)
                                           (compose1 length unbox)
                                           #:kill-older-than kot))))

