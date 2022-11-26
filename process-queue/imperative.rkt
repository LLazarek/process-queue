#lang at-exp racket

(provide (contract-out
          [make-process-queue
           ({(and/c natural? (>/c 0))}
            {any/c
             #:kill-older-than (or/c positive-integer? #f)}
            . ->* .
            (and/c process-queue?
                   process-queue-empty?))])
         (all-from-out "private/interface.rkt"))

(require "private/interface.rkt"
         "private/generic-imperative-process-queue.rkt"
         data/queue)

(define (make-process-queue process-limit
                            [data-init #f]
                            ;; These timeouts are enforced on a best-effort basis.
                            ;; I.e. whenever we "notice" a process that has exceeded its timeout,
                            ;; we kill it. But no guarantees about how quickly we will notice.
                            #:kill-older-than [proc-timeout-secs #f])
  (make-generic-imperative-process-queue process-limit
                                         make-queue
                                         (λ (q v extra)
                                           (enqueue! q v))
                                         dequeue!
                                         queue-length
                                         #:data data-init
                                         #:kill-older-than proc-timeout-secs))
(module+ test
  (require "private/test-common.rkt")

  (test-imperative-process-queue-basics make-process-queue))

