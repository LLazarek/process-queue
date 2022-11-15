#lang at-exp racket

(provide (contract-out
          [make-process-queue
           ({(and/c natural? (>/c 0))}
            {any/c}
            . ->* .
            (and/c process-queue?
                   process-queue-empty?))])
         (all-from-out "private/interface.rkt"))

(require "private/interface.rkt"
         (submod "private/interface.rkt" internal)
         "private/generic-functional-process-queue.rkt"
         (prefix-in pfds: pfds/queue/bankers))

(define (make-process-queue process-limit
                            [data-init #f]
                            ;; These timeouts are enforced on a best-effort basis.
                            ;; I.e. whenever we "notice" a process that has exceeded its timeout,
                            ;; we kill it. But no guarantees about how quickly we will notice.
                            #:kill-older-than [proc-timeout-secs #f])
  (make-generic-functional-process-queue process-limit
                                         (λ _ (pfds:queue))
                                         (λ (q v ignored)
                                           (pfds:enqueue v q))
                                         (λ (q)
                                           (match-define (cons head tail)
                                             (pfds:head+tail q))
                                           (list tail head))
                                         data-init
                                         #:kill-older-than proc-timeout-secs))

(module+ test
  (require "private/test-common.rkt")

  (test-process-queue-basics make-process-queue))