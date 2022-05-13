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
         (prefix-in pfds: pfds/heap/pairing))

(struct prioritized-process (thunk priority))
(define (prioritized-process< a b)
  (< (prioritized-process-priority a)
     (prioritized-process-priority b)))

(define (make-process-queue process-limit
                        [data-init #f]
                        ;; These timeouts are enforced on a best-effort basis.
                        ;; I.e. whenever we "notice" a process that has exceeded its timeout,
                        ;; we kill it. But no guarantees about how quickly we will notice.
                        #:kill-older-than [proc-timeout-secs #f])
  (make-generic-functional-process-queue process-limit
                                     (λ _ (pfds:heap prioritized-process<))
                                     (λ (q v priority)
                                       (pfds:insert (prioritized-process v priority)
                                                    q))
                                     (λ (q)
                                       (define el (pfds:find-min/max q))
                                       (define rest (pfds:delete-min/max q))
                                       (list rest (prioritized-process-thunk el)))
                                     data-init
                                     #:kill-older-than proc-timeout-secs))

(module+ test
  (require "test-common.rkt")

  (test-process-queue-basics make-process-queue)
  (test-priority-process-queue-basics make-process-queue))
