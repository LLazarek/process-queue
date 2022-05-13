#lang at-exp racket

(provide (contract-out
          [make-process-queue
           ({(and/c natural? (>/c 0))}
            {any/c (any/c any/c . -> . boolean?)}
            . ->* .
            (and/c process-queue?
                   process-queue-empty?))])
         (all-from-out "private/interface.rkt"))

(require "private/interface.rkt"
         (submod "private/interface.rkt" internal)
         "private/generic-functional-process-queue.rkt"
         (prefix-in pfds: pfds/heap/pairing))

(struct prioritized-process (thunk priority))
(define ((make-prioritized-process< <) a b)
  (< (prioritized-process-priority a)
     (prioritized-process-priority b)))

(define (make-process-queue process-limit
                            [data-init #f]
                            [priority< <]
                            ;; These timeouts are enforced on a best-effort basis.
                            ;; I.e. whenever we "notice" a process that has exceeded its timeout,
                            ;; we kill it. But no guarantees about how quickly we will notice.
                            #:kill-older-than [proc-timeout-secs #f])
  (make-generic-functional-process-queue process-limit
                                         (λ _ (pfds:heap (make-prioritized-process< priority<)))
                                         (λ (q v [priority 0])
                                           (pfds:insert (prioritized-process v priority)
                                                        q))
                                         (λ (q)
                                           (define el (pfds:find-min/max q))
                                           (define rest (pfds:delete-min/max q))
                                           (list rest (prioritized-process-thunk el)))
                                         data-init
                                         #:kill-older-than proc-timeout-secs))

(module+ test
  (require "private/test-common.rkt")

  (test-process-queue-basics make-process-queue)
  (test-priority-process-queue-basics make-process-queue))
