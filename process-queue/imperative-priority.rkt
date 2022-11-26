#lang at-exp racket

(provide (contract-out
          [make-process-queue
           ({(and/c natural? (>/c 0))}
            {any/c
             (-> any/c any/c boolean?)
             #:kill-older-than (or/c positive-integer? #f)}
            . ->* .
            (and/c process-queue?
                   process-queue-empty?))])
         (all-from-out "private/interface.rkt"))

(require "private/interface.rkt"
         "private/generic-imperative-process-queue.rkt"
         data/heap)

(struct prioritized-process (thunk priority))
(define ((make-prioritized-process-comparator <) a b)
  (< (prioritized-process-priority a)
     (prioritized-process-priority b)))

(define (make-process-queue process-limit
                            [data-init #f]
                            [priority> >]
                            ;; These timeouts are enforced on a best-effort basis.
                            ;; I.e. whenever we "notice" a process that has exceeded its timeout,
                            ;; we kill it. But no guarantees about how quickly we will notice.
                            #:kill-older-than [proc-timeout-secs #f])
  (make-generic-imperative-process-queue process-limit
                                         (λ () (make-heap (make-prioritized-process-comparator priority>)))
                                         (λ (q v [priority #f])
                                           (heap-add! q (prioritized-process v (or priority 0))))
                                         (λ (q)
                                           (begin0 (prioritized-process-thunk (heap-min q))
                                             (heap-remove-min! q)))
                                         heap-count
                                         #:data data-init
                                         #:kill-older-than proc-timeout-secs))
(module+ test
  (require "private/test-common.rkt")

  (test-imperative-process-queue-basics make-process-queue)
  (test-priority-process-queue-basics make-process-queue))

