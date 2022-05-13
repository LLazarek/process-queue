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
  (require ruinit)

  (struct simple-process-info (stdout stdin pid stderr)
    #:transparent)
  (define (simple-process cmd will)
    (define proc-info+ctl (process cmd))
    (process-info (apply simple-process-info
                         (drop-right proc-info+ctl 1))
                  (last proc-info+ctl)
                  will))

  (define (close-process-ports! info)
    (match-define (struct* process-info
                           ([data (struct* simple-process-info
                                           ([stdout stdout]
                                            [stdin stdin]
                                            [stderr stderr]))]))
      info)
    (close-output-port stdin)
    (close-input-port stdout)
    (close-input-port stderr))

  (test-begin
    #:name basic
    (ignore (define q (make-process-queue 1))
            (define will-called? (box #f))
            (define q1 (process-queue-enq q
                                      (λ _
                                        (simple-process
                                         "sleep 1; echo hi"
                                         (λ (q info)
                                           (set-box! will-called? #t)
                                           (close-process-ports! info)
                                           q))))))
    (test-= (process-queue-waiting-count q1) 0)
    (not (unbox will-called?))
    (test-= (process-queue-active-count q1) 1)

    (ignore (define q1* (process-queue-wait q1)))
    (test-= (process-queue-waiting-count q1*) 0)
    (test-= (process-queue-waiting-count q1) 0)
    (unbox will-called?)
    (test-= (process-queue-active-count q1*) 0)
    (test-= (process-queue-active-count q1) 1))

  (test-begin
    #:name will
    (ignore (define q (make-process-queue 1))
            (define will-1-called? (box #f))
            (define will-2-called? (box #f))
            (define will-3-called? (box #f))
            (define q1 (process-queue-enq q
                                      (λ _
                                        (simple-process
                                         "sleep 2; echo hi"
                                         (λ (q* info)
                                           (set-box! will-1-called? #t)
                                           (close-process-ports! info)
                                           q*)))))
            (define q2 (process-queue-enq q1
                                      (λ _
                                        (simple-process
                                         "echo good"
                                         (λ (q* info)
                                           (set-box! will-2-called? #t)
                                           (close-process-ports! info)
                                           (process-queue-enq
                                            q*
                                            (λ _
                                              (simple-process
                                               "echo bye"
                                               (λ (q** info)
                                                 (set-box! will-3-called? #t)
                                                 (close-process-ports! info)
                                                 q**))))))))))
    (test-= (process-queue-active-count q1) 1)
    (test-= (process-queue-waiting-count q1) 0)
    (test-= (process-queue-active-count q2) 1)
    (test-= (process-queue-waiting-count q2) 1)
    (not (unbox will-1-called?))
    (not (unbox will-2-called?))
    (not (unbox will-3-called?))

    (ignore (define q2* (process-queue-wait q2)))
    (test-= (process-queue-waiting-count q1) 0)
    (test-= (process-queue-waiting-count q2) 1)
    (test-= (process-queue-waiting-count q2*) 0)
    (test-= (process-queue-active-count q1) 1)
    (test-= (process-queue-active-count q2) 1)
    (test-= (process-queue-active-count q2*) 0)
    (unbox will-1-called?)
    (unbox will-2-called?)
    (unbox will-3-called?))

  (test-begin
    #:name a-little-complex
    (ignore (define q (make-process-queue 2))
            (define wills-called? (vector #f #f #f #f #f))
            (define (will-for i)
              (λ (the-q* info)
                (vector-set! wills-called?
                             i
                             (port->string (simple-process-info-stdout
                                            (process-info-data info))))
                (close-process-ports! info)
                (match i
                  [(or 0 2)
                   (define i+ (match i
                                [0 3]
                                [2 4]))
                   (process-queue-enq
                    the-q*
                    (λ _ (simple-process @~a{sleep 0.5 && echo @i+}
                                         (will-for i+))))]
                  [else the-q*])))
            (define the-q
              (for/fold ([the-q q])
                        ([i (in-range 3)])
                (process-queue-enq
                 the-q
                 (λ _
                   (simple-process @~a{sleep 0.5 && echo @i}
                                   (will-for i)))))))
    (test-= (process-queue-active-count the-q) 2)
    (test-= (process-queue-waiting-count the-q) 1)

    (ignore (define the-q* (process-queue-wait the-q)))
    (test-= (process-queue-active-count the-q) 2)
    (test-= (process-queue-waiting-count the-q) 1)
    (test-= (process-queue-active-count the-q*) 0)
    (test-= (process-queue-waiting-count the-q*) 0)
    (for/and/test ([i (in-range 5)])
      (test-equal? (vector-ref wills-called? i)
                   (~a i "\n"))))

  (test-begin
    #:name wait
    (process-queue-empty? (process-queue-wait (make-process-queue 2))))

  (test-begin
    #:name process-limit
    ;; observed bug:
    ;; You have three active procs,
    ;; 0. running, will: nothing
    ;; 1. done, will: spawn another
    ;; 2. running, will: nothing
    ;;
    ;; And one waiting proc,
    ;; 3. waiting, will: nothing
    ;;
    ;; Now you call wait. It sweeps 1, and executes its will which enq's another
    ;; proc (4), which is waiting at first, but then sweep is called again,
    ;; which decides to spawn 3 because only two procs are active, filling the Q
    ;; back up to three active and that sweep call returns. But now we return to
    ;; the first sweep call, which says that it has a `free-spawning-capacity`
    ;; of 1 and pulls 4 off the waiting list and spawns a proc, making four
    ;; active procs at once!
    (ignore (define current-active (box 0))
            (define active-history (box empty))
            (define (record-active-proc! do)
              (define new-active (do (unbox current-active)))
              (set-box! current-active new-active)
              (set-box! active-history
                        (cons new-active
                              (unbox active-history))))
            (define (enq-proc! q i)
              (process-queue-enq q
                             (λ _
                               (record-active-proc! add1)
                               (simple-process
                                (match i
                                  [2 "echo done"]
                                  [else "sleep 2"])
                                (λ (q* info)
                                  (record-active-proc! sub1)
                                  (close-process-ports! info)
                                  (match i
                                    [2 (enq-proc! q* 4)]
                                    [else q*]))))))
            (define q (for/fold ([q (make-process-queue 3)])
                                ([i (in-range 4)])
                        (enq-proc! q i)))
            (define q* (process-queue-wait q)))
    (extend-test-message
     (not (findf (>/c 3) (unbox active-history)))
     "process-queue spawns active processes exceeding the process limit"))

  (test-begin
    #:name wait/long-running-procs
    (ignore
     (define done-vec (vector #f #f #f #f))
     (define q (for/fold ([q (make-process-queue 2)])
                         ([i (in-range 4)])
                 (process-queue-enq q
                                (λ _
                                  (simple-process
                                   "sleep 10"
                                   (λ (q* info)
                                     (close-process-ports! info)
                                     (vector-set! done-vec i #t)
                                     q*))))))
     (define q/done (process-queue-wait q)))
    (process-queue-empty? q/done)
    (andmap identity (vector->list done-vec))))
