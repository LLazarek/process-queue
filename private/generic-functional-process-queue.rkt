#lang at-exp racket

(define queue? any/c)
(provide (contract-out
          [make-generic-functional-process-queue
           ({(and/c natural? (>/c 0))
             (-> queue?)
             (-> queue? any/c any/c queue?)
             (-> queue? (list/c queue? any/c))}
            {any/c
             #:kill-older-than (or/c positive-integer? #f)}
            . ->* .
            (and/c process-queue?
                   generic-functional-process-queue?
                   process-queue-empty?))]
          [generic-functional-process-queue? (any/c . -> . boolean?)]))

(require "interface.rkt"
         (submod "interface.rkt" internal)
         "common.rkt")

(struct generic-functional-process-queue process-queue (active-limit

                                                active
                                                active-count

                                                waiting
                                                waiting-count

                                                timeout

                                                enqueue ;; queue? any/c any/c -> queue?
                                                dequeue ;; queue? -> (list/c queue? any/c)
                                                ))

(define (generic-functional-process-queue-set-data q v)
  (struct-copy generic-functional-process-queue q
               [data #:parent process-queue v]))

(struct process (thunk))

(struct active-process (info start-time))

(define (make-generic-functional-process-queue process-limit
                                           make-queue ;; -> queue?
                                           enqueue ;; queue? any/c any/c -> queue?
                                           dequeue ;; queue? -> (list/c queue? any/c)
                                           [data-init #f]
                                           ;; These timeouts are enforced on a best-effort basis.
                                           ;; I.e. whenever we "notice" a process that has exceeded its timeout,
                                           ;; we kill it. But no guarantees about how quickly we will notice.
                                           #:kill-older-than [proc-timeout-secs #f])
  (generic-functional-process-queue generic-functional-process-queue-empty?
                                enq-process
                                wait
                                generic-functional-process-queue-active-count
                                generic-functional-process-queue-waiting-count
                                process-queue-data
                                generic-functional-process-queue-set-data

                                data-init

                                process-limit
                                empty
                                0
                                (make-queue)
                                0

                                proc-timeout-secs

                                enqueue
                                dequeue))

(define (generic-functional-process-queue-empty? q)
  (and (zero? (generic-functional-process-queue-active-count q))
       (zero? (generic-functional-process-queue-waiting-count q))))

;; start-process should return a process-info?
(define (enq-process q start-process [extra-arg #f])
  (define enqueue (generic-functional-process-queue-enqueue q))
  (sweep-dead/spawn-new-processes
   (struct-copy generic-functional-process-queue q
                [waiting (enqueue (generic-functional-process-queue-waiting q)
                              (process start-process)
                              extra-arg)]
                [waiting-count (add1 (generic-functional-process-queue-waiting-count q))])))

(define (wait q #:delay [delay (current-process-queue-polling-period-seconds)])
  (let loop ([current-q q])
    (define new-q (sweep-dead/spawn-new-processes current-q))
    (match new-q
      [(struct* generic-functional-process-queue
                ([active-count  0]
                 [waiting-count 0]))
       new-q]
      [else
       (sleep delay)
       (loop new-q)])))

(define (sweep-dead/spawn-new-processes q)
  (kill-timed-out-active-processes! q)
  (define-values {still-active dead}
    (partition (λ (active-proc)
                 (define ctl (process-info-ctl (active-process-info active-proc)))
                 (equal? (ctl 'status) 'running))
               (generic-functional-process-queue-active q)))
  (define dead-proc-infos (map active-process-info dead))
  (define temp-q (struct-copy generic-functional-process-queue q
                              [active still-active]
                              [active-count (length still-active)]))
  (define temp-q+wills
    (for/fold ([temp-q+wills temp-q])
              ([dead-proc-info (in-list dead-proc-infos)])
      ((process-info-will dead-proc-info) temp-q+wills dead-proc-info)))
  (define free-spawning-capacity
    (- (generic-functional-process-queue-active-limit temp-q+wills)
       (generic-functional-process-queue-active-count temp-q+wills)))
  (define procs-waiting-to-spawn
    (generic-functional-process-queue-waiting-count temp-q+wills))
  (define procs-to-spawn
    (if (< free-spawning-capacity procs-waiting-to-spawn)
        free-spawning-capacity
        procs-waiting-to-spawn))
  (for/fold ([new-q temp-q+wills])
            ([i (in-range procs-to-spawn)])
    (spawn-next-process new-q)))

(define (spawn-next-process q)
  (match q
    [(struct* generic-functional-process-queue
              ([active-limit limit]
               [active active]
               [active-count active-count]
               [waiting waiting]
               [waiting-count waiting-count]))
     (define data (process-queue-data q))
     (define dequeue (generic-functional-process-queue-dequeue q))
     (match-define (list new-waiting dequeued) (dequeue waiting))
     (define start-process (process-thunk dequeued))
     (define the-process-info (start-process))
     (struct-copy generic-functional-process-queue q
                  [active (cons (active-process the-process-info
                                                (current-seconds))
                                active)]
                  [active-count (add1 active-count)]
                  [waiting new-waiting]
                  [waiting-count (sub1 waiting-count)])]))

(define (kill-timed-out-active-processes! q)
  (define timeout (generic-functional-process-queue-timeout q))
  (when timeout
    (define now (current-seconds))
    (for ([an-active-process (in-list (generic-functional-process-queue-active q))])
      (define lifespan (- now (active-process-start-time an-active-process)))
      (when (> lifespan timeout)
        (define ctl (process-info-ctl (active-process-info an-active-process)))
        (ctl 'kill)))))


(module+ test
  (require ruinit
           (prefix-in r/sys: racket/system)
           (prefix-in Pqueue: pfds/heap/pairing))

  (struct simple-process-info (stdout stdin pid stderr)
    #:transparent)
  (define (simple-process cmd will)
    (define proc-info+ctl (r/sys:process cmd))
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
    (ignore (define q (make-generic-functional-process-queue 1
                                                         (thunk empty)
                                                         (λ (q v ignored) (append q (list v)))
                                                         (λ (q) (list (rest q) (first q)))))
            (define will-called? (box #f))
            (define q1 (enq-process q
                                    (λ _
                                      (simple-process
                                       "sleep 1; echo hi"
                                       (λ (q info)
                                         (set-box! will-called? #t)
                                         (close-process-ports! info)
                                         q))))))
    (test-= (generic-functional-process-queue-waiting-count q1) 0)
    (not (unbox will-called?))
    (test-= (generic-functional-process-queue-active-count q1) 1)

    (ignore (define q1* (wait q1)))
    (test-= (generic-functional-process-queue-waiting-count q1*) 0)
    (test-= (generic-functional-process-queue-waiting-count q1) 0)
    (unbox will-called?)
    (test-= (generic-functional-process-queue-active-count q1*) 0)
    (test-= (generic-functional-process-queue-active-count q1) 1))

  (test-begin
    #:name will
    (ignore (define q (make-generic-functional-process-queue 1
                                                         (thunk empty)
                                                         (λ (q v ignored) (append q (list v)))
                                                         (λ (q) (list (rest q) (first q)))))
            (define will-1-called? (box #f))
            (define will-2-called? (box #f))
            (define will-3-called? (box #f))
            (define q1 (enq-process q
                                    (λ _
                                      (simple-process
                                       "sleep 2; echo hi"
                                       (λ (q* info)
                                         (set-box! will-1-called? #t)
                                         (close-process-ports! info)
                                         q*)))))
            (define q2 (enq-process q1
                                    (λ _
                                      (simple-process
                                       "echo good"
                                       (λ (q* info)
                                         (set-box! will-2-called? #t)
                                         (close-process-ports! info)
                                         (enq-process
                                          q*
                                          (λ _
                                            (simple-process
                                             "echo bye"
                                             (λ (q** info)
                                               (set-box! will-3-called? #t)
                                               (close-process-ports! info)
                                               q**))))))))))
    (test-= (generic-functional-process-queue-active-count q1) 1)
    (test-= (generic-functional-process-queue-waiting-count q1) 0)
    (test-= (generic-functional-process-queue-active-count q2) 1)
    (test-= (generic-functional-process-queue-waiting-count q2) 1)
    (not (unbox will-1-called?))
    (not (unbox will-2-called?))
    (not (unbox will-3-called?))

    (ignore (define q2* (wait q2)))
    (test-= (generic-functional-process-queue-waiting-count q1) 0)
    (test-= (generic-functional-process-queue-waiting-count q2) 1)
    (test-= (generic-functional-process-queue-waiting-count q2*) 0)
    (test-= (generic-functional-process-queue-active-count q1) 1)
    (test-= (generic-functional-process-queue-active-count q2) 1)
    (test-= (generic-functional-process-queue-active-count q2*) 0)
    (unbox will-1-called?)
    (unbox will-2-called?)
    (unbox will-3-called?))

  (test-begin
    #:name a-little-complex
    (ignore (define q (make-generic-functional-process-queue 2
                                                         (thunk empty)
                                                         (λ (q v ignored) (append q (list v)))
                                                         (λ (q) (list (rest q) (first q)))))
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
                   (enq-process
                    the-q*
                    (λ _ (simple-process @~a{echo @i+}
                                         (will-for i+))))]
                  [else the-q*])))
            (define the-q
              (for/fold ([the-q q])
                        ([i (in-range 3)])
                (enq-process
                 the-q
                 (λ _
                   (simple-process @~a{echo @i}
                                   (will-for i)))))))
    (test-= (generic-functional-process-queue-active-count the-q) 2)
    (test-= (generic-functional-process-queue-waiting-count the-q) 1)

    (ignore (define the-q* (wait the-q)))
    (test-= (generic-functional-process-queue-active-count the-q) 2)
    (test-= (generic-functional-process-queue-waiting-count the-q) 1)
    (test-= (generic-functional-process-queue-active-count the-q*) 0)
    (test-= (generic-functional-process-queue-waiting-count the-q*) 0)
    (for/and/test ([i (in-range 5)])
                  (test-equal? (vector-ref wills-called? i)
                               (~a i "\n"))))

  (test-begin
    #:name wait
    (generic-functional-process-queue-empty? (wait (make-generic-functional-process-queue 2
                                                                                  (thunk empty)
                                                                                  (λ (q v ignored) (append q (list v)))
                                                                                  (λ (q) (list (rest q) (first q)))))))

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
    ;; which decides to spawn 3 because only two procs are active, filling the queue
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
              (enq-process q
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
            (define q (for/fold ([q (make-generic-functional-process-queue 3
                                                                       (thunk empty)
                                                                       (λ (q v ignored) (append q (list v)))
                                                                       (λ (q) (list (rest q) (first q))))])
                                ([i (in-range 4)])
                        (enq-proc! q i)))
            (define q* (wait q)))
    (extend-test-message
     (not (findf (>/c 3) (unbox active-history)))
     "process-queue spawns active processes exceeding the process limit"))

  (test-begin
    #:name wait/long-running-procs
    (ignore
     (define done-vec (vector #f #f #f #f))
     (define q (for/fold ([q (make-generic-functional-process-queue 2
                                                                (thunk empty)
                                                                (λ (q v ignored) (append q (list v)))
                                                                (λ (q) (list (rest q) (first q))))])
                         ([i (in-range 4)])
                 (enq-process q
                              (λ _
                                (simple-process
                                 "sleep 10"
                                 (λ (q* info)
                                   (close-process-ports! info)
                                   (vector-set! done-vec i #t)
                                   q*))))))
            (define q/done (wait q)))
    (generic-functional-process-queue-empty? q/done)
    (andmap identity (vector->list done-vec)))

  (test-begin
    #:name priority-simple
    (ignore
     (define order-box (box 0))
     (define spawn-vec (vector #f #f #f #f))
     (define (record-spawned! index)
       (define position (unbox order-box))
       (set-box! order-box (add1 position))
       (vector-set! spawn-vec index position))
     (define q (for/fold ([q (make-generic-functional-process-queue 2
                                                                (thunk empty)
                                                                (λ (q v priority)
                                                                  (append q (list (list v priority))))
                                                                (λ (q)
                                                                  (define el (argmin second q))
                                                                  (list (remove el q) (first el))))])
                         ([i (in-range 4)])
                 (enq-process q
                              (λ _
                                (record-spawned! i)
                                (simple-process
                                 "echo done"
                                 (λ (q* info)
                                   (close-process-ports! info)
                                   q*)))
                              (match i
                                [2 3]
                                [3 2]
                                [else i]))))
     (define q/done (wait q)))
    (generic-functional-process-queue-empty? q/done)
    (test-equal? spawn-vec
                 #(0 1 3 2)))

  (test-begin
    #:name priority-children
    (ignore
     (define order-box (box 0))
     (define spawn-vec (vector #f #f #f #f #f))
     (define (record-spawned! index)
       (define position (unbox order-box))
       (set-box! order-box (add1 position))
       (vector-set! spawn-vec index position))
     (define (enq-proc q i [priority i])
       (enq-process q
                    (λ _
                      (record-spawned! i)
                      (simple-process
                       (match i
                         [1 "sleep 1"]
                         [else "echo done"])
                       (λ (q* info)
                         (close-process-ports! info)
                         (match i
                           [0 (enq-proc q* 4 0)]
                           [else q*]))))
                    priority))
     (define q (for/fold ([q (make-generic-functional-process-queue 2
                                                                (thunk empty)
                                                                (λ (q v priority)
                                                                  (append q (list (list v priority))))
                                                                (λ (q)
                                                                  (define el (argmin second q))
                                                                  (list (remove el q) (first el))))])
                         ([i (in-range 4)])
                 (enq-proc q i)))
     (define q/done (wait q)))
    (generic-functional-process-queue-empty? q/done)
    (test-match spawn-vec
                (vector 0
                        1
                        _
                        _
                        2)))

  (test-begin
    #:name process-timeouts
    (ignore (define q (make-generic-functional-process-queue 2
                                                         (thunk empty)
                                                         (λ (q v ignored) (append q (list v)))
                                                         (λ (q) (list (rest q) (first q)))
                                                         empty
                                                         #:kill-older-than 2))
            (define will:record-output
              (λ (q info)
                (define output
                  (string-trim
                   (port->string
                    (simple-process-info-stdout (process-info-data info)))))
                (close-process-ports! info)
                (process-queue-set-data q
                                    (cons output
                                          (process-queue-get-data q)))))
            (define q0 (enq-process q
                                    (λ _
                                      (simple-process "sleep 1; echo hi" will:record-output))))
            (define q1 (enq-process q0
                                    (λ _
                                      (simple-process "sleep 3; echo hi" will:record-output)))))
    (test-= (generic-functional-process-queue-waiting-count q1) 0)
    (test-= (generic-functional-process-queue-active-count q1) 2)

    (ignore (define q1* (wait q1)))
    (test-= (generic-functional-process-queue-waiting-count q1*) 0)
    (test-= (generic-functional-process-queue-waiting-count q1) 0)
    (test-= (generic-functional-process-queue-active-count q1*) 0)
    (test-= (generic-functional-process-queue-active-count q1) 2)
    (test-match (process-queue-get-data q1*)
                (list-no-order "hi"
                               ;; empty bc killed
                               ""))))
