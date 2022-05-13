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
  (require "test-common.rkt")

  (test-process-queue-basics
   (λ (n [d #f] #:kill-older-than [kot #f])
     (make-generic-functional-process-queue n
                                            (thunk empty)
                                            (λ (q v ignored) (append q (list v)))
                                            (λ (q) (list (rest q) (first q)))
                                            d
                                            #:kill-older-than kot)))
  
  (test-priority-process-queue-basics
   (λ (n)
     (make-generic-functional-process-queue n
                                            (thunk empty)
                                            (λ (q v priority)
                                              (append q (list (list v priority))))
                                            (λ (q)
                                              (define el (argmin second q))
                                              (list (remove el q) (first el)))))))
