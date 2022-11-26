#lang scribble/manual

@;;;;;;;;;;;;;;;@
@; Boilerplate ;@
@;;;;;;;;;;;;;;;@

@(require (for-label racket process-queue)
          process-queue
          scribble/example)

@(define process-queue-eval (make-base-eval))
@examples[#:eval process-queue-eval #:hidden (require racket)]

@title{process-queue}
@author[(author+email "Lukas Lazarek" "lukas.lazarek@eecs.northwestern.edu"
#:obfuscate? #t)]

@defmodule[process-queue]

This library implements a queue to manage the execution of many OS level processes in parallel.

Processes are described by a function that launches something and returns information about the launched thing.
Specifically, the function returns @racket[process-info], which packages together
@itemlist[
@item{a function to control the process as it runs, in the style of @racket[process],}
@item{a @tech{process will}, and}
@item{data to associate with the process}
]

The @deftech{process will} describes what to do with a process when it terminates.
For instance, the will might collect the results of the process, perform cleanup, and even enqueue a new process to "follow up" on the one that just terminated.
In detail, the will is a function accepting the current state of the process queue and the process's @racket[process-info] and producing an updated process queue.

The process queue keeps track of the processes actively running and those waiting to run.
The operations on the process queue add processes to the waiting list, manage process termination and will execution, and wait for all processes to terminate.

For convenience, the process queue also has a field for storing client data.
This is useful for storing information related to processes on the side, since the queue is accessible to process wills.


This library provides several implementations of this basic concept which all conform to the same @secref{interface}.
First, @secref{functional} implements the basic process queue in a functional style (such that the process queue is immutable).
Second, @secref{priority} implements a priority-queue version in a functional style.
Third, @secref{imperative} implements an imperative version of the basic process queue.
Finally, @secref{imperative-priority} implements a priority-queue version in an imperative style.


@section{Example usage}

This is an illustrative example of the usage of the @secref{imperative} implementation.

It creates a queue that runs up to two processes simultaneously, and then enqueues three processes to be run (1, 2, and 4).

The first two are launched immediately.
The second process terminates quickly, and its @tech{process will} enqueues a followup process (3).

Process 4 is then launched, and after it terminates process 3 is launched and terminates.

Finally process 1, which had been running since the start, terminates.

@examples[#:eval process-queue-eval
(require process-queue)

(define (simple-process cmd will)
  (match-define (list stdout stdin pid stderr ctl) (process cmd))
  (close-output-port stdin)
  (close-input-port stderr)
  (process-info stdout
                ctl
                will))

(define (will:show-result/close-ports q info)
  (define stdout (process-info-data info))
  (displayln (port->string stdout))
  (close-input-port stdout)
  q)

(define q (make-process-queue 2))
(begin
  (process-queue-enqueue q
  			 (λ ()
			   (displayln "launch 1")
			   (simple-process "sleep 5; echo done 1"
					   will:show-result/close-ports)))
  (process-queue-enqueue q
  			 (λ ()
			   (displayln "launch 2")
			   (simple-process "sleep 1; echo done 2"
					   (λ (q info)
					     (will:show-result/close-ports q info)
					     (process-queue-enqueue
					      q
					      (λ ()
			   		        (displayln "launch 3")
					        (simple-process "echo done 3"
						                will:show-result/close-ports)))))))
  (process-queue-enqueue q
  			 (λ ()
			   (displayln "launch 4")
			   (simple-process "echo done 4"
					   will:show-result/close-ports)))

  (void (process-queue-wait q)))
]


@section[#:tag "interface"]{Process queue interface}

All of the implementations below provide the following operations on process queues.

@defproc[(process-queue? [q any/c]) boolean?]{
The predicate recognizing process queues.
}

@defproc[(process-queue-empty? [q process-queue?]) boolean?]{
A process queue is empty if it has no actively running processes and no waiting processes.
}

@defproc[(process-queue-enqueue [q process-queue?] [launch (-> process-info/c)] [extra-data any/c #f]) process-queue?]{
Enqueues a process on the queue.
@racket[launch] should launch the process (e.g. with @racket[process], but not necessarily) and return its @racket[process-info].

@racket[extra-data] provides optional extra information that may or may not be used depending on the implementation (for example, a priority value).

Returns the updated process queue.

}

@defproc[(process-queue-wait [q process-queue?]) (and/c process-queue? process-queue-empty?)]{
Blocks waiting for all of the processes in the queue to terminate, handling the @tech{process will}s of processes as they terminate.
}

@defproc[(process-queue-active-count [q process-queue?]) natural?]{
Returns the number of actively running processes at the time of call.
}

@defproc[(process-queue-waiting-count [q process-queue?]) natural?]{
Returns the number of waiting processes at the time of call.
}

@defproc[(process-queue-set-data [q process-queue?] [data any/c]) process-queue?]{
Sets the value of the queue's data field.
}
@defproc[(process-queue-get-data [q process-queue?]) any/c]{
Gets the value of the queue's data field.
}

@defstruct*[process-info ([data any/c] [ctl ((or/c 'status 'wait 'interrupt 'kill) . -> . any)] [will process-will/c])]{
The struct packaging together information about a running process.
}
@defthing[#:kind "contract" process-info/c contract? #:value (struct/c process-info
		 	    		   	     	     	       any/c
								       ((or/c 'status 'wait 'interrupt 'kill) . -> . any)
								       process-will/c)]{
The contract for @racket[process-info]s.
}
@defthing[#:kind "contract" process-will/c contract? #:value (process-queue? process-info? . -> . process-queue?)]{
The contract for @tech{process wills}.
}




@section[#:tag "imperative"]{Imperative process queues}

These imperative process queue implementations mutate a single process queue in-place.
Hence, all of the @secref{interface} operations that return a new process queue simply return the input queue after mutating it.
(The interface returns the queue to support the @secref{functional}.)

@subsection[#:tag "imperative-plain"]{Plain imperative process queue}

@defproc[(make-process-queue [active-limit positive-integer?]
			     [data any/c #f]
			     [#:kill-older-than process-timeout-seconds (or/c positive-real? #f) #f])
			     (and/c process-queue? process-queue-empty?)]{
Creates an empty imperative process queue.

@racket[active-limit] is the maximum number of processes that can be active at once.

@racket[data] initializes the data field of the queue which can be accessed with @racket[process-queue-get-data] and @racket[process-queue-set-data].

@racket[process-timeout-seconds], if non-false, specifies a "best effort" limit on the real running time of each process in seconds.
Best effort here means that the timeout is not strictly enforced in terms of timing --- i.e. a process may run for longer than @racket[process-timeout-seconds].
Instead, the library checks for over-running processes periodically while performing other operations on the queue and kills any that it finds.
This is useful as a crude way to ensure that no process runs forever.
If you need precise/strict timeout enforcement, you might consider using the @tt{timeout} unix utility or other racket tools, and using @racket[process-timeout-seconds] as a fallback.

}



@subsection[#:tag "imperative-priority"]{Imperative process priority queue}
@defmodule[process-queue/imperative-priority]

Imperative process priority queues prioritize processes to run first using a sorting function, provided when creating the queue.
These queues use the third argument of @racket[process-queue-enqueue] as the priority, which defaults to 0 if not provided.

@defproc[(make-process-queue [active-limit positive-integer?]
			     [data any/c #f]
			     [priority> (any/c any/c . -> . boolean?) >]
			     [#:kill-older-than process-timeout-seconds (or/c positive-real? #f) #f])
			     (and/c process-queue? process-queue-empty?)]{
Creates an empty imperative process priority queue, which prioritizes processes according to @racket[priority>].

See @secref{imperative-plain} for more details on the remaining arguments.
}




@section[#:tag "functional"]{Functional process queues}

These implementations mirror the imperative versions, but all queue operations functionally transform the queue instead of mutating it in-place.

That means that you (the user of this library) must thread the queue around your program and take care never to use a stale queue: @bold{only the latest queue is valid}, because queue values reflect the state of external and stateful processes.
Hence, the functional implementations are a little strange, and you're probably better off using the @secref{imperative}.
That said, sometimes a functional interface fits better with the rest of one's program structure, caveats and all.

@subsection{Plain functional process queue}
@defmodule[process-queue/functional]

@defproc[(make-process-queue [active-limit positive-integer?]
			     [data any/c #f]
			     [#:kill-older-than process-timeout-seconds (or/c positive-real? #f) #f])
			     (and/c process-queue? process-queue-empty?)]{
Creates an empty functional process queue.

See @secref{imperative-plain} for more details on the remaining arguments.

}



@subsection[#:tag "priority"]{Functional process priority queue}
@defmodule[process-queue/priority]

Functional process priority queues prioritize processes to run first using a sorting function, provided when creating the queue.
These queues use the third argument of @racket[process-queue-enqueue] as the priority, which defaults to 0 if not provided.

@defproc[(make-process-queue [active-limit positive-integer?]
			     [data any/c #f]
			     [priority> (any/c any/c . -> . boolean?) >]
			     [#:kill-older-than process-timeout-seconds (or/c positive-real? #f) #f])
			     (and/c process-queue? process-queue-empty?)]{
Creates an empty functional process priority queue, which prioritizes processes according to @racket[priority>].

See @secref{imperative-plain} for more details on the remaining arguments.
}

