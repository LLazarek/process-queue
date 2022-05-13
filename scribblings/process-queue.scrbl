#lang scribble/manual

@(require scribble/core)

@;;;;;;;;;;;;;;;@
@; Boilerplate ;@
@;;;;;;;;;;;;;;;@

@(require (for-label racket process-queue)
          scribble/example)

@(define process-queue-eval (make-base-eval))
@examples[#:eval process-queue-eval #:hidden (require racket process-queue)]

@title{process-queue}
@author{Lukas Lazarek}

@section{Concepts}

This library implements a queue to manage the execution of many OS level processes in parallel.
As such, the primary abstractions of the library are the process and the queue.

Processes are described by a function that launches something and returns information about the launched thing.
Specifically, the function returns @racket[process-info], which packages together
@itemlist[
@item{a function to get the status of the process as it runs, in the style of @racket[process],}
@item{a @tech{process will}, and}
@item{some data to associate with the process}
]
The @deftech{process will} describes what to do with a process when it terminates.
For instance, the will might collect the results of the process, perform cleanup, or even enqueue a new process to "follow up" on the one that just terminated.
In detail, the will is a function accepting the current state of the process queue and the process's @racket[process-info] and producing an updated process queue.

The process queue, then, is a data structure that keeps track of the processes actively running and those waiting to run.
The operations on the process queue add processes to the waiting list, manage process termination and will execution, and wait for all processes to terminate.

For convenience, the process queue also has a field for storing client data.
This is useful for storing information related to processes on the side, since the queue is accessible to process wills, for instance.


This library provides three implementations of this basic concept which all conform to the same @secref{interface}.
First, @secref{functional} implements the basic process queue in a functional style (such that the process queue is immutable).
Second, @secref{priority} implements a priority-queue version in a functional style.
Finally, @secref{imperative} implements an imperative version of the basic process queue.

@section[#:tag "interface"]{Process queue interface}
@defmodule[process-queue]

@defproc[(process-queue? [q any/c]) boolean?]{
The predicate recognizing process queues.
}

@defproc[(process-queue-empty? [q process-queue?]) boolean?]{
A process queue is empty if it has no actively running processes, and no waiting processes.
}

@defproc[(process-queue-enqueue [q process-queue?] [launch (-> process-info/c)] [extra-data any/c #f]) process-queue?]{
Enqueues a process on the queue.
@racket[launch] should launch the process (e.g. with @racket[process], but not necessarily) and return its information.

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
Sets the value of the data field.
}
@defproc[(process-queue-get-data [q process-queue?]) any/c]{
Gets the value of the data field.
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
The contract for process wills.
}



@section[#:tag "functional"]{Functional process queue}
@defmodule[process-queue]

@defproc[(make-process-queue [active-limit positive-integer?]
			     [data any/c #f]
			     [#:kill-older-than process-timeout-seconds (or/c positive-real? #f) #f])
			     (and/c process-queue? process-queue-empty?)]{
Creates an empty functional process queue.

@racket[active-limit] is the maximum number of processes that can be active at once.

@racket[process-timeout-seconds], if non-false, specifies a "best effort" limit on the real running time of each process in seconds.
Best effort here means that the timeout is not strictly enforced in terms of timing --- i.e. a process may run for longer than @racket[process-timeout-seconds].
Instead, the library checks for over-running processes periodically while performing other operations on the queue and kills any that it finds.
This is useful as a crude way to ensure that no process runs forever.
If you need precise/strict timeout enforcement, you might consider using the @tt{timeout} unix utility or other racket tools, and using @racket[process-timeout-seconds] as a fallback.
}

@section[#:tag "priority"]{Functional process priority queue}
@defmodule[process-queue/priority]

Functional process priority queues prioritize processes to run first using a sorting function, provided when creating the queue.
These queues use the third argument of @racket[process-queue-enqueue] as the priority, which defaults to 0.

@defproc[(make-process-queue [active-limit positive-integer?]
			     [data any/c #f]
			     [priority< (any/c any/c . -> . boolean?) <]
			     [#:kill-older-than process-timeout-seconds (or/c positive-real? #f) #f])
			     (and/c process-queue? process-queue-empty?)]{
Creates an empty functional process priority queue, which prioritizes processes according to @racket[priority<].

See @secref{functional} for more details on the remaining arguments.
}

@section[#:tag "imperative"]{Imperative process queue}
@defmodule[process-queue/imperative]

The imperative process queue implementation mutates a single process queue in-place instead of functionally transforming it.
Hence, all of the @secref{interface} operations that return a new process queue simply return the input queue after mutating it.

@defproc[(make-process-queue [active-limit positive-integer?]
			     [data any/c #f]
			     [#:kill-older-than process-timeout-seconds (or/c positive-real? #f) #f])
			     (and/c process-queue? process-queue-empty?)]{
Creates an empty imperative process priority queue.

See @secref{functional} for more details on the remaining arguments.
}

