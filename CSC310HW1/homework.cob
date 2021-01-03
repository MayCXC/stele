program-id. homework.

environment division.
configuration section.
       repository.
           function all intrinsic.

data division.
       working-storage section.
           01 selection pic X. *> keyboard input
           01 quantum pic V999 value .001. *> tick delay
           01 ones.
               10 thickness pic 9.     *> max threads per process
               10 repetition pic 999.  *> how long to spend creating processes
               10 creating pic 999.    *> next PID to create
               10 terminating pic 999. *> next PID to remove
               10 paging pic 999.      *> next PID to page
               10 folio pic 999.       *> page width
               10 recto pic 99999.     *> page end
               10 verso pic 99999.     *> page start
               10 dogear pic 99999.    *> next verso

           01 processfactory. *> process record with fields taken from copybook
               copy "myprocess.cpy" replacing
                   ==:n:== by 2
                   ==:tag:== by ==processfactory==
                   ==:stacksize:== by 100.

           01 threadfactory. *> thread record with fields taken from copybook
               copy "mythread.cpy" replacing
                   ==:n:== by 2
                   ==:tag:== by ==threadfactory==
                   ==:stacksize:== by 100.

           *> welcome to hell :^)
           01 processlist occurs 100 times indexed by i, ii, iii, iiii. *> process stack
               copy "myprocess.cpy" replacing
                   ==:n:== by 2
                   ==:tag:== by ==processlist==
                   ==:stacksize:== by 100.

           01 threadlist occurs 400 times indexed by j, jj, jjj, jjjj. *> thread table
               copy "mythread.cpy" replacing
                   ==:n:== by 2
                   ==:tag:== by ==threadlist==
                   ==:stacksize:== by 100.

           01 pagetable pic 99999 value 0 occurs 100 times indexed by k. *> long term memory

           01 ram pic X value "0" occurs 10000 times indexed by l. *> short term memory

procedure division.
       initialize ones replacing numeric data by 1. *> state fields
       set i, ii, iii, iiii, j, jj, jjj, jjjj to 1. *> index fields
       set k, l to 1. *> loop fields

       request. *> interactive menu
           display "Y: create and run processes until memory runs out."
           display "T: list processes and threads."
           display "R: randomly create and terminate processes for ten minutes."
           display "E: create a process."
           display "W: terminate a process."
           display "Q: exit."
           display " "
           accept selection
           if upper-case(selection)="Y" perform roundrobin
           else if upper-case(selection)="T" perform taskmanager
           else if upper-case(selection)="R" perform randomprocess
           else if upper-case(selection)="E" perform createprocess
           else if upper-case(selection)="W" perform terminateprocess
           else if upper-case(selection)="Q" display "Goodbye." goback
           else display "Invalid input."
           end-if.

       perform request.

       randomprocess. *> create a new process with random PID and thickness
           perform varying repetition from 600 by -1 until repetition=0
               display "round " repetition
               if random() > i/100.0 perform createprocess end-if
               if random() < i/100.0 perform terminateprocess end-if
               if mod(repetition, 15)=0 perform taskmanager end-if
               call "C$SLEEP" using quantum end-call
           end-perform
           display "shutting down."
           perform terminateprocess until i=1.

       createprocess. *> create a new process
           display "creating process..."
           if i<=100
               perform varying ii from 1 by 1 until ii=i *> look for unused PID
                   move processlist(ii) to processfactory
                   if creating=processfactory-identity
                       set creating to mod(creating,100)
                       set creating up by 1
                       set ii to 1
                   end-if
               end-perform

               initialize processfactory replacing *> reset process fields
                   numeric data by 0
                   alphanumeric data by " "
               set processfactory-identity to creating
               compute thickness = random()*4
               set thickness up by 1

               perform createthread varying thickness *> add process threads
                   from thickness by -1 until thickness=0
               move processfactory to processlist(i)
               set i up by 1
           else display "processes list full!"
           end-if.

       createthread. *> create a new thread
           display "creating thread..."
           initialize threadfactory replacing
                   numeric data by 0
                   alphanumeric data by " "
           if j<=400 *> threads have the same parent
               set threadfactory-identity to thickness
               set threadfactory-parent to creating
               set processfactory-children up by 1
               move threadfactory to threadlist(j)
               set j up by 1
           else display "threads list full!"
           end-if.

       terminateprocess. *> remove a process
           compute terminating = i - 1 *> range of processes
           compute terminating = terminating * random()
           compute terminating = terminating + 1
           move processlist(terminating) to processfactory *> read process fields
           move processfactory-identity to terminating *> reuse index

           display "terminating process #" terminating "..."
           if i>1 *> final boss
               copy "remove.cpy" replacing
                   ==:backwards:==    by ==ii==
                   ==:finish:==       by ==i==
                   ==:tablefactory:== by ==processfactory==
                   ==:condition:==    by ==processfactory-identity=terminating==
                   ==:forewards:==    by ==iii==
                   ==:shift:==        by ==iiii==
                   ==:tablelist:==    by ==processlist==.
               copy "remove.cpy" replacing
                   ==:backwards:==    by ==jj==
                   ==:finish:==       by ==j==
                   ==:tablefactory:== by ==threadfactory==
                   ==:condition:==    by ==threadfactory-parent=terminating==
                   ==:forewards:==    by ==jjj==
                   ==:shift:==        by ==jjjj==
                   ==:tablelist:==    by ==threadlist==.
           else display "out of processes!"
           end-if.
                  
       taskmanager. *> display process stack
           display " "
           compute ii = i - 1
           display ii " processes are active"
           display " "
           perform varying ii from 1 by 1 until ii=i
               move processlist(ii) to processfactory
               display "process #" processfactory-identity ": " processfactory-children " threads"
           end-perform
           display " "
           compute jj = j - 1
           display jj " threads are active"
           display " "
           perform varying jj from 1 by 1 until jj=j
               move threadlist(jj) to threadfactory
               display "thread #" threadfactory-identity ": " threadfactory-parent " is parent"
           end-perform.

       roundrobin. *> run processes with round robin scheduler
           perform createprocess until i=100
           perform varying paging from 1 by 1 until recto>10000
               perform pageprocess
               if pagetable(k)>0
                   display "running process #" paging
                   call "C$SLEEP" using quantum end-call
               else
                  display "out of memory!"
               end-if
               compute paging = mod(paging,i)
           end-perform.

       pageprocess. *> put process memory in ram
           display "paging process #" paging "..."
           set k to paging
           if pagetable(k)>0
               display "process is mapped to memory address " pagetable(k)
           else
               display "page fault!"
               perform firstfit
           end-if.

       firstfit. *> fit process memory in ram
           set ii to paging
           move processlist(ii) to processfactory
           set folio to 1
           set folio up by processfactory-children
           compute folio = folio * 100
           compute verso = 1
           compute recto = verso + folio

           perform varying k from 1 by 1 until k=i *> virtual memory without fragmentation
               if pagetable(k)>0
                   set ii to k
                   move processlist(ii) to processfactory
                   set dogear to 1
                   set dogear up by processfactory-children
                   compute dogear = dogear * 100
                   compute dogear = dogear + pagetable(k)
                   if( (pagetable(k) <= verso and verso < dogear) or
                       (pagetable(k) <= recto and recto < dogear)
                   )
                       compute verso = dogear
                       compute recto = verso + folio
                       set k to 1
                   end-if
               end-if
           end-perform

           if recto <= 10000 *> put thread memory in ram
               display "process allocated memory [" verso "," recto ")"
               set k to paging
               set pagetable(k) to verso
               set l to verso
               move processfactory-instructions to ram(l)
               perform varying jj from 1 by 1 until jj=j
                   move threadlist(jj) to threadfactory
                   if threadfactory-parent=paging
                       set l up by 100
                       move threadfactory-instructions to ram(l)
                   end-if
               end-perform
           end-if.
