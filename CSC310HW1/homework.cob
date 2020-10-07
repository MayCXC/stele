program-id. homework.

environment division.
configuration section.
       repository.
           function all intrinsic.

data division.
       working-storage section.
           01 selection pic X.
           01 quantum pic V999 value .001.
           01 ones.
               10 thickness pic 9.
               10 repetition pic 999.
               10 creating pic 999.
               10 terminating pic 999.
               10 paging pic 999.
               10 folio pic 999.
               10 recto pic 99999.
               10 verso pic 99999.
               10 dogear pic 99999.

           01 processfactory.
               copy "myprocess.cpy" replacing
                   ==:n:== by 2
                   ==:tag:== by ==processfactory==
                   ==:stacksize:== by 100.

           01 threadfactory.
               copy "mythread.cpy" replacing
                   ==:n:== by 2
                   ==:tag:== by ==threadfactory==
                   ==:stacksize:== by 100.

           *> welcome to hell :^)
           01 processlist occurs 100 times indexed by i, ii, iii, iiii.
               copy "myprocess.cpy" replacing
                   ==:n:== by 2
                   ==:tag:== by ==processlist==
                   ==:stacksize:== by 100.

           01 threadlist occurs 400 times indexed by j, jj, jjj, jjjj.
               copy "mythread.cpy" replacing
                   ==:n:== by 2
                   ==:tag:== by ==threadlist==
                   ==:stacksize:== by 100.

           01 pagetable pic 99999 value 0 occurs 100 times indexed by k.

           01 ram pic X value "0" occurs 10000 times indexed by l.

procedure division.
       initialize ones replacing numeric data by 1.
       set i, ii, iii, iiii, j, jj, jjj, jjjj to 1.
       set k, l to 1.

       request.
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

       randomprocess.
           perform varying repetition from 600 by -1 until repetition=0
               display "round " repetition
               if random() > i/100.0 perform createprocess end-if
               if random() < i/100.0 perform terminateprocess end-if
               if mod(repetition, 15)=0 perform taskmanager end-if
               call "C$SLEEP" using quantum end-call
           end-perform
           display "shutting down."
           perform terminateprocess until i=1.

       createprocess.
           display "creating process..."
           if i<=100
               perform varying ii from 1 by 1 until ii=i
                   move processlist(ii) to processfactory
                   if creating=processfactory-identity
                       set creating to mod(creating,100)
                       set creating up by 1
                       set ii to 1
                   end-if
               end-perform

               initialize processfactory replacing
                   numeric data by 0
                   alphanumeric data by " "
               set processfactory-identity to creating
               compute thickness = random()*4
               set thickness up by 1

               perform createthread varying thickness
                   from thickness by -1 until thickness=0
               move processfactory to processlist(i)
               set i up by 1
           else display "processes list full!"
           end-if.

       createthread.
           display "creating thread..."
           initialize threadfactory replacing
                   numeric data by 0
                   alphanumeric data by " "
           if j<=400
               set threadfactory-identity to thickness
               set threadfactory-parent to creating
               set processfactory-children up by 1
               move threadfactory to threadlist(j)
               set j up by 1
           else display "threads list full!"
           end-if.

       terminateprocess.
           compute terminating = i - 1
           compute terminating = terminating * random()
           compute terminating = terminating + 1
           move processlist(terminating) to processfactory
           move processfactory-identity to terminating

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
                  
       taskmanager.
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

       roundrobin.
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

       pageprocess.
           display "paging process #" paging "..."
           set k to paging
           if pagetable(k)>0
               display "process is mapped to memory address " pagetable(k)
           else
               display "page fault!"
               perform firstfit
           end-if.

       firstfit.
           set ii to paging
           move processlist(ii) to processfactory
           set folio to 1
           set folio up by processfactory-children
           compute folio = folio * 100
           compute verso = 1
           compute recto = verso + folio

           perform varying k from 1 by 1 until k=i
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

           if recto <= 10000
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
