program-id. hello-world.

environment division.
configuration section.
       repository.
           function all intrinsic.
       special-names.
           symbolic characters
               NOP is 1.

data division.
       working-storage section.
           01 selection pic X.
           01 ones.
                  02 thickness pic 99.
                  02 repetition pic 999.
                  02 creating pic 999.
                  02 terminating pic 999.

           01 processfactory.
               copy "myprocess.cpy"
                   replacing
                       ==:n:== by 2
                       ==:tag:== by ==processfactory==
                       ==:stacksize:== by 100.

           01 threadfactory.
               copy "mythread.cpy"
                   replacing
                       ==:n:== by 2
                       ==:tag:== by ==threadfactory==
                       ==:stacksize:== by 100.

           *> welcome to hell :^)
           01 processlist occurs 100 times indexed by i, ii, iii, iiii.
               copy "myprocess.cpy"
                   replacing
                       ==:n:== by 2
                       ==:tag:== by ==processlist==
                       ==:stacksize:== by 100.

           01 threadlist occurs 400 times indexed by j, jj, jjj, jjjj.
               copy "mythread.cpy"
                   replacing
                       ==:n:== by 2
                       ==:tag:== by ==threadlist==
                       ==:stacksize:== by 100.

procedure division.
       initialize ones replacing numeric data by 1.
       set i, ii, iii, iiii, j, jj, jjj, jjjj to 1.

       request.
           display "T: list processes and threads".
           display "R: randomly create and terminate processes for ten minutes.".
           display "E: create process.".
           display "W: terminate process.".
           display "Q: exit".
           display " ".
           accept selection.
           if upper-case(selection)="T" perform taskmanager
           else if upper-case(selection)="R" perform randomprocess
           else if upper-case(selection)="E" perform createprocess
           else if upper-case(selection)="W" perform terminateprocess
           else if upper-case(selection)="Q" display "Goodbye." goback
           else display "Invalid input." perform request.
           display " "
       perform request.

       randomprocess.
           perform varying repetition from 600 by -1 until repetition=0
               display "round " repetition
               if random() > i/100.0 perform createprocess end-if
               if random() < i/100.0 perform terminateprocess end-if
               if random() < .05 or random() > .95 perform taskmanager end-if
               call "C$SLEEP" using 1 end-call
           end-perform.
           display "shutting down.".
           perform terminateprocess until i=1.

       createprocess.
           display "creating process...".
           initialize processfactory replacing
                   numeric data by 0
                   alphanumeric data by " ".
           if i<=100
               set processfactory-identity to creating
               compute thickness = random()*4.0
               set thickness up by 1
               perform createthread
                   varying thickness from thickness by -1 until thickness=0
               move processfactory to processlist(i)
               set creating to mod(creating,100)
               set creating up by 1
               set i up by 1
           else display "processes list full!".

       createthread.
           display "creating thread...".
           initialize threadfactory replacing
                   numeric data by 0
                   alphanumeric data by " ".
           if j<=400
               set threadfactory-identity to thickness
               set threadfactory-parent to creating
               set processfactory-children up by 1
               move threadfactory to threadlist(j)
               set j up by 1
           else display "threads list full!".

       terminateprocess.
           compute terminating = i - 1.
           compute terminating = terminating * random().
           compute terminating = terminating + 1.
           move processlist(terminating) to processfactory.
           move processfactory-identity to terminating.

           display "terminating process #" terminating "...".
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
           else display "out of processes!".
                  
       taskmanager.
           display " "
           compute ii = i - 1.
           display ii " processes are active"
           display " "
           perform varying ii from 1 by 1 until ii=i
               move processlist(ii) to processfactory
               display "process #" processfactory-identity ": " processfactory-children " threads"
           end-perform.
           display " "
           compute jj = j - 1.
           display jj " threads are active"
           display " "
           perform varying jj from 1 by 1 until jj=j
               move threadlist(jj) to threadfactory
               display "thread #" threadfactory-identity ": " threadfactory-parent " is parent"
           end-perform.
