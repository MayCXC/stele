perform with test after varying :backwards: from :finish: by -1 until :backwards:=1
       if :backwards:<:finish:
           move :tablelist:(:backwards:) to :tablefactory:
           if :condition:
               perform varying :forewards: from :backwards: by 1 until :forewards:=:finish:
                  compute :shift: = :forewards: + 1 end-compute
                  move :tablelist:(:shift:) to :tablelist:(:forewards:)
               end-perform
               set :finish: down by 1
           end-if
       end-if
end-perform
