     subroutine chntoi(m,a6,n)
     character*6 a6
     integer*2 m,n,n1,n2,n3,n4,n5
     
     if(m.ge.6) then
       print *,'ERROR! subroutine chntoi(): m=>6'
       stop
     endif
     if(m.eq.1) then
       call ch1toi1(a6(1:1),n)
     endif
     if(m.eq.2) then
       call ch1toi1(a6(1:1),n1)
       call ch1toi1(a6(2:2),n2)
       n=n1*10+n2  
     endif
     if(m.eq.3) then
       call ch1toi1(a6(1:1),n1)
       call ch1toi1(a6(2:2),n2)
       call ch1toi1(a6(3:3),n3)
       n=n1*100+n2*10+n3  
     endif
     if(m.eq.4) then
       call ch1toi1(a6(1:1),n1)
       call ch1toi1(a6(2:2),n2)
       call ch1toi1(a6(3:3),n3)
       call ch1toi1(a6(4:4),n4)
       n=n1*1000+n2*100+n3*10+n4  
     endif
     if(m.eq.5) then
       call ch1toi1(a6(1:1),n1)
       call ch1toi1(a6(2:2),n2)
       call ch1toi1(a6(3:3),n3)
       call ch1toi1(a6(4:4),n4)
       call ch1toi1(a6(5:5),n5)
       n=n1*10000+n2*1000+n3*100+n4*10+n5  
     endif
     return
     end
