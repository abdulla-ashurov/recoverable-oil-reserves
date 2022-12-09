     subroutine itochn(m,a4,n)
     character*4 a4
     integer*2 m,n,m1,m2,m3,m4
     
     if(m.gt.9999) then
       print *,'ERROR! subroutine itochn(): m=>4'
       stop
     endif
     if(m.le.9) then
       n=1
       call i1toch1(m,a4(1:1))
       return 
     endif
     if(m.le.99) then
       n=2
       m1=m/10
       m2=m-m1*10
       call i1toch1(m1,a4(1:1))
       call i1toch1(m2,a4(2:2))
       return  
     endif
     if(m.le.999) then
       n=3
       m1=m/100
       m2=(m-m1*100)/10
       m3=m-m1*100-m2*10 
       call i1toch1(m1,a4(1:1))
       call i1toch1(m2,a4(2:2))
       call i1toch1(m3,a4(3:3))
       return  
     endif
     if(m.le.9999) then
       n=4
       m1=m/1000
       m2=(m-m1*1000)/100
       m3=(m-m1*1000-m2*100)/10
       m4=m-m1*1000-m2*100-m3*10 
       call i1toch1(m1,a4(1:1))
       call i1toch1(m2,a4(2:2))
       call i1toch1(m3,a4(3:3))
       call i1toch1(m4,a4(4:4))
       return  
     endif
     return
     end
