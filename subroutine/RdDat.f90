      subroutine rddat(a400,date)
      character*128 a400
      character*8   date
      character*3   mes
      character*2   a2
      character*1   a1
      integer*2     n,m
      
!6 возможных вариантов: 
!2 JUL 2010     -1 
!02 JUL 2010    -2
!2 'JUL' 2010   -3
!21 'JUL' 2010  -4
!01.07.1986     -5 
!1.07.1986      -6 
! 6:
      if(a400(2:2).eq.".") then
        date='0'//a400(1:1)//a400(3:4)//a400(6:7) 
        goto 1
      endif
! 5:
      if(a400(3:3).eq.".") then
        date=a400(1:2)//a400(4:5)//a400(7:8)
        goto 1
      endif  

      if(a400(4:4).eq."'") then
! 4
        mes=a400(5:7)
        call mestom(mes,m)
! Преобразую месяц 'mes' в число 'm':
        if(m.le.9) then
          call i1toch1(m,a1)
          a2='0'//a1  
        else
          call i2toch2(m,a2)
        endif
        date=a400(1:2)//a2//a400(10:13)
        goto 1
      endif
      
      if(a400(3:3).eq."'") then
! 3
        mes=a400(4:6)
        call mestom(mes,m)
! Преобразую месяц 'mes' в число 'm':
        if(m.le.9) then
          call i1toch1(m,a1)
          a2='0'//a1  
        else
          call i2toch2(m,a2)
        endif
        date='0'//a400(1:1)//a2//a400(9:12)
        goto 1
      endif
      
      if(a400(3:3).eq." ") then
! 2
        mes=a400(4:6)
        call mestom(mes,m)
! Преобразую месяц 'mes' в число 'm':
        if(m.le.9) then
          call i1toch1(m,a1)
          a2='0'//a1  
        else
          call i2toch2(m,a2)
        endif
        date=a400(1:2)//a2//a400(8:11)
        goto 1
      endif
      
      if(a400(2:2).eq." ") then
! 1
        mes=a400(3:5)
        call mestom(mes,m)
! Преобразую месяц 'mes' в число 'm':
        if(m.le.9) then
          call i1toch1(m,a1)
          a2='0'//a1  
        else
          call i2toch2(m,a2)
        endif
        date='0'//a400(1:1)//a2//a400(7:10)        
      endif
1     continue      
      return
      end
     