      subroutine i2toch2(m,chm)
!c Преобразование int*2 в char*2
      integer*2 m
      character*2 chm

      m1=m/10
      m2=m-m1*10

      if(m1.eq.0) chm(1:1)='0' 
      if(m1.eq.1) chm(1:1)='1'
      if(m1.eq.2) chm(1:1)='2'
      if(m1.eq.3) chm(1:1)='3'
      if(m1.eq.4) chm(1:1)='4'
      if(m1.eq.5) chm(1:1)='5'
      if(m1.eq.6) chm(1:1)='6'
      if(m1.eq.7) chm(1:1)='7'
      if(m1.eq.8) chm(1:1)='8'
      if(m1.eq.9) chm(1:1)='9'

      if(m2.eq.0) chm(2:2)='0'
      if(m2.eq.1) chm(2:2)='1'
      if(m2.eq.2) chm(2:2)='2'
      if(m2.eq.3) chm(2:2)='3'
      if(m2.eq.4) chm(2:2)='4'
      if(m2.eq.5) chm(2:2)='5'
      if(m2.eq.6) chm(2:2)='6'
      if(m2.eq.7) chm(2:2)='7'
      if(m2.eq.8) chm(2:2)='8'
      if(m2.eq.9) chm(2:2)='9'

      return
      end
      


