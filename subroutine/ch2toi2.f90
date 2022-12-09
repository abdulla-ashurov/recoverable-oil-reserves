      subroutine ch2toi2(a2,ic)
      character*2 a2
      integer*2 ic,j,k

      if(a2(1:1).eq.' ') j=0
      if(a2(1:1).eq.'0') j=0
      if(a2(1:1).eq.'1') j=1
      if(a2(1:1).eq.'2') j=2
      if(a2(1:1).eq.'3') j=3
      if(a2(1:1).eq.'4') j=4
      if(a2(1:1).eq.'5') j=5
      if(a2(1:1).eq.'6') j=6
      if(a2(1:1).eq.'7') j=7
      if(a2(1:1).eq.'8') j=8
      if(a2(1:1).eq.'9') j=9

      if(a2(2:2).eq.'0') k=0
      if(a2(2:2).eq.'1') k=1
      if(a2(2:2).eq.'2') k=2
      if(a2(2:2).eq.'3') k=3
      if(a2(2:2).eq.'4') k=4
      if(a2(2:2).eq.'5') k=5
      if(a2(2:2).eq.'6') k=6
      if(a2(2:2).eq.'7') k=7
      if(a2(2:2).eq.'8') k=8
      if(a2(2:2).eq.'9') k=9

      ic=10*j+k

      return
      end
