      subroutine proverka(i,j,k,oe,idren,soil,sowcr,velos,vcrit,scrit,ni,nj,nk)
      real*4   soil(ni,nj,nk),sowcr(ni,nj,nk),velos(ni,nj,nk)
      integer*2 idren(1)
      integer*4 oe(1)

      n=(k-1)*ni*nj+(j-1)*ni+i
      m=oe(n)+1
! +1, тк в РН-КИМ счет начинается с "0"      
      if(m.lt.1) goto 1

      if(idren(n).eq.0) then
        if(soil(i,j,k)-sowcr(i,j,k).ge.scrit) then
          idren(n)=1
        else
          if(velos(i,j,k).ge.vcrit) then
            idren(n)=1
          else
            idren(n)=3
          endif     
        endif
      endif
 1    continue 
      return
100   format(a16,3i3,a3,i4,a10,i2)      
      end
