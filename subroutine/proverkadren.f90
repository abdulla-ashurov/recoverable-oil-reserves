      subroutine proverkadren(soil,sowcr,velos,i,j,k,vcrit,scrit,ires,ni,nj,nk)
      real*4   soil(ni,nj,nk),sowcr(ni,nj,nk),velos(ni,nj,nk)
      if(soil(i,j,k)-sowcr(i,j,k).ge.scrit) then
        ires=1
      else
        if(velos(i,j,k).le.vcrit) then
          ires=0
        else
          ires=1 
        endif     
      endif
      return
      end
