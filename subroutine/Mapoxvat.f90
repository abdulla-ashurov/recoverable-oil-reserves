        subroutine mapoxvat(namemap,lmap,par,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)
        character*30 namemap
        real*4    scel(ni,nj),xcel(ni,nj),ycel(ni,nj),par(ni1,nj1)
        real*4    xmin,ymin,xmax,ymax
        integer*4 ni,nj
        integer*2 imin,jmin
        !print *,' mapoxvat:'
        open(1,file=namemap(1:lmap))
        do i=1,ni1
          i1=i+imin-1
          do j=1,nj1
            j1=j+jmin-1
            if(par(i,j).ge.0.) write(1,100) xcel(i1,j1),ycel(i1,j1),par(i,j)
            
            !if(i1.eq.6.and.j1.eq.37) then
            !  print *,'i1=',i1,' j1=',j1,' par=',par(i,j)
            !endif
            !if(i1.eq.12.and.j1.eq.37) then
            !  print *,'i1=',i1,' j1=',j1,' par=',par(i,j)
            !endif
            !if(i1.eq.20.and.j1.eq.32) then
            !  print *,'i1=',i1,' j1=',j1,' par=',par(i,j)
            !endif
            
            if(par(i,j).lt.0.) write(1,100) xcel(i1,j1),ycel(i1,j1),0.
          enddo
        enddo
        write(1,100) xmin,ymin
        write(1,100) xmax,ymax
        close(1)
100     format(3e17.7)
        return
        end
