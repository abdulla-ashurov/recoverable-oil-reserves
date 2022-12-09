        subroutine maps(namemap,lmap,par,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)
        character*128 namemap
        real*4    scel(ni,nj),xcel(ni,nj),ycel(ni,nj),par(ni1,nj1)
        real*4    ro,bo,xmin,ymin,xmax,ymax,b
        integer*4 ni,nj
        integer*2 imin,jmin
        
        open(1,file=namemap(1:lmap))
        do i=1,ni1
          i1=i+imin-1
          do j=1,nj1
            j1=j+jmin-1
            if(scel(i1,j1).gt.0.) then
              a=b/scel(i1,j1)
            else
              a=0.
            endif  
            write(1,100) xcel(i1,j1),ycel(i1,j1),par(i,j)*a
          enddo
        enddo
        write(1,100) xmin,ymin
        write(1,100) xmax,ymax
        close(1)
100     format(3e17.7)
        return
        end
