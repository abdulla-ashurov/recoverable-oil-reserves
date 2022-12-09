      subroutine sxy2(ni,nj,xcel,ycel,scel)                           
! –асчет площадей €чеек                          
! –асчет x,y-координат центров €чеек!
      real*4,allocatable:: a(:),b(:),c(:),coord(:)
      real*4 xcel(ni,nj),ycel(ni,nj),scel(ni,nj)
      integer*4 ni,nj
!COORD
!  2188430.00  6508737.00     2040.78  2188430.00  6508737.00     2555.06
!  2188527.25  6508737.00     2040.87  2188527.25  6508737.00     2555.00
!  2188624.25  6508737.00     2041.04  2188624.25  6508737.00     2554.90

      allocate(a(1:ni),b(1:ni),c(1:ni),coord(1:(ni+1)*(nj+1)*6),stat=ierr)
      
      open(1,file='COORD.bnr',form='binary')
      read(1)(coord(i),i=1,(ni+1)*(nj+1)*6)
      close(1)
      
      do j=1,nj
        do i=1,ni
          x1=coord((j-1)*(ni+1)*6+(i-1)*6+1)
          x2=coord((j-1)*(ni+1)*6+(i-1)*6+7)
          x3=coord(j*(ni+1)*6+(i-1)*6+1)
          x4=coord(j*(ni+1)*6+(i-1)*6+7)
          x=(x1+x2+x3+x4)/4
! x-координата средины €чейки
          y1=coord((j-1)*(ni+1)*6+(i-1)*6+2)
          y2=coord((j-1)*(ni+1)*6+(i-1)*6+8)
          y3=coord(j*(ni+1)*6+(i-1)*6+2)
          y4=coord(j*(ni+1)*6+(i-1)*6+8)
          y=(y1+y2+y3+y4)/4 
! y-координата средины €чейки                                
          sax=(x4-x2)**2
          say=(y4-y2)**2
          sa=sqrt(sax+say)
          sax=(x4-x3)**2
          say=(y4-y3)**2
          sb=sqrt(sax+say)
          s=sa*sb
! scel - площадь €чейки                                             
          scel(i,j)=s
! xcel,ycel - координаты центров €чеек          
          xcel(i,j)=x                                             
          ycel(i,j)=y                                             
        enddo
      enddo  
          
      deallocate(a,b,c,coord,stat=ierr)
      return                                                 
      end                                                    
