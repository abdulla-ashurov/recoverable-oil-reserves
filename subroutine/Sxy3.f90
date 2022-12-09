      subroutine sxy3(ni,nj,nk,xcel,ycel,scel,invert)                           
! –асчет площадей €чеек                          
! –асчет x,y-координат центров €чеек!
      real*4 xcel(ni,nj),ycel(ni,nj),scel(ni,nj)
      integer*4 ni,nj,nk
      real*4,allocatable:: a(:),b(:),c(:),par(:,:,:)
!COORD
!  2188430.00  6508737.00     2040.78  2188430.00  6508737.00     2555.06
!  2188527.25  6508737.00     2040.87  2188527.25  6508737.00     2555.00
!  2188624.25  6508737.00     2041.04  2188624.25  6508737.00     2554.90

      allocate(a(1:ni),b(1:ni),c(1:ni),par(1:ni,1:nj,1:nk),stat=ierr)
      
      lrec=ni*nj*nk*4
      open(1,file='INIT.bnr',access='direct',form='binary',recl=lrec)
      read(1,rec=16)(((par(i,j,k),i=1,ni),j=1,nj),k=1,nk)
      close(1)
      do j=1,nj
        do i=1,ni
! scel - площадь €чейки                                             
          scel(i,j)=par(i,j,1)
        enddo
      enddo
      
      open(1,file='INIT.bnr',access='direct',form='binary',recl=lrec)
      read(1,rec=17)(((par(i,j,k),i=1,ni),j=1,nj),k=1,nk)
      close(1)
      do j=1,nj
        do i=1,ni
! xcel - X-координаты центра €чейки                                             
          xcel(i,j)=par(i,j,1)
        enddo
      enddo
      
      open(1,file='INIT.bnr',access='direct',form='binary',recl=lrec)
      read(1,rec=18)(((par(i,j,k),i=1,ni),j=1,nj),k=1,nk)
      close(1)
      if(invert.eq.0) then
        do j=1,nj
          do i=1,ni
! ycel - Y-координаты центра €чейки                                              
            ycel(i,j)=par(i,j,1)
          enddo
        enddo
      else
        do j=1,nj
          do i=1,ni
! ycel - Y-координаты центра €чейки
            ycel(i,j)=-par(i,j,1)
          enddo
        enddo
      endif    
103   format(a11,2i4,a6,f10.1,a6,f10.1)
      
      deallocate(a,b,c,par,stat=ierr)
      return                                                 
      end                             