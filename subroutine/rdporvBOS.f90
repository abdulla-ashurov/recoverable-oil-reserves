    subroutine rdporvBOS(ni,nj,nk,porv)
! Формирование массива porv
    real*4 porv(ni,nj,nk)
    integer*4 ni,nj,nk
    integer*4 lrec
    real*4,allocatable:: a(:,:,:)
    
    allocate(a(1:ni,1:nj,1:nk),stat=ierr)
    
    lrec=ni*nj*nk*4
    open(1,file='INIT.bnr',access='direct',form='binary',recl=lrec)
! PORO:
    read(1,rec=6)(((porv(i,j,k),i=1,ni),j=1,nj),k=1,nk)
    
!dx для равномерной сетки:
    read(1,rec=1)(((a(i,j,k),i=1,ni),j=1,nj),k=1,nk)
    
      do k=1,nk
        do j=1,nj
          do i=1,ni  
            porv(i,j,k)=porv(i,j,k)*a(i,j,k)
          enddo
        enddo
      enddo
!dy для равномерной сетки:
    read(1,rec=2)(((a(i,j,1),i=1,ni),j=1,nj),k=1,nk)
    
      do k=1,nk
        do j=1,nj
          do i=1,ni  
            porv(i,j,k)=porv(i,j,k)*a(i,j,k)
          enddo
        enddo
      enddo
!dz:
    read(1,rec=3)(((a(i,j,k),i=1,ni),j=1,nj),k=1,nk)
! porv:    
      do k=1,nk
        do j=1,nj
          do i=1,ni  
            porv(i,j,k)=porv(i,j,k)*a(i,j,k)
          enddo
        enddo
      enddo
    close(1)
     
      deallocate(a,stat=ierr)
      
      return
      end
