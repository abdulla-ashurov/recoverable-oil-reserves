      subroutine soil0(ni,nj,nk,ier)               
! Формирование массивов *.bn#                
      real*4,allocatable:: soil(:)
      allocate(soil(1:ni),stat=ierr)

      m=ni*4
      ier=0 
      open(1,file='soil0.bnr',form='binary')
      read(1) nx,ny,nz
      open(2,file='soil0.bn#',form='binary',access='direct',recl=m)
      nrec=0
      do k=1,nk
        do j=1,nj
          read (1,1,end=5) (soil(i),i=1,ni)
          do i=1,ni  
            if(soil(i).lt.0.) soil(i)=0.
          enddo
          nrec=nrec+1
          write(2,rec=nrec) (soil(i),i=1,ni)
        enddo                                  
      enddo
      goto 6
  5   ier=1                                  
  6   close(2)
      close(1)
      
      deallocate(soil,stat=ierr)
      
 1    format(281e17.7)                             
      return                                   
      end                                      
