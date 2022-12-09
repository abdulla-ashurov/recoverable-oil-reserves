      subroutine soili(ni,nj,nk,ier)               
! Формирование массивов *.bn#                
      real*4,allocatable:: soil(:),swat(:),drt(:)
      allocate(soil(1:ni),swat(1:ni),drt(1:ni),stat=ierr)

      m=ni*4
      ier=0 
      
      open(1,file='soili.bnr',form='binary')
      read(1) nx,ny,nz
      open(2,file='soili.bn#',form='binary',access='direct',recl=m)
      nrec=0
      do k=1,nk
        do j=1,nj
          read (1,1,end=7) (soil(i),i=1,ni)
          do i=1,ni  
            if(soil(i).lt.0.) soil(i)=0.
          enddo
          nrec=nrec+1
          write(2,rec=nrec) (soil(i),i=1,ni)
        enddo                                  
      enddo
      goto 8
  7   ier=1                                  
  8   close(2)
      close(1)
      
      open(1,file='swati.bnr',form='binary')
      read(1) nx,ny,nz
      open(2,file='swati.bn#',form='binary',access='direct',recl=m)
      nrec=0
      do k=1,nk
        do j=1,nj
          read (1,1,end=9) (swat(i),i=1,ni)
          do i=1,ni  
            if(swat(i).lt.0.) swat(i)=0.
          enddo
          nrec=nrec+1
          write(2,rec=nrec) (swat(i),i=1,ni)
        enddo                                  
      enddo
      goto 10
  9   ier=1                                  
 10   close(2)
      close(1)

      open(1,file='drti.bnr',form='binary')
      read(1) nx,ny,nz
      open(2,file='drti.bn#',form='binary',access='direct',recl=m)
      nrec=0
      do k=1,nk
        do j=1,nj
          read (1,1,end=11) (drt(i),i=1,ni)
          do i=1,ni  
            if(drt(i).lt.0.) drt(i)=0.
          enddo
          nrec=nrec+1
          write(2,rec=nrec) (drt(i),i=1,ni)
        enddo                                  
      enddo
      goto 12
 11   ier=1                                  
 12   close(2)
      close(1)

      deallocate(soil,swat,drt,stat=ierr)
      
 1    format(281e17.7)                             
      return                                   
      end                                      
