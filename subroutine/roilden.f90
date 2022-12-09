      subroutine roilden(ni,nj,nk,oil_den,nrec)               
! SOWCRI - текущий SOWCR:
      real*4 oil_den(ni,nj,nk)
      integer*4 ni,nj,nk
      integer*2 nrec

      lrec=ni*nj*nk*4
      open(1,file='REC_OILDEN.bnr',access='direct',form='binary',recl=lrec)
      read(1,rec=nrec,err=1)(((oil_den(i,j,k),i=1,ni),j=1,nj),k=1,nk)
      close(1)      
      
      do k=1,nk
        do j=1,nj
          do i=1,ni  
            if(oil_den(i,j,k).lt.0.) oil_den(i,j,k)=0.
          enddo
        enddo                                  
      enddo
      
      !print *,'roilden: file open= REC_OILDEN.bnr'
      !print *,'roilden: nrec=',nrec
      !print *,(((oil_den(i,j,k),i=1,ni),j=1,nj),k=1,nk)
      !print *,'--------'
      
      return
1     print *,' ERROR! Конец файла REC_OILDEN.bnr nrec=', nrec
      stop
      end                                      
