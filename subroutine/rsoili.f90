      subroutine rsoili(ni,nj,nk,soil0,nrec)               
! SOIL0 - ��������� SOIL:
      real*4 soil0(ni,nj,nk)
      integer*4 ni,nj,nk
      integer*2 nrec

      lrec=ni*nj*nk*4
      open(1,file='REC_SOIL.bnr',access='direct',form='binary',recl=lrec)
      read(1,rec=nrec,err=1)(((soil0(i,j,k),i=1,ni),j=1,nj),k=1,nk)
      close(1)
      
      do k=1,nk
        do j=1,nj
          do i=1,ni  
            if(soil0(i,j,k).lt.0.) soil0(i,j,k)=0.
            !print *,' rsoili: soil0(i,j,k)=',soil0(i,j,k)
          enddo
        enddo                                  
      enddo
      
      return
1     print *,' ERROR! RSOILI: ����� ����� REC_SOIL.bnr nrec=', nrec
      stop
      end                                      
