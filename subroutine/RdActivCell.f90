      subroutine rdactivcell(ni,nj,nk,oe)                           
      integer*4 ni,nj,nk,oe(1)
      
      lrec=ni*nj*nk*4
      open(1,file='INIT.bnr',access='direct',form='binary',recl=lrec)
      read(1,rec=20)(oe(i),i=1,ni*nj*nk)
      close(1)

      return                                                 
      end                             