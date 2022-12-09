      subroutine RdZapInit(ni,nj,nk,par,nrec)
    
      real*4 par(ni,nj,nk)
    
      lrec=ni*nj*nk*4
      open(1,file='INIT.bnr',access='direct',form='binary',recl=lrec)
      read(1,rec=nrec)(((par(i,j,k),i=1,ni),j=1,nj),k=1,nk)
      close(1)
      
      return
      end