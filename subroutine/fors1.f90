      subroutine fors1(is1,js1,ks1,idren,ni,nj,nk,nc)
! Формирую списки блоков (is1,js1,ks1), в которых idren()=1     
      integer*2 idren(1),is1(1),js1(1),ks1(1)
      nc=0
      do k=1,nk
        do j=1,nj
          do i=1,ni
            m=(k-1)*ni*nj+(j-1)*ni+i  
            if(idren(m).eq.1) then
              nc=nc+1
              is1(nc)=i
              js1(nc)=j
              ks1(nc)=k
            endif
          enddo
        enddo
      enddo
      return
      end
