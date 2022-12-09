     subroutine RdFlBOS(ibos,ni,nj,nk,xcel,ycel,xmin,ymin,xmax,ymax)
     use IFPORT
! „тение потоков      

    character*400 a400 
    character*8   datenew,date2
    real*4        xcel(ni,nj),ycel(ni,nj)
    integer*4     ibos(1) 
    
    real*4,allocatable::  flrgasi(:,:,:),flrgasj(:,:,:),flrgask(:,:,:),flroili(:,:,:),flroilj(:,:,:),flroilk(:,:,:),flrwati(:,:,:),flrwatj(:,:,:),flrwatk(:,:,:),vel(:,:)
    integer*4,allocatable:: iact(:),jact(:),kact(:)
    
    nn=ni*nj*nk
! –езервирую пам€ть:
    allocate(iact(1:nn),jact(1:nn),kact(1:nn),stat=ierr)
!   allocate(flrgasi(1:ni,1:nj,1:nk),flrgasj(1:ni,1:nj,1:nk),flrgask(1:ni,1:nj,1:nk),flroilk(1:ni,1:nj,1:nk),flrwati(1:ni,1:nj,1:nk),flrwatj(1:ni,1:nj,1:nk),stat=ierr)
    allocate(flroili(1:ni,1:nj,1:nk),flroilj(1:ni,1:nj,1:nk),flrwati(1:ni,1:nj,1:nk),flrwatj(1:ni,1:nj,1:nk),vel(1:ni,1:nj),stat=ierr)
    
! ‘ормирую iact(:),jact(:),kact(:) :
    nact=0
    ii=0
    do k=1,nk
      do j=1,nj
        do i=1,ni
          ii=ii+1  
          n=ibos(ii)
          if(n.lt.0) goto 1
! n-номер активной €чейки в кубе
          nact=nact+1
          iact(n+1)=i
          jact(n+1)=j
          kact(n+1)=k
1         continue 
        enddo
      enddo
    enddo  
    
    la=58
    flrgasi=0.d0
    flrgasj=0.d0
    flrgask=0.d0
    
    flroili=0.d0
    flroilj=0.d0
    flroilk=0.d0
    
    flrwati=0.d0
    flrwatj=0.d0
    flrwatk=0.d0
    
! FLOGASI+, m3/сут:
!    a400(1:la)='hdf_con.exe 6-10.h5 temp.# b r results/FLRGASI+ 01.04.1991'
!    result=systemqq(a400(1:la))
!    open(1,file='temp.#',form='binary')
!    do ii=1,nact
!      read(1)a
!      flrgasi(iact(ii),jact(ii),kact(ii))=a
!    enddo
!    close(1,status='delete')
    
! FLOGASJ+, m3/сут:
!    a400(1:la)='hdf_con.exe 6-10.h5 temp.# b r results/FLRGASJ+ 01.04.1991'
!    result=systemqq(a400(1:la))
!    open(1,file='temp.#',form='binary')
!    do ii=1,nact
!      read(1)a
!      flrgasj(iact(ii),jact(ii),kact(ii))=a
!    enddo    
!    close(1,status='delete')
    
! FLOGASK+, m3/сут:
!    a400(1:la)='hdf_con.exe 6-10.h5 temp.# b r results/FLRGASK+ 01.04.1991'
!    result=systemqq(a400(1:la))
!    open(1,file='temp.#',form='binary')
!    do ii=1,nact
!      read(1)a
!      flrgask(iact(ii),jact(ii),kact(ii))=a
!    enddo    
!    close(1,status='delete')
    
! FLOOILI+, m3/сут:
    a400(1:la)='hdf_con.exe 6-10.h5 temp.# b r results/FLROILI+ 01.04.1991'
    result=systemqq(a400(1:la))
    open(1,file='temp.#',form='binary')
    do ii=1,nact
      read(1)a
      flroili(iact(ii),jact(ii),kact(ii))=a
    enddo    
    close(1,status='delete')
    
! FLOOILJ+, m3/сут:
    a400(1:la)='hdf_con.exe 6-10.h5 temp.# b r results/FLROILJ+ 01.04.1991'
    result=systemqq(a400(1:la))
    open(1,file='temp.#',form='binary')
    do ii=1,nact
      read(1)a
      flroilj(iact(ii),jact(ii),kact(ii))=a
    enddo    
    close(1,status='delete')
! FLOOILK+, m3/сут:
!    a400(1:la)='hdf_con.exe 6-10.h5 temp.# b r results/FLROILK+ 01.04.1991'
!    result=systemqq(a400(1:la))
!    open(1,file='temp.#',form='binary')
!    do ii=1,nact
!      read(1)a
!      flroilk(iact(ii),jact(ii),kact(ii))=a
!    enddo    
!    close(1,status='delete')
    
! FLOWATI+, m3/сут:
    a400(1:la)='hdf_con.exe 6-10.h5 temp.# b r results/FLRWATI+ 01.04.1991'
    result=systemqq(a400(1:la))
    open(1,file='temp.#',form='binary')
   do ii=1,nact
      read(1)a
      flrwati(iact(ii),jact(ii),kact(ii))=a
    enddo    
    close(1,status='delete')
! FLOWATJ+, m3/сут:
    a400(1:la)='hdf_con.exe 6-10.h5 temp.# b r results/FLRWATJ+ 01.04.1991'
    result=systemqq(a400(1:la))
    open(1,file='temp.#',form='binary')
    do ii=1,nact
      read(1)a
      flrwatj(iact(ii),jact(ii),kact(ii))=a
    enddo    
    close(1,status='delete')
    
! FLOWATK+, m3/сут:
!    a400(1:la)='hdf_con.exe 6-10.h5 temp.# b r results/FLRWATK+ 01.04.1991'
!    result=systemqq(a400(1:la))
!    open(1,file='temp.#',form='binary')
!    do ii=1,nact
!      read(1)a
!      flrwatk(iact(ii),jact(ii),kact(ii))=a
!    enddo    
!    close(1,status='delete')
    
    do k=63,63
      do j=1,nj
        do i=1,ni
          a=flroili(i,j,k)+flrwati(i,j,k)
          b=flroilj(i,j,k)+flrwatj(i,j,k)
          vel(i,j)=sqrt(a*a+b*b)  
        enddo
      enddo
    enddo 
    
! vel - потоки через €чейки:
        open(1,file='Flow.txt')
        do j=1,nj                                                                                                                              
          do i=1,ni                                                                                                                            
            write(1,100) xcel(i,j),ycel(i,j),vel(i,j)
          enddo                                                                                                                                
        enddo                                                                                                                                  
        write(1,100) xmin,ymin
        write(1,100) xmax,ymax
        close(1)
100   format(3e17.7)
    
!    deallocate(flrgasi,flrgasj,flrgask,flroilk,flrwatk,stat=ierr)
    deallocate(iact,jact,kact,flroili,flroilj,flrwati,flrwatj,vel,stat=ierr)

    return
    end
