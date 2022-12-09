      subroutine foridren(ni,nj,nk,idren,oe,soil,sowcr,velos,vcrit,scrit,is1,js1,ks1,nz,nt)
! ������������ ������� IDREN - ���������� ������������� �����:
! idren=0 - ��������� ���������
! idren=1 - ������ (i,j,k) ����������� ���������� ���������, �� ���� ��������� ���������� ������
! idren=2 - ������ (i,j,k) ����������� � ��� ���������� ������ ��������� 
! idren=3 - ������ (i,j,k) �� ����������� ���������� ���������
! k1-k2 - �������� ����� ������
! integer*2 idren(1:nact)
! integer*4 oen(1:ni*nj*nk)
!
! is1(),js1(),ks1() - ������� ������� - ������ (i,j,k)-��������� ������, � ������� idren=1
! 
      real*4    soil(ni,nj,nk),sowcr(ni,nj,nk),velos(ni,nj,nk)
      integer*4 oe(1)
      integer*2 idren(1),is1(1),js1(1),ks1(1)
      integer*2 nt
      
! !!!!!!!!!!!!!!!!!!!!!!
!      integer*2 irab(41)
! !!!!!!!!!!!!!!!!!!!!!!
      
      do i=1,ni*nj*nk
        idren(i)=0
      enddo
      
! !!!!!!!!!!!!!!!!!!!!!!
!        print *,'--1 nt=',nt
!        ii=0
!        do k=1,nk
!          do j=1,nj
!            do i=1,ni
!              ii=ii+1  
!              irab(i)=idren(ii)  
!            enddo    
!            if(k.eq.67)print 100,(irab(i),i=1,ni)  
!          enddo
!        enddo  
!100     format(41i1)        
! !!!!!!!!!!!!!!!!!!!!!!

! nc-������� ��������� �������� is1(),js1(),ks1() 
      nc=0
      
! nz - ���������� ������, � ������� �������� ���������� ��������
      do ii=1,nz
! ������ (i,j,k):
        i=is1(ii)
        j=js1(ii)
        k=ks1(ii)
        
! !!!!!!!!!!!!!!!!!!!!!!
!        if(k.ne.67)goto 1
! !!!!!!!!!!!!!!!!!!!!!!
        
! �������� (i,j,k) � ����� m:
        m=(k-1)*ni*nj+(j-1)*ni+i
        n=oe(m)+1
        
! +1, �� � ��-��� ������ ���������� � 0        
        if(n.lt.1) goto 1  
! n<1 - ���������� ����
        idren(m)=1
 1      continue
      enddo
      
! !!!!!!!!!!!!!!!!!!!!!!
!        print *,'--2'
!        ii=0
!        do k=1,nk
!          do j=1,nj
!            do i=1,ni
!              ii=ii+1
!              irab(i)=idren(ii)  
!            enddo    
!           if(k.eq.67)print 100,(irab(i),i=1,ni)  
!          enddo
!        enddo  
! !!!!!!!!!!!!!!!!!!!!!!
       
      nci=0
      nz1=nz
2     continue
      nci=nci+1
      
      do ii=1,nz1
        i=is1(ii)
        j=js1(ii)
        k=ks1(ii)
        
! !!!!!!!!!!!!!!!!!!!!!!
!        if(k.ne.67) goto 3
! !!!!!!!!!!!!!!!!!!!!!!
        
 ! �������� (i,j,k) � ����� m:
        m=(k-1)*ni*nj+(j-1)*ni+i
        n=oe(m)+1
! +1, �� � ��-��� ������ ���������� � 0        
! n<1 - ���������� ����
        if(n.lt.1) goto 3
        call shablon(i,j,k,idren,oe,ni,nj,nk,soil,sowcr,velos,vcrit,scrit)
3       continue
      enddo

! !!!!!!!!!!!!!!!!!!!!!!
!        print *,'--3',nci
!        iii=0
!        do k=1,nk
!          do j=1,nj
!            do i=1,ni
!              iii=iii+1  
!              irab(i)=idren(iii)  
!            enddo    
!           if(k.eq.67)print 100,(irab(i),i=1,ni)  
!          enddo
!        enddo  
! !!!!!!!!!!!!!!!!!!!!!!
      
      call fors1(is1,js1,ks1,idren,ni,nj,nk,nz1)
      
! !!!!!!!!!!!!!!!!!!!!!!
!      print *,'foridren: nz1=',nz1,' nt=',nt
! !!!!!!!!!!!!!!!!!!!!!!
      if(nz1.ge.1) goto 2

      return
      end
