     subroutine MapZapas(ni,nj,nk,k1,k2,idensity,ros,ror,oe,idren,porv,oil_den,soil,sowcr0,sowcr,xcel,ycel,scel,imin,imax,jmin,jmax,xmin,xmax,ymin,ymax,izap,nt,ikoxvp,plast,lplast)
! ������ ���������� ������������� �������:
! ----------------------------------------
! OIP(i,j)       - ������� ������������� ������ �����,rm3
! OIPDren(i,j)   - ������� ������������� ����������� ������ �����,rm3
! OIPNoDren(i,j) - ������� ������������� ������������� ������ �����,rm3
!
! OMP(i,j)       - ������� ����������� ������ �����,rm3
! OMPDren(i,j)   - ������� ����������� ����������� ������ �����,rm3
! OMPNoDren(i,j) - ������� ����������� �������������  ������ �����,rm3
! OxvatOMPWat(i,j)  - ����� ����������� ������� ����

! sowcr(i,j,k) - ��� ������� ����������� �����
! sowcr0(i,j,k)- ��� ��������� ����������� �����
! oil_den(i,j,k) - ��� ��������� ����� � ��������� ��������
      character*128 a30
      character*10 plast
      real*4 porv(ni,nj,nk),soil(ni,nj,nk),sowcr(ni,nj,nk),sowcr0(ni,nj,nk),oil_den(ni,nj,nk)
      real*4 xcel(ni,nj),ycel(ni,nj),scel(ni,nj)
      integer*4 oe(1)  
      integer*4 ni,nj,nk
      integer*2 k1,k2,imin,imax,jmin,jmax,izap,nt,lplast
      integer*2 idren(1)
      
      real*4,allocatable:: OIP(:,:),OIPDren(:,:),OIPNoDren(:,:)
      real*4,allocatable:: OMP(:,:),OMPDren(:,:),OMPNoDren(:,:)

! porv  - ������� �����, �3                                                                                                                       
! soil  - �����������������                                                                                                        
! ����� ����� � ������� � ��������� �������� = ������������ ������*�����������������,�3

      ni1=imax-imin+1
      nj1=jmax-jmin+1
      allocate(OIP(1:ni1,1:nj1),OIPDren(1:ni1,1:nj1),OIPNoDren(1:ni1,1:nj1),stat=ierr)
      allocate(OMP(1:ni1,1:nj1),OMPDren(1:ni1,1:nj1),OMPNoDren(1:ni1,1:nj1),stat=ierr)

      do i=1,ni1                                                                                                                              
        do j=1,nj1                                                                                                                            
          OIP(i,j)=0.
          OIPDren(i,j)=0.
          OIPNoDren(i,j)=0.
          OMP(i,j)=0.
          OMPDren(i,j)=0.
          OMPNoDren(i,j)=0.
        enddo
      enddo  

! � ��������� k-����� ������:
      do i=1,ni1
        i1=i+imin-1
        do j=1,nj1
          j1=j+jmin-1
          do k=k1,k2
! ���� ������ ���������� goto 2:
            m=(k-1)*ni*nj+(j1-1)*ni+i1
            if(oe(m).le.0) goto 2
            
            sowcri=sowcr(i1,j1,k)
            if(ikoxvp.eq.1) then
! ������ c����� ������ ��� ������, ����������� �������� � ���������� ���������,
! � ������ �� ��������� �������������� ������� �� ��������������:
              if(idren(m).eq.0) sowcri=soil(i1,j1,k)  
            endif
            if(sowcri.ge.soil(i1,j1,k)) sowcri=soil(i1,j1,k)
            
            if(i1.eq.177.and.j1.eq.39) print *,'i1=',i1,' j1=',j1,' k=',k,' soil=',soil(i1,j1,k)
            if(i1.eq.177.and.j1.eq.39) print *,'i1=',i1,' j1=',j1,' k=',k,' sowcr=',sowcr(i1,j1,k)
            if(i1.eq.177.and.j1.eq.39) print *,'sowcri=',sowcri
            
! ������������ ����������� ����������������� � ������:
            if(sowcri.lt.sowcr0(i1,j1,k)) then
              sopot=soil(i1,j1,k)-sowcri
            else  
              sopot=soil(i1,j1,k)-sowcr0(i1,j1,k)
            endif  
            if(sopot.lt.0.) sopot=0.
            if(i1.eq.177.and.j1.eq.39) print *,'i1=',i1,' j1=',j1,' k=',k,' sowcr0=',sowcr0(i1,j1,k)
            if(i1.eq.177.and.j1.eq.39) print *,' sopot=',sopot
            
! ������� ������������� ����������������� � ������:
            !if(sowcri.lt.sowcr0(i1,j1,k)) then
            !if(soil(i1,j1,k).gt.sowcri) then
            sononizvl=sowcri
            !else
            !  sononizvl=soil(i1,j1,k)
            !endif
            !else
            !  if(soil(i1,j1,k).gt.sowcr0(i1,j1,k)) then
            !    sononizvl=sowcr0(i1,j1,k)
            !  else
            !    sononizvl=soil(i1,j1,k)
            !  endif
            !endif    
            if(sononizvl.lt.0.)sononizvl=0.
            
! ������� ����������� ����������������� � ������,�.�.:
            so=soil(i1,j1,k)-sowcri
            if(so.lt.0.)so=0.
! ������� ����� ������,rm3:            
            p=porv(i1,j1,k)
           
            if(p.lt.0.)p=0.
            if(i1.eq.177.and.j1.eq.39) print *,'i1=',i1,' j1=',j1,' k=',k,' porv=',p

! ������� ������������� ������ ����� � ������, rm3:            
            oil =p*soil(i1,j1,k)
! ������� ������������� ������ ����� � ������, tonn:
            if(idensity.eq.1) then
              oil=oil*ror/1000.
            else
              oil=oil*oil_den(i1,j1,k)/1000.
            endif
            if(oil.lt.0.)oil=0.
            
            if(i1.eq.177.and.j1.eq.39) print *,' oil=',oil,' ror=',ror 
            
! oilpot - ������e ������������ ����������� ������ ����� � ������,rm3:
            oilpot=p*sopot
! oilpot - ������e ������������ ����������� ������ ����� � ������,tonn:
            if(idensity.eq.1) then
              oilpot=oilpot*ror/1000. 
            else
              oilpot=oilpot*oil_den(i1,j1,k)/1000.
            endif
            if(oilpot.lt.0.)oilpot=0.
            
            if(i1.eq.177.and.j1.eq.39) print *,' oilpot=',oilpot
            
! oilnmpot - ������� ������������� ������������ ������������ ������ ����� � ������,rm3:
            !oilnmpot=p*(sowcri-sowcr0(i1,j1,k))
            oilnmpot=p*(sopot-so)
            if(oilnmpot.lt.0.) oilnmpot=0.
! oilnmpot - ������� ������������� ������������ ����������� ������ ����� � ������,tonn:
            if(idensity.eq.1) then
              oilnmpot=oilnmpot*ror/1000.
            else
              oilnmpot=oilnmpot*oil_den(i1,j1,k)/1000.
            endif
            if(oilnmpot.lt.0.)oilnmpot=0.
            
            if(i1.eq.177.and.j1.eq.39) print *,' oilnmpot=',oilnmpot

! ������� ����������� ������ ����� � ������,rm3:
            oilm=p*so
! ������� ����������� ������ ����� � ������,tonn:
            if(idensity.eq.1) then
              oilm=oilm*ror/1000.
            else
              oilm=oilm*oil_den(i1,j1,k)/1000.
            endif
            if(oilm.lt.0.)oilm=0.
            
            if(i1.eq.177.and.j1.eq.39) print *,' oilm=',oilm

! ������� ������������� ������������� ������ ����� � ������,rm3:            
            oilnm=oil-oilm
            if(oilnm.lt.0.)oilnm=0.
! ������� ������������� ������ ����� � ������,tonn:            
            if(idensity.eq.1) then
              oilnm=oilnm*ror/1000.
            else
              oilnm=oilnm*oil_den(i1,j1,k)/1000.
            endif
            
            if(i1.eq.177.and.j1.eq.39) print *,' oilnm=',oilnm

            
! OIP - ������� ������������� ������ �����,tonn:
            OIP(i,j)=OIP(i,j)+oil
! OIPDren  - ������� ������������� ����������� ������ �����,tonn:
            if(oilm.gt.0.) then
              OIPDren(i,j)=OIPDren(i,j)+oilm
            endif  
! OIPNoDren  - ������� ������������� ������������� ������ �����,tonn:
            if(oilm.gt.0.) then
              OIPNoDren(i,j)=OIPNoDren(i,j)+oilnm
            endif
            
! OMP - ������� ������������ ����������� ������ �����,tonn:
            OMP(i,j)=OMP(i,j)+oilm+oilnmpot
! OMPDren  - ������� ����������� ������������ ����������� ������ �����,tonn:
            OMPDren(i,j)=OMPDren(i,j)+oilm
! OMPNoDren  - ������� �������������� ������������ ����������� ������ �����,tonn:
            OMPNoDren(i,j)=OMPNoDren(i,j)+oilnmpot
            if(i1.eq.177.and.j1.eq.39) print *,'i=',i,' j1=',j,' k=',k,' OMP=',omp(i,j)
            if(i1.eq.177.and.j1.eq.39) print *,'i=',i,' j1=',j,' k=',k,' OMPDREN=',ompdren(i,j)
            if(i1.eq.177.and.j1.eq.39) print *,'i=',i,' j1=',j,' k=',k,' OMPNoDren=',ompnodren(i,j)
            if(i1.eq.177.and.j1.eq.39) print *,'=============='
            
            
!            if(izap.eq.1) then
!              print 100,'i,j=',i,j,' oilpot=',oilpot,' oilmpot=',oilmpot,' oilnmpot=',oilnmpot
!              print 101,'     soil=',soil(i1,j1,k),' sowcr0=',sowcr0(i1,j1,k),' sowcr=',sowcr(i1,j1,k)
!              print *,' '
!            endif
!100         format(a4,2i4,a9,f10.1,a6,f10.1,a7,f10.1)            
!101         format(a10,f6.3,a8,f6.3,a7,f6.3)            
            
  2         continue
          enddo
        enddo 
      enddo
      
      print *,' izap=',izap
      
      if(izap.eq.1) then
      print *,'������ ����'
      
! ������ ����:
!        b=ro/(bo*100.)
        b=10.      
! OIP - ��������� ������������� ������� ������������� ������� �����, ���.�/��:
        lmap=8+lplast
        a30='                              '
        a30='OIP_'//plast(1:lplast)//'.txt'
        call maps(a30,lmap,OIP,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)
        
! OIPDren  - ��������� ������������� ������� ����������� ������������� ������� �����, ���.�/��:
        lmap=12+lplast
        a30='                              '
        a30='OIPDren_'//plast(1:lplast)//'.txt'
        call maps(a30,lmap,OIPDren,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)

! OIPNonDren  - ��������� ������������� ������� ������������� ������������� ������� �����, ���.�/��:
        lmap=14+lplast
        a30='                              '
        a30='OIPNoDren_'//plast(1:lplast)//'.txt'
        call maps(a30,lmap,OIPNoDren,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)

! OMP - ��������� ������������� ������� ������������ ����������� ������� �����, ���.�/��:
        lmap=8+lplast
        a30='                              '
        a30='OMP_'//plast(1:lplast)//'.txt'
        call maps(a30,lmap,OMP,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)
        
! OMPDren  - ��������� ������������� ������� ������������ ����������� ����������� ������� �����, ���.�/��:
        lmap=12+lplast
        a30='                              '
        a30='OMPDren_'//plast(1:lplast)//'.txt'
        call maps(a30,lmap,OMPDren,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)

! OMPNoDren  - ��������� ������������� ������� ������������� ������������ ����������� ������� �����, ���.�/��:
        lmap=14+lplast
        a30='                              '
        a30='OMPNoDren_'//plast(1:lplast)//'.txt'
        call maps(a30,lmap,OMPNoDren,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)

      endif
      
      deallocate(OIP,OIPDren,OIPNoDren,OMP,OMPDren,OMPNoDren,stat=ierr)   

      return                                                                                                                                 
      end
