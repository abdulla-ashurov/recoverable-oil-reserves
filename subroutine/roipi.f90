     subroutine roipi(ni,nj,nk,k1,k2,ro,bo,oe,idren,porv,soil,sowcr0,sowcr,velocity,xcel,ycel,scel,imin,imax,jmin,jmax,xmin,xmax,ymin,ymax,oilsum,oildrensum,izap,nt,ikoxvp,vcrit,scrit,koxvoil,koxvwat)
    
! ������ ���������� ������������� �������:
! ----------------------------------------
! OIP(i,j)       - ������� ������������� ������ �����,rm3
! OIPDren(i,j)   - ������� ������������� ����������� ������ �����,rm3
! OIPNoDren(i,j) - ������� ������������� ������������� ������ �����
!
! OMP(i,j)       - ������� ����������� ������ �����,rm3
! OMPDren(i,j)   - ������� ����������� ����������� ������ �����,rm3
! OMPNoDren(i,j) - ������� ������������������������  ������ �����

! ������ ������ ������������:
! -----------------------
! OxvatOIP(i,j)     - ����� ������������� ������� ����� � ����
! OxvatOIPOil(i,j)  - ����� ������������� ������� �����
!
! OxvatOMP(i,j)     - ����� ����������� ������� ����� � ����
! OxvatOMPOil(i,j)  - ����� ����������� ������� �����
! OxvatOMPWat(i,j)  - ����� ����������� ������� ����

! ������ �������:
! ---------------
! oilsum - ������������� ������ �����,rm3
! oildrensum- ����������� ������, r�3
! watsum - ������������� ������ ����,rm3
! watdrensum - ����������� ������ ����,rm3

! ������ ������������� ������:
! ----------------------------
! KoxvOIL - ������� ����.������ ����� 
! KoxvWAT - ������� ����.������ ����

! sowcr(i,j,k) - ��� ������� ����������� �����
! sowcr0(i,j,k)- ��� ��������� ����������� �����

      real*4 porv(ni,nj,nk),soil(ni,nj,nk),sowcr(ni,nj,nk),sowcr0(ni,nj,nk),velocity(ni,nj,nk)
      real*4 xcel(ni,nj),ycel(ni,nj),scel(ni,nj)
      real*4 koxvoil,koxvwat
      integer*4 oe(1)  
      integer*4 ni,nj,nk
      integer*2 k1,k2,imin,imax,jmin,jmax,izap,nt
      integer*2 idren(1)
      
      real*4,allocatable:: OIP(:,:),OIPDren(:,:),OIPNoDren(:,:)
      real*4,allocatable:: OxvatOIP(:,:),OxvatOIPOil(:,:)     
      real*4,allocatable:: OMP(:,:),OMPDren(:,:),OMPNoDren(:,:)
      real*4,allocatable:: OxvatOMP(:,:),OxvatOMPOil(:,:),OxvatOMPWat(:,:)   
      real*4,allocatable:: WIP(:,:),WIPDren(:,:)
      real*4,allocatable:: OxvatOil(:,:),OxvatWat(:,:)   
      
! porv  - ������� �����, �3                                                                                                                       
! soil  - �����������������                                                                                                        
! ����� ����� � ������� � ��������� �������� = ������������ ������*�����������������,�3

      ni1=imax-imin+1
      nj1=jmax-jmin+1
      allocate(OIP(1:ni1,1:nj1),OIPDren(1:ni1,1:nj1),OIPNoDren(1:ni1,1:nj1),stat=ierr)
      allocate(OxvatOIP(1:ni1,1:nj1),OxvatOIPOil(1:ni1,1:nj1),stat=ierr)      
      allocate(OMP(1:ni1,1:nj1),OMPDren(1:ni1,1:nj1),OMPNoDren(1:ni1,1:nj1),stat=ierr)
      allocate(OxvatOMP(1:ni1,1:nj1),OxvatOMPOil(1:ni1,1:nj1),OxvatOMPWat(1:ni1,1:nj1),stat=ierr)      
      allocate(WIP(1:ni1,1:nj1),WIPDren(1:ni1,1:nj1),stat=ierr)      
      allocate(OxvatOil(1:ni1,1:nj1),OxvatWat(1:ni1,1:nj1),stat=ierr) 
      
      
!      if(ikoxvp.eq.1) then
!      print *,'roipi:'
!      print 100,(idren(i),i=1,ni*nj*nk)
!100   format(99i2)      
!      endif
      
      swcr=0.2
! swcr(��������� ����)=0.2
      
      do i=1,ni1                                                                                                                              
        do j=1,nj1                                                                                                                            
          OIP(i,j)=0.
          OIPDren(i,j)=0.
          OIPNoDren(i,j)=0.
! ---          
          OxvatOIP(i,j)=-0.00001
          OxvatOIPOil(i,j)=-0.00001
! ---          
          OMP(i,j)=0.
          OMPDren(i,j)=0.
          OMPNoDren(i,j)=0.
! ---          
          OxvatOMP(i,j)=-0.00001
          OxvatOMPOil(i,j)=-0.00001
          OxvatOMPWat(i,j)=-0.00001
          
          OxvatOil(i,j)=0.
          OxvatWat(i,j)=0.
          
          WIP(i,j)=0.
          WIPDREN(i,j)=0.
        enddo
      enddo  

! � ��������� k-����� ������:
! oilsum - ������������� ������ �����,rm3
      oilsum=0.
! oildrensum- ����������� ������, r�3
      oildrensum=0.
! oilndrensum-�������������������      
      oilndrensum=0.
! watsum - ������������� ������ ����,rm3
      watsum=0.
! watdrensum - ����������� ������ ����,rm3
      watdrensum=0.

      do i=1,ni1
        i1=i+imin-1
        do j=1,nj1
          j1=j+jmin-1
!          aLiqdren=0.
!          aOilPvdren=0.
!          aWatPvdren=0.
          
          do k=k1,k2
            m=(k-1)*ni*nj+(j1-1)*ni+i1
! ���� ������ ���������� goto 2:
            if(oe(m).lt.0) goto 2
            
! ������� ����� ������:            
            p=porv(i1,j1,k)
            if(p.lt.0.)p=0.
! ������� ������������� ������ ����� � ������:            
            oil =p*soil(i1,j1,k)
            
! ������� ������������ ���� � ������ (��������������: sgas=0):
            swat=1.-soil(i1,j1,k)
! ������� ����� ���� � ������:
            wat=p*swat

            sopot=soil(i1,j1,k)-sowcr0(i1,j1,k)
            if(sopot.lt.0.) sopot=0. 
! oilmpot - ������e ����������� (��� ���������� �����) ������ ����� � ������:
            oilmpot=p*sopot
! oilnmpot - ������� ������������� ������ ����� � ������:
            oilnmpot=p*sowcr0(i1,j1,k)
            
            sw=1.-soil(i1,j1,k)-swcr
            if(sw.lt.0.)sw=0.
! watm - ������� ��������� ������ ���� � ������
            watm=p*sw
            
! ������� ����������� ����������������� � ������:
            so=soil(i1,j1,k)-sowcr(i1,j1,k)
            
            if(so.lt.0.)so=0.
! ������� ����������� ������ ����� � ������:
            oilm=p*so
! ������� ������������� ������ ����� � ������:            
            oilnm=p*sowcr(i1,j1,k)
! ����� ������������� ����� � ����:            
            oilndrensum  =oilndrensum+oilnm
            
! ������ c����� ������ ��� ������, ����������� �������� � ���������� ���������,
! (������ �� ��������� �������������� ������� �� ��������������):
            if(ikoxvp.eq.1) then
              if(idren(m).eq.0) goto 1
            endif  
           
! ����������� �������������� ������:
            if(so.gt.scrit) then
! ����� ����������� ��������������� ������� ������� �������, �3:  
!              aOilPvdren=aOilPvdren+p
! ����� ����������� ����� � ����:               
              oildrensum=oildrensum+oilm
            endif
! �����.������ ��������������� ������:
            oxvoil=0.
            if(oil.gt.0.and.sopot.gt.0.) oxvoil=so/sopot
            if(oxvoil.gt.1.)oxvoil=1.

!  ����.������ �������������� ������:
!           oxvwat=0.
!            if(velocity(i1,j1,k).ge.vcrit.and.sw.gt.0.) then
! ����� ����������� �������������� ������� ������� �������, �3:  
!              aWatPvdren=aWatPvdren+p
! ����� ����������� ������� ���� ����:  
!              watdrensum=watdrensum+watm
!            endif

!  ����.������ �������������� ������:
!oxvwat=a*b*c
!if(oxvwat.gt.1) oxvwat=1.

! a=0, if velosity</=vcrit
! a=1, if velosity>=100*vcrit
          if(vcrit.gt.0.) then
            a=(velocity(i1,j1,k)-vcrit)/(100*vcrit-vcrit)
          else
            a=(velocity(i1,j1,k)-0.0001)/(0.01-0.0001)
          endif
          if(a.lt.0)a=0.
          
            !if((i.eq.1.and.j.eq.1).or.(i.eq.5.and.j.eq.5).or.(i.eq.15.and.j.eq.15).or.(i.eq.49.and.j.eq.49)) then
            !  print *,'i,j=',i,j
            !  print *,' a=',a,' velosity=',velocity(i1,j1,k),' vcrit=',vcrit
            !  print *,' '
            !endif
          

! b=0, if swat<=0.2
! b=1, if swat>=0.5
          b=(swat-0.2)/(0.75-0.2)
          if(b.lt.0.)b=0.
          
            !if((i.eq.1.and.j.eq.1).or.(i.eq.5.and.j.eq.5).or.(i.eq.15.and.j.eq.15).or.(i.eq.49.and.j.eq.49)) then
            !  print *,' b=',b,' swat=',swat
            !  print *,' '
            !endif

          d=sowcr(i1,j1,k)-sowcr0(i1,j1,k)
! c=0, if sowcr-sowcr0=0.3
! c=1, if sowcr-sowcr0=0.
          c=(0.3-d)/0.3
          if(c.lt.0.) c=0.
          
            !if((i.eq.1.and.j.eq.1).or.(i.eq.5.and.j.eq.5).or.(i.eq.15.and.j.eq.15).or.(i.eq.49.and.j.eq.49)) then
            !  print *,' c=',c,' sowcr=',sowcr(i1,j1,k),'sowcr0=',sowcr0(i1,j1,k)
            !  print *,' '
            !endif

          oxvwat=a*b*c
!            ox=0.
!            c=0.
!            if(swat.gt.0.) then
!              if(velocity(i1,j1,k).le.vcrit) then
!                c=0.
!                if((i.eq.1.and.j.eq.1).or.(i.eq.5.and.j.eq.5).or.(i.eq.15.and.j.eq.15).or.(i.eq.49.and.j.eq.49)) then
!                print *,'i,j=',i,j
!                print *,'1 c=',c,' velocity=',velocity(i1,j1,k),' vcrit=',vcrit 
!                endif
!              else
!                c=velocity(i1,j1,k)/(100*vcrit)
!                if(c.gt.1.)c=1.
!                if((i.eq.1.and.j.eq.1).or.(i.eq.5.and.j.eq.5).or.(i.eq.15.and.j.eq.15).or.(i.eq.49.and.j.eq.49)) then
!                print *,'i,j=',i,j
!                print *,'3 c=',c
!                endif
!              endif
!              ox=swat
!                if((i.eq.1.and.j.eq.1).or.(i.eq.5.and.j.eq.5).or.(i.eq.15.and.j.eq.15).or.(i.eq.49.and.j.eq.49)) then
!                print *,'i,j=',i,j
!                print *,'4 ox=',ox
!                endif
!            endif    
!
!            oxvwat=ox*c
!            if(oxwat.gt.1.)oxvwat=1.
!            
            !if((i.eq.1.and.j.eq.1).or.(i.eq.5.and.j.eq.5).or.(i.eq.15.and.j.eq.15).or.(i.eq.49.and.j.eq.49)) then
            !  print *,'5 oxvwat=',oxvwat,' a=',a,' b=',b,' c=',c
            !  print *,'----'
            !endif
            
! ����� ����������� ������� ���� ����:
            watdrensum=watdrensum+oxvwat*watm

! ����� ��� ������� ������ ����������� �����:
            OxvatOil(i,j)=OxvatOil(i,j)+oxvoil*oilmpot
            
! ����� ��� ������� ������ ���������� ����:
            OxvatWat(i,j)=OxvatWat(i,j)+oxvwat*p*(swat-swcr)
  1         continue
! ����� ������� ������� �������:  
            aLiqsum=aLiqsum+p
! ����� ������������� ������� ����� ����:  
            oilsum=oilsum+oil
! ����� ������� ���� ����:  
            watsum=watsum+wat
! -------------
! OIP - ������� ������������� ������ �����:
            OIP(i,j)=OIP(i,j)+oil
! OIPDren  - ������� ������������� ����������� ������ �����:
            OIPDren(i,j)=OIPDren(i,j)+oilmpot
! OIPNoDren  - ������� ������������� ������������� ������ �����:
            OIPNoDren(i,j)=OIPNoDren(i,j)+oilnmpot
            
! OMP - ������� ����������� ������ �����:
            OMP(i,j)=OMP(i,j)+oilmpot
          
! OMPDren  - ������� ����������� ����������� ������ �����:
            OMPDren(i,j)=OMPDren(i,j)+oilm
! OMPNoDren  - ������� ����������� ������������� ������ �����, ���.�/��:
            OMPNoDren(i,j)=OMPNoDren(i,j)+oilnm
            
! WIP - ������� ������ ����            
            WiP(i,j)=WIP(i,j)+wat
! WIPDren - ������� ����������� ������ ����
            WiPDren(i,j)=WIPDren(i,j)+watm 
            
  2         continue
          enddo
          
! OxvatOIP - ������ 3D-o���� ������������ ������������� ������� ����� � ���� (���� ������������), �.e.:
          if(OIP(i,j)+WIP(i,j).gt.0.) OxvatOIP(i,j)=(OxvatOil(i,j)+OxvatWat(i,j))/(OIP(i,j)+WIP(i,j))
! OxvatOIPOIl - ������ 3D-o���� ������������ ������������� ������� �����, �.e.:
          if(OIP(i,j).gt.0.) OxvatOIPOil(i,j)=OxvatOil(i,j)/OIP(i,j)
          
! OxvatOMP - ������ 3D-o���� ������������ ����������� ������� ����� � ���� (���� ������������), �.e.:
          if(OMP(i,j)+WIPDren(i,j).gt.0.) OxvatOMP(i,j)=(OxvatOil(i,j)+OxvatWat(i,j))/(OMP(i,j)+WIPDren(i,j))
! OxvatOMPOil - ������ 3D-o���� ������������ ������������� ������� �����, �.e.:

          if(OMP(i,j).gt.0.) OxvatOMPOil(i,j)=OxvatOil(i,j)/OMP(i,j)
! OxvatOMPWat - ������ 3D-o���� ������������ ������� ����, �.e.:
          if(WIPDren(i,j).gt.0.) OxvatOMPWat(i,j)=OxvatWat(i,j)/WIPDren(i,j)          
        enddo 
      enddo
      
! ����� ������������ ������������� �������  
      if(oilsum.gt.0.000000000001) then
        koxvoil=oildrensum/oilsum
      else
        koxvoil=0.
      endif
! ����� ���������� ������� ����  
      if(watsum.gt.0.000000000001) then
        koxvwat=watdrensum/watsum
      else
        koxvwat=0.
      endif    
      if(izap.eq.1) then
! ������ ����:
! ===============================
        b=ro/(bo*100.)       
! OIP - ��������� ������������� ������� ������������� ������� �����, ���.�/��:
        call maps('OIP_________.txt',OIP,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)
! OIPDren  - ��������� ������������� ������� ������������� ����������� ������� �����, ���.�/��:
        call maps('OIPDren_____.txt',OIPDren,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)
! OIPNonDren  - ��������� ������������� ������� ������������� ����������� ������� �����, ���.�/��:
        call maps('OIPNoDren___.txt',OIPNoDren,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)
! ===============================
! OxvatOIP - ������ 3D-o���� ������������ ������������� ������� ����� � ���� (���� ������������), �.e.:
        call mapoxvat('OxvatOIP____.txt',OxvatOIP,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)
! OxvatOIPOil - ������ 3D-o���� ������������ ������������� ������� �����, �.e.:
        call mapoxvat('OxvatOIPOil_.txt',OxvatOIPOil,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)
! ===============================
! OMP - ��������� ������������� ������� ����������� ������� �����, ���.�/��:
        call maps('OMP_________.txt',OMP,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)
! OMPDren  - ��������� ������������� ������� ����������� ����������� ������� �����, ���.�/��:
        call maps('OMPDren_____.txt',OMPDren,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)
! OMPNoDren  - ��������� ������������� ������� ������������� ������������� ������� �����, ���.�/��:
        call maps('OMPNoDren___.txt',OMPNoDren,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)
! ===============================
! OxvatOMP - ������ 3D-o���� ������������ ������������� ������� ����� � ���� (���� ������������), �.e.:
        call mapoxvat('OxvatOMP____.txt',OxvatOMP,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)
! OxvatOMPOil - ������ 3D-o���� ������������ ������������� ������� �����, �.e.:
        call mapoxvat('OxvatOMPOil_.txt',OxvatOMPOil,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)
! OxvatOMPWat - ������ 3D-o���� ������������ ������������� ������� �����, �.e.:
        call mapoxvat('OxvatOMPWat_.txt',OxvatOMPWat,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)
! ===============================
      endif
      
      deallocate(OIP,OIPDren,OIPNoDren,OxvatOIP,OxvatOIPOil,OMP,OMPDren,OMPNoDren,OxvatOMP,OxvatOMPOil,OxvatOMPWat,WIP,WIPDren,OxvatOil,OxvatWat,stat=ierr)   
      
      return                                                                                                                                 
      end
