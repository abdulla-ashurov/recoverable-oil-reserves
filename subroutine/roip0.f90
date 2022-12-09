     subroutine roip0(ni,nj,nk,k1,k2,idensity,ros,ror,oe,porv,oil_den,soil0,sowcr0,sgas,xcel,ycel,scel,imin,imax,jmin,jmax,xmin,xmax,ymin,ymax,roiipgeo,roiipmob,soiipgeo,soiipmob,oilgeo,oilmob,vporsum,rwiipgeo,nfluids,plast,lplast)
! ������ ��������� �������������/��������� ������� �����
! ������ ��������� ��������� ������������� �������������/��������� �������   
!
! rOIIPgeo - ��������� ������������� ������ �����,rm3
! sOIIPgeo - ��������� ������������� ������ �����,sm3
!
! rOiipmob - ��������� ����������� ������ �����,rm3
! sOiipmob - ��������� ����������� ������ �����,sm3
!
! vporsum  - ������� ������ ���������, rm3
! rwiipgeo - ��������� ������������� ������ ����,sm3
      character*10 plast
      real*4 porv(ni,nj,nk),soil0(ni,nj,nk),sowcr0(ni,nj,nk),oil_den(ni,nj,nk),sgas(ni,nj,nk)
      real*4 xcel(ni,nj),ycel(ni,nj),scel(ni,nj)
      integer*4 oe(1)  
      integer*4 ni,nj,nk
      integer*2 k1,k2,imin,imax,jmin,jmax,lplast
      
      real*4,allocatable:: oipmob(:,:),oipgeo(:,:)
                                                                                                                                             
! porv  - ������� �����, �3                                                                                                                       
! soil0 - �����������������                                                                                                        
! roip  - ����� ����� � ������� � ��������� �������� = ������������ ������*�����������������,r�3

      ni1=imax-imin+1
      nj1=jmax-jmin+1
      allocate(oipmob(1:ni1,1:nj1),oipgeo(1:ni1,1:nj1),stat=ierr)
      
      do i=1,ni1                                                                                                                              
        do j=1,nj1                                                                                                                            
          oipmob(i,j)=0.
          oipgeo(i,j)=0.
        enddo
      enddo  

! roiipgeo - ������������� ������ ����� � ��������� k-����� ������,rm3
      roiipgeo=0.
! soiipgeo - ������������� ������ ����� � ��������� k-����� ������,sm3
      soiipgeo=0.
! oilgeo - ������������� ������ ����� � ��������� k-����� ������,tonn
      oilgeo=0.

! roiipmob- ��������� ������ ����� � ��������� k-����� ������,r�3
      roiipmob=0.
! soiipmob- ��������� ������ ����� � ��������� k-����� ������,s�3
      soiipmob=0.
! oilmob - ��������� ������ ����� � ��������� k-����� ������,tonn
      oilmob=0.
      
! rwiipgeo - ������������� ������ ���� � ��������� k-����� ������,rm3
      rwiipgeo=0.
      
      vporsum=0.

      n=0
      do k=k1,k2
        do i=1,ni1
          i1=i+imin-1
          do j=1,nj1
            j1=j+jmin-1
            if(porv(i1,j1,k).le.0.) goto 2
            m=(k-1)*ni*nj+(j1-1)*ni+i1
! ���� ������ ���������� goto 2:
            if(oe(m).lt.0) goto 2
            
            n=n+1
!            print 100,'roip0: soil=',soil(i1,j1,k),' i1=',i1,' j1=',j1,' k=',k,' n=',n
!100         format(a12,e12.3,a4,i4,a4,i3,a3,i4,a3,i5)
            
            vporsum=vporsum+porv(i1,j1,k)
            if(nfluids.eq.2) rwiipgeo=rwiipgeo+porv(i1,j1,k)*(1.-soil0(i1,j1,k))
            if(nfluids.eq.3) rwiipgeo=rwiipgeo+porv(i1,j1,k)*(1.-soil0(i1,j1,k)-sgas(i1,j1,k))
            
            a=porv(i1,j1,k)*soil0(i1,j1,k)
!roiipgeo - ������������� ������ �����, rm3
!soiipgeo - ������������� ������ �����, sm3
!  oilgeo  - ������������� ������ �����,����
! oipgeo() - ������������� ������ �����,����
            roiipgeo=roiipgeo+a
            if(idensity.eq.1) then
              oipgeo(i,j)=oipgeo(i,j)+a*ror/1000.
              soiipgeo=soiipgeo+a*ror/ros
              oilgeo=oilgeo+a*ror
            else
              oipgeo(i,j)=oipgeo(i,j)+a*oil_den(i1,j1,k)/1000.
              soiipgeo=soiipgeo+a*oil_den(i1,j1,k)/ros
              oilgeo=oilgeo+a*oil_den(i1,j1,k)
!              print *,' i1,j1,k=',i1,j1,k
!              print *,' porv=',porv(i1,j1,k),' soil=',soil(i1,j1,k)
!              print *,' oil_den=',oil_den(i1,j1,k),' ros=',ros
!              print *,' -----------'
            endif

            b=porv(i1,j1,k)*(soil0(i1,j1,k)-sowcr0(i1,j1,k))
            if(b.le.0.)b=0.
!roiipmob - ��������� ������ �����, rm3
!soiipmob - ��������� ������ �����, sm3
!  oilmob - ��������� ������ �����,���� 
! oipmob()- ��������� ������ �����,����
            roiipmob=roiipmob+b
            if(idensity.eq.1) then
              oipmob(i,j)=oipmob(i,j)+b*ror/1000.
              oilmob=oilmob+b*ror
              soiipmob=soiipmob+b*ror/ros
            else
              oipmob(i,j)=oipmob(i,j)+b*oil_den(i1,j1,k)/1000.
              oilmob=oilmob+b*oil_den(i1,j1,k)
              soiipmob=soiipmob+b*oil_den(i1,j1,k)/ros
            endif
            
2           continue            
          enddo    
        enddo 
      enddo
      
      !print *,'roip0: vporsum=',vporsum
      !print *,'roip0: roiipgeo=',roiipgeo
      !print *,'roip0: soiipgeo=',soiipgeo
      !print *,'roip0: rwiipgeo=',rwiipgeo
      
      open(1,file='OMPInit_'//plast(1:lplast)//'.txt')   
      open(2,file='OIPInit_'//plast(1:lplast)//'.txt')
      do i=1,ni1                                                                                                                              
        i1=i+imin-1
        do j=1,nj1                                                                                                                            
          j1=j+jmin-1
          if(scel(i1,j1).gt.0.) then
!            a=oipmob(i,j)/(100.*scel(i1,j1))
!            b=oipgeo(i,j)/(100.*scel(i1,j1))
            a=oipmob(i,j)/(0.1*scel(i1,j1))
            b=oipgeo(i,j)/(0.1*scel(i1,j1))
          else
            a=0.
            b=0.
          endif  
! oipmob - ��������� ������������� ��������� ������� �����, ���.�/��:
          write(1,1) xcel(i1,j1),ycel(i1,j1),a
! roipgeo0 - ��������� ������������� ������������� ������� �����, ���.�/��:
          write(2,1) xcel(i1,j1),ycel(i1,j1),b
        enddo                                                                                                                                
      enddo                                                                                                                                  
      write(1,1) xmin,ymin
      write(1,1) xmax,ymax
      write(2,1) xmin,ymin
      write(2,1) xmax,ymax
      close(2)
      close(1)
      
      deallocate(oipmob,oipgeo,stat=ierr)

1     format(3e17.7)                                                                                                                       
      return                                                                                                                                 
      end
