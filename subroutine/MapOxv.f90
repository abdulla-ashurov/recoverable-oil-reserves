      subroutine MapOxv(ni,nj,nk,k1,k2,oe,idren,porv,soil,sgas,sowcr0,sowcr,swcr,velosity,xcel,ycel,scel,imin,imax,jmin,jmax,xmin,xmax,ymin,ymax,izap,nt,ikoxvp,vcrit,scrit,nfluids,plast,lplast,critzap)
! Расчет охвата воздействием:
! -----------------------
! OxvatOIP(i,j)     - охват геологических запасов нефти и воды
! OxvatOIPOil(i,j)  - охват геологических запасов нефти
!
! OxvatOMP(i,j)     - охват извлекаемых запасов нефти и воды
! OxvatOMPOil(i,j)  - охват извлекаемых запасов нефти
! OxvatOMPWat(i,j)  - охват извлекаемых запасов воды

! sowcr(i,j,k) - куб текущей критической нефти
! sowcr0(i,j,k)- куб начальной критической нефти
! oil_den(i,j,k) - куб плотности нефти в пластовых условиях
!
      character*30 a30 
      character*10 plast
      real*4 porv(ni,nj,nk),soil(ni,nj,nk),sowcr(ni,nj,nk),sowcr0(ni,nj,nk),velosity(ni,nj,nk),swcr(ni,nj,nk),sgas(ni,nj,nk)
      real*4 xcel(ni,nj),ycel(ni,nj),scel(ni,nj)
      integer*4 oe(1)  
      integer*4 ni,nj,nk
      integer*2 k1,k2,imin,imax,jmin,jmax,izap,nt,lplast
      integer*2 idren(1)
      
      real*4,allocatable:: OxvatOMP(:,:),OxvatOIP(:,:),OxvatOIPOil(:,:),OxvatOMPOil(:,:),OxvatOMPWat(:,:)    
      real*4,allocatable:: OIP(:,:),OMP(:,:),OMPDren(:,:),OMPNoDren(:,:)
      real*4,allocatable:: WIP(:,:),WIPDren(:,:)
      real*4,allocatable:: OxvatOilGeo(:,:),OxvatOilPot(:,:),OxvatWat(:,:)   
      
! porv  - поровый объем, м3                                                                                                                       
! soil  - нефтенасыщенность                                                                                                        
! Объем нефти в ячейках в пластовых условиях = произведению объема*нефтенасыщенность,м3

      ni1=imax-imin+1
      nj1=jmax-jmin+1
      allocate(OIP(1:ni1,1:nj1),OMP(1:ni1,1:nj1),stat=ierr)
      allocate(OxvatOIP(1:ni1,1:nj1),OxvatOIPOil(1:ni1,1:nj1),stat=ierr)      
      allocate(OxvatOMP(1:ni1,1:nj1),OxvatOMPOil(1:ni1,1:nj1),OxvatOMPWat(1:ni1,1:nj1),stat=ierr)      
      allocate(WIP(1:ni1,1:nj1),WIPDren(1:ni1,1:nj1),stat=ierr)      
      allocate(OxvatOilGeo(1:ni1,1:nj1),OxvatOilPot(1:ni1,1:nj1),OxvatWat(1:ni1,1:nj1),stat=ierr) 
      
      
!      if(ikoxvp.eq.1) then
!      print *,'roipi:'
!      print 100,(idren(i),i=1,ni*nj*nk)
!100   format(99i2)      
!      endif

      do i=1,ni1                                                                                                                              
        do j=1,nj1                                                                                                                            
          OxvatOIP(i,j)=-0.00001
          OxvatOIPOil(i,j)=-0.00001
          OxvatOMP(i,j)=-0.00001
          OxvatOMPOil(i,j)=-0.00001
          OxvatOMPWat(i,j)=-0.00001
          OxvatOilGeo(i,j)=0.         
          OxvatOilPot(i,j)=0.
          OxvatWat(i,j)=0.
          WIP(i,j)=0.
          WIPDREN(i,j)=0.
          OMP(i,j)=0.
          OIP(i,j)=0.
        enddo
      enddo  

      oilndrensum=0.
! В выбранных k-слоях модели:
      do i=1,ni1
        i1=i+imin-1
        do j=1,nj1
          j1=j+jmin-1
          sumporv=0.
          
          iprint=0
          !if(izap.eq.1) then
          !  if(i1.eq.6.and.j1.eq.37)  iprint=1
          !  if(i1.eq.12.and.j1.eq.37) iprint=1
          !  if(i1.eq.20.and.j1.eq.32) iprint=1
          !endif
          
          !if(iprint.eq.1) then
          !  print *,'====================='
          !  print 200,' i1=',i1,' j1=',j1
          !endif  
200 format(a4,i3,a4,i3)

          do k=k1,k2
            !if(iprint.eq.1) print *,'*** k=',k 

              sowcri=0.
              p=0.
              sopot=0.
              so=0.
              oilmpot=0.
              oilm=0.
              oxvopot=0.
              oxvogeo=0.
            
            m=(k-1)*ni*nj+(j1-1)*ni+i1
! Если ячейка неактивная goto 2:
            !if(iprint.eq.1.and.oe(m).le.0) then
            !  print *,'   Неактивная ячейка !'
            !endif
            if(oe(m).le.0) goto 2
! Запасы cчитаю только для блоков, дренируемых потоками к добывающим скважинам,
! (потоки от удаленных нагнетательных скважин мб игнорироваться):
            sowcri=sowcr(i1,j1,k)
            if(ikoxvp.eq.1) then
              if(idren(m).eq.0) sowcri=soil(i1,j1,k)
            endif
            if(sowcri.ge.soil(i1,j1,k)) sowcri=soil(i1,j1,k)
            
! Поровый объем ячейки:            
            p=porv(i1,j1,k)
            if(p.lt.0.)p=0.
            sumporv=sumporv+p
            
! Текущие геологические запасы нефти в ячейке:            
            oil =p*soil(i1,j1,k)
            
! Текущая насыщенность воды в ячейке:
            swat=1.-soil(i1,j1,k)
            if(nfluids.eq.2) then
              sw=1.-soil(i1,j1,k)-swcr(i1,j1,k)
              swat=1.-soil(i1,j1,k)
            endif  
            if(nfluids.eq.3) then
              sw=1.-soil(i1,j1,k)-swcr(i1,j1,k)-sgas(i1,j1,k)
              swat=1.-soil(i1,j1,k)-sgas(i1,j1,k)
            endif
            if(sw.lt.0.)sw=0.
            if(swat.lt.0.)swat=0.
            
! Текущий объем воды в ячейке:
            wat=p*swat
! watm - Текущие подвижные запасы воды в ячейке
            watm=p*sw

            if(sowcri.lt.sowcr0(i1,j1,k)) then
              sopot=soil(i1,j1,k)-sowcri
            else
              sopot=soil(i1,j1,k)-sowcr0(i1,j1,k)
            endif    
            if(sopot.lt.0.) sopot=0.
            
! oilmpot - Текущиe извлекаемые потенциальные запасы нефти в ячейке
            oilmpot=p*sopot
! oilnmpot - Текущие неизвлекаемые запасы нефти в ячейке:
            !oilnmpot=oil-oilmpot 
            
! Текущая дренируемая нефтенасыщенность в ячейке:
            so=soil(i1,j1,k)-sowcri
            if(so.lt.0.)so=0.
! Текушие дренируемые запасы нефти в ячейке:
            oilm=p*so
! Текущие недренируемые потенциально извлекаемые запасы нефти в ячейке:
            !!oilnm=p*(sowcri-sowcr0(i1,j1,k))
            !oilnm=p*(sopot-so)
            !if(oilnm.le.0.)oilnm=0.
! Объем недренируемой потенциально извлекаемой нефти:            
            !oilndrensum  =oilndrensum+oilnm
           
! Коэфф.охвата нефтенасыщенной ячейки с потенциально извлекаемыми запасами:
            if(oil.gt.0.and.sopot.gt.0.) then
              oxvopot=so/sopot
            else
              oxvopot=0.
            endif    
            if(oxvopot.gt.1.)oxvopot=1.
            
! Коэфф.охвата нефтенасыщенной ячейки с геологическими запасами:
            if(oil.gt.0.and.so.gt.0.) then
              oxvogeo=so/soil(i1,j1,k)
            else
              oxvogeo=0.
            endif    
            if(oxvogeo.gt.1.)oxvogeo=1.

! Коэф.охвата водонасыщенной ячейки:
            call roxvatwat(vcrit,velosity(i1,j1,k),sw,swcr(i1,j1,k),oxvwat)
            
! Сумма для расчета охвата геологических запасов нефти:
            OxvatOilGeo(i,j)=OxvatOilGeo(i,j)+oxvogeo*oil
! Сумма для расчета охвата потенциально вытесненяемой нефти:
            OxvatOilPot(i,j)=OxvatOilPot(i,j)+oxvopot*oilmpot
            !print *,'i,j=',i,j,' oxvoil=',oxvoil,' oilmpot=',oilmpot 
! Сумма для расчета охвата вытеснения воды:
            OxvatWat(i,j)=OxvatWat(i,j)+oxvwat*p*(swat-swcr(i1,j1,k))

! OIP - Текущие геологические запасы нефти:
            OIP(i,j)=OIP(i,j)+oil
! OMP - Текущие потенциально извлекаемые запасы нефти:
            OMP(i,j)=OMP(i,j)+oilmpot
! WIP - Текущие запасы воды            
            WiP(i,j)=WIP(i,j)+wat
! WIPDren - Текущие дренируемые запасы воды
            WiPDren(i,j)=WIPDren(i,j)+watm 
2           continue

            !if(iprint.eq.1) then
            !  print 201,'  sowcr0=',sowcr0(i1,j1,k),' sowcri=',sowcri,' soil=',soil(i1,j1,k)
            !  print 202,'  porv=',p,' sopot=',sopot,' so=',so
            !  print 203,'  p*sopot=',oilmpot,' p*so=',oilm,' so/sopot=',oxvoil
            !  print 204,'  oxvatoilPot=',oxvatoilpot(i,j),' omp=',omp(i,j)
            !  print 204,'  oxvatoilGeo=',oxvatoilgeo(i,j),' oip=',oip(i,j)
            !endif

          enddo
          
 201 format(a9,f6.3,a8,f6.3,a6,f6.3)
 202 format(a7,f9.1,a7,f8.5,a4,f8.5)
 203 format(a10,f9.1,a6,f9.1,a10,f8.5)
 204 format(a11,f15.1,a5,f15.1)
 
! OxvatOIP - Теущий 3D-oхват воздействием геологических запасов нефти и воды (зоны дренирования), д.e.:
          if(OIP(i,j)+WIP(i,j).gt.0.) OxvatOIP(i,j)=(OxvatOilGeo(i,j)+OxvatWat(i,j))/(OIP(i,j)+WIP(i,j))

! OxvatOIPOIl - Теущий 3D-oхват воздействием геологических запасов нефти, д.e.:
          if(sumporv.gt.0) then
             if(OIP(i,j)/sumporv.gt.critzap) OxvatOIPOil(i,j)=OxvatOilGeo(i,j)/OIP(i,j)
          endif         
          
! OxvatOMP - Теущий 3D-oхват воздействием извлекаемых запасов нефти и воды (зоны дренирования), д.e.:
          if(OMP(i,j)+WIPDren(i,j).gt.0.) OxvatOMP(i,j)=(OxvatOilPot(i,j)+OxvatWat(i,j))/(OMP(i,j)+WIPDren(i,j))

! OxvatOMPOil - Теущий 3D-oхват воздействием геологических запасов нефти, д.e.:
! critzap - критические запасы - параметр для "чистки" краеваых зон карт (игнорирую ячейки с малыми запасами) 
! (critzap - задается в optionMaps.txt)
          if(sumporv.gt.0) then
             if(OMP(i,j)/sumporv.gt.critzap) OxvatOMPOil(i,j)=OxvatOilPot(i,j)/OMP(i,j)
            !if(iprint.eq.1) then
            !  print *,'omp(i,)=',omp(i,j),' sumporv=',sumporv,' critzap=',critzap
            !  print 205,'OxvatOmpOil=',OxvatOMPOil(i,j),' OxvatOilPot=',OxvatOilPot(i,j),' OMP=',OMP(i,j)
            !endif
          else
            !if(iprint.eq.1) then
            !  print 205,'OxvatOmpOil=',OxvatOMPOil(i,j)
            !endif
          endif
205 format(a12,f7.4,a10,f15.1,a5,f15.1)            

! OxvatOMPWat - Теущий 3D-oхват воздействием объемов воды, д.e.:
          if(WIPDren(i,j).gt.0.) OxvatOMPWat(i,j)=OxvatWat(i,j)/WIPDren(i,j)          
        enddo 
      enddo

      if(izap.eq.1) then
! Расчет карт:

! OxvatOIP - Теущий 3D-oхват воздействием геологических запасов нефти и воды (зоны дренирования), д.e.:
!        lmap=13+lplast
!        a30='                              '
!        a30='OxvatOIP_'//plast(1:lplast)//'.txt'
!        call mapoxvat(a30,lmap,OxvatOIP,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)

! OxvatOIPOil - Теущий 3D-oхват воздействием геологических запасов нефти, д.e.:
        lmap=16+lplast
        a30='                              '
        a30='OxvatOIPOil_'//plast(1:lplast)//'.txt'
        call mapoxvat(a30,lmap,OxvatOIPOil,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)

! OxvatOMP - Теущий 3D-oхват воздействием извлекаемых запасов нефти и воды (зоны дренирования), д.e.:
!        lmap=13+lplast
!        a30='                              '
!        a30='OxvatOMP_'//plast(1:lplast)//'.txt'
!        call mapoxvat(a30,lmap,OxvatOMP,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)

! OxvatOMPOil - Теущий 3D-oхват воздействием извлекаемых запасов нефти, д.e.:
        lmap=16+lplast
        a30='                              '
        a30='OxvatOMPOil_'//plast(1:lplast)//'.txt'
        call mapoxvat(a30,lmap,OxvatOMPOil,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)

! OxvatOMPWat - Теущий 3D-oхват воздействием извлекаемых запасов воды, д.e.:
!        lmap=16+lplast
!        a30='                              '
!        a30='OxvatOMPWat_'//plast(1:lplast)//'.txt'
!        call mapoxvat(a30,lmap,OxvatOMPWat,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)
      endif
      
      deallocate(OxvatOIP,OxvatOIPOil,OIP,OMP,OxvatOMP,OxvatOMPOil,OxvatOMPWat,WIP,WIPDren,OxvatOilGeo,OxvatOilPot,OxvatWat,stat=ierr)   
      
      return                                                                                                                                 
      end
