     subroutine roipi(ni,nj,nk,k1,k2,ro,bo,oe,idren,porv,soil,sowcr0,sowcr,velocity,xcel,ycel,scel,imin,imax,jmin,jmax,xmin,xmax,ymin,ymax,oilsum,oildrensum,izap,nt,ikoxvp,vcrit,scrit,koxvoil,koxvwat)
    
! Расчет плотностей распределения запасов:
! ----------------------------------------
! OIP(i,j)       - текущие геологические запасы нефти,rm3
! OIPDren(i,j)   - текущие геологические дренируемые запасы нефти,rm3
! OIPNoDren(i,j) - текущие геологические недренируемые запасы нефти
!
! OMP(i,j)       - текущие извлекаемые запасы нефти,rm3
! OMPDren(i,j)   - текущие извлекаемые дренируемые запасы нефти,rm3
! OMPNoDren(i,j) - текущие извлекаемыенедренируемые  запасы нефти

! Расчет охвата воздействием:
! -----------------------
! OxvatOIP(i,j)     - охват геологических запасов нефти и воды
! OxvatOIPOil(i,j)  - охват геологических запасов нефти
!
! OxvatOMP(i,j)     - охват извлекаемых запасов нефти и воды
! OxvatOMPOil(i,j)  - охват извлекаемых запасов нефти
! OxvatOMPWat(i,j)  - охват извлекаемых запасов воды

! Расчет запасов:
! ---------------
! oilsum - геологические запасы нефти,rm3
! oildrensum- дренируемые запасы, rм3
! watsum - геологические запасы воды,rm3
! watdrensum - дренируемые запасы воды,rm3

! Расчет Коэффициентов охвата:
! ----------------------------
! KoxvOIL - текущий коэф.охвата нефти 
! KoxvWAT - текущий коэф.охвата воды

! sowcr(i,j,k) - куб текущей критической нефти
! sowcr0(i,j,k)- куб начальной критической нефти

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
      
! porv  - поровый объем, м3                                                                                                                       
! soil  - нефтенасыщенность                                                                                                        
! Объем нефти в ячейках в пластовых условиях = произведению объема*нефтенасыщенность,м3

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
! swcr(связанная вода)=0.2
      
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

! В выбранных k-слоях модели:
! oilsum - геологические запасы нефти,rm3
      oilsum=0.
! oildrensum- дренируемые запасы, rм3
      oildrensum=0.
! oilndrensum-недренируемыезапасы      
      oilndrensum=0.
! watsum - геологические запасы воды,rm3
      watsum=0.
! watdrensum - дренируемые запасы воды,rm3
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
! Если ячейка неактивная goto 2:
            if(oe(m).lt.0) goto 2
            
! Поровый объем ячейки:            
            p=porv(i1,j1,k)
            if(p.lt.0.)p=0.
! Текущие геологические запасы нефти в ячейке:            
            oil =p*soil(i1,j1,k)
            
! Текущая насыщенность воды в ячейке (предполагается: sgas=0):
            swat=1.-soil(i1,j1,k)
! Текущий объем воды в ячейке:
            wat=p*swat

            sopot=soil(i1,j1,k)-sowcr0(i1,j1,k)
            if(sopot.lt.0.) sopot=0. 
! oilmpot - Текущиe извлекаемые (для вытеснения водой) запасы нефти в ячейке:
            oilmpot=p*sopot
! oilnmpot - Текущие неизвлекаемые запасы нефти в ячейке:
            oilnmpot=p*sowcr0(i1,j1,k)
            
            sw=1.-soil(i1,j1,k)-swcr
            if(sw.lt.0.)sw=0.
! watm - Текущие подвижные запасы воды в ячейке
            watm=p*sw
            
! Текущие дренируемая нефтенасыщенность в ячейке:
            so=soil(i1,j1,k)-sowcr(i1,j1,k)
            
            if(so.lt.0.)so=0.
! Текушие дренируемые запасы нефти в ячейке:
            oilm=p*so
! Текущие недренируемые запасы нефти в ячейке:            
            oilnm=p*sowcr(i1,j1,k)
! Объем недренируемой нефти в кубе:            
            oilndrensum  =oilndrensum+oilnm
            
! Запасы cчитаю только для блоков, дренируемых потоками к добывающим скважинам,
! (потоки от удаленных нагнетательных скважин мб игнорироваться):
            if(ikoxvp.eq.1) then
              if(idren(m).eq.0) goto 1
            endif  
           
! Дренируемые нефтенасыщеные ячейки:
            if(so.gt.scrit) then
! Сумма дренируемых нефтенасыщенных поровых объемов столбца, м3:  
!              aOilPvdren=aOilPvdren+p
! Объем дренируемой нефти в кубе:               
              oildrensum=oildrensum+oilm
            endif
! Коэфф.охвата нефтенасыщенной ячейки:
            oxvoil=0.
            if(oil.gt.0.and.sopot.gt.0.) oxvoil=so/sopot
            if(oxvoil.gt.1.)oxvoil=1.

!  Коэф.охвата водонасыщенной ячейки:
!           oxvwat=0.
!            if(velocity(i1,j1,k).ge.vcrit.and.sw.gt.0.) then
! Сумма дренируемых водонасыщенных поровых объемов столбца, м3:  
!              aWatPvdren=aWatPvdren+p
! Сумма дренируемых объемов воды куба:  
!              watdrensum=watdrensum+watm
!            endif

!  Коэф.охвата водонасыщенной ячейки:
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
            
! Сумма дренируемых объемов воды куба:
            watdrensum=watdrensum+oxvwat*watm

! Сумма для расчета охвата вытеснением нефти:
            OxvatOil(i,j)=OxvatOil(i,j)+oxvoil*oilmpot
            
! Сумма для расчета охвата вытеснения воды:
            OxvatWat(i,j)=OxvatWat(i,j)+oxvwat*p*(swat-swcr)
  1         continue
! Сумма поровых объемов столбца:  
            aLiqsum=aLiqsum+p
! Сумма геологических запасов нефти куба:  
            oilsum=oilsum+oil
! Сумма объемов воды куба:  
            watsum=watsum+wat
! -------------
! OIP - Текущие геологические запасы нефти:
            OIP(i,j)=OIP(i,j)+oil
! OIPDren  - Текущие геологические дренируемые запасы нефти:
            OIPDren(i,j)=OIPDren(i,j)+oilmpot
! OIPNoDren  - Текущие геологические недренируемые запасы нефти:
            OIPNoDren(i,j)=OIPNoDren(i,j)+oilnmpot
            
! OMP - Текущие извлекаемые запасы нефти:
            OMP(i,j)=OMP(i,j)+oilmpot
          
! OMPDren  - Текущие извлекаемые дренируемые запасы нефти:
            OMPDren(i,j)=OMPDren(i,j)+oilm
! OMPNoDren  - Текущие извлекаемые недренируемые запасы нефти, тыс.т/га:
            OMPNoDren(i,j)=OMPNoDren(i,j)+oilnm
            
! WIP - Текущие запасы воды            
            WiP(i,j)=WIP(i,j)+wat
! WIPDren - Текущие дренируемые запасы воды
            WiPDren(i,j)=WIPDren(i,j)+watm 
            
  2         continue
          enddo
          
! OxvatOIP - Теущий 3D-oхват воздействием геологических запасов нефти и воды (зоны дренирования), д.e.:
          if(OIP(i,j)+WIP(i,j).gt.0.) OxvatOIP(i,j)=(OxvatOil(i,j)+OxvatWat(i,j))/(OIP(i,j)+WIP(i,j))
! OxvatOIPOIl - Теущий 3D-oхват воздействием геологических запасов нефти, д.e.:
          if(OIP(i,j).gt.0.) OxvatOIPOil(i,j)=OxvatOil(i,j)/OIP(i,j)
          
! OxvatOMP - Теущий 3D-oхват воздействием извлекаемых запасов нефти и воды (зоны дренирования), д.e.:
          if(OMP(i,j)+WIPDren(i,j).gt.0.) OxvatOMP(i,j)=(OxvatOil(i,j)+OxvatWat(i,j))/(OMP(i,j)+WIPDren(i,j))
! OxvatOMPOil - Теущий 3D-oхват воздействием геологических запасов нефти, д.e.:

          if(OMP(i,j).gt.0.) OxvatOMPOil(i,j)=OxvatOil(i,j)/OMP(i,j)
! OxvatOMPWat - Теущий 3D-oхват воздействием объемов воды, д.e.:
          if(WIPDren(i,j).gt.0.) OxvatOMPWat(i,j)=OxvatWat(i,j)/WIPDren(i,j)          
        enddo 
      enddo
      
! Охват воздействием геологических запасов  
      if(oilsum.gt.0.000000000001) then
        koxvoil=oildrensum/oilsum
      else
        koxvoil=0.
      endif
! Охват вовлечения объемов воды  
      if(watsum.gt.0.000000000001) then
        koxvwat=watdrensum/watsum
      else
        koxvwat=0.
      endif    
      if(izap.eq.1) then
! Расчет карт:
! ===============================
        b=ro/(bo*100.)       
! OIP - Плотность распределения текущих геологических запасов нефти, тыс.т/га:
        call maps('OIP_________.txt',OIP,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)
! OIPDren  - Плотность распределения текущих недренируемых извлекаемых запасов нефти, тыс.т/га:
        call maps('OIPDren_____.txt',OIPDren,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)
! OIPNonDren  - Плотность распределения текущих недренируемых извлекаемых запасов нефти, тыс.т/га:
        call maps('OIPNoDren___.txt',OIPNoDren,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)
! ===============================
! OxvatOIP - Теущий 3D-oхват воздействием геологических запасов нефти и воды (зоны дренирования), д.e.:
        call mapoxvat('OxvatOIP____.txt',OxvatOIP,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)
! OxvatOIPOil - Теущий 3D-oхват воздействием геологических запасов нефти, д.e.:
        call mapoxvat('OxvatOIPOil_.txt',OxvatOIPOil,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)
! ===============================
! OMP - Плотность распределения текущих извлекаемых запасов нефти, тыс.т/га:
        call maps('OMP_________.txt',OMP,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)
! OMPDren  - Плотность распределения текущих извлекаемых дренируемых запасов нефти, тыс.т/га:
        call maps('OMPDren_____.txt',OMPDren,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)
! OMPNoDren  - Плотность распределения текущих недренируемых геологических запасов нефти, тыс.т/га:
        call maps('OMPNoDren___.txt',OMPNoDren,ni,nj,ni1,nj1,imin,jmin,ro,bo,scel,xcel,ycel,xmin,ymin,xmax,ymax,b)
! ===============================
! OxvatOMP - Теущий 3D-oхват воздействием геологических запасов нефти и воды (зоны дренирования), д.e.:
        call mapoxvat('OxvatOMP____.txt',OxvatOMP,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)
! OxvatOMPOil - Теущий 3D-oхват воздействием геологических запасов нефти, д.e.:
        call mapoxvat('OxvatOMPOil_.txt',OxvatOMPOil,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)
! OxvatOMPWat - Теущий 3D-oхват воздействием геологических запасов нефти, д.e.:
        call mapoxvat('OxvatOMPWat_.txt',OxvatOMPWat,ni,nj,ni1,nj1,imin,jmin,scel,xcel,ycel,xmin,ymin,xmax,ymax)
! ===============================
      endif
      
      deallocate(OIP,OIPDren,OIPNoDren,OxvatOIP,OxvatOIPOil,OMP,OMPDren,OMPNoDren,OxvatOMP,OxvatOMPOil,OxvatOMPWat,WIP,WIPDren,OxvatOil,OxvatWat,stat=ierr)   
      
      return                                                                                                                                 
      end
