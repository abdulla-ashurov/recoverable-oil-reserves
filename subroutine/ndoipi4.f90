      subroutine ndoip4(ni,nj,nk,k1,k2,ro,bo,porv,soil0,sowcr0,soili,swati,sowcri,xcel,ycel,scel,velosity,dat,imin,imax,jmin,jmax,xmin,ymin,xmax,ymax,koxvoil,koxvwat,oilsum1,vcrit,nt,nstep,idren,ikoxvp,oe)
! Расчет карты дренирования, коэффициентов охвата OIL/WATER, дренирукемых/недренируемых объемов OIL/WATER
! ------------------
! moboil - 2D-карта плотности текущих подвижных запасов нефти
! zdren  - 2D-карта зон дренирования всех ячеек(i,j) всех слоев к
! koxvoil=oildr1/oilsum1 - Текущий объемный коэффициент охвата воздействием текущих геологических запасов нефти
! koxvwat=watdr1/watsum1 - Текущий объемный коэффициент охвата воздействием текущих геологических запасов воды
! oildr1    - Геологические текущие запасы нефти дренируемые  
! oilsum1   - Геологические текущие запасы нефти общие  
! watdr1    - Геологические текущие запасы воды дренируемые  
! watsum1   - Геологические текущие запасы воды общие
! vcrut     - критическая скорость для определения дреиремых ячеек  
      character*8 dat 
      real*4    koxvoil,koxvwat,oilsum1
      real*4    soil0(ni,nj,nk),sowcr0(ni,nj,nk),soili(ni,nj,nk),sowcri(ni,nj,nk),swati(ni,nj,nk),porv(ni,nj,nk),velosity(ni,nj,nk)
      real*4    xcel(ni,nj),ycel(ni,nj),scel(ni,nj)
      integer*4 oe(1)
      integer*4 ni,nj,nk
      integer*2 nt,k1,k2,nstep
      integer*2 imin,imax,jmin,jmax
      integer*2 idren(1)
      
      real*4,allocatable:: oildr(:,:),watdr(:,:),oilsum(:,:),watsum(:,:),koxv(:,:)
      
      ni1=imax-imin+1
      nj1=jmax-jmin+1
      
      allocate(oildr(1:ni1,1:nj1),watdr(1:ni1,1:nj1),oilsum(1:ni1,1:nj1),watsum(1:ni1,1:nj1),koxv(1:ni1,1:nj1),stat=ierr)

      oildr1=0.
      watdr1=0.
      oilsum1=0.
      watsum1=0.
      koxvoil=0.
      koxvwat=0.
      
      do j=1,nj1
        do i=1,ni1
          oildr(i,j)=0.
          watdr(i,j)=0.
          oilsum(i,j)=0.
          watsum(i,j)=0.
          koxv(i,j)=0.
        enddo
      enddo
          
      do i=1,ni1
        i1=i+imin-1  
        do j=1,nj1
          j1=j+jmin-1
            do k=k1,k2
! oil - подвижная+неподвижная нефть в ячейке, rm3
! wat - вода в ячейке, rm3
              oil=porv(i1,j1,k)*soili(i1,j1,k)
              wat=porv(i1,j1,k)*swati(i1,j1,k)

! oilsum(i,j) - нефть в столбце (i,j), rm3
! watsum(i,j) - воды  в столбце (i,j), rm3
              oilsum(i,j)=oilsum(i,j)+oil
              watsum(i,j)=watsum(i,j)+wat
              
! oilsum1 - нефть в кубе, m3
! watsum1 - вода в кубе, m3
              oilsum1=oilsum1+oil
              watsum1=watsum1+wat

              oxwat=0.
              oxoil=0.

              if(soili(i1,j1,k).gt.0.01) then
! нефть 
                if(soili(i1,j1,k).ge.sowcri(i1,j1,k)) then
                  so=soili(i1,j1,k)-sowcri(i1,j1,k)
                  s1=soili(i1,j1,k)-sowcr0(i1,j1,k)
                  if(abs(s1).gt.0.) oxoil=so/s1
                  
     print 100,'i,j,k=',i,j,k,' soili=',soili(i1,j1,k),' sowcri=',sowcri(i1,j1,k),' sowcr0=',sowcr0(i1,j1,k)
100  format(a6,3i3,a7,f7.4,a8,f7.4,a8,f7.4)
     print 102,'velocity=',velosity(i1,j1,k)
102  format(a9,f15.12)                               
     print 101,' oxoil=(soili-sowcri)/(soili-sowcr0)=',oxoil,' oil=porv*soili=',oil
101  format(a37,f7.4,a16,f12.4)
     print *,'----'

                  if(oxoil.lt.0.)oxoil=0.
                  if(oxoil.gt.1.)oxoil=1.
! oxoil - кохв*запасы нефтенасыщенной ячейки:

                  oxoil=oxoil*oil
                  
                  if(oxoil.gt.0.) then
! Считаю запасы только для блоков, дренируемых добывающими скважинами:
                    if(koxvp.eq.1) then
                      m=(k-1)*ni*nj+(j1-1)*ni+i1
                      mm=oe(m)
                      if(mm.gt.0) then
                        if(idren(mm).eq.0) goto 2
                      else
                        goto 2   
                      endif
                    endif  
! oildr(i,j) - сумма подвижной+неподвижная нефти в дренируемых ячейках столбца (i,j), rm3
                    oildr(i,j)=oildr(i,j)+oil
2                   continue                    
! oildr1 - сумма подвижной+неподвижная нефти в дренируемых ячейках, rm3
                    oildr1=oildr1+oil
                  endif    
                else
                  if(velosity(i1,j1,k).gt.vcrit) then
! oxwat - кохв*запасы водонасыщенной ячейки:
                    oxwat=1.*wat
! Считаю запасы только для блоков, дренируемых добывающими скважинами:
                    if(koxvp.eq.1) then
                      m=(k-1)*ni*nj+(j1-1)*ni+i1
                      mm=oe(m)
                      if(mm.gt.0) then
                        if(idren(mm).eq.0) goto 3
                      else
                        goto 3   
                      endif
                    endif  
! watdr(i,j) - сумма воды в дренируемых ячейках столбца (i,j), rm3
                    watdr(i,j)=watdr(i,j)+wat
3                   continue                    
! watdr1 - сумма воды в дренируемых ячейках, rm3
                    watdr1=watdr1+wat
                  else
                    oxwat=0.  
                  endif                        
                endif    
              else
! вода                  
                if(velosity(i1,j1,k).gt.vcrit) then
! oxwat - кохв*запасы водонасыщенной ячейки:
                  oxwat=1.*wat
! Считаю запасы только для блоков, дренируемых добывающими скважинами:
                  if(koxvp.eq.1) then
                    m=(k-1)*ni*nj+(j1-1)*ni+i1
                    mm=oe(m)
                    if(mm.gt.0) then
                      if(idren(mm).eq.0) goto 5
                    else
                      goto 5   
                    endif
                  endif  
! watdr(i,j) - сумма воды в дренируемых ячейках столбца (i,j), rm3
                  watdr(i,j)=watdr(i,j)+wat
5                 continue                   
! watdr1 - сумма воды в дренируемых ячейках, rm3
                  watdr1=watdr1+wat
                else
                  oxwat=0.  
                endif                        
              endif
              
              koxv(i,j)=koxv(i,j)+oxwat+oxoil
     print 103,'koxv(i,j)=oxwat+oxoil=',koxv(i,j),' oxwat=',oxwat,' oxoil=',oxoil
103  format(a22,f7.4,a7,f7.4,a7,f7.4)
     print *,'====='
     
              koxvoil=koxvoil+oxoil
              koxvwat=koxvwat+oxwat
            enddo
            
            if(oilsum(i,j)+watsum(i,j).lt.0.000000001) then
              koxv(i,j)=-1.
            else
              koxv(i,j)=koxv(i,j)/(oilsum(i,j)+watsum(i,j))
            endif
            
        enddo
      enddo
     
      if(oilsum1.le.0.000000001) then
        koxvoil=0.         
      else    
        koxvoil=koxvoil/oilsum1
!        koxvoil=oildr1/oilsum1
      endif  
! koxvoil=oildr1/oilsum1 - Текущий объемный коэффициент охвата воздействием текущих геологических запасов нефти
      if(watsum1.le.0.000000001) then
        koxvwat=0.          
      else    
        koxvwat=koxvwat/watsum1     
!        koxvwat=watdr1/watsum1
      endif  
! koxvwat=watdr1/watsum1 - Текущий объемный коэффициент охвата воздействием текущих геологических запасов воды

! Карта зон дренирования для SURFER: 
      open(1,file='oxvat.txt')
! moboil - плотность текущих подвижных запасов нефти
      do j=1,nj1
        j1=j+jmin-1  
        do i=1,ni1
          i1=i+imin-1  
!          write(1,4) xcel(i1,j1),ycel(i1,j1),zdren(i,j)/(k2-k1+1)
          if(koxv(i,j).ge.0.)write(1,4) xcel(i1,j1),ycel(i1,j1),koxv(i,j)
        enddo
      enddo
      write(1,4) xmin,ymin
      write(1,4) xmax,ymax
      close(1)
 4    format(3e17.7)
      
      deallocate(oildr,watdr,oilsum,watsum,koxv,stat=ierr)
      return
      end