     subroutine KoefOxvat(nfluids,idensity,ikoxvp,ni,nj,nk,k1,k2,ros,ror,oil_den,oe,idren,porv,soil,soil0,sgas,sowcr,sowcr0,swcr,velosity,imin,imax,jmin,jmax,qq6t,KINrec,qq7r,qq7s,qq7t,qq5t,qq4t,qq3t,koxvoiltotal,koxvoilrec,KINproject,koxvwat)
! Расчет коэффициентов охвата и КИН:
!
! swcr(i,j,k)   - куб SWCR
! sowcr(i,j,k)  - куб текущей критической нефти
! sowcr0(i,j,k) - куб начальной критической нефти
! oil_den(i,j,k)-куб плотности нефти в пластовых условиях
! porv  - поровый объем, м3                                                                                                                       
! soil  - нефтенасыщенность                                                                                                        
! Объем нефти в ячейках в пластовых условиях = произведению объема*нефтенасыщенность,м3
      real*4 porv(ni,nj,nk),soil(ni,nj,nk),soil0(ni,nj,nk),sowcr(ni,nj,nk),sowcr0(ni,nj,nk),sgas(ni,nj,nk),velosity(ni,nj,nk),swcr(ni,nj,nk),oil_den(ni,nj,nk)
      real*4 koxvoiltotal,koxvoilrec,koxvwat,KINrec,KINproject
      integer*4 oe(1)  
      integer*4 ni,nj,nk
      integer*2 k1,k2,imin,imax,jmin,jmax
      integer*2 idren(1)

      ni1=imax-imin+1
      nj1=jmax-jmin+1

      qq1r=0.
      qq1t=0.
      qq1s=0.
      
      qq2r=0.
      
      qq3r=0.
      qq3t=0.
      
      qq4r=0.
      qq4t=0.
      
      qq5r=0.
      qq5t=0.
      
      qq6r=0.
      qq6t=0.
      qq6s=0.
      
      qwatr=0.
      qwatmr=0.
      
      do i=1,ni1
        i1=i+imin-1
        do j=1,nj1
          j1=j+jmin-1
          do k=k1,k2
            m=(k-1)*ni*nj+(j1-1)*ni+i1
! Если ячейка неактивная goto 2:
            if(oe(m).le.0) goto 2
! Запасы cчитаю только для блоков, дренируемых потоками к добывающим скважинам,
! (потоки от удаленных нагнетательных скважин мб игнорироваться):
            sowcri=sowcr(i1,j1,k)
            if(ikoxvp.eq.1) then
              if(idren(m).eq.0) sowcri=soil(i1,j1,k) 
            endif
            if(sowcri.ge.soil(i1,j1,k)) sowcri=soil(i1,j1,k)
            
! Поровый объем ячейки,rm3:            
            p=porv(i1,j1,k)
            if(p.lt.0.)p=0.
            
            
            
! Начальные геологические запасы нефти в ячейке,rm3:
            q1r=p*soil0(i1,j1,k)
! Начальные геологические запасы нефти в ячейке,tonn:
            if(idensity.eq.1) q1t=q1r*ror/1000.
            if(idensity.eq.2) q1t=q1r*oil_den(i1,j1,k)/1000.
! Начальные геологические запасы нефти в ячейке,sm3:
            if(idensity.eq.1) q1s=q1r*ror/ros
            if(idensity.eq.2) q1s=q1r*oil_den(i1,j1,k)/ros
            
! Начальные подвижные запасы в нефти ячейке по линейной модели,rm3:
            q2r=p*(soil0(i1,j1,k)-sowcr0(i1,j1,k))
            if(q2r.lt.0)q2r=0.
            
! Текущие дренируемые извлекаемые запасы в нефти ячейке,rm3:
            q3r=p*(soil(i1,j1,k)-sowcri)
            if(q3r.lt.0)q3r=0.
! Текущие дренируемые извлекаемые запасы в нефти ячейке,tonn:
            if(idensity.eq.1) q3t=q3r*ror/1000.
            if(idensity.eq.2) q3t=q3r*oil_den(i1,j1,k)/1000.
            
! Текущие извлекаемые (потенциально подвижные) запасы в нефти ячейке,rm3:
            !q5r=p*(soil(i1,j1,k)-sowcr0(i1,j1,k))
            if(sowcri.lt.sowcr0(i1,j1,k)) then
              q5r=p*(soil(i1,j1,k)-sowcri)
            else  
              q5r=p*(soil(i1,j1,k)-sowcr0(i1,j1,k))
            endif  
            if(sopot.lt.0.) sopot=0.
            if(q5r.lt.0)q5r=0.
! Текущие извлекаемые (потенциально подвижные) запасы в нефти ячейке,tonn:
            if(idensity.eq.1) q5t=q5r*ror/1000.
            if(idensity.eq.2) q5t=q5r*oil_den(i1,j1,k)/1000.
            
!            q4t=q5t-q3t
! Текущие недренируемые извлекаемые запасы в нефти ячейке,rm3:
!            q4r=p*(sowcri-sowcr0(i1,j1,k))
!            if(q4r.lt.0)q4r=0.
!! Текущие недренируемые извлекаемые запасы в нефти ячейке,tonn:
!            if(idensity.eq.1) q4t=q4r*ror/1000.
!            if(idensity.eq.2) q4t=q4r*oil_den(i1,j1,k)/1000.
            
!! Текущие извлекаемые запасы в нефти ячейке,tonn:
!            q5r=q3r+q4r 
!            q5t=q3t+q4t
            
! Текущие недренируемые запасы в нефти ячейке,tonn:
            q4r=q5r-q3r 
            q4t=q5t-q3t
            
! Добытая нефть из ячейки,rm3:
            q6r=p*(soil0(i1,j1,k)-soil(i1,j1,k))
            if(q6r.lt.0)q6r=0.
! Добытая нефть из ячейки,tonn:
            if(idensity.eq.1) q6t=q6r*ror/1000.
            if(idensity.eq.2) q6t=q6r*oil_den(i1,j1,k)/1000.
! Добытая нефть из ячейки,sm3:
            if(idensity.eq.1) q6s=q6r*ror/ros
            if(idensity.eq.2) q6s=q6r*oil_den(i1,j1,k)/ros
            
            if(nfluids.eq.2) then
! Текущая обводненность в ячейке,rm3:            
              swr=1.-soil(i1,j1,k)
! Текущая подвижная обводненность в ячейке,rm3:            
              swmr=1.-soil(i1,j1,k)-swcr(i1,j1,k)
            endif
            
            if(nfluids.eq.3) then
              swr=1.-soil(i1,j1,k)-sgas(i1,j1,k)
              swmr=1.-soil(i1,j1,k)-swcr(i1,j1,k)-sgas(i1,j1,k)
            endif  
            if(swr.lt.0.)swr=0.
            if(swmr.lt.0.)swmr=0.
! wat - Текущие запасы воды в ячейке,rm3
            watr=p*swr
! watm - Текущие подвижные запасы воды в ячейке,rm3
            watmr=p*swmr
            
            call roxvatwat(vcrit,velosity(i1,j1,k),swr,swcr(i1,j1,k),oxvwat)

! ====Начальные запасы нефти
! Начальные геологические запасы нефти,rm3:  
            qq1r=qq1r+q1r
! Начальные геологические запасы нефти,sm3:  
            qq1s=qq1s+q1s
! Начальные геологические запасы нефти,tonn:  
            qq1t=qq1t+q1t
! Начальные извлекаемые (потенциально подвижные) запасы нефти,rm3:
            qq2r=qq2r+q2r
! ====Добытая нефть
! Накопленная добытая нефть,rm3:
            qq6r=qq6r+q6r
! Накопленная добытая нефть,tonn:
            qq6t=qq6t+q6t
            
! Накопленная добытая нефть,sm3:
            qq6s=qq6s+q6s
! ====Дренируемые запасы нефти
! Текушие дренируемые запасы нефти,rm3:               
            qq3r=qq3r+q3r
! Текушие дренируемые запасы нефти,tonn:               
            qq3t=qq3t+q3t
! Текущие недренируемые запасы нефти,rm3:            
            qq4r=qq4r+q4r
! Текущие недренируемые запасы нефти,tonn:            
            qq4t=qq4t+q4t
! Текущие извлекаемые (потенциально подвижные) запасы нефти,rm3:
            qq5r=qq5r+q5r
! Текущие извлекаемые (потенциально подвижные) запасы нефти,tonn:
            qq5t=qq5t+q5t
! ====Текущие и дренируемыезапасы воды
! Текущие объемы воды,rm3:  
            qwatr=qwatr+watr
! Текушие дренируемые объемы воды,rm3:
            qwatmr=qwatmr+oxvwat*watmr
  2         continue
          enddo
        enddo 
      enddo
! Текущие геологические запасы нефти,rm3:
      qq7r=qq1r-qq6r 
! Текущие геологические запасы нефти,sm3:
      qq7s=qq1s-qq6s 
! Текущие геологические запасы нефти,tonn:
      qq7t=qq1t-qq6t 

! Коэффициент охвата извлекаемых запасов нефти накопленный:
      if(qq2r.gt.0.000000000001) then
        koxvoiltotal=(qq6r+qq3r)/qq2r
        if(koxvoiltotal.gt.1.)koxvoiltotal=1.
      else
        koxvoiltotal=0.
      endif
! Коэффициент охвата извлекаемых запасов нефти текущий:
      if(qq5r.gt.0.000000000001) then
        koxvoilrec=qq3r/qq5r
        if(koxvoilrec.gt.1.)koxvoilrec=1.
      else
        koxvoilrec=0.
      endif
! Охват вовлечения объемов воды 
      if(qwatr.gt.0.000000000001) then
        koxvwat=qwatmr/qwatr
        if(koxvwat.gt.1.)koxvwat=1.
      else
        koxvwat=0.
      endif
      
! КИН текущий фактический:  
      if(qq1r.gt.0.000000000001) then
        KINrec=qq6r/qq1r
      else
        KINrec=0.
      endif
      
! КИН проектный c учетом текущего охвата:  
      KINproject=(qq6r+qq3r)/qq1r
      if(qq1r.gt.0.000000000001) then
        KINproject=(qq6r+qq3r)/qq1r
      else
         KINproject=0.
      endif
      return                                                                                                                                 
      end
