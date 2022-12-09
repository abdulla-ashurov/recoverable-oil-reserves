      subroutine wells(namefile,lnamefl,xmin,ymin,xmax,ymax,datemap,k1,k2,imin,imax,jmin,jmax,xcel,ycel,ni,nj,plast,lplast)
      character*128 a128,b128
      character*100 namefile
      character*12  namewell
      character*10  well,plast
      character*8   date,datemap
      character*6   a6
      character*4   ak1,ak2
      real*4        xcel(ni,nj),ycel(ni,nj)      
      integer*4     ni,nj
      integer*2     k1,k2,n1,n2,lnamefl,imin,imax,jmin,jmax,iwell,jwell,k1well,k2well,i,lplast

      character*12  namewe(1000),namew
      integer*2     lnamewe(1000)
      integer*2     iwe(1000,500),jwe(1000,500)
      integer*2     marker(1000),setac(1000)
! ќграничение: 1000 скважин, в каждой перфорирорвано до 500 €чек
!
! datemap - дата,на которую будет построена карта
! k1,k2 - номера слоев, дл€ которых будет строитьс€ карта
! imin,imax,jmin,jmax - участок модели, дл€ которого будет строитьс€ карта
! xmin,ymin,xmax,ymax - участок пласта. дл€ которого будет строитьс€ карта

      iend=0
      nwell=0
      nwellg=0
      nz=0
! ќбнул€ю счетчик €чеек дл€ скважин:      
      do i=1,1000
        setac(i)=0
      enddo  

      !print *,'wells: open=',namefile(1:lnamefl)
      
      open(1,file=namefile(1:lnamefl))
      !print *,'wells: 1' 
1     continue
      read(1,100,end=200)a128
      
      nz=nz+1
      call uplot4(a128,b128,lb128,ip1,ip2,ip3,ip4,ip5,ier)
      if(b128(1:2).eq.'--') goto 1
      if(ier.eq.1) goto 1
      if(b128(1:3).eq.'DAT') then
          
!  онец просмотра файла ,т.к. достигнута date=datemap:
        if(iend.eq.1) then
          goto 200
        endif  
        
20      read(1,100,end=200)a128
        call uplot4(a128,b128,lb128,ip1,ip2,ip3,ip4,ip5,ier)         
        !print *,'wells 4 b128=',b128(1:20)
        if(ier.eq.1) goto 20
        call rddat(b128,date)
        if(date.eq.datemap) iend=1
! iend=1 - начинаетс€ обработка завершающего времеенного шага
      endif

      if(b128(1:8).eq.'WELSPECS')then
2       read(1,100,end=200)a128
        call uplot4(a128,b128,lb128,ip1,ip2,ip3,ip4,ip5,ier)
        !print *,'wells 5 b128=',b128(1:20)
        if(ier.eq.1) goto 2
        if(b128(1:2).eq.'--') goto 2
        if(b128(1:1).eq.'/') goto 1
! ¬ строке
!1540 BOND 7 37 1* OIL 1* 1* 1* NO 1* 1* /
! ip1 - положение первого пробела:
! namewell()-им€ скважины:
        i=ip1-1
        lwell=i
        namewell(1:12)='           '
        namewell(1:i)=b128(1:i)
! namegroup()-им€ группы скважины:           
!        i=ip2-ip1-1
!        lgroup=i
!        namegroupold(1:i)=b128(ip1+1:ip2-1)
! iwell-i-координата скважины:
        i=ip3-ip2-1
        a6(1:i)=b128(ip2+1:ip3-1)
        call chntoi(i,a6,iwell)
        !print *,'wells31 a6=',a6,' iwell=',iwell,' i=',i,' ip3=',ip3,' ip2=',ip2 
! jwell-j-координата скважины:            
        i=ip4-ip3-1
        a6(1:i)=b128(ip3+1:ip4-1)
        call chntoi(i,a6,jwell)
        !print *,'wells32 a6=',a6,' jwell=',jwell
        !print *,' imin=',imin,' imax=',imax
        !print *,' jmin=',jmin,' jmax=',jmax
        !print *,' '
! ѕроверка: скважина лежит вне участка карты
        if(iwell.lt.imin.or.iwell.gt.imax) goto 2
        if(jwell.lt.jmin.or.jwell.gt.jmax) goto 2
        do i=1,nwell
          namew=namewe(i)
          if(lwell.eq.lnamewe(i).and.namewell(1:lwell).eq.namew(1:lwell)) goto 2
        enddo
        nwell=nwell+1
        if(nwell.gt.1000) then
          print *,'WELLS: nwell>1000 STOP'
          stop
        endif
        namewe(nwell)=namewell
        lnamewe(nwell)=lwell
        setac(nwell)=setac(nwell)+1
        ia=setac(nwell)
        iwe(nwell,ia)=iwell
        jwe(nwell,ia)=jwell
        !print *,'wells 33 iwe(nwell)=',iwe(nwell),' jwe(nwell)=',jwe(nwell),' nwell=',nwell 
        marker(nwell)=0
! marker=0-транзитна€(не перфорирована),1-перфорирована,но не работает
! 2-добывающа€ работает,3-добывающа€ остановлена
! 4-нагнетательна€ работает,5-нагнетательна€ остановлена       
        !print *,'WELSPECS: nwell=',nwell,' namewell=',namewell(1:lwell),' marker=',marker(nwell)
        goto 2
      endif
      
      if(b128(1:8).eq.'WELLCOMP')then        
3       read(1,100,end=200)a128
        call uplot4(a128,b128,lb128,ip1,ip2,ip3,ip4,ip5,ier)
        !print *,'wells 40 b128=',b128(1:20)
        if(ier.eq.1) goto 3
        if(b128(1:2).eq.'--') goto 3
        if(b128(1:1).eq.'/') goto 1        
        goto 3        
      endif
      
      if(b128(1:7).eq.'COMPDAT')then
!COMPDAT                                                                                                                         
!1 4 4 1 1 OPEN * * 0.200 * * * Z /                                                
!/                                                                                                                               
4       continue
        read(1,100,end=200)a128
        call uplot4(a128,b128,lb128,ip1,ip2,ip3,ip4,ip5,ier)
        !print *,'wells 50 b128=',b128(1:20)
        if(ier.eq.1) goto 4
        if(b128(1:2).eq.'--') goto 4
        if(b128(1:1).eq.'/') goto 1
! ¬ строке
!1540 7 37 1 2 ... /
! ќпредел€ю положени€ первых 5-х пробелов:

! namewell()-им€ скважины:
        i=ip1-1
        lwell=i
        namewell(1:12)='           '
        namewell(1:i)=b128(1:i)
      !print *,' '  
      !print *,'51 namewell=',namewell(1:i)
! iwell-i-координата скважины:
        i=ip2-ip1-1
        a6(1:i)=b128(ip1+1:ip2-1)
        call chntoi(i,a6,iwell) 
      !print *,'52 iwell=',iwell
! jwell-j-координата скважины:            
        i=ip3-ip2-1
        a6(1:i)=b128(ip2+1:ip3-1)
        call chntoi(i,a6,jwell)
      !print *,'53 jwell=',jwell
! k1well-номер сло€ модели:
        i=ip4-ip3-1
        a6(1:i)=b128(ip3+1:ip4-1)
        call chntoi(i,a6,k1well) 
      !print *,'54 k1well=',k1well
! k2well-координата скважины:            
        i=ip5-ip4-1
        a6(1:i)=b128(ip4+1:ip5-1)
        call chntoi(i,a6,k2well)
      !print *,'55 i=',i
      !print *,'56 a6(1:i)=',a6(1:i)
      !print *,'57 k2well=',k2well
! проверка: скважина работает на слои модели k1-k2
      !print *,' k1=',k1,' k2=',k2
        do k=k1,k2
          do kk=k1well,k2well
            if(k.eq.kk) goto 13
          enddo
        enddo
        !print *,'58 goto 4'
        goto 4 
13      continue        
        do i=1,nwell
          namew=namewe(i)
          if(lwell.eq.lnamewe(i).and.namewell(1:lwell).eq.namew(1:lwell)) then
            iw=i
        !print *,'59 goto 14'
            goto 14
          endif  
        enddo
        !print *,'59-1 goto 4'
        goto 4
14      continue 
        if(marker(iw).eq.0) marker(iw)=1
        setac(iw)=setac(iw)+1
        ia=setac(iw)
        !nwellg=nwellg+1
        !if(nwellg.gt.20000) then
        !  print *,'WELLS: nwellg>20000 STOP!'
        !  stop
        !endif
        if(ia.gt.500) then
          print *,'WELLS: €чеек_nwell>500 STOP!'
          stop
        endif
        
! —охранение работающей траектории скважины:
        iwe(iw,ia)=iwell
        jwe(iw,ia)=jwell
        !nameweg(nwellg)=namew
        !print *,'wells 59-2 marker(iw)=',marker(iw),' iw=',iw
        !print *,'59-3 goto 4'
        !print *,'wells WCOMPDAT: iw=',iw,' namewell=',namewell(1:lw),' marker=',marker(iw)

        goto 4        
      endif
      
      if(b128(1:8).eq.'WCONPROD'.or.b128(1:8).eq.'WCONHIST')then
!WCONPROD                                                                                                                        
!1 OPEN BHP * * * * * 100.0 /    
!/
5     continue
      read(1,100,end=200)a128
      call uplot4(a128,b128,lb128,ip1,ip2,ip3,ip4,ip5,ier)
      !print *,'wells 60 b128=',b128(1:20)
      if(ier.eq.1) goto 5
        if(b128(1:2).eq.'--') goto 5
        if(b128(1:1).eq.'/') goto 1
! namewell()-им€ скважины:
        i=ip1-1
        lwell=i
        namewell(1:12)='           '
        namewell(1:i)=b128(1:i)
! open/schut скважины:
        i=ip2-ip1-1
        a6(1:i)=b128(ip1+1:ip2-1)
        ista=0
        if(a6(1:4).eq.'open')ista=1        
        if(a6(1:4).eq.'OPEN')ista=1        
        if(a6(1:4).eq.'shut')ista=2        
        if(a6(1:4).eq.'SHUT')ista=2        
        if(a6(1:4).eq.'stop')ista=2        
        if(a6(1:4).eq.'STOP')ista=2        
        do i=1,nwell
          namew=namewe(i)
          if(lwell.eq.lnamewe(i).and.namewell(1:lwell).eq.namew(1:lwell)) then
            iw=i
            goto 16
          endif  
        enddo
        goto 5
16      continue
        if(ista.eq.1) then
          if(marker(iw).eq.0) marker(iw)=0
          if(marker(iw).eq.1) marker(iw)=2
          if(marker(iw).eq.2) marker(iw)=2
          if(marker(iw).eq.3) marker(iw)=2
          if(marker(iw).eq.4) marker(iw)=2
          if(marker(iw).eq.5) marker(iw)=2
        endif
        if(ista.eq.2) then
          if(marker(iw).eq.0) marker(iw)=0
          if(marker(iw).eq.1) marker(iw)=3
          if(marker(iw).eq.2) marker(iw)=3
          if(marker(iw).eq.3) marker(iw)=3
          if(marker(iw).eq.4) marker(iw)=3
          if(marker(iw).eq.5) marker(iw)=3
        endif
! 0-транзитна€(не перфорирована), 1-перфорирована,но не работает
! 2-добывающа€ работает, 3- добывающа€ остановлена
! 4-нагнетательна€ работает, 5- нагнетательна€ остановлена       
        !print *,'WCONPROD/WCONHIST: iw=',iw,' namewell=',namewell(1:lwell),' marker=',marker(iw)
        goto 5        
      endif
      
      if(b128(1:7).eq.'WCONINJ')then
!2014 WATER OPEN 87.97 4
6       continue
        read(1,100,end=200)a128
        call uplot4(a128,b128,lb128,ip1,ip2,ip3,ip4,ip5,ier)
        !print *,'wells 70 b128=',b128(1:20)
        if(ier.eq.1) goto 6
        if(b128(1:2).eq.'--') goto 6
        if(b128(1:1).eq.'/') goto 1
! namewell()-им€ скважины:
        i=ip1-1
        lwell=i
        namewell(1:12)='           '
        namewell(1:i)=b128(1:i)
! open/schut скважины:
        i=ip3-ip2-1
        a6(1:i)=b128(ip2+1:ip3-1)
        ista=0
        if(a6(1:4).eq.'open')ista=1        
        if(a6(1:4).eq.'OPEN')ista=1        
        if(a6(1:4).eq.'shut')ista=2        
        if(a6(1:4).eq.'SHUT')ista=2        
        if(a6(1:4).eq.'STOP')ista=2        
        if(a6(1:4).eq.'stop')ista=2        
        do i=1,nwell
          namew=namewe(i)
          if(lwell.eq.lnamewe(i).and.namewell(1:lwell).eq.namew(1:lwell)) then
            iw=i
            goto 18
          endif  
        enddo
        goto 6
18      continue
        if(ista.eq.1) then
          if(marker(iw).eq.0) marker(iw)=0
          if(marker(iw).eq.1) marker(iw)=4
          if(marker(iw).eq.2) marker(iw)=4
          if(marker(iw).eq.3) marker(iw)=4
          if(marker(iw).eq.4) marker(iw)=4
          if(marker(iw).eq.5) marker(iw)=4
        endif
        if(ista.eq.2) then
          if(marker(iw).eq.0) marker(iw)=0
          if(marker(iw).eq.1) marker(iw)=5
          if(marker(iw).eq.2) marker(iw)=5
          if(marker(iw).eq.3) marker(iw)=5
          if(marker(iw).eq.4) marker(iw)=5
          if(marker(iw).eq.5) marker(iw)=5
        endif
! 0-транзитна€(не перфорирована), 1-перфорирована,но не работает
! 2-добывающа€ работает, 3- добывающа€ остановлена
! 4-нагнетательна€ работает, 5- нагнетательна€ остановлена 
        !print *,'wells WCONINJ: iw=',iw,' namewell=',namewell(1:lwell),' marker=',marker(iw)
        goto 6        
      endif
      
      goto 1
200   continue
      !print *,' LABEL 200'
      if(nz.eq.0) then
        close(1)
        print *,'ERROR! Wells: ќтсутствует файл ',namefile(1:lnamefl) 
        stop
      else
        close(1)
      endif  

      call itochn(k1,ak1,n1)
      call itochn(k2,ak2,n2)

! –аботающие producers:         
      open(2,file='wp1_'//plast(1:lplast)//'.txt')   
! ѕростаивающие producers:         
      open(3,file='wp0_'//plast(1:lplast)//'.txt')
! –аботающие injectors:         
      open(4,file='wi1_'//plast(1:lplast)//'.txt')
! ѕростаивающие injectors:         
      open(5,file='wi0_'//plast(1:lplast)//'.txt')
! “ранзитные скважины:         
      open(6,file='wt_'//plast(1:lplast)//'.txt')
! “ранзитные скважины:         
      open(1,file='w0_'//plast(1:lplast)//'.txt')

      nz1=0
      nz2=0
      nz3=0
      nz4=0
      nz5=0
      nz6=0
      
      write(1,300) xmin,ymin
      write(1,300) xmax,ymax
      write(2,300) xmin,ymin
      write(2,300) xmax,ymax
      write(3,300) xmin,ymin
      write(3,300) xmax,ymax
      write(4,300) xmin,ymin
      write(4,300) xmax,ymax
      write(5,300) xmin,ymin
      write(5,300) xmax,ymax
      write(6,300) xmin,ymin
      write(6,300) xmax,ymax

      !print *,'wells 8 nwell=',nwell
      do i=1,nwell
        namew=namewe(i)
        if(marker(i).eq.0) then
          do ii=1,setac(i)
            iii=iwe(i,ii)
            jjj=jwe(i,ii)
            x=xcel(iii,jjj)
            y=ycel(iii,jjj)
            if(x.le.xmax.and.x.ge.xmin) then
              if(y.le.ymax.and.y.ge.ymin) then
              
            if(ii.eq.1) then
              write(6,300)x,y,' ',namew(1:lnamewe(i))
            else
              write(6,300)x,y
            endif
            
              endif
            endif
          enddo
          nz6=1
        endif
        !print *,'wells 15'
        if(marker(i).eq.1) then
          do ii=1,setac(i)
            iii=iwe(i,ii)
            jjj=jwe(i,ii)
            x=xcel(iii,jjj)
            y=ycel(iii,jjj)
            if(x.le.xmax.and.x.ge.xmin) then
              if(y.le.ymax.and.y.ge.ymin) then
            
            if(ii.eq.1) then
              write(1,300)x,y,' ',namew(1:lnamewe(i))
            else
              write(1,300)x,y
            endif
              endif
            endif
            
          enddo
          nz1=1
        endif
        !print *,'wells 20'
        if(marker(i).eq.2) then
          do ii=1,setac(i)
            iii=iwe(i,ii)
            jjj=jwe(i,ii)
            x=xcel(iii,jjj)
            y=ycel(iii,jjj)
            if(x.le.xmax.and.x.ge.xmin) then
              if(y.le.ymax.and.y.ge.ymin) then
            
            if(ii.eq.1) then
              write(2,300)x,y,' ',namew(1:lnamewe(i))
            else
              write(2,300)x,y
            endif
              endif
            endif
            
          enddo
          nz2=1
        endif
        !print *,'wells 30'
        if(marker(i).eq.3) then
          do ii=1,setac(i)
            iii=iwe(i,ii)
            jjj=jwe(i,ii)
            x=xcel(iii,jjj)
            y=ycel(iii,jjj)
             if(x.le.xmax.and.x.ge.xmin) then
              if(y.le.ymax.and.y.ge.ymin) then
           
            if(ii.eq.1) then
              write(3,300)x,y,' ',namew(1:lnamewe(i))
            else
              write(3,300)x,y
            endif
               endif
            endif
           
          enddo
          nz3=1
        endif
        !print *,'wells 40'
        if(marker(i).eq.4) then
          do ii=1,setac(i)
            iii=iwe(i,ii)
            jjj=jwe(i,ii)
            x=xcel(iii,jjj)
            y=ycel(iii,jjj)
            if(x.le.xmax.and.x.ge.xmin) then
              if(y.le.ymax.and.y.ge.ymin) then
            
            if(ii.eq.1) then
              write(4,300)x,y,' ',namew(1:lnamewe(i))
            else
              write(4,300)x,y
            endif
               endif
            endif
           
          enddo
          nz4=1
        endif
        !print *,'wells 50'
        if(marker(i).eq.5) then
          do ii=1,setac(i)
            iii=iwe(i,ii)
            jjj=jwe(i,ii)
            x=xcel(iii,jjj)
            y=ycel(iii,jjj)
            if(x.le.xmax.and.x.ge.xmin) then
              if(y.le.ymax.and.y.ge.ymin) then
            
            if(ii.eq.1) then
              write(5,300)x,y,' ',namew(1:lnamewe(i))
            else
              write(5,300)x,y
            endif
              endif
            endif
            
          enddo
          nz5=1
        endif
        !print *,'wells 60'
      enddo
      !print *,'wells 70'
      
      close(1)
      close(6)
      close(5)
      close(4)
      close(3)
      close(2)
      
      !print *,'wells 8'
       
      if(nz1.eq.0) then
        open(1,file='w0_'//ak1(1:n1)//'-'//ak2(1:n2)//'.txt')
        close(1,status='delete')  
      endif
      if(nz2.eq.0) then
        open(1,file='wp1_'//ak1(1:n1)//'-'//ak2(1:n2)//'.txt')   
        close(1,status='delete')  
      endif
      if(nz3.eq.0) then
        open(1,file='wp0_'//ak1(1:n1)//'-'//ak2(1:n2)//'.txt')
        close(1,status='delete')  
      endif
      if(nz4.eq.0) then
        open(1,file='wi1_'//ak1(1:n1)//'-'//ak2(1:n2)//'.txt')
        close(1,status='delete')  
      endif
      if(nz5.eq.0) then
        open(1,file='wi0_'//ak1(1:n1)//'-'//ak2(1:n2)//'.txt')
        close(1,status='delete')  
      endif
      if(nz6.eq.0) then
        open(1,file='wt_'//ak1(1:n1)//'-'//ak2(1:n2)//'.txt')
        close(1,status='delete')  
      endif
      
100   format(a128)      
300   format(2e17.7,a1,a<lnamewe(i)>) 
301   format(2e17.7,a1,a12)

      return
      end 
