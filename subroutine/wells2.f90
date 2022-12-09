      subroutine wells2(namefile,lnamefl,datemap,k1,k2,imin,imax,jmin,jmax,is1,js1,ks1,nz)
! ‘ормирование списка i,j,k -€чеек, с которыми работают добывающие скважины с ограничени€ми:
! дата "datemap", слои "k1-k2", скважины расположены в пр€моугольнике "imin,imax,jmin,jmax"

      character*128 a128,b128
      character*100 namefile
      character*12  namewell,namewe(20000),namew
      character*10  well
      character*8   date,datemap
      character*6   a6
      character*4   ak1,ak2
      integer*4     iw
      integer*2     k1,k2,n1,n2,lnamefl,imin,imax,jmin,jmax,iwell,jwell,k1well,k2well,i,kwe
      integer*2     ii2,jj2,kk2
      integer*2     lnamewe(20000),iwe(20000),jwe(20000),marker(20000)
      integer*2     is1(1),js1(1),ks1(1)

      iend=0
      nwell=0
      nz=0
      
      open(2,file='iwejwekwe.###',form='binary')
! nzap- счетчик заиисей в файле 'iwejwekwe.###'
      nzap=0
      
      open(1,file=namefile(1:lnamefl),err=200)
1     read(1,100,end=200)a128
      nz=nz+1
      call uplot4(a128,b128,lb128,ip1,ip2,ip3,ip4,ip5,ier)
      if(ier.eq.1) goto 1
      if(b128(1:2).eq.'--') goto 1
      if(b128(1:3).eq.'DAT') then
          
!  онец просмотра файла ,т.к. достигнута date=datemap:
        if(iend.eq.1) then
          goto 200
        endif  
        
21      read(1,100,end=200)a128
        call uplot4(a128,b128,lb128,ip1,ip2,ip3,ip4,ip5,ier)
        if(ier.eq.1) goto 21
        call rddat(b128,date)
        if(date.eq.datemap) iend=1
! iend=1 - начинаетс€ обработка завершающего времеенного шага
      endif
      
      if(b128(1:8).eq.'WELSPECS')then
2       read(1,100,end=200)a128
        call uplot4(a128,b128,lb128,ip1,ip2,ip3,ip4,ip5,ier)
        if(ier.eq.1) goto 2
        if(b128(1:2).eq.'--') goto 2
        if(b128(1:1).eq.'/') goto 1
! ¬ строке
!1540 BOND 7 37 1* OIL 1* 1* 1* NO 1* 1* /

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
! jwell-j-координата скважины:            
        i=ip4-ip3-1
        a6(1:i)=b128(ip3+1:ip4-1)
        call chntoi(i,a6,jwell)
                   
! ѕроверка: скважина лежит вне участка 
        if(iwell.lt.imin.or.iwell.gt.imax) goto 2
        if(jwell.lt.jmin.or.jwell.gt.jmax) goto 2
       
        do i=1,nwell
          namew=namewe(i)
          if(lwell.eq.lnamewe(i).and.namewell(1:lwell).eq.namew(1:lwell)) goto 2
        enddo
        nwell=nwell+1
        namewe(nwell)=namewell
        lnamewe(nwell)=lwell
        iwe(nwell)=iwell
        jwe(nwell)=jwell
        marker(nwell)=0
! marker=0 - транзитна€(не перфорирована)
        goto 2
      endif
     
      if(b128(1:8).eq.'WELLCOMP')then        
3       read(1,100,end=200)a128
        call uplot4(a128,b128,lb128,ip1,ip2,ip3,ip4,ip5,ier)
        if(ier.eq.1) goto 3
        if(b128(1:2).eq.'--') goto 3
        if(b128(1:1).eq.'/') goto 1
        goto 3        
      endif
      
      if(b128(1:7).eq.'COMPDAT')then
!COMPDAT                                                                                                                         
!1 4 4 1 1 OPEN * * 0.200 * * * Z /                                                
!/                                                                                                                               
4       read(1,100,end=200)a128
        call uplot4(a128,b128,lb128,ip1,ip2,ip3,ip4,ip5,ier)
        if(ier.eq.1) goto 4
        if(b128(1:2).eq.'--') goto 4
        if(b128(1:1).eq.'/') goto 1
! ¬ строке
!1540 7 37 1 2 ... /
! namewell()-им€ скважины:
        i=ip1-1
        lwell=i
        namewell(1:12)='           '
        namewell(1:i)=b128(1:i)
! iwell-i-координата скважины:
        i=ip2-ip1-1
        a6(1:i)=b128(ip1+1:ip2-1)
        call chntoi(i,a6,iwell) 
! jwell-j-координата скважины:            
        i=ip3-ip2-1
        a6(1:i)=b128(ip2+1:ip3-1)
        call chntoi(i,a6,jwell)
! k1well-номер верхнего сло€:
        i=ip4-ip3-1
        a6(1:i)=b128(ip3+1:ip4-1)
        call chntoi(i,a6,k1well) 
! k2well-номер нижнего сло€:            
        i=ip5-ip4-1
        a6(1:i)=b128(ip4+1:ip5-1)
        call chntoi(i,a6,k2well)
! im-номер скважины:          
        do i=1,nwell
          namew=namewe(i)
          if(lwell.eq.lnamewe(i).and.namewell(1:lwell).eq.namew(1:lwell)) then
            iw=i
            goto 14
          endif  
        enddo
        goto 4
14      continue 
! ѕроверка: скважина работает на слои k1-k2?
        
        marker(iw)=1
        do kk=k1well,k2well
          if(kk.ge.k1.or.kk.le.k2) then
            nzap=nzap+1
            kk2=kk
            write(2)iwe(iw),jwe(iw),kk2,iw
          endif    
        enddo
        goto 4
      endif
      
      if(b128(1:8).eq.'WCONPROD'.or.b128(1:8).eq.'WCONHIST')then
!WCONPROD                                                                                                                        
!1 OPEN BHP * * * * * 100.0 /    
!/
5     read(1,100,end=200)a128
        call uplot4(a128,b128,lb128,ip1,ip2,ip3,ip4,ip5,ier)
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
        endif
        if(ista.eq.2) then
          if(marker(iw).eq.0) marker(iw)=0
          if(marker(iw).eq.1) marker(iw)=3
          if(marker(iw).eq.2) marker(iw)=3
          if(marker(iw).eq.3) marker(iw)=3
        endif
! 0-транзитна€(не перфорирована), 1-перфорирована,но не работает
! 2-добывающа€ работает, 3- добывающа€ остановлена
        goto 5        
      endif
      goto 1
      
200   if(nz.eq.0) then
        close(1)
        print *,'ERROR! Wells2: ќтсутствует файл ',namefile(1:lnamefl) 
        stop
      else
        close(1)
      endif
      close(2)
      
      open(1,file='iwejwekwe.###',form='binary')
      nz=0
      do i=1,nzap
        read(1,end=20)ii2,jj2,kk2,iw
        if(marker(iw).eq.2) then
          nz=nz+1
          is1(nz)=ii2
          js1(nz)=jj2
          ks1(nz)=kk2
        endif
      enddo
      close(1,status='delete')
      return
      
20    print *,'ERROR! Wells2: проблема чтени€ файла ','iwejwekwe.###' 
      stop
      
100   format(a128)      
      end 
