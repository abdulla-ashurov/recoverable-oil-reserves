      subroutine PreSCH(path,lpath,namenp,lnamenp,date1,datestart,ndate,nwellsec,nsec,imi,ima,jmi,jma,imin,imax,jmin,jmax)
! Предварительная обработка файла *.sch
      character*400 path
      character*128 a128,b128
      character*80  a80
      character*40  a41
      character*12  w,namewell
      character*10  namenp(1),name
      character*8   date1,datestart,dat
      character*6   ai,aj
      character*20  namegroupold
      character*4   namegroupnew
      character*2   a2,a21
      character*1   a1
      integer*4     ndate
      integer*2     ic,ic1,ig1,ig2,lpath,lgroup,lwell,nsec
      integer*2     i,iwell,jwell,iwellnew,jwellnew
      integer*2     imi(1),ima(1),jmi(1),jma(1),imin(1),imax(1),jmin(1),jmax(1),lnamenp(1),nwellsec(1)
! nwelsec() - количество скважин в секторныч моделях
! lnamenp() - длины имен секторных моделей
! namenp()  - имена секторных моделей 

! Нормализация файла *.sch (исключение лишних пробелов) и создание уплотненного файла full.sc# 
      open(1,file=path(1:lpath))
        open(2,file='full.sc#',access='direct',form='formatted',recl=128)
          ir=0
3         read(1,100,end=2) a128
          if(a128(1:2).eq.'--') goto 3
          if(a128(1:20).eq.'                    ') goto 3
          call uplot(a128,b128)
          ir=ir+1
          write(2,100,rec=ir) b128
          goto 3
2         continue      
        close(2)
      close(1)
      
      do ns=1,nsec
        nwellsec(ns)=0
        name=namenp(ns)
        open(2,file=name(1:lnamenp(ns))//'.bnr',form='binary',position='append')
        close(2,status='delete')
        open(3,file='welspecs'//name(1:lnamenp(ns))//'.txt',position='append')
        close(3,status='delete')
      enddo
      
! Читаю date1 - первую дату DATE
      open(1,file='full.sc#',access='direct',form='formatted',recl=128)
      ir=0
4     ir=ir+1      
      read(1,100,rec=ir,err=5) a128
      if(a128(1:4).eq.'DATE') then
        ir=ir+1  
        read(1,100,rec=ir,err=5) a128
        call rddat(a128,date1)
        goto 5
      endif
      goto 4
5     continue
      close(1)
! Формирую дату datestart для ключевого слова START
      a2=date1(1:2)
      call ch2toi2(a2,ic)

        
      if(ic.gt.1) then
        ic1=ic-1
        if(ic1.le.9) then
          call i1toch1(ic1,a1)
          a2=' '//a1  
        else
          call i2toch2(ic1,a2)
        endif
        datestart=a2//date1(3:8)
      else
        if(date1(3:4).eq.'01') then
! 01/01/gg
          a2=date1(5:6)  
          call ch2toi2(a2,ig1)
          a2=date1(7:8)  
          call ch2toi2(a2,ig2)
          ig=ig1*100+ig2-1
          ig1=ig/100
          ig2=ig-ig1*100
          call i2toch2(ig1,a2)
          call i2toch2(ig2,a21)
          datestart='3112'//a2//a21    
        endif  
        if(date1(3:4).eq.'02') then
! 01/02/gg
          datestart='3001'//date1(5:8)   
        endif  
        if(date1(3:4).eq.'03') then
! 01/03/gg
          datestart='2702'//date1(5:8)   
        endif  
        if(date1(3:4).eq.'04') then
! 01/04/gg
          datestart='3003'//date1(5:8)   
        endif  
        if(date1(3:4).eq.'05') then
! 01/05/gg
          datestart='3004'//date1(5:8)   
        endif  
        if(date1(3:4).eq.'06') then
! 01/06/gg
          datestart='3105'//date1(5:8)   
        endif  
        if(date1(3:4).eq.'07') then
! 01/07/gg
          datestart='3006'//date1(5:8)   
        endif  
        if(date1(3:4).eq.'08') then
! 01/08/gg
          datestart='3007'//date1(5:8)   
        endif  
        if(date1(3:4).eq.'09') then
! 01/09/gg
          datestart='3108'//date1(5:8)   
        endif  
        if(date1(3:4).eq.'10') then
! 01/10/gg
          datestart='3009'//date1(5:8)   
        endif  
        if(date1(3:4).eq.'11') then
! 01/11/gg
          datestart='3010'//date1(5:8)   
        endif  
        if(date1(3:4).eq.'12') then
! 01/12/gg
          datestart='3011'//date1(5:8)   
        endif  
      endif
      
! Считаю количество DATE в full.sc#
      ndate=0
      open(1,file='full.sc#',access='direct',form='formatted',recl=128)
      ir=0
6     ir=ir+1
      read(1,100,rec=ir,err=7) a128
      if(a128(1:4).eq.'DATE') ndate=ndate+1
      goto 6
7     continue
      close(1)
      
      open(1,file='full.sc#',access='direct',form='formatted',recl=128)
      ir=0
8     ir=ir+1
      read(1,100,rec=ir,err=9) a128
      if(a128(1:8).eq.'WELSPECS') then
10      ir=ir+1
        read(1,100,rec=ir,err=9) a128
        if(a128(1:1).eq.'/') goto 8
! Преобразую строку            
!'1540' 'BOND' 7 37 1* 'OIL' 1* 1* 1* 'NO' 1* 1* /
! в 
!1540 BOND 7 37 1* OIL 1* 1* 1* NO 1* 1* /
! is1-счетчик символов, отличных от "'"  
        is1=0
        is2=0
        do i=1,40
          if(a128(i:i).ne."'") then
            is1=is1+1
            a41(is1:is1)=a128(i:i)
          endif
          if(a128(i:i).eq.' ') then
            is2=is2+1
          endif
          if(is2.eq.4) then
            a80(1:80)=a128(i:i+79)
!            print *,' a128=',a128(1:128)
!            print *,' a80=',a80(1:80)
!            print *,'---'
            goto 12
          endif  
        enddo
12      continue              
! В строке
!1540 BOND 7 37 1* OIL 1* 1* 1* NO 1* 1* /
! Определяю положения первых 4-х пробелов:
! is2-счетчик пробелов
        is2=0
        do i=1,40
          if(a41(i:i).eq.' ') then
            is2=is2+1
            if(is2.eq.1) ip1=i
            if(is2.eq.2) ip2=i
            if(is2.eq.3) ip3=i
            if(is2.eq.4)then
              ip4=i
              goto 11
            endif  
          endif
        enddo
11      continue
            
! namewell()-имя скважины:
        i=ip1-1
        lwell=i
        namewell(1:12)='           '
        namewell(1:i)=a41(1:i)
! namegroup()-имя группы скважины:           
        i=ip2-ip1-1
        lgroup=i
        
        namegroupold(1:i)=a41(ip1+1:ip2-1)
! iwell-i-координата скважины:
        i=ip3-ip2-1
        ai(1:i)=a41(ip2+1:ip3-1)
        call chntoi(i,ai,iwell) 
! jwell-j-координата скважины:            
        i=ip4-ip3-1
        aj(1:i)=a41(ip3+1:ip4-1)
        call chntoi(i,aj,jwell)
        
        do ns=1,nsec
          name=namenp(ns)
          open(2,file=name(1:lnamenp(ns))//'.bnr',form='binary',position='append')
          open(3,file='welspecs'//name(1:lnamenp(ns))//'.txt',position='append')
          
          if(imin(ns).le.iwell.and.iwell.le.imax(ns)) then
            if(jmin(ns).le.jwell.and.jwell.le.jmax(ns)) then
! Перехожу в систему координат секторной модели:
              nwellsec(ns)=nwellsec(ns)+1
              iwellnew=iwell-imin(ns)+1
              jwellnew=jwell-jmin(ns)+1
!              lgroup=lgroup+5
!              namegroupnew=namegroupold(1:lgroup)//'_MAIN'
              lgroup=4
              namegroupnew(1:4)='MAIN'
              if(imi(ns).le.iwell.and.iwell.le.ima(ns)) then
                if(jmi(ns).le.jwell.and.jwell.le.jma(ns)) then
!                  namegroupnew=namegroupold(1:lgroup)//'_BOND'
                   namegroupnew(1:4)='BOND'
                endif
              endif
              write(2)lwell,namewell(1:lwell)
              write(3,103)namewell," '",namegroupnew,"' ",iwellnew,' ',jwellnew,a80(1:80)
            endif
          endif
          
          close(2)
          close(3)
        enddo
        goto 10
      endif
      goto 8
      
9     continue
      close(1)
      
 100  format(a128)      
 101  format(a40)
 103  format(a12,a2,a4,a2,i5,a1,i5,a80) 
      return
      end
    
     