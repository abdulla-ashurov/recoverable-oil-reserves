    subroutine MKPointControl(pathSECT,lsect,PathCOMMON,lcommon,namenp,lname)
   
    character*400 PathCOMMON,pathSECT
    character*128 a128,b128
    character*40  a41
    character*10  namenp
    character*6   ai
    integer*2     lcommon,lsect,lname
    integer*2     i,iwell,jwell,k1well,k2well
    
    init1=100
    init2=101
    
    open(init1,file=pathSECT(1:lsect)//namenp(1:lname)//'.sch',err=1)
! Нормализация файла *.sch (исключение лишних пробелов) и создание уплотненного файла full.sc# 
    open(init2,file='full.sc#',access='direct',form='formatted',recl=128)
      ir=0
3     read(init1,100,end=2) a128
      if(a128(1:2).eq.'--') goto 3
      if(a128(1:20).eq.'                    ') goto 3
      call uplot(a128,b128)
      ir=ir+1
      write(init2,100,rec=ir) b128
      goto 3
2     continue      
      close(init2)
    close(init1)
100 format(a128)

      open(init2,file=PathCOMMON(1:lcommon)//'PointControlPressure.txt',err=22)
      open(init1,file='full.sc#',access='direct',form='formatted',recl=128)
      ir=0
8     ir=ir+1
      read(init1,100,rec=ir,err=9) a128
      if(a128(1:7).eq.'COMPDAT') then
10      ir=ir+1
        read(init1,100,rec=ir,err=9) a128
        if(a128(1:1).eq.'/') goto 8
! Преобразую строку            
!'16098' 23 54 63 67 'OPEN' 2* 0.16 1* 0 1* 'Z' /
!в строку
!16098 23 54 63 67 
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
          if(is2.eq.5) goto 12
        enddo
12      continue              
! В строке
!16098 23 54 63 67 
! Определяю положения первых 5-х пробелов:
! is2-счетчик пробелов
        is2=0
        do i=1,40
          if(a41(i:i).eq.' ') then
            is2=is2+1
            if(is2.eq.1) ip1=i
            if(is2.eq.2) ip2=i
            if(is2.eq.3) ip3=i
            if(is2.eq.4) ip4=i
            if(is2.eq.5)then
              ip5=i
              goto 11
            endif  
          endif
        enddo
11      continue
        
! iwell-i-координата скважины:
        i=ip2-ip1-1
        ai(1:i)=a41(ip1+1:ip2-1)
        call chntoi(i,ai,iwell) 
! jwell-j-координата скважины:            
        i=ip3-ip2-1
        ai(1:i)=a41(ip2+1:ip3-1)
        call chntoi(i,ai,jwell)
! k1well-k1-координата скважины:            
        i=ip4-ip3-1
        ai(1:i)=a41(ip3+1:ip4-1)
        call chntoi(i,ai,k1well)
! k2well-k2-координата скважины:            
        i=ip5-ip4-1
        ai(1:i)=a41(ip4+1:ip5-1)
        call chntoi(i,ai,k2well)
        
        do k=k1well,k2well
          write(init2,101) iwell,jwell,k
        enddo
        
101     format(3i5)     
        goto 10
      endif
      goto 8
      
9     continue
      close(init1)
      close(init2)
    
    return
    
1   print *,'ERROR! MKPointControl: Ошибка открытия файла ',pathSECT(1:lsect)//namenp(1:lname)//'.sch'
    stop
22  print *,'ERROR! MKPointControl: Ошибка открытия файла ',PathCOMMON(1:lcommon)//'PointControlPressure.txt'
    stop
    end
