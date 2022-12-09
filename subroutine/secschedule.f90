     subroutine secschedule(path,lpath,name,lname,nwellsec,ndate,nstrok,matrix,namedata,begindates,datestart,imin,jmin)

     logical(4) result
     character*400 path
     character*128 a128
     character*89  a89
     character*80  a80
     character*40  a41 
     character*12  well,wel
     character*10  name
     character*8   namedata(1),datestart
     character*6   ai,aj
     character*4   namegroup,namegod
     character*3   mes
     character*2   a21,a22
     character*1   a1
     integer*4     ndate
     integer*4     begindates(ndate,2),matrix(ndate,nstrok,2)
     integer*2     imin,jmin
     integer*2     lpath,lname,nwellsec
     integer*2     i,iwell,jwell,ia21
     
     character*12,allocatable::namewell(:)
     integer*2,allocatable   ::lnamewell(:)
     
! name   - имя создаваемой секторной модели 
! ndate   - количество DATE в SCHEDULE полномасштабной модели
! matrix(ndate,nstrok,1/2)- номера строк файла с началами/концами блоков "nstrok" 
! imin,imax,jmin,jmax - размеры секторной модели наружные - группа скважин BOND+MAIN
! datestart - дата START: ddmmyyyy 

! Размеры секторной модели
      nis=imax-imin+1
      njs=jmax-jmin+1
      
      allocate(namewell(1:nwellsec),lnamewell(1:nwellsec))
      
      open(2,file=name(1:lname)//'.bnr',form='binary')
      do iw=1,nwellsec
        well(1:12)='            '
        read(2)lnamewell(iw),well(1:lnamewell(iw))
        namewell(iw)=well
      enddo
      close(2)
      
      open(1,file='full.sc#',access='direct',form='formatted',recl=128)
! Формирование SCHEDULE:           
      open(2,file=path(1:lpath)//'\'//name(1:lname)//'.sch')
! Пишу старт:
! START
! 01.07.2010       
! /
      write(2,201)'START   '
      a21=datestart(1:2)
      a22=datestart(3:4)
      call ch2toi2(a22,ia21)
      call mtomes(ia21,mes)
      namegod=datestart(5:8)
      write(2,203) a21//' '//mes//' '//namegod
      write(2,202)'/'
     
      do nd=1,ndate
        ir1=begindates(nd,1)
        ir2=begindates(nd,2)
        do ir=ir1,ir2
          read(1,200,rec=ir,err=5)a128
          write(2,200)a128
        enddo
        
        if(nd.eq.1)then
          if(nwellsec.ne.0) then
            write(2,201)'WELSPECS'
            open(3,file='welspecs'//name(1:lname)//'.txt')
  1         read(3,103,end=2)well,a21,namegroup,a22,iwell,a1,jwell,a80(1:80)
            write(2,103)     well,a21,namegroup,a22,iwell,a1,jwell,a80(1:80)
            goto 1
  2         continue        
            close(3)       
            write(2,202)'/'
          endif    
        endif    
        
        if(nwellsec.eq.0) goto 7
        
        iwrite=0
        do is=1,nstrok
          ir1=matrix(nd,is,1)
          ir2=matrix(nd,is,2)
          if(ir1.eq.0) goto 6
          do ir=ir1,ir2
            read(1,200,rec=ir,err=5)a128
!Определяю номер скважины в строке a128:            
!  is1-счетчик символов, отличных от "'"  
            is1=0
            do i=1,40
              if(a128(i:i).ne."'") then
                is1=is1+1
                a41(is1:is1)=a128(i:i)
              endif
              if(a128(i:i).eq.' ') goto 11
             enddo
11           continue              
!  В строке a41 определяю положение 1-го пробела:
             well(1:12)='           '
             do i=1,40
               if(a41(i:i).eq.' ') then
                 lwell=i-1
                 well(1:lwell)=a41(1:lwell)
                 goto 12
               endif
             enddo
12           continue
! namewell()-имя скважины
 
            do iw=1,nwellsec
              if(lwell.eq.lnamewell(iw))then
                wel=namewell(iw)
                if(well(1:lwell).eq.wel(1:lwell))then
                  iwrite=iwrite+1
                  if(iwrite.eq.1)then
                    if(is.eq.1) write(2,201)'WELLCOMP'        
                    if(is.eq.2) write(2,201)'COMPDAT '        
                    if(is.eq.3) write(2,201)'WCONPROD'        
                    if(is.eq.4) write(2,201)'WCONINJ '        
                    if(is.eq.5) write(2,201)'WCONINJE'        
                    if(is.eq.6) write(2,201)'WCONINJH'        
                    if(is.eq.7) write(2,201)'WCONHIST'        
                    if(is.eq.8) write(2,201)'WEFAC   '                       
                  endif
                  if(is.eq.2) then
! COMPDAT:
! Преобразую строку            
!'1540' 7 37 1 1 .../
! в 
!1540 7 37 1 1 .../
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
                      if(is2.eq.3) then
                        a89(1:89)=a128(i:i+88)
                        goto 13
                      endif  
                    enddo
13                  continue              
! В строке a41 определяю положения первых 3-х пробелов:
! is2-счетчик пробелов
                    is2=0
                    do i=1,40
                      if(a41(i:i).eq.' ') then
                        is2=is2+1
                        if(is2.eq.1) ip1=i
                        if(is2.eq.2) ip2=i
                        if(is2.eq.3)then
                          ip3=i
                          goto 14
                        endif  
                      endif
                    enddo
14                  continue
       
! iwell-i-координата скважины:
                    i=ip2-ip1-1
                    ai(1:i)=a41(ip1+1:ip2-1)
                    call chntoi(i,ai,iwell) 
! jwell-j-координата скважины:            
                    i=ip3-ip2-1
                    aj(1:i)=a41(ip2+1:ip3-1)
                    call chntoi(i,aj,jwell)
! Перехожу в систему координат секторной модели:
                    iwellnew=iwell-imin+1
                    jwellnew=jwell-jmin+1
                    
                    write(2,204)well,' ',iwellnew,' ',jwellnew,a89          
                  else
                    write(2,200)a200
                  endif
                endif
              endif
              if(iwrite.gt.0) write(2,202)'/'
            enddo
          enddo
6         continue          
        enddo
7       continue        
      enddo
5     continue
      
      close(2)
      close(1)
103   format(a12,a2,a4,a2,i5,a1,i5,a80) 
200   format(a128)      
201   format(a8)
202   format(a1)
203   format(a11)
204   format(a12,a1,i5,a1,a5,a89)
      deallocate(namewell,lnamewell)
      return
      end
