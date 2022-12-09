    subroutine RdDates(nstep,dates) 
! Чтение временных шагов
!   15    5    5    1
!02111990
!01121990
!01011991
    character*8 dates(1)
    integer*2 nstep
    
    open(1,file='RECURENT.hdr',err=100)
    read(1,2,end=100) 
    do i=1,nstep
      read(1,2,end=101) dates(i)       
    enddo   
    close(1)
2   format(a8)
    return
100 print *,'ERROR! пп RdDates: Ошибка чтения файла RECURENT.hdr'   
    stop
101 print *,'ERROR! пп RdDates: количество записей в файле RECURENT.hdr < nstep'   
    stop
    end
