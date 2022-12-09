    subroutine RdRecyrentHead(ni,nj,nk,nstep)
! Чтение информации о модели
!   15    5    5    1
!02111990
    integer*4 ni,nj,nk
    integer*2 nstep
    
    open(1,file='RECURENT.hdr',err=100)
    read(1,1,end=100) nstep,ni,nj,nk
    close(1)
1   format(4i5)          
    return

100 print *,'ERROR! RdRecyrentHead: Ошибка чтения файла RECURENT.hdr'     
    stop
    end
! =================    
    
