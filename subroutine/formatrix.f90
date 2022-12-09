    subroutine formatrix(ndate,nstrok,matrix,namedata,begindates)
! Формирование matrix(nstrok=8):
!1 WELLCOMP
!2 COMPDAT
!3 WCONPROD
!4 WCONINJ
!5 WCONINJE
!6 WCONINJH
!7 WCONHIST
!8 WEFAC
!
! namedata - массив ddmmyyyy
! dates - массив хранит номера строк файла full.sh#: 1-строка с "DATE", 2-строка "/" или "W"
! matrix(ndate,nstrok,1) номер строки файла full.sh#:, с которой начинается блок nstrok=1-9 для ndate, включая ключевое слово 
! matrix(ndate,nstrok,2) номер строки файла full.sh#:, с которой начинается блок nstrok=1-9 для ndate, включая '/'

    character*128 a200
    character*8   namedata(1)
    integer*4     ndate
    integer*4     matrix(1:ndate,1:nstrok,1:2)
    integer*4     begindates(1:ndate,1:2)

      do i=1,ndate
        do j=1,nstrok
          do k=1,2  
            matrix(i,j,k)=0
          enddo  
        enddo
      enddo
  
! i - номер строки файла full.sc#
    i=0
! nd - номер "DATE" в файле full.sc#
    nd=0
    open(1,file='full.sc#',access='direct',form='formatted',recl=128)
 1  i=i+1
    read(1,100,rec=i,err=2) a200
    if(a200(1:4).ne.'DATE') goto 1
 3  nd=nd+1
! Фиксирую номер строки с началом "DATE":
    begindates(nd,1)=i
    i=i+1 
    read(1,100,rec=i,err=2) a200

! Выделяю дата "ddmmyyyy" из строки a200:
    call rddat(a200,namedata(nd))
    
 4  i=i+1 
    read(1,100,rec=i,err=2) a200
    if(a200(1:1).eq.'/') goto 5
    if(a200(1:1).eq.'W'.or.a200(1:1).eq.'C') then
      begindates(nd,2)=i-1
      goto 7  
    else
      goto 4
    endif 
! Фиксирую номер строки с концом "DATE":
 5  begindates(nd,2)=i
 6  i=i+1 
    read(1,100,rec=i,err=2) a200
    if(a200(1:4).eq.'DATE') goto 3
 7  continue
    if(a200(1:8).eq.'WELLCOMP') then
      ns=1
      goto 8
    endif
    if(a200(1:7).eq.'COMPDAT') then
      ns=2
      goto 8
    endif
    if(a200(1:8).eq.'WCONPROD') then
      ns=3
      goto 8
    endif
    if(a200(1:7).eq.'WCONINJ') then
      ns=4
      goto 8
    endif
    if(a200(1:8).eq.'WCONINJE') then
      ns=5
      goto 8
    endif
    if(a200(1:8).eq.'WCONINJH') then
      ns=6
      goto 8
    endif
    if(a200(1:8).eq.'WCONHIST') then
      ns=7
      goto 8
    endif
    if(a200(1:5).eq.'WEFAC') then
      ns=8
      goto 8
    endif
    goto 6 
8   continue
    matrix(nd,ns,1)=i
    
 9  i=i+1
    read(1,100,rec=i,err=2) a200
    if(a200(1:1).eq.'/') then
      matrix(nd,ns,2)=i
      goto 6
    endif
    goto 9

 2  continue
    close(1)    
100 format(a128)    
    return
    end
      
    