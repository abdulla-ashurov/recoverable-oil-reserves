    subroutine RdOptionMaps2(roils,roilr,kmin,kmax,namepl,lnamepl,namefile,lnamefl,datemap,vcrit,scrit,ikoxvp,irotate,nfluids,idensity,iporv,teta,fi,critzap)
!          1 - i min
!         48 - i max    
!         95 - j min                                                                
!        164 - j max
!   0.010000 - критическая скорость фильтрации, m3/сут
!   0.000001 - критическая насыщенность: предельная разность SOIL-SOWCR для дренируемых/недренируемых ячеек
!          0 - 0/1. 1 - включить режим расчета Кохвата только потоками к добывающим скважинам (не учитывать непродуктивные потоки от нагнетания).         
!   01012013 - дата построения карты (DDMMYYYY) - как в файле SCHEDULE
!<CASE_1.sch> - файл SCHEDULE (не более 100 символов)
!          0 - 0/1 - переворот карты вдоль оси Y: Y/-Y
!          2 - 2/3 - двух/трехфазная фильтрация (нефть-вода или нефть-вода-газ)
!          1 - 1/2 - расчет приближенный(по средней плотности нефти в пл.условиях)/уточненный(по кубу плотности нефти в пл.условиях)  
!          1 - Количество карт
!<Ю1>        - имя пласта 1 (не болee 11 символов, в т.ч.<>) 
!          1 - kmin1 карты для пласта 1
!         36 - kmax1 карты для пласта 1
!      801.0 - плотность нефти для слоев kmin1-kmax1 в ст.условиях, кг/m3
!      801.0 - плотность нефти для слоев kmin1-kmax1 в пл.условиях, кг/m3

    character*100 namefile,namef
    character*11  daterab,name
    character*10  namepl(1)
    character*8   datemap
    real*4        roils(1),roilr(1)
    integer*2     kmin(1),kmax(1),lnamepl(1)
    integer*2     lnamefl
    
    !print *,'RdOptionMaps2 1'
    !print *,'file=OptionMaps'
    open(1,file='OptionMaps')
    !print *,'RdOptionMaps2 1-1'
    read(1,1,end=100) imin
    !print *,'RdOptionMaps2 1-2'
    read(1,1,end=100) imax
     !print *,'RdOptionMaps2 1-3'
   
    read(1,1,end=100) jmin
    !print *,'RdOptionMaps2 1-4'
    
    read(1,1,end=100) jmax
    !print *,'RdOptionMaps2 1-5'
    
    read(1,7,end=100) vcrit
      !print *,'RdOptionMaps2 1-6'
  
    read(1,7,end=100) scrit
    !print *,'RdOptionMaps2 1-7'
    
    read(1,1,end=100) ikoxvp
     !print *,'RdOptionMaps2 1-8'
   
    read(1,6,end=100) daterab
    !print *,'RdOptionMaps2 1-9'
    
    datemap(1:8)=daterab(4:11)
      !print *,'RdOptionMaps2 1-10'
  
    read(1,5,end=100) namef
    !print *,'RdOptionMaps2 2 namef=',namef
    k=0
    do j=1,100
      if(namef(j:j).eq.'<') then
        k=1
        j1=j+1
      endif  
      if(namef(j:j).eq.'>') then
        j2=j-1
        goto 9
      endif  
    enddo
9   lnamefl=j2-j1+1
    !print *,'RdOptionMaps2 lnamefl=',lnamefl
    namefile=namef(j1:j2)
    !print *,'RdOptionMaps2 3 namefile=',namefile(1:lnamefl)

!  irotate= 0 - 0/1 - переворот карты вдоль оси Y: Y/-Y
!  nfluids= 2 - 2/3 - 2 - двух, 3-трехфазная фильтрация (нефть-вода или нефть-вода-газ)
!  idensity=1 - 1/2 - 1- расчет приближенный(по средней плотности нефти в пл.условиях), 2-уточненный(по кубу плотности нефти в пл.условиях)  
!  iporv   =1 - 1/2 - 1- расчет vporv=s*hср; 2- чтение vporv из файла *.INIT ECLIPSE
    read(1,1,end=100) irotate
    read(1,1,end=100) nfluids
    read(1,1,end=100) idensity
    read(1,1,end=100) iporv
    !print *,'RdOptionMaps2 4'
    
    read(1,1,end=100) nmaps
    !print *,'RdOptionMaps2 5 nmaps=',nmaps
      do i=1,nmaps
    !print *,'RdOptionMaps2 6 i=',i
        namepl(i)='          '
        read(1,4,end=100) name
        k=0
        do j=1,11
          if(name(j:j).eq.'<') then
            k=1
            j1=j+1
          endif  
          if(name(j:j).eq.'>') then
            j2=j-1
            goto 10
          endif  
        enddo
10      lnamepl(i)=j2-j1+1
        namepl(i)=name(j1:j2)
    !print *,'RdOptionMaps2 7 namepl=',namepl(i)
        read(1,1,end=100) kmin(i)
        read(1,1,end=100) kmax(i)
        read(1,2,end=100) roils(i)
        read(1,3,end=100) roilr(i)
    !print *,'RdOptionMaps2 8 kmin(i)=',kmin(i)
    !print *,'RdOptionMaps2 9 kmax(i)=',kmax(i)
    !print *,'RdOptionMaps2 10 roils(i)=',roils(i)
    !print *,'RdOptionMaps2 11 roilr(i)=',roilr(i)
      enddo
! Параметр тета - сдвиг нелинейности влево-вправо
      teta=0.
      fi=0.
      read(1,3,end=100) teta
      read(1,7,end=100) fi
      read(1,7,end=100) critzap
! critzap - критические запасы - параметр для "чистки" краеваых зон карт (игнорирую ячейки с малыми запасами) 
    close(1)
    !print *,'RdOptionMaps2 12'
    
1   format(i11)
2   format(f11.1)
3   format(f11.1)
4   format(a11)
5   format(a100)
6   format(a11)
7   format(f11.6)    
    return
100 continue
    print *,'ERROR! RdOptionMaps2: Ошибка чтения файла optionMaps'
    stop
    end