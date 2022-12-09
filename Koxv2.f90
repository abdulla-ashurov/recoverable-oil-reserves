    program Koxv2

    character*100 namefile
    character*10  plast
    character*8   datemap,dat
    character*4   year
    character*2   day,mon
    real*4        koxvoiltotal,koxvoilrec,KINrec,KINproject,koxvwat
    integer*4     ni,nj,nk
    integer*2     lnamefl,k1,k2,imin,imax,jmin,jmax,nmaps,nstep,izap,nt,nrec,nimin,njmin,nimax,njmax
    integer*2     aaa(5)
    
    character*10,allocatable:: namepl(:) 
    character*8,allocatable::  dates(:)
    real*4,allocatable::       actnum(:,:,:),soil0(:,:,:),soili(:,:,:),swati(:,:,:),porv(:,:,:),sgasi(:,:,:)
    real*4,allocatable::       sold(:,:,:)
    real*4,allocatable::       sowcr0(:,:,:),sowcri(:,:,:),velosity(:,:,:),oil_den(:,:,:),swcr(:,:,:)
    real*4,allocatable::       roils(:),roilr(:),xcel(:,:),ycel(:,:),scel(:,:)
    integer*4,allocatable::    oe(:)  
    integer*2,allocatable::    kmin(:),kmax(:),lnamepl(:)
    integer*2,allocatable::    is1(:),js1(:),ks1(:),idren(:)

    call RdOptionMaps1(imin,imax,jmin,jmax,nmaps) 
! imin,imax,jmin,jmax - ������� ������� �������� ������� � ������������ ����
! nmaps - ���������� ���� = ���������� ����� �����

    allocate(roils(1:nmaps),roilr(1:nmaps),kmin(1:nmaps),kmax(1:nmaps),namepl(1:nmaps),lnamepl(1:nmaps),stat=ierr)
! kmin(1:nmaps),kmax(1:nmaps) - ������ ����� ��� ���� � �������� �������
! boil(1:nmaps),roil(1:nmaps) - �������� ������� � ��������� ����� ��� ����� �����
! namepl(1:nmaps),lnamepl(1:nmaps) - ��� ������, ����� ����� ������

    print *,'RdOptionMaps2...'
    call RdOptionMaps2(roils,roilr,kmin,kmax,namepl,lnamepl,namefile,lnamefl,datemap,vcrit,scrit,ikoxvp,irotate,nfluids,idensity,iporv,teta,fi,critzap)
!irotate= 0 - 0/1 - ��������� ����� ����� ��� Y: Y/-Y
!nfluids= 2 - 2/3 - ����/���������� ���������� (�����-���� ��� �����-����-���)
!idensity=1 - 1-������ ������������(�� ������� ��������� ����� � ��.��������),2-����������(�� ���� ��������� ����� � ��.��������)
    !print *,'irotate=',irotate
    !print *,'idensity=',idensity
    !print *,'nfluids=',nfluids    
    !print *,'RdOptionMaps2 end'

    print *,'RdRecyrentHead...'
    call RdRecyrentHead(ni,nj,nk,nstep)
    print *,'RdRecyrentHead end'
    if(imax.gt.ni) then
      print *,'ERROR! ������ � ������� ������� ��� ������ ����: imax �.�.<= ni!'
      stop
    endif  
        
    if(jmax.gt.nj) then
      print *,'ERROR! ������ � ������� ������� ��� ������ ����: jmax �.�.<= nj!'
      stop
    endif  

    print *,'KoxvDOS: nstep=',nstep 
    
! ni,nj,nk - ������ ��������� ����� xcel(:,:),ycel(:,:),scel(:,:) 
    allocate(dates(1:nstep),xcel(1:ni,1:nj),ycel(1:ni,1:nj),scel(1:ni,1:nj),stat=ierr)
    allocate(actnum(1:ni,1:nj,1:nk),soil0(1:ni,1:nj,1:nk),soili(1:ni,1:nj,1:nk),swati(1:ni,1:nj,1:nk),SGASi(1:ni,1:nj,1:nk),velosity(1:ni,1:nj,1:nk),stat=ierr)
    allocate(sold(1:ni,1:nj,1:nk),stat=ierr)
    allocate(porv(1:ni,1:nj,1:nk),sowcr0(1:ni,1:nj,1:nk),sowcri(1:ni,1:nj,1:nk),oe(1:ni*nj*nk),oil_den(1:ni,1:nj,1:nk),swcr(1:ni,1:nj,1:nk),stat=ierr)
    if(ikoxvp.eq.1)allocate(is1(1:ni*nj*nk),js1(1:ni*nj*nk),ks1(1:ni*nj*nk),idren(1:ni*nj*nk),stat=ierr) 
    
    print *,'RdDates...'
    call RdDates(nstep,dates) 
! dates(:) - ������ ��� DDMMGGGG
! nstep    - ���������� ��� DDMMGGGG
!    
! ������ ���� �������� ������:
!    call rdporvBOS(ni,nj,nk,porv)
    print *,'RdDates end'
    print *,'rdporvBOS3 ...'
    if(iporv.eq.1) call rdporvBOS3(ni,nj,nk,porv)
    if(iporv.eq.2) call rdZapInit(ni,nj,nk,porv,31)
    print *,'rdporvBOS3 end'
    print *,'sxy3 ...'
! ������ �������� ����� � x,y-��������� ������� �����:
!    call sxy2(ni,nj,xcel,ycel,scel)
! irotate=0/1 ��������� ����� �� ��� Y     
    call sxy3(ni,nj,nk,xcel,ycel,scel,irotate)
    print *,'sxy3 end'
    
! ������ ���� ��������� SWCR:
    print *,'rdZapInit...'
    call rdZapInit(ni,nj,nk,swcr,21)
    print *,'rdZapInit end'
    
    nimin=1
    njmin=1
    nimax=ni
    njmax=nj   
    print *,'minmaxcel ...'
    call minmaxcel(ni,nj,xcel,ycel,xmin,ymin,xmax,ymax,nimin,njmin,nimax,njmax)
    print *,'minmaxcel end'

! ���� ��� ���������� �������� ��������� SOIL:
    open(100,file='dynamic_of_soil')

    do n=1,nmaps
      ros=roils(n)                            
! ros - ��������� ����� � ����������� ��������, kr/m3                      
      ror=roilr(n)
! ror - ��������� ����� � ��������� ��������, ��/m3                           

! k1-k2 - c��� ������ ��� ������� �������:    
      k1=kmin(n)
      k2=kmax(n)
! plast - ��� ����������� �������      
      plast=namepl(n)
! ��������� ��������:      
!      oiipgeo0=0.
!      oiipmob0=0.
!      koxvoil=0.
!      koxvwat=0.

! ������ ��������� ������� � ��������� ����� ������� �� ���� datemap, ������������ ������ wt.txt, wi1.txt, wi0.txt, wp0.txt, wp1.txt :
      print *,'wells ...'
      call wells(namefile,lnamefl,xmin,ymin,xmax,ymax,datemap,k1,k2,imin,imax,jmin,jmax,xcel,ycel,ni,nj,plast,lnamepl(n))
      print *,'wells end'
! ������ ���� ���������� SOIL:
      print *,'rsoili ...'
      call rsoili(ni,nj,nk,soil0,1)
      print *,'rsoili end'
! ������ ���� �������� SGASI ��� ���������� ����������:
      if(nfluids.eq.3) then
        print *,'rsgasi ...'
        call rsgasi(ni,nj,nk,sgasi,1)
        print *,'rsgasi end'
      endif
! ������ ���� ���������� SOWCR:
      print *,'rsowcri ...'
      call rsowcri(ni,nj,nk,sowcr0,1)
      print *,'rsowcri end'
! ������ ���� ��������� ��������� ����� OIL_DEN:
      print *,'roilden ...'
      if(idensity.eq.2) call roilden(ni,nj,nk,oil_den,1)
      print *,'roilden end'
      
      print *,'rdactivcell ...'
      call rdactivcell(ni,nj,nk,oe)
      print *,'rdactivcell end'

! ������ ��������� ������������� ������� � ��������� ��������� ������������� �������:
! ������ ��������� ��������� ������� � ��������� ��������� ��������� �������:
      print *,'roip0 ...'
      call roip0(ni,nj,nk,k1,k2,idensity,ros,ror,oe,porv,oil_den,soil0,sowcr0,sgasi,xcel,ycel,scel,imin,imax,jmin,jmax,xmin,xmax,ymin,ymax,roiipgeo0,roiipmob0,soiipgeo0,soiipmob0,oilgeo0,oilmob0,vporsum,rwiipgeo0,nfluids,plast,lnamepl(n))
      print *,'roip0 end'
! ����� �������:
      print *,'tabl1 ...'
      call tabl1(k1,k2,ros,ror,roiipgeo0,roiipmob0,soiipgeo0,oilgeo0,oilmob0,rwiipgeo0,vporsum,plast,lnamepl(n),imin,imax,jmin,jmax,idensity,nfluids,irotate,teta,fi)
      print *,'tabl1 end'
      
! dat - ���� ��� nt=1          
      nt=1 
      dat=dates(nt)
      day=dat(1:2)
      mon=dat(3:4)
      year=dat(5:8)
! ������ �������:
      qq6t=0.
      qq7r=roiipgeo0
      qq7s=soiipgeo0
      qq7t=oilgeo0/1000.
      qq5t=oilmob0/1000.
      qq4t=oilmob0/1000.
      qq3t=0.
      koxvoiltotal=0.
      koxvoilrec=0.
      koxvwat=0.
      KINrec=0.
      KINproject=0.
      print *,'tabl2 ...'
      call tabl2(idensity,ros,ror,nt,day,mon,year,qq6t,KINrec,qq7r,qq7s,qq7t,qq5t,qq4t,qq3t,koxvoiltotal,koxvoilrec,KINproject,koxvwat,plast,lnamepl(n))
      print *,'tabl2 end'

      do nt=2,nstep
! dat - ���� ��� nt=2,nstep

        dat=dates(nt)
        day=dat(1:2)
        mon=dat(3:4)
        year=dat(5:8)

        izap=0
        if(day.eq.datemap(1:2).and.mon.eq.datemap(3:4).and.year.eq.datemap(5:8)) izap=1

! ������ ���� �������� soil:
        nrec=nt
        print *,'************'
        print *,'rsoili ... nt=',nt,' dat=',dat
! ������ ���� �������� SWAT:
        print *,'rswati ...'

        call rsoili(ni,nj,nk,soili,nrec)
        print *,'rsoili end'

        call rswati(ni,nj,nk,swati,nrec)
        print *,'rswati end'
        if(nfluids.eq.3) then
! ������ ���� �������� SGASI ��� ���������� ����������:
          print *,'rsgasi ...'
          call rsgasi(ni,nj,nk,sgasi,nrec)
          print *,'rsgasi end'
        endif

        if(n.eq.1) then
          if(nt.eq.2) then
            do k=1,nk
              do j=1,nj
                do i=1,ni
                  sold(i,j,k)=sowcr0(i,j,k)
                enddo
              enddo
            enddo
          else
            do k=1,nk
              do j=1,nj
                do i=1,ni
                  sold(i,j,k)=sowcri(i,j,k)
                enddo
              enddo
            enddo
          endif
        endif 

        !if(n.eq.1.and.nt.eq.nstep) then
        !  open(101,file='')
        !    do k=1,nk
        !      do j=1,nj
        !        write(101) (soili(i,j,k),i=1,ni)
        !      enddo
        !    enddo
        !  close(101)          
        !endif
        
        
! ������ ���� �������� SOWCR:
        print *,'rsowcri ...'
        call rsowcri(ni,nj,nk,sowcri,nrec)
        print *,'rsowcri end'

!������ ������� ����� soil �� �������� ��������� �����: 
        !if(n.eq.1) call soilLinNonlin(nt,dat,ni,nj,nk,sold,sowcri)

! ������ ���� ��������� velocity:
        print *,'rvelosity ...'
        call rvelosity(ni,nj,nk,velosity,nrec)
        print *,'rvelosity end'

! ������ ���� ������� ��������� ����� OIL_DEN:
      if(idensity.eq.2) then
        print *,'roilden ...'
        call roilden(ni,nj,nk,oil_den,nrec)
        print *,'roilden end'
      endif
      
      if(ikoxvp.eq.1) then
! ikoxvp=1 - ��������� ������� ������ �����, ����������� ������ �������� � ���������� ���������:
! ������ i,j,k- ��������� ������, � ������� ��������� ���������� ���������� �������� �� ���� dates(np):
        print *,'wells2 ...'
        call wells2(namefile,lnamefl,dat,k1,k2,imin,imax,jmin,jmax,is1,js1,ks1,nz)
        print *,'wells2 end'
        print *,'foridren ...'
        call foridren(ni,nj,nk,idren,oe,soili,sowcri,velosity,vcrit,scrit,is1,js1,ks1,nz,nt)
        print *,'foridren end'
      endif  

! ������ ������� ������������� ������� � ��������� ������� ������������� �������,
! ������� ��������� ������� � ��������� ������� ��������� �������:
! ������ ���� ������������, ������������� ������ OIL/WATER, �����������/������������� ������� OIL/WATER
      print *,'MapZapas ...'
      call MapZapas(ni,nj,nk,k1,k2,idensity,ros,ror,oe,idren,porv,oil_den,soili,sowcr0,sowcri,xcel,ycel,scel,imin,imax,jmin,jmax,xmin,xmax,ymin,ymax,izap,nt,ikoxvp,plast,lnamepl(n))
      print *,'KoefOxvat ...'
      call KoefOxvat(nfluids,idensity,ikoxvp,ni,nj,nk,k1,k2,ros,ror,oil_den,oe,idren,porv,soili,soil0,sgasi,sowcri,sowcr0,swcr,velosity,imin,imax,jmin,jmax,qq6t,KINrec,qq7r,qq7s,qq7t,qq5t,qq4t,qq3t,koxvoiltotal,koxvoilrec,KINproject,koxvwat)
      print *,'MapOxv ... koxvwat=',koxvwat
      call MapOxv(ni,nj,nk,k1,k2,oe,idren,porv,soili,sgasi,sowcr0,sowcri,swcr,velosity,xcel,ycel,scel,imin,imax,jmin,jmax,xmin,xmax,ymin,ymax,izap,nt,ikoxvp,vcrit,scrit,nfluids,plast,lnamepl(n),critzap)
      print *,'MapOxv end'  
! ������������ ����� �������:
      print *,'tabl2 ...'
      call tabl2(idensity,ros,ror,nt,day,mon,year,qq6t,KINrec,qq7r,qq7s,qq7t,qq5t,qq4t,qq3t,koxvoiltotal,koxvoilrec,KINproject,koxvwat,plast,lnamepl(n))
      print *,'tabl2 end'
      enddo
    enddo
    
    close(100)
    
    if(ikoxvp.eq.1)deallocate(is1,js1,ks1,idren,stat=ierr) 
    deallocate(roils,roilr,kmin,kmax,namepl,lnamepl,dates,xcel,ycel,scel,oe,oil_den,stat=ierr)
    deallocate(actnum,soil0,soili,swati,sgasi,porv,sowcr0,sowcri,velosity,swcr,stat=ierr)
    deallocate(sold,stat=ierr)    
    stop
 2  format(a14,38i3)
 3  format(a25,f7.4)   

    end program Koxv2

! =======================
!    subroutine soilLinNonlin(nt,dat,ni,nj,nk,sold,s)
!! ��������� ����� SOIL �������� � ���������� ������� �� �������� ���� �������
!    real*4 s(ni,nj,nk),sold(ni,nj,nk)
!   
!    rab=0.
!    anevazka=0.
!  
!    do k=1,nk
!      do j=1,nj
!        do i=1,ni
!           if(s(i,j,k).gt.0.) then 
!             rab=rab+s(i,j,k)
!             anevazka=anevazka+abs(s(i,j,k)-sold(i,j,k))
!            endif
!        enddo
!      enddo
!    enddo
!    
!    if(rab.eq.0.) then
!      anevazka=0.
!    else
!      anevazka=anevazka/rab
!    endif
!
!    print 100,'nt=',nt,' dat=',dat,' nevazka=',anevazka
!100 format(a3,i3,a5,a8,a9,f9.6)     
!    
!    return
!    end    
!
!
