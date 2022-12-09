     subroutine roip(ni,nj,nk,k1,k2,ro,bo,porv,soil,sowcr,xcel,ycel,scel,imin,imax,jmin,jmax,xmin,xmax,ymin,ymax,oiipgeo,oiipmob,izap,nt,idren,ikoxvp,oe)
! Расчет геологических запасов и плотности распределения геологических запасов
! Расчет подвижных запасов и плотности распределения подвижных запасов   
! OIIPGEO - геологические запасы,rm3
! OIIPMOB - подвижные запасы,rm3
      real*4 porv(ni,nj,nk),soil(ni,nj,nk),sowcr(ni,nj,nk)
      real*4 xcel(ni,nj),ycel(ni,nj),scel(ni,nj)
      integer*4 oe(1)  
      integer*4 ni,nj,nk
      integer*2 k1,k2,imin,imax,jmin,jmax,izap,nt
      integer*2 idren(1)
      
      real*4,allocatable:: oipmob(:,:),oipgeo(:,:)
                                                                                                                                             
! porv  - поровый объем, м3                                                                                                                       
! soil0 - нефтенасыщенность                                                                                                        
! roip  - объем нефти в ячейках в пластовых условиях = произведению объема*нефтенасыщенность,м3

      ni1=imax-imin+1
      nj1=jmax-jmin+1
      allocate(oipmob(1:ni1,1:nj1),oipgeo(1:ni1,1:nj1),stat=ierr)

      do i=1,ni1                                                                                                                              
        do j=1,nj1                                                                                                                            
          oipmob(i,j)=0.
          oipgeo(i,j)=0.
        enddo
      enddo  

! oiip - геологические запасы нефти,rm3
      oiipgeo0=0.
! oiip1- подвижные запасы в выбранных k-слоях модели, м3
      oiipmob0=0.
 
      do k=k1,k2
        do i=1,ni1
          i1=i+imin-1
          do j=1,nj1
            j1=j+jmin-1
            a=porv(i1,j1,k)*soil(i1,j1,k)
! oipgeo0 - геологические запасы нефти, rm3:
            oipgeo(i,j)=oipgeo(i,j)+a
            oiipgeo=oiipgeo+a
            b=porv(i1,j1,k)*(soil(i1,j1,k)-sowcr(i1,j1,k))
! Считаю запасы только для блоков, дренируемых добывающими скважинами
            if(koxvp.eq.1) then
              m=(k-1)*ni*nj+(j1-1)*ni+i1
              mm=oe(m)
              if(mm.gt.0) then
                if(idren(mm).eq.0) goto 2
              else
                goto 2   
              endif
            endif  
            if(b.le.0.)b=0.
! oipmob0 - подвижные запасы нефти, rm3: 
            oipmob(i,j)=oipmob(i,j)+b
            oiipmob=oiipmob+b
2           continue             
          enddo    
        enddo 
      enddo
      
      if(izap.eq.1) then
        open(1,file='oipmobi.txt')   
        open(2,file='oipgeoi.txt')
        do i=1,ni1                                                                                                                              
          i1=i+imin-1
          do j=1,nj1                                                                                                                            
            j1=j+jmin-1
            if(scel(i1,j1).gt.0.) then
              a=ro/(bo*100.*scel(i1,j1))
             else
              a=0.
            endif  
! roipmob0 - Плотность распределения подвижных запасов нефти, тыс.т/га:
            roipmob=oipmob(i,j)*a   
            write(1,1) xcel(i1,j1),ycel(i1,j1),roipmob
! roipgeo0 - Плотность распределения геологических запасов нефти, тыс.т/га:
            roipgeo=oipgeo(i,j)*a
            write(2,1) xcel(i1,j1),ycel(i1,j1),roipgeo
          enddo                                                                                                                                
        enddo                                                                                                                                  
        write(1,1) xmin,ymin
        write(1,1) xmax,ymax
        write(2,1) xmin,ymin
        write(2,1) xmax,ymax
        close(2)
        close(1)
      endif 
      deallocate(oipmob,oipgeo,stat=ierr)

1     format(3e17.7)                                                                                                                       
      return                                                                                                                                 
      end
