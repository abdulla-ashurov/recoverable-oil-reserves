     subroutine oip0(ni,nj,nk,npl,ro,socr,b0,oiip,oiip1,nvar,xmin,xmax,ymin,ymax)                                                                                    
! Расчет начальных геологических запасов и плотности распределения начальных геологических запасов, плотности начальных подвижных запасов   
! OIIP - начальные геологические запасы,rm3
! OIIP1- начальные подвижные запасы,rm3

      integer*2 npl(1)                                                                                                                     
      real*4 a(281),b(281),c(281),d(281)                                                                                                            
                                                                                                                                             
      m=ni*4
! длина записи в файле прямого доступа                                                                                                       
                                                                                                                                             
      open(1,file='porv.bn#',form='binary',access='direct',recl=m)                                                                         
! porv - Поровый объем, м3                                                                                                                       
      open(2,file='soili.bn#',form='binary',access='direct',recl=m)                                                                        
! soil0 - Начальная нефтенасыщенность                                                                                                        
      open(3,file='ro1.bn#',form='binary',access='direct',recl=m)                                                                         
! roip.## - объем подвижной нефти в ячейках в пластовых условиях = произведению объема*(нефтенасыщенность-socr),м3                                              
      open(4,file='ro2.bn#',form='binary',access='direct',recl=m)                                             
! roip.### - объем нефти в ячейках в пластовых условиях = произведению объема*нефтенасыщенность,м3 
                                                                                                                                             
      do j=1,nj                                                                                                                              
        do i=1,ni                                                                                                                            
          c(i)=0.
          d(i)=0.                                                                                                                            
        enddo                                                                                                                                
        write(3,rec=j) (c(i),i=1,ni)                                                                                                       
        write(4,rec=j) (d(i),i=1,ni)
        do k=1,nk                                                                                                                            
          n=j+(k-1)*nj                                                                                                                       
          if(npl(k).eq.1) then                                                                                                               
            read(1,rec=n) (a(i),i=1,ni)                                                                                              
            read(2,rec=n) (b(i),i=1,ni)                                                                                              
            do i=1,ni
! Вычисление геологичеких запасов нефти в столбцах ячеек, rm3: 
              d(i)=d(i)+a(i)*b(i)  
! Вычисление начальных подвижных запасов нефти в столбцах ячеек, rm3: 
              b(i)=b(i)-socr             
              if(b(i).lt.0.) b(i)=0.
              c(i)=c(i)+a(i)*b(i)                                                                                                            
            enddo                                                                                                                            
          endif                                                                                                                              
        enddo                                                                                                                                
! Запись начальнх подвижных запасов нефти в столбцах ячеек, rm3: 
        write(3,rec=j) (c(i),i=1,ni)                                                                                                       
! Запись геологичеких запасов нефти в столбцах ячеек, rm3: 
        write(4,rec=j) (d(i),i=1,ni)                                                                                                                                     
      enddo                                                                                                                                  
  7   continue                                                                                                                               
      close(4)
      close(3)                                                                                                                               
      close(2)                                                                                                                               
      close(1)                                                                                                                               

      open(1,file='s.bn#',form='binary')                                                                                                                    
! s.## - Площадь ячеек                                                                                                                       
      open(2,file='ro1.bn#',form='binary',access='direct',recl=m)                                                                                                                 
! ro1.bn# - распределение начальных подвижных запасов в выбранных k-слоях модели,rm3                                                 
      open(3,file='ro2.bn#',form='binary',access='direct',recl=m)                              
! roip.### - распределение начальных геологических запасов нефти в ячейках,rм3   
      open(4,file='moboil0.tx#')                                                                                                               
! roip.txt - распределение начальных подвижных запасов в выбранных k-слоях модели, тыс.т/га                                                 

      nrec=0
      oiip=0.                                                                                                                                
      do j=1,nj                                                                                                                              
        read(1,end=8) (a(i),i=1,ni)
        nrec=nrec+1                                                                                                        
        read(2,rec=nrec) (b(i),i=1,ni)                                                                                                        
        read(3,rec=nrec) (d(i),i=1,ni)
        do i=1,ni                                                                                                                            
          oiip=oiip+d(i)
! Начальные геологические запсы нефти,rm3
!
! Плотность распределения начальных подвижных запасов нефти,кг/m2:                                                                                    
          if(a(i).gt.0.) then                                                                                                                
            a(i)=b(i)/a(i)*ro/b0                                                                                                             
          endif                                                                                                                              
        enddo                                                                                                                                
        write(4,4) (a(i),i=1,ni)                                                                                                             
      enddo                                                                                                                                  
  8   continue                                                                                                                               
      close(4)                                                                                                                               
      close(3)                                                                                                                               
      close(2)                                                                                                                               
      close(1)
      
      oiip1=oiip*(1-socr)                                                                                                                    
! Начальные подвижные запасы в выбранных k-слоях модели, м3

      open(1,file='roip.txt')                                                                                                               
! roip1.inc - распределения начальных подвижных запасов, тыс.т/га
      open(2,file='x.bn#',form='binary')                                                                                                                    
! x.## - Х-координаты центров ячеек                                                                                                          
      open(3,file='y.bn#',form='binary')                                                                                                                    
! y.## - Y-координаты центров ячеек
      open(5,file='moboil0.txt')   
! roip0.txt - Плотность распределения начальных подвижных запасов для SURFER                                                                                                                                             
      do j=1,nj                                                                                                                              
        read(1,4,end=9) (a(i),i=1,ni)                                                                                                        
        read(2,end=9) (b(i),i=1,ni)                                                                                                        
        read(3,end=9) (c(i),i=1,ni)                                                                                                        
        do i=1,ni                                                                                                                            
          write(5,4) b(i),c(i),a(i)/100.                                                                                                          
        enddo                                                                                                                                
      enddo
      write(5,4) xmin,ymin
      write(5,4) xmax,ymax                                                                                                                                  
  9   continue                                                                                                                               
      close(5)                                                                                                                               
      close(3)                                                                                                                               
      close(2)                                                                                                                               
      close(1)                                                                                                                               

 1    format(e13.6,e14.6)                                                                                                                    
 2    format(a11,i4)                                                                                                                         
 4    format(400e17.7)                                                                                                                       
      return                                                                                                                                 
     end
