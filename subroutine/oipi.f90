     subroutine oipi(ni,nj,nk,npl,ro,socr,b0,oiip,oiip1,nvar,ntime,ntstep,nt,xmin,xmax,ymin,ymax)                                                                                                                                                                                    
! Расчет текущих геологических запасов, плотности текущих геологических запасов                         
! Расчет текущих подвижных запасов, плотности текущих подвижных запасов                                  
      character*3 ant,cha
      integer*4 nt       
      integer*2 npl(1),ntstep(1)                                                                                 
      real*4,allocatable:: a(:),b(:),c(:),d(:)  
      
      allocate(a(1:ni),b(1:ni),c(1:ni),d(1:ni),stat=ierr)
                                                                                                         
      m=ni*4
! длина записи в файле прямого доступа                                                                   
                                                                                                         
      open(1,file='porv.bn#',form='binary',access='direct',recl=m)                                     
! porv - Поровый объем                                                                                   
      open(2,file='soili.bn#',form='binary',access='direct',recl=m)                                    
! soil0 - Текущая нефтенасыщенность                                                                      
      open(3,file='ro1.bn#',form='binary',access='direct',recl=m)                                     
! roip.## - объем подвижной нефти в ячейках в пластовых условиях = произведению объема* (нефтенасыщенность-socr)          
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
              d(i)=d(i)+a(i)*b(i)  
! Вычисление подвижной нефтенасыщенности:    
              b(i)=b(i)-socr                 
              if(b(i).lt.0.) b(i)=0.         
              c(i)=c(i)+a(i)*b(i)                                                                        
            enddo                                                                                        
          endif                                                                                          
        enddo                                                                                            
        write(3,rec=j) (c(i),i=1,ni)
        write(4,rec=j) (d(i),i=1,ni)                                                                                                         
      enddo                                                                                              
  7   continue                                                                                           
      close(4)
      close(3)                                                                                           
      close(2)                                                                                           
      close(1)                                                                                           
                                                                                                         
      open(1,file='s.bnr',form='binary')                                                                                
! s.## - Площадь ячеек                                                                                   
      open(2,file='ro1.bn#',form='binary',access='direct',recl=m)                                                                             
      open(3,file='ro2.bn#',form='binary',access='direct',recl=m)                            
! roip.### - объем нефти в ячейках в пластовых условиях = произведению объема*нефтенасыщенность,м3 
      open(4,file='roipi1.txt')                                                                          
! roipi1.inc - Плотность распределения текущих подвижных запасов в выбранных k-слоях модели              
                                                                                                         
      oiip=0.                                                                                            
      do j=1,nj                                                                                          
        read(1,end=8) (a(i),i=1,ni)                                                                    
        read(2,rec=j) (b(i),i=1,ni)                                                                    
        read(3,rec=j) (d(i),i=1,ni)
        do i=1,ni                                                                                        
          oiip=oiip+d(i)
! Текущие объемы нефти в выбранных k-слоях модели, кг:                                                  
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
                                                                                                         
      oiip=oiip*ro/b0                                                                                    
! Текущие геологические запасы в выбранных k-слоях модели, кг                                          
      oiip1=oiip*(1-socr)                                                                                
! Текущие подвижные запасы в выбранных k-слоях модели, кг                                              

!      do ii=1,ntime                   
!        if(ntstep(ii).eq.nt) then     
!          WRITE(CHA,121) nt           
!          READ(CHA,122) ant                                                                                                                    

          open(1,file='roipi1.txt')                                                                           
! roipi1.inc - Плотность распределения текущих подвижных запасов в выбранных k-слоях модели, тыс.т/га
          open(2,file='x.bnr',form='binary')                                                                                
! x.## - Х-координаты центров ячеек                                                                      
          open(3,file='y.bnr',form='binary')                                                                                
! y.## - Y-координаты центров ячеек                                                                      

          open(5,file='roipi.txt')  
!            if(nvar.eq.1) open(5,file='c:\urna\1-35\roipi.txt')  
!            if(nvar.eq.2) open(5,file='c:\urna\36-99\roipi.txt')
!            if(nvar.eq.3) open(5,file='roipi.txt')
   
! roipi.txt - Плотность распределения текущих подвижных запасов в выбранных k-слоях модели для SURFER              
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
  9       continue                                                                                           
          close(5)                                                                                           
          close(3)                                                                                           
          close(2)                                                                                           
          close(1)                                                                                           
!          goto 10 
!        endif     
!      enddo       
! 10   continue    
       deallocate(a,b,c,d,stat=ierr)
                                                                                                        
 1    format(e13.6,e14.6)                                                                                
 2    format(a11,i4)                                                                                     
 4    format(400e17.7)                                                                                   
 121  format(I3)
 122  format(A3)
      return                                                                                             
      end                                                                                                
                                                                                                        
