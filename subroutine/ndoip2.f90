      subroutine ndoip2(ni,nj,nk,npl,ro,b0,nvar,ntime,ntstep,nt)                                                                                                     
! Расчет плотности текущих геологических запасов, средневзвешенных по времени дренирования слоев модели 
      character*3 ant,cha                                                                                            
      real*4 a(248),b(248),c(248),d(248)                                                                                    
      integer*4 nt                                                                                                   
      integer*2 npl(166),ntstep(100)                                                                                  
                                                                                                                     
      m=ni*4                                                                                                         
! длина записи в файле прямого доступа                                                                               

!      do ii=1,ntime                  
!        if(ntstep(ii).eq.nt) then    
                                     
!          WRITE(CHA,121) nt          
!          READ(CHA,122) ant          

          open(1,file='porv.##',form='unformatted',access='direct',recl=m)                                               
! porv - Поровый объем                                                                                               
          open(2,file='soili.##',form='unformatted',access='direct',recl=m)                                              
! soil0 - Текущая нефтенасыщенность                                                                                  
          open(3,file='roi.##',form='unformatted',access='direct',recl=m)                                               
! roip.## - объем нефти в ячейках в пластовых условиях = произведению объема*нефтенасыщенность                
          open(4,file='drti.##',form='unformatted',access='direct',recl=m)
          open(5,file='sdrt.##',form='unformatted',access='direct',recl=m)                                               
                                                                                                                     
          do j=1,nj                                                                                                      
            do i=1,ni                                                                                                    
              c(i)=0.
              d(i)=0.                                                                                                    
            enddo                                                                                                        
            write(3,rec=j) (c(i),i=1,ni)
            write(5,rec=j) (d(i),i=1,ni)
            do k=1,nk                                                                                                    
              n=j+(k-1)*nj                                                                                               
              if(npl(k).eq.1) then                                                                                       
                read(1,rec=n) (a(i),i=1,ni)                                                                        
                read(2,rec=n) (b(i),i=1,ni)                                                                        
                do i=1,ni                                                                                                
                  b(i)=a(i)*b(i)                                                                                         
                enddo                                                                                                    
                read(4,rec=n) (a(i),i=1,ni)                                                                        
                do i=1,ni
                  a(i)=a(i)/365.
! Перевод в годы дренирования
                  if(a(i).ge.0.) then
                    if(a(i).le.1.) then                                                                                                
                      c(i)=c(i)+b(i)/1.
                      d(i)=d(i)+1. 
                    else
                      c(i)=c(i)+b(i)/a(i)
                      d(i)=d(i)+1./a(i) 
                    endif
                  endif                                                                                        
                enddo                                                                                                    
                                                                                                                         
              endif                                                                                                      
            enddo                                                                                                        
            write(3,rec=j) (c(i),i=1,ni)                                                                                 
! Объем нефти в i-ячейках в пластовых условиях / DRTi ,м3/сут
            write(5,rec=j) (d(i),i=1,ni)                                                                 
          enddo                                                                                                          
                                                                                                                     
  7       continue                                                                                                       
          close(5)
          close(4)                                                                                                       
          close(3)                                                                                                       
          close(2)                                                                                                       
          close(1)                                                                                                       
        
          open(1,file='s.##',form='unformatted')                                                                         
! s.## - Площадь ячеек, метры                                                                                        
          open(2,file='roi.##',form='unformatted',access='direct',recl=m)                                               
          open(4,file='ndopi1.inc')                                                                                      
! ndopi1.inc - Плотность распределения текущих геологических запасов в выбранных k-слоях модели, норимированнная на DRT        
          open(5,file='sdrt.##',form='unformatted',access='direct',recl=m)                                                                                                                     
        
          do j=1,nj                                                                                                      
            read(1,end=8) (a(i),i=1,ni)                                                                                  
            read(2,rec=j) (b(i),i=1,ni)                                                                            
            read(5,rec=j) (d(i),i=1,ni)                                                                                                                     
            do i=1,ni                                                                                                    
              if(a(i).ne.0..and.d(i).ne.0.) then                                                                                        
                a(i)=b(i)/a(i)*ro/b0/d(i)
              else
                a(i)=0.                                                                                      
              endif                                                                                                      
            enddo                                                                                                        
            write(4,4) (a(i),i=1,ni)                                                                                     
          enddo                                                                                                          
  8       continue                                                                                                       
                                                                                                                         
          close(5)
          close(4)                                                                                                       
          close(2)                                                                                                       
          close(1)                                                                                                       

          open(1,file='ndopi1.inc')                                                                                  
! ndopi1.inc - Плотность распределения текущих геологических запасов в выбранных k-слоях модели,нормированная на DRT 
          open(2,file='x.##',form='unformatted')                                                                     
! x.## - Х-координаты центров ячеек                                                                                  
          open(3,file='y.##',form='unformatted')                                                                     
! y.## - Y-координаты центров ячеек                                                                                  

            if(nvar.eq.1) open(5,file=  'a-d/ndrt.txt')
            if(nvar.eq.2) open(5,file='a/ndrt.txt')
            if(nvar.eq.3) open(5,file='b/ndrt.txt')
            if(nvar.eq.4) open(5,file='v/ndrt.txt')
            if(nvar.eq.5) open(5,file='d/ndrt.txt')
                                                       
! ndrt*.txt - Плотность распределения текущих запасов в выбранных k-слоях модели,нормированная на DRT                                                                                                                     
          do j=1,nj                                                                                                  
            read(1,4,end=9) (a(i),i=1,ni)                                                                            
            read(2,end=9) (b(i),i=1,ni)                                                                              
            read(3,end=9) (c(i),i=1,ni)                                                                              
                                                                                                                     
            do i=1,ni                                                                                                
              if(j.eq.1.and.i.eq.1) write(5,4) b(i),-c(i),999999.    
              if(j.eq.nj.and.i.eq.ni) write(5,4) b(i),-c(i),999999.  
              if(a(i).gt.0.) write(5,4) b(i),-c(i),a(i)     
            enddo                                                                                                    
          enddo                                                                                                      
                                                                                                                     
  9       continue                                                                                                   
                                                                                                                     
          close(5)                                                                                                   
          close(3)                                                                                                   
          close(2)                                                                                                   
          close(1)                                                                                                   
          goto 10                                                                                                    
                                                                                                                     
!        endif                                                                                                        
!      enddo                                                                                                          
                                                                                                                     
 10   continue                                                                                                       
 1    format(e13.6,e14.6)                                                                                            
 2    format(a11,i4)                                                                                                 
 4    format(400e17.7)                                                                                               
 121  format(I3)                                                                                                     
 122  format(A3)
      return                                                                                                         
      end                                                                                                            
