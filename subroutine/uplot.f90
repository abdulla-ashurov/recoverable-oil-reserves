      subroutine uplot(a128,b128)   
      character*128 a128,b128
      do i=1,128                    
        b128(i:i)=' '               
      enddo                         
! ia - c������ ��������, ������ ������
      ia=0                          
! j  - ������� �������� � ������ b128
      j=0                           
      do i=1,128
! ��������� ���������:            
        if(a128(i:i).eq."'") goto 2
        if(a128(i:i).eq.' ') then
! ��������� ������, ������� � ������ �������:
          if(i.eq.1) goto 2  
          ia=ia+1                   
          if(ia.gt.1) goto 2        
        else                        
          ia=0                      
        endif                       
        j=j+1                       
        b128(j:j)=a128(i:i)         
 2      continue                    
      enddo
      return
      end