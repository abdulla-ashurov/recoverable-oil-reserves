     subroutine uplot4(a128,b128,lb128,ip1,ip2,ip3,ip4,ip5,ier)   
! ����������� a128 � b128 � �������� ������ ���� ������ ������ ���������� ������ ������.
! ����� ����������� ������� ��� ������������� � b128 �����������
!
! a128 - �������� ������
! b128 - ������-���������, � ������� ������ ���� �� ����� 1 �������. ������ � ������ ������ (���� �� ����) ��� ������������� �����������
! lb128 - ����� ������ b128
! �128 - ������� ������
! ip1 - ����� ������� ������ b128, � ������� ��������� 1-� ������� 
! ip2 - ����� ������� ������ b128, � ������� ��������� 2-� ������� 
! ip3 - ����� ������� ������ b128, � ������� ��������� 3-� ������� 
! ip4 - ����� ������� ������ b128, � ������� ��������� 4-� ������� 
! ip5 - ����� ������� ������ b128, � ������� ��������� 5-� ������� 
!
!  2116 'GROUP 1' 39 110 1* OIL /   

     character*128 a128,b128,c128
   
     ip1=0
     ip2=0
     ip3=0
     ip4=0
     ip5=0
! ��������� ������ � ��������� ������, �������� �� �������:
     !print *,' uplot4 1'
     iprob1=0
     !print *,' uplot4 2 a128=',a128(1:128)
     do i=1,128
      if(a128(i:i).ne.' ') then
         iprob1=i
         goto 1
       endif
     enddo
     
     if(iprob1.eq.0) then
! � ������ a128 ������ �������:  
       ier=1
       return
     endif

 1   continue
     !print *,' uplot4 3 iprob1=',iprob1
 
     do i=iprob1,128
       if(a128(i:i).ne.' ') then
         iprob2=i
       endif
     enddo
! iprob1- ������ "��������" � a128 
! iprob2- ��������� "��������" � a128
     !print *,' uplot4 4 iprob2=',iprob2

     nj=0
     do i=iprob1,iprob2
       nj=nj+1
       b128(nj:nj)=a128(i:i)
     enddo
     !print *,' uplot4 5 b128=',b128(1:nj)

     inn=0
     is=0
     do i=1,nj
       if(b128(i:i).eq."'".or.b128(i:i).eq.'"') then
         is=is+1  
         if(is.eq.1) inn=1
         if(is.eq.2) then
           inn=0
           is=0
         endif  
       endif
       
       !print *,' uplot4 51 i=',i,' inn=',inn

       if(inn.eq.1) then
! ������ ������ '' ������� ������ ��  '_':
         if(b128(i:i).eq.' ')b128(i:i)='_' 
       endif
     enddo
     !print *,' uplot4 6 nj=',nj,' b128=',b128(i:nj)

! �������� ��������� � �128:
     nk=0
     do i=1,nj     
       if(b128(i:i).ne.'"'.and.b128(i:i).ne."'") then
         nk=nk+1
         c128(nk:nk)=b128(i:i)
       endif
     enddo
     !print *,' uplot4 7 c128=',c128(1:nk)
     
! �������� 2 � ����� ������ ������� ��������
! ia - ������� ��������:
     ia=0 
     ni=0
     do i=1,nk
       if(c128(i:i).ne.' ') then
         ni=ni+1
         b128(ni:ni)=c128(i:i)
         ia=0
       else
         ia=ia+1
         if(ia.eq.1) then
           ni=ni+1
           b128(ni:ni)=c128(i:i)
         endif
       endif
     enddo
     !print *,' uplot4 8 b128=',b128(1:ni)

! ��� ������ �������� ip1-ip5:
     j=0
     do i=1,ni
       if(b128(i:i).eq.' ') then
         j=j+1
         if(j.eq.1) ip1=i
         if(j.eq.2) ip2=i
         if(j.eq.3) ip3=i
         if(j.eq.4) ip4=i
         if(j.eq.5) ip5=i
       endif
     enddo
     
    !print *,' uplot4 8 ip1=',ip1,' ip2=',ip2,' ip3=',ip3
    !print *,'          np4=',ip4,' ip5=',ip5
     
     do i=1,ni
       if(b128(i:i).eq.'_')b128(i:i)=' '
     enddo
     lb128=ni
     ier=0
     !print *,' uplot4 9'
    
     return
     end 