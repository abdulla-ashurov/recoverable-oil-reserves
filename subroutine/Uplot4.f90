     subroutine uplot4(a128,b128,lb128,ip1,ip2,ip3,ip4,ip5,ier)   
! ѕереписываю a128 в b128 и оставл€ю только один пробел вместо нескольких подр€д идущих.
! ћежду апострофами пробелы при переписывании в b128 сохран€ютс€
!
! a128 - исходна€ строка
! b128 - строка-результат, в которой подр€д идут не более 1 пробела. ѕробел в начале строки (если он есть) при переписывании исключаетс€
! lb128 - длина строки b128
! с128 - рабоча€ строка
! ip1 - номер позиции строки b128, в которой находитс€ 1-й прообел 
! ip2 - номер позиции строки b128, в которой находитс€ 2-й прообел 
! ip3 - номер позиции строки b128, в которой находитс€ 3-й прообел 
! ip4 - номер позиции строки b128, в которой находитс€ 4-й прообел 
! ip5 - номер позиции строки b128, в которой находитс€ 5-й прообел 
!
!  2116 'GROUP 1' 39 110 1* OIL /   

     character*128 a128,b128,c128
   
     ip1=0
     ip2=0
     ip3=0
     ip4=0
     ip5=0
! ќпредел€ю первый и последний символ, отличный от пробела:
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
! ¬ строке a128 только пробелы:  
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
! iprob1- первый "непробел" в a128 
! iprob2- последний "непробел" в a128
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
! ¬нутри скобок '' замен€ю пробел на  '_':
         if(b128(i:i).eq.' ')b128(i:i)='_' 
       endif
     enddo
     !print *,' uplot4 6 nj=',nj,' b128=',b128(i:nj)

! »сключаю апострофы в с128:
     nk=0
     do i=1,nj     
       if(b128(i:i).ne.'"'.and.b128(i:i).ne."'") then
         nk=nk+1
         c128(nk:nk)=b128(i:i)
       endif
     enddo
     !print *,' uplot4 7 c128=',c128(1:nk)
     
! »сключаю 2 и более подр€д сто€щих пробелов
! ia - счетчик пробелов:
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

! »щу номера пробелов ip1-ip5:
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