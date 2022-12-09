       subroutine shablon(i,j,k,idren,oe,ni,nj,nk,soil,sowcr,velos,vcrit,scrit)
! Проверка 3-D шаблона - 7 блоков, где (i,j,k) - центральный блок 
      integer*2 idren(1)
      integer*4 oe(1)
      real*4   soil(ni,nj,nk),sowcr(ni,nj,nk),velos(ni,nj,nk)
! Левый блок:
      i1=i-1
      if(i1.lt.1) goto 1
      call proverka(i1,j,k,oe,idren,soil,sowcr,velos,vcrit,scrit,ni,nj,nk)
1     continue
! Правый блок:
      i1=i+1
      if(i1.gt.ni) goto 2
      call proverka(i1,j,k,oe,idren,soil,sowcr,velos,vcrit,scrit,ni,nj,nk)
2     continue
! Задний блок:
      j1=j-1
      if(j1.lt.1) goto 3
      call proverka(i,j1,k,oe,idren,soil,sowcr,velos,vcrit,scrit,ni,nj,nk)
3     continue
! Передний блок:
      j1=j+1
      if(j1.gt.nj) goto 4
      call proverka(i,j1,k,oe,idren,soil,sowcr,velos,vcrit,scrit,ni,nj,nk)
4     continue
! Верхний блок:
      k1=k-1
      if(k1.lt.1) goto 5
      call proverka(i,j,k1,oe,idren,soil,sowcr,velos,vcrit,scrit,ni,nj,nk)
5     continue
! Нижний блок:i
      k1=k+1
      if(k1.gt.nk) goto 6
      call proverka(i,j,k1,oe,idren,soil,sowcr,velos,vcrit,scrit,ni,nj,nk)
6     continue
      n=(k-1)*ni*nj+(j-1)*ni+i
      m=oe(n)+1
! +1, тк в РН-КИМ счет начинается с 0       
      if(m.ge.1) idren(n)=2
      return
100   format(a13,3i3,a3,i4,a10,i2)       
      end
