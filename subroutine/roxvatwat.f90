      subroutine roxvatwat(vcrit,velosity,swat,swcr,oxvwat)
      
      if(vctit.gt.0.) then
        a=(velosity-vcrit)/(100*vcrit-vcrit)
      else
       a=(velosity-0.0001)/(0.01-0.0001)
      endif
      if(a.lt.0.)a=0.
      if(a.gt.1.)a=1.
      
      if(swcr.eq.1.) then
        b=0.
      else
        b=(swat-swcr)/(1.-swcr)
      endif
      if(b.lt.0.)b=0.
      if(b.gt.1.)b=1.
      
      oxvwat=a*b
      
      return
      end