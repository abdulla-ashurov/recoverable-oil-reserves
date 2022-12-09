      subroutine mestom(mes,m)                                                                                              
! ѕреобразую мес€ц 'mes' в число 'm':                                                                                       
      character*3 mes                                                                                                       
      integer*2   m                                                                                                         
! ƒекодирую мес€ц:                                                                                                          
      if(mes.eq.'JAN'.or.mes.eq.'jan'.or.mes.eq.'Jan') m=1                                                                  
      if(mes.eq.'FEB'.or.mes.eq.'feb'.or.mes.eq.'Feb') m=2                                                                  
      if(mes.eq.'MAR'.or.mes.eq.'mar'.or.mes.eq.'Mar') m=3                                                                  
      if(mes.eq.'APR'.or.mes.eq.'apr'.or.mes.eq.'Apr') m=4                                                                  
      if(mes.eq.'MAY'.or.mes.eq.'may'.or.mes.eq.'May') m=5                                                                  
      if(mes.eq.'JUN'.or.mes.eq.'jun'.or.mes.eq.'Jun') m=6                                                                  
      if(mes.eq.'JUL'.or.mes.eq.'jul'.or.mes.eq.'Jul') m=7                                                                  
      if(mes.eq.'JLY'.or.mes.eq.'jly'.or.mes.eq.'Jly') m=7                                                                  
      if(mes.eq.'AUG'.or.mes.eq.'aug'.or.mes.eq.'Aug') m=8                                                                  
      if(mes.eq.'SEP'.or.mes.eq.'sep'.or.mes.eq.'Sep') m=9                                                                  
      if(mes.eq.'OCT'.or.mes.eq.'oct'.or.mes.eq.'Oct') m=10                                                                 
      if(mes.eq.'NOV'.or.mes.eq.'nov'.or.mes.eq.'Nov') m=11                                                                 
      if(mes.eq.'DEC'.or.mes.eq.'dec'.or.mes.eq.'Dec') m=12                                                                 
      return                                                                                                                
      end                                                                                                                   
