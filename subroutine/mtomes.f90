      subroutine mtomes(m,mes)                                                                                              
!c ¬осстанавливаю название мес€ца 'mes' по его номеру 'm':                                                                   
      character*3 mes,charmes(12)                                                                                           
      integer*2   m                                                                                                         
                                                                                                                            
      charmes(1)='JAN'                                                                                                      
      charmes(2)='FEB'                                                                                                      
      charmes(3)='MAR'                                                                                                      
      charmes(4)='APR'                                                                                                      
      charmes(5)='MAY'                                                                                                      
      charmes(6)='JUN'                                                                                                      
      charmes(7)='JUL'
      charmes(8)='AUG'                                                                                                      
      charmes(9)='SEP'                                                                                                      
      charmes(10)='OCT'                                                                                                     
      charmes(11)='NOV'                                                                                                     
      charmes(12)='DEC'                                                                                                     
                                                                                                                            
      mes=charmes(m)                                                                                                        
                                                                                                                            
      return                                                                                                                
      end                                                                                                                   
