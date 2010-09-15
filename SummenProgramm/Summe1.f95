
program Summewerte


  implicit none  

  !Definition der verwendeten Variablen:
  real :: summe
  integer :: i, n 


  print *, "Summe(i=1...n,1/i**2)"
  print *, "Eingabewert: n"
  read *, n
  print *, " n = ", n

  summe = 0 
  do i = 1, n
     summe = summe + 1.0 / real(i)**2
  end do

  print*, "summe = ", summe



end program Summewerte
