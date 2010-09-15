program Potenzfunktion

  ! Dieses Programm rechnet a**b = e

  implicit none

  real :: a      ! Basis
  integer :: b   ! Exponent
  real :: e      ! Ergebnis
  integer :: i   ! Laufvariable


  print *, "Programm zur Berechnung von a^b. Vorsicht bei Kommazahlen Bsp.: 2,4 = 2.4"


  print *, "geben Sie bitte den Basiswert an:"
  read *, a

  print *, "geben Sie bitte den Exponenten an:" 
  read *, b


  e = 1

  do i = 1, b
     e = e * a
  end do

  write (*, '(/A//)') "Ergebnis:"
  write (*, '(/F10.1 //)' ) e
 

end program Potenzfunktion
