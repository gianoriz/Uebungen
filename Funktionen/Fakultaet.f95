program fakultaet

  implicit none


  real :: n 
  real  :: fak
  integer :: i

  print *, "Dies ist ein Programm zur Berechnung der Fakultaet"

  print *, "n-Wert eingeben:"
  read *, n

  !  write (*, '(/A//)') "Ergebnis:"   !Befehl für weniger Kommastellen
  !  write (*, '(/F10.1 //)' ) fak(n)  !Befehl für weniger Kommastellen

  print *, fak(n)


end program fakultaet



real function fak (n)
  implicit none
  real :: n

  !fak(0) = 1
  !fak(n) = n * (n-1)

  do i = 1, n

     fak(n) = n - i + 1
  end do
end function fak
