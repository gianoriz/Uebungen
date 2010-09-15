program Deltafunktion


  implicit none

  
  real :: x, y 
  real :: delta

  print *, "Dies ist ein Programm zur Berechnung der Delta-Funktion"

  print *, "x-Wert eingeben:"
  read *, x

  print *, "y-Wert eingeben:" 
  read *, y
  write (*, '(/A//)') "Ergebnis:" !Befehl für weniger Kommastellen
  write (*, '(/F10.1 //)' ) delta(x, y) !Befehl für weniger Kommastellen
 

end program Deltafunktion


!Die Funktion wird eingegeben nachdem das Programm beendet wurde:

real function delta (x,y)
  implicit none
  real :: x, y  
  if (x == y)  then 
     delta = 1. 
  else 
     delta = 0.
  end if
end function delta


