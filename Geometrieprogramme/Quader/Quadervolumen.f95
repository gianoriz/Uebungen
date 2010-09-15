! Volumen eines Quaders


! Definiere die Variablen

real :: LaengeCM
real :: BreiteCM
real :: HoeheCM
real :: a

! Definiere Parameter 

print *, "Dieses Programm wurde in Fortran programmiert und berechnet das Volumen eines Quaders fahren sie fort mit Taste 1"
read *, a

print *, "Wert für Laenge in cm eingeben"
read *, LaengeCM

print *, "Wert für Breite in cm eingeben"
read *, BreiteCM

print *, "Wert für Laenge Hoehe in cm eingeben"
read *, HoeheCM


! Rechnung

print *, "Das Volumen eines Quaders in Kubikzentimeter ist:"
print *, (LaengeCM) * (BreiteCM) * (HoeheCM)




end


