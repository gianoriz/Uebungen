
! Volumen einer Pyramide


! Definiere die Variablen

real :: Grundlaenge
real :: Grundbreite 
real :: Hoehe


! Definiere die Parameter

print *, "Dieses Programm wurde in Fortran programmiert und berechnet das Volumen einer Pyramide. Fahren Sie fort mit Taste 1"
read *, a

print *, "Grundlaenge in cm eingeben"
read *, Grundlaenge

print *, "Grundbreite in cm eingeben"
read *, Grundbreite

print *, "Hoehe in cm eingeben"
read *, Hoehe


! Rechnung

Print *, "Das Pyramidenvolumen in Kubikzentimetern betraegt:"
Print *, ((Grundlaenge) * (Grundbreite) * (Hoehe))/3


end
