
! Volumen einer Kugel



! Definiere die Variabeln 

real :: konstante
real :: Pi
real :: Radius


! Definiere die Konstanten 

konstante = 0.75
Pi = 3.141592654



! Definiere die Parameter

print *, "Dieses Programm wurde in Fortran programmiert und berechnet das Volumen eines Quaders fahren Sie fort mit Taste 1"
read *, a

print *, "Kugelradius in cm eingeben"
read *, Radius



! Rechnung

print *, "Das Kugelvolumen in Kubikzentimetern betraegt:"
print *, (konstante) * (Pi) * (Radius**3)


end
