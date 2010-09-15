! Volumen eines Wuerfels


! Definiere die Variablen

real :: Seitenlaenge
real :: a

!Definiere Parameter 

print *, "Dieses Programm wurde in Fortran programmiert und berechnet das Volumen eines Wuerfels fahren sie fort mit Taste 1"
read *, a

print *, "Wert für Seitenlaenge in cm eingeben"
read *, Seitenlaenge


!Rechnung

print *, "Das Volumen eines Wuerfels in Kubikzentimeter ist:"
print *, (Seitenlaenge) * (Seitenlaenge)* (Seitenlaenge)





end

