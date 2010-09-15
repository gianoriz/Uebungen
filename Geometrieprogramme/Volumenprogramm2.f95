ls
! Volumen berechnen


real :: RadiusM, HoeheM
real :: Pi

! Setze Werte
! Der RadiusM**2 heißt Radius^2 

 
Pi = 3.1416
 
print *, "Wert für Radius eingeben"
read *, RadiusM 
print *, "Wert für HoeheM eingeben"
read *, HoeheM 

print *, "Das Ergebnis in Metern ist:"
print *, Pi * (RadiusM**2) * HoeheM  

end



