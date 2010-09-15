program Wertetabelle

implicit none 

! Definiere Variablen 
real :: x
real :: g 
real :: v_0
real :: alpha
real :: dx
real :: rad2deg
integer :: i


! Lege fest, wo die Ergebnisse ausgegeben werden sollen
open(7, File='/afs/.geo.uni-koeln.de/usr/rizzo/Desktop/DatenNEU.txt')


! Definiere Konstanten 
g = 9.81
rad2deg=3.14159/180.0  !Umrechnung von rad zu Grad

print *, "Bitte geben Sie die Werte fuer v_0 und alpha an:"
read *, v_0
read *,  alpha 

print *, "Bitte geben Sie die Werte fuer die Schrittweite an"
read *, dx

write (7,*)  "   x             f(x)         "
write (7,*)  '-----------------------------------------'



do i = -10 * int(1/dx), 10 * int (1/dx)
   x = i * dx


write (7,*) x, (-1) * (g/ (2 * (v_0**2) * (cos(rad2deg * alpha)**2) )) * x**2 - tan(rad2deg * alpha) * x 
           

end do 



write(7,*) " Text " 
close(7)
print *, "beendet"

end program Wertetabelle


  


