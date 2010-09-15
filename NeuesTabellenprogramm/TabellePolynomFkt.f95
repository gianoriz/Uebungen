program Wertetabelle

implicit none

real :: x
real :: a
real :: b
real :: c
real :: dx
integer :: i



print *, "Die Polynomfunktion lautet: a*x^2 + b*x +c"

print *, "Bitte geben Sie die Werte fuer a, b, c ein:" 
read *, a, b, c 

print *, "Bitte geben Sie die Werte fuer die Schrittweite an:"
read *, dx 

write(*,*) "   x             f(x)         "
write(*,*) '-----------------------------------------'

      
do i = -10 * int(1/dx), 10 * int (1/dx) 
   x = i * dx 


print *, x, a * (x**2) + b * x + c 
 

end do  




end program Wertetabelle
