program inverse2X2Matrix

implicit none

real :: det
real, dimension(2,2) :: A !Matrixelemente A(1,1), A(1,2), A(2,1), A(2,2)
real, dimension(2,2) :: B !Matrixelemente B(1,1), B(1,2), B(2,1), B(2,2)
real, dimension(2,2) :: Ainv


!Werte fuer die Matrixelemente festlegen:
write(*,*)" Wert fuer A(1,1) eingeben:"
read (*,*) A(1,1)
write(*,*)" Wert fuer A(1,2) eingeben:"
read (*,*) A(1,2)
write(*,*)" Wert fuer A(2,1) eingeben:"
read (*,*) A(2,1)
write(*,*)" Wert fuer A(2,2) eingeben:"
read (*,*) A(2,2)



!Definition der Matrix B:
B(1,1) = A(2,2)
B(2,1) = -A(1,2)
B(1,2) = -A(2,1)
B(2,2) = A(1,1)


!Berechnung der Determinante: 
det = A(1,1) * A(2,2) - A(2,1) * A(1,2)


!Berechnung der inversen Matrix:
Ainv = (1/det) * B

print *,  "Die Determinante lautet:"
write (*,*) det
print *,  "Ainv lautet:" 
print *," A(1,1)           A(1,2)           A(2,1)         A(2,2)" 
print *,"--------------------------------------------------------"
write (*,*) Ainv


end program inverse2X2Matrix



