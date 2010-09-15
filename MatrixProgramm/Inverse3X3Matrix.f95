program inverse3X3Matrix

implicit none

real :: det, det1, det2
real, dimension(3,3) :: A !Matrixelemente A(1,1), A(1,2), A(1,3), A(2,1), A(2,2), A(2,3), A(3,1), A(3,2), A(3,3)
real, dimension(3,3) :: B !Matrixelemente B(1,1), B(1,2), B(2,1), B(2,2), B(2,2), B(2,3), B(3,1), B(3,2), B(3,3)
real, dimension(3,3) :: Ainv


!Werte fuer die Matrixelemente festlegen:
write(*,*)" Wert fuer A(1,1) eingeben:"
read (*,*) A(1,1)
write(*,*)" Wert fuer A(1,2) eingeben:"
read (*,*) A(1,2)
write(*,*)" Wert fuer A(1,3) eingeben:"
read (*,*) A(1,3)
write(*,*)" Wert fuer A(2,1) eingeben:"
read (*,*) A(2,1)
write(*,*)" Wert fuer A(2,2) eingeben:"
read (*,*) A(2,2)
write(*,*)" Wert fuer A(2,3) eingeben:"
read (*,*) A(2,3)
write(*,*)" Wert fuer A(3,1) eingeben:"
read (*,*) A(3,1)
write(*,*)" Wert fuer A(3,2) eingeben:"
read (*,*) A(3,2)
write(*,*)" Wert fuer A(3,3) eingeben:"
read (*,*) A(3,3)


!Definition der Matrix B:
B(1,1) = A(2,2) * A(3,3) - A(3,2) * A(2,3)
B(2,1) = A(1,3) * A(3,2) - A(1,2) * A(3,3) 
B(3,1) = A(1,2) * A(2,3) - A(1,3) * A(2,2)
B(1,2) = A(2,3) * A(3,1) - A(2,1) * A(3,3)
B(2,2) = A(1,1) * A(3,3) - A(1,3) * A(3,1)
B(3,2) = A(1,3) * A(2,1) - A(1,1) * A(2,3)
B(1,3) = A(2,1) * A(3,2) - A(2,2) * A(3,1)
B(2,3) = A(1,2) * A(3,1) - A(1,1) * A(3,2) 
B(3,3) = A(1,1) * A(2,2) - A(1,2) * A(2,1)



!Berechnung der Determinante: 
det1 = A(1,1)*A(2,2)*A(3,3) + A(1,2)*A(2,3)*A(3,1) + A(1,3)*A(2,1)*A(3,2) 
det2 = (-1) * ( A(3,1)*A(2,2)*A(1,3) + A(3,2)*A(2,3)*A(1,1) + A(3,3)*A(2,1)*A(1,2) ) 
det = det1 + det2

!Berechnung der inversen Matrix:
Ainv = (1/det) * B

print *,  "Die Determinante lautet:"
write (*,*) det
print *,  "Ainv lautet:" 
print *," A(1,1)           A(1,2)           A(2,1)         A(2,2)" 
print *,"--------------------------------------------------------"
write (*,*) Ainv


end program inverse3X3Matrix
