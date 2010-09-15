program inverse4X4Matrix

  implicit none

  !Definition der Variablen:
  real, dimension(4,4) :: A    !Matrixelemente A(1,1), A(1,2), A(1,3), A(2,1), A(2,2), A(2,3), A(3,1), A(3,2), A(3,3)
  real, dimension(4,4) :: V    !Matrixelemente V(1,1), V(1,2), V(2,1), V(2,2), V(2,2), V(2,3), V(3,1), V(3,2), V(3,3)
  real, dimension(4,4) :: Ainv !Inverse Matrix zu A
  real :: det, det1, det2, det3, det4, det5, det6, det7, det8, det9, det10, det11, det12 !Determinante in 2 Teilsummen zerlegt um Zeilenüberlaenge zu vermeiden

  !Werte fuer die Matrixelemente festlegen:
  write(*,*)" Wert fuer A(1,1) eingeben:"
  read (*,*) A(1,1)
  write(*,*)" Wert fuer A(1,2) eingeben:"
  read (*,*) A(1,2)
  write(*,*)" Wert fuer A(1,3) eingeben:"
  read (*,*) A(1,3)
  write(*,*)" Wert fuer A(1,4) eingeben:" 
  read (*,*) A(1,4)
  write(*,*)" Wert fuer A(2,1) eingeben:"
  read (*,*) A(2,1)
  write(*,*)" Wert fuer A(2,2) eingeben:"
  read (*,*) A(2,2)
  write(*,*)" Wert fuer A(2,3) eingeben:"
  read (*,*) A(2,3)
  write(*,*)" Wert fuer A(2,4) eingeben:"
  read (*,*) A(2,4)
  write(*,*)" Wert fuer A(3,1) eingeben:"
  read (*,*) A(3,1)
  write(*,*)" Wert fuer A(3,2) eingeben:"
  read (*,*) A(3,2)
  write(*,*)" Wert fuer A(3,3) eingeben:"
  read (*,*) A(3,3)
  write(*,*)" Wert fuer A(3,4) eingeben:"
  read (*,*) A(3,4)
  write(*,*)" Wert fuer A(4,1) eingeben:"
  read (*,*) A(4,1)
  write(*,*)" Wert fuer A(4,2) eingeben:"
  read (*,*) A(4,2)
  write(*,*)" Wert fuer A(4,3) eingeben:"
  read (*,*) A(4,3)
  write(*,*)" Wert fuer A(4,4) eingeben:"
  read (*,*) A(4,4)



  !Definition der Matrix V:
  V(1,1) = (A(2,2)*A(3,3)*A(4,4) + A(2,3)*A(3,4)*A(4,2) & 
       + A(2,4)*A(3,2)*A(4,3) - A(4,2)*A(3,3)*A(2,4) - A(4,3)*A(3,4)*A(2,2) - A(4,4)*A(3,2)*A(2,3))  
  V(1,2) = (-1)*(A(2,1)*A(3,3)*A(4,4) + A(2,3)*A(3,4)*A(4,1) & 
       + A(2,4)*A(3,1)*A(4,3) - A(4,1)*A(3,3)*A(2,4) - A(4,3)*A(3,4)*A(2,1) - A(4,4)*A(3,1)*A(2,3))
  V(1,3) = (A(2,1)*A(3,2)*A(4,4) + A(2,2)*A(3,4)*A(4,1) &
       + A(2,4)*A(3,1)*A(4,2) - A(4,1)*A(3,2)*A(2,4) - A(4,2)*A(3,4)*A(2,1) - A(4,4)*A(3,1)*A(2,2))
  V(1,4) = (-1)*(A(2,1)*A(3,2)*A(4,3) + A(2,2)*A(3,3)*A(4,1) &
       + A(2,3)*A(3,1)*A(4,2) - A(4,1)*A(3,2)*A(2,3) - A(4,2)*A(3,3)*A(2,1) - A(4,3)*A(3,1)*A(2,2))
  V(2,1) = (-1)*(A(1,2)*A(3,3)*A(4,4) + A(1,3)*A(3,4)*A(4,2) &
       + A(1,4)*A(3,2)*A(4,3) - A(4,2)*A(3,3)*A(1,4) - A(4,3)*A(3,4)*A(1,2) - A(4,4)*A(3,2)*A(1,3)) 
  V(2,2) = (A(1,1)*A(3,3)*A(4,4) + A(1,3)*A(3,4)*A(4,1) &
       + A(1,4)*A(3,1)*A(4,3) - A(4,1)*A(3,3)*A(1,4) - A(4,3)*A(3,4)*A(1,1) - A(4,4)*A(3,1)*A(1,3)) 
  V(2,3) = (-1)*(A(1,1)*A(3,2)*A(4,4) + A(1,2)*A(3,4)*A(4,1) &
       + A(1,4)*A(3,1)*A(4,2) - A(4,1)*A(3,2)*A(1,4) - A(4,2)*A(3,4)*A(1,1) - A(4,4)*A(3,1)*A(1,2))
  V(2,4) = (A(1,1)*A(3,2)*A(4,3) + A(1,2)*A(3,3)*A(4,1) &
       + A(1,3)*A(3,1)*A(4,2) - A(4,1)*A(3,2)*A(1,3) - A(4,2)*A(3,3)*A(1,1) - A(4,3)*A(3,1)*A(1,2)) 
  V(3,1) = (A(1,2)*A(2,3)*A(4,4) + A(1,3)*A(2,4)*A(4,2) &
       + A(1,4)*A(2,2)*A(4,3) - A(4,2)*A(2,3)*A(1,4) - A(4,3)*A(2,4)*A(1,2) - A(4,4)*A(2,2)*A(1,3))
  V(3,2) = (-1)*(A(1,1)*A(2,3)*A(4,4) + A(1,3)*A(2,4)*A(4,1) &
       + A(1,4)*A(2,1)*A(4,3) - A(4,1)*A(2,3)*A(1,4) - A(4,3)*A(2,4)*A(1,1) - A(4,4)*A(2,1)*A(1,3))
  V(3,3) = (A(1,1)*A(2,2)*A(4,4) + A(1,2)*A(2,4)*A(4,1) &
       + A(1,4)*A(2,1)*A(4,2) - A(4,1)*A(2,2)*A(1,4) - A(4,2)*A(2,4)*A(1,1) - A(4,4)*A(2,1)*A(1,2))
  V(3,4) = (-1)*(A(1,1)*A(2,2)*A(4,3) + A(1,2)*A(2,3)*A(4,1) &
       + A(1,3)*A(2,1)*A(4,2) - A(4,1)*A(2,2)*A(1,3) - A(4,2)*A(2,3)*A(1,1) - A(4,3)*A(2,1)*A(1,2))
  V(4,1) = (-1)*(A(1,2)*A(2,3)*A(3,4) + A(1,3)*A(2,4)*A(3,2) &
       + A(1,4)*A(2,2)*A(3,3) - A(3,2)*A(2,3)*A(1,4) - A(3,3)*A(2,4)*A(1,2) - A(3,4)*A(2,2)*A(1,3))
  V(4,2) = (A(1,1)*A(2,3)*A(3,4) + A(1,3)*A(2,4)*A(3,1) &
       + A(1,4)*A(2,1)*A(3,3) - A(3,1)*A(2,3)*A(1,4) - A(3,3)*A(2,4)*A(1,1) - A(3,4)*A(2,1)*A(1,3))  
  V(4,3) = (-1)*(A(1,1)*A(2,2)*A(3,4) + A(1,2)*A(2,4)*A(3,1) &
       + A(1,4)*A(2,1)*A(3,2) - A(3,1)*A(2,2)*A(1,4) - A(3,2)*A(2,4)*A(1,1) - A(3,4)*A(2,1)*A(1,2))  
  V(4,4) = (A(1,1)*A(2,2)*A(3,3) + A(1,2)*A(2,3)*A(3,1) &
       + A(1,3)*A(2,1)*A(3,2) - A(3,1)*A(2,2)*A(1,3) - A(3,2)*A(2,3)*A(1,1) - A(3,3)*A(2,1)*A(1,2)) 


  !Berechnung der Determinante: 
  !Wichtig: um Zeilenrangprobleme zu vermeiden, hab ich die Determinante in einzelterme unterteilt
  det1 = + A(1,4) * A(2,3) * A(3,2) * A(4,1) - A(1,3) * A(2,4) * A(3,2) * A(4,1) 
  det2 = - A(1,4) * A(2,2) * A(3,3) * A(4,1) + A(1,2) * A(2,4) * A(3,3) * A(4,1) 
  det3 = + A(1,3) * A(2,2) * A(3,4) * A(4,1) - A(1,2) * A(2,3) * A(3,4) * A(4,1) 
  det4 = - A(1,4) * A(2,3) * A(3,1) * A(4,2) + A(1,3) * A(2,4) * A(3,1) * A(4,2) 
  det5 = + A(1,4) * A(2,1) * A(3,3) * A(4,2) - A(1,1) * A(2,4) * A(3,3) * A(4,2) 
  det6 = - A(1,3) * A(2,1) * A(3,4) * A(4,2) + A(1,1) * A(2,3) * A(3,4) * A(4,2)
  det7 = + A(1,4) * A(2,2) * A(3,1) * A(4,3) - A(1,2) * A(2,4) * A(3,1) * A(4,3) 
  det8 = - A(1,4) * A(2,1) * A(3,2) * A(4,3) + A(1,1) * A(2,4) * A(3,2) * A(4,3) 
  det9 = + A(1,2) * A(1,1) * A(3,4) * A(4,3) - A(1,1) * A(2,2) * A(3,4) * A(4,3) 
  det10 = - A(1,3) * A(2,2) * A(3,1) * A(4,4) + A(1,2) * A(2,3) * A(3,1) * A(4,4) 
  det11 = + A(1,3) * A(2,1) * A(3,2) * A(4,4) - A(1,1) * A(2,3) * A(3,2) * A(4,4)
  det12 = - A(1,2) * A(2,1) * A(3,3) * A(4,4) + A(1,1) * A(2,2) * A(3,3) * A(4,4) 

  det = det1+ det2+ det3+ det4 + det5 + det6 + det7 + det8 + det9 + det10 + det11 + det12



  ! Ausgabe der inversen Matrix:

  write (*,*) "-------------------------------------------"
  write (*,*) "Die Determinante lautet:"
  write (*,*)  det
  write (*,*) "-------------------------------------------"
  write (*,*) "Ainv lautet:" 
  write (*,*) "Ainv(1,1)", V(1,1)*(1/det)
  write (*,*) "Ainv(1,2)", V(2,1)*(1/det)
  write (*,*) "Ainv(1,3)", V(3,1)*(1/det)
  write (*,*) "Ainv(1,4)", V(4,1)*(1/det)
  write (*,*) "Ainv(2,1)", V(1,2)*(1/det)
  write (*,*) "Ainv(2,2)", V(2,2)*(1/det)
  write (*,*) "Ainv(2,3)", V(3,2)*(1/det)
  write (*,*) "Ainv(2,4)", V(3,2)*(1/det)
  write (*,*) "Ainv(3,1)", V(1,3)*(1/det)
  write (*,*) "Ainv(3,2)", V(2,3)*(1/det)
  write (*,*) "Ainv(3,3)", V(3,3)*(1/det)
  write (*,*) "Ainv(3,4)", V(4,3)*(1/det)
  write (*,*) "Ainv(4,1)", V(1,4)*(1/det)
  write (*,*) "Ainv(4,2)", V(2,4)*(1/det)
  write (*,*) "Ainv(4,3)", V(3,4)*(1/det)
  write (*,*) "Ainv(4,4)", V(4,4)*(1/det)


end program inverse4X4Matrix
