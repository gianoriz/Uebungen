program inverseNXNMatrix

  implicit none


  integer :: p
  integer , dimension (3,3) :: a
  integer :: i, j, n, k
  integer , dimension (3-1,3-1) :: b


  !  write(*,*) "Einen Wert fuer die Dimension n der Matrix eingeben:"
  ! read(*,*) n


  n = 3




  ! Erstellen einer Matrix
  do j = 1, n 
     do i = 1, n
        a(i,j) = (i -1)* 5 + (j -1)
     end do
  end do




  do k = 1, n 

  !   ( (a(i,j), i=1 ) , j=1, k)

  write (*,*) ( (a(i,j), j=k,k ) , i=1,3)
  end do

  !write(*,*) p




  !Ausgabe der Matrix a
 ! write(*,*) "Matrix a:"
 ! write(*,*) a

  !Festlegung der Untermatrix:
  !b = a(1:3,1:3) !Untermatrix
 ! write (*,*) "Untermatrix  b"
  ! write (*,*) a(1:3,1:3)
 ! write (*,*) ( (a(i,j), j=2,3 ) , i=1,2) !i zaehlt die Spalte und j die Zeile



end program inverseNXNMatrix
