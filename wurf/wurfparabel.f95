program Wurfparabel
  implicit none

  ! Wurfparabel

  real :: alpha= 70
  real :: v0 =40
  real :: g = 9.81
  real :: x = 10
  real :: Pi = 3.141592654
  integer :: counter
  ! Setze Werte



  print *,"Bitte Winkel eingeben:"
!  read *, alpha
  print *,"Bitte v0 eingeben:"
!  read *, v0
  print *,"x-Wert"
!  read *, x

  ! Funktion eingeben

  do counter = 1, 1000
     x = x + 0.1
     print *, counter, x , - tan(alpha*Pi/180) * x - g *x*x /(2 * v0**2 * cos(alpha*Pi/180)**2)
  end do



end program Wurfparabel



