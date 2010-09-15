program exponential

  ! Dieses Programm rechnet a**b

  implicit none

  real :: a      ! Basis
  !  integer :: b   ! Exponent
  !  real :: e      ! Ergebnis
  !  integer :: i   ! Laufvariable
  real :: x, y 
  real :: quat
  real :: pi
  real :: delta

  print *, "Dieses Programm rechnet a^b. Vorsicht: Kommazahlen werden mit Punkt geschrieben Bsp.: 2,4 = 2.4"


  print *, "geben Sie bitte einen Wert an:"
  read *, a

  !print *, "geben Sie bitte einen Exponent an:" 
  !read *, b


  !e = 1

  !write (*, '(/A//)') "Ergebnis:"
  !write (*, '(/F10.1 //)' ) e

  print *, quat(a)
  print *, pi()
  print *, "Wert fuer x und y eingeben"
  read *, x, y 
  print *, delta(x, y)
end program exponential



real function delta (x,y)
  implicit none
  real :: x, y  
  if (x == y)  then 
     delta = 1. 
  else 
     delta = 0.
  end if
end function delta



real function quat ( x )
  implicit none
  real :: x  
  quat = x**2
end function quat


real function pi (  )
  implicit none
  pi = 3.14
end function pi
