program Wurfparabel

  implicit none 


  !Das Programm vorallem der Berechnung der Wurfweite


  double precision y, x, v_x, v_y  
  real :: g = 9.81
  real :: Pi = 3.1415926535898 
  integer :: i, n
  double precision t, dt 
  double precision alpha 
  double precision v_0
  double precision yneu


  !Startwerte:
  alpha = 45.0 * (Pi/180.0)
  v_0 = 20.0
  t = 0.0 
  y = 0.0
  x = 0.0
  dt = 0.2
  n = 14


  !write(*,*) t, x, y
  write(*,*) x, y

  v_x = v_0 * cos(alpha)
  v_y = v_0 * sin(alpha)


  !Rechnung:

  do i = 1, n

     t = i * dt

     y = v_y * t - (g/2) * t**2
     x = v_x * t



     !write(*,*) t, x, y
     write(*,*) x, y     

  end do



end program Wurfparabel
