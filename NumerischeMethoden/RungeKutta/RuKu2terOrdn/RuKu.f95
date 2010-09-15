program RungeKutta

  implicit none   !RungeKutta 2ter Ordnung

  !Definition der verwendeten Variablen:
  real :: y, v_y, ynew, v_ynew   !y-Koord. [m/s] & Geschw. in y-Richtung [m]
  real :: x, v_x
  real :: v_0                    !Anfangsgeschwindig. [m/s]
  real :: t, dt                  !Flugzeit [s] & Zeitintervall [s]
  real :: g = 9.81               !Erdbeschleunigung   [m/s²]
  real :: alpha                  !Abwurfwinkel        [rad]    
  integer :: i, m                !Laufvariablen
  real :: Pi = 3.14159265358979323846


  !Setzte Parameter:
  alpha = 45.0 * (Pi/180.0)
  v_0 = 20.0
  dt = 0.5
  m = 5


  !Startwerte: 
  t = 0.0 
  x = 0.0 
  y = 0.0
  write(*,*) t, y                !Ausgeben des Anfangswertes y=0 bei t=0    


  !Geschw. der Komponenten:
  v_x = v_0 * cos(alpha)         !bleibt waehrend der Flugzeit konstant
  v_y = v_0 * sin(alpha)         !aendert sich waehrend der Flugzeit  



  do i = 0, m   

     ynew = y + (dt/2) * (2 * v_y - g * dt) !RungeKuttaAlgorithmus fuer Hoehe     

     v_ynew = v_y - g * dt       !RungeKuttaAlgorithmus fuer Geschw. in y 


     v_y = v_ynew                !Puffer
     y = ynew                    !Puffer

     t = (i+1) * dt              !i+1 gerechnet um Offset-Probleme zu vermeiden
     write(*,*) t, y


  end do


end program RungeKutta



