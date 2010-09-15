program EulerRichardson

  implicit none   !RICHTIGE VERSION

  !Definition der verwendeten Variablen:
  real :: y, v_y, ynew, v_ynew   !y-Koord. [m/s] & Geschw. in y-Richtung [m]
  real :: Vy, ynew2              !neue Koordinaten
  real :: x, v_x                 !Geschw. in x-Richtung [m]
  real :: v_0                    !Anfangsgeschwindig. [m/s]
  real :: t, dt                  !Flugzeit [s] & Zeitintervall [s]
  real :: g = 9.81               !Erdbeschleunigung   [m/s²]
  real :: alpha                  !Abwurfwinkel        [rad]    
  integer :: i, m                !Laufvariablen
  real :: Pi = 3.14159265358979323846


  !Setzte Parameter:
  alpha = 45.0 * (Pi/180.0)
  v_0 = 20.0
  dt = 0.2
  m = 14


  !Startwerte: 
  t = 0.0 
  x = 0.0 
  y = 0.0
  write(*,*) t, y                !Ausgeben des Anfangswertes y=0 bei t=0    


  !Geschw. der Komponenten:
  v_x = v_0 * cos(alpha)         !bleibt waehrend der Flugzeit konstant
  v_y = v_0 * sin(alpha)         !aendert sich waehrend der Flugzeit  



  do i = 0, m   

     ynew = y + v_y * (dt/2)     !mittlere Position y 
     v_ynew = v_y - g * (dt/2)   !mittlere Geschwindigkeit v_y     


     ynew2 = y + v_ynew * dt     !neuer y Wert
     v_y = v_y - g * dt          !neuer v_y Wert 


     y = ynew2                   !der neue y-Wert ist jetzt ynew2


     t = (i+1) * dt              !i+1 gerechnet um Offset-Probleme zu vermeiden
     write(*,*) t, y


  end do


end program EulerRichardson
