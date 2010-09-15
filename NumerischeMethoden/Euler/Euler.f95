
program EulerVerfahren

  implicit none

  !Definition der verwendeten Variablen:
  real :: x, v_x       !x-Koordinate [m/s] & Geschw. in x-Richtung [m] 
  real :: y, v_y, ynew, v_ynew  !y-Koordinate [m/s] & Geschw. in y-Richtung [m]
  real :: t            !Flugzeit            [s]
  real :: v_0          !Anfangsgeschwindig. [m/s]  
  real :: dt           !Zeitintervall       [s]
  real :: g = 9.81     !Erdbeschleunigung   [m/s]
  real :: a            !Abwurfwinkel        [°]    
  real :: Pi = 3.14159265358979323846 
  integer :: i, m, j   !Laufvariablen


  !Setzte Parameter:
  a = 45.0
  v_0 = 20.0
  dt = 0.2
  m = 15


  !Startwerte: 
  t = 0.0 
  x = 0.0 
  y = 0.0

  !Ausgeben:
  print *, t, y 


  v_x = v_0 * cos(a * (Pi/180.0)) !bleibt waehrend der Flugzeit konstant
  v_y = v_0 * sin(a * (Pi/180.0)) !aendert sich waehrend der Flugzeit  



  !Erstellen der Wertetabelle:

  do i = 0, m

     v_ynew = v_y - g * dt  !(siehe Fussnote(1)) (siehe Fussnote(2))
     ynew = y + v_y * dt 

     v_y = v_ynew
     y = ynew


     t = (i+1) * dt      !Habe i+1 gerechnet um Offset-Probleme zu vermeiden
     !Ausgeben:
     write(*,*) t, ynew

  end do


end program EulerVerfahren

 
! (1) Hier wird nach dem Gleichheitszeichen nicht v_y0 eingesetzt, da wir sonst Probleme mit der Schleife bekommen. Das Programm weiss schon, dass nach dem Gleichheitszeichen das v_y der Startwert ist, das wurde vor der do-Schleife mit v_y = v_0 * sin(a * (Pi/180.0)) festgelegt. Das Programm berechnet jetzt den neuen Wert v_y (links) & setzt den neuen Wert in (v_y) rechts ein.   

! (2) Das hat schon seinen Grund, warum in der do-Schleife nur v_y & nicht v_x, da v_x zeitlich konstant beleibt. 



