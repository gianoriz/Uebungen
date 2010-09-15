program RungeKuttaUltraKurz

  !PROGRAMM: RUNGE-KUTTA-4TER-ORDNUNG

  implicit none   

  !DEFINITION DER VARIABLEN:
  double precision y, v_y               !y-Koord. [m] & Geschw. in y-Richt.[m/s]
  double precision x, v_x               !x-Koord. [m] & Geschw. in x-Richt.[m/s] 
  double precision z, v_z               !z-Koord. [m] & Geschw. in z-Richt.[m/s]
  double precision t, dt                !Flugzeit [s] & Zeitintervall [s]
  double precision v_0                  !Anfangsgeschw.    [m/s]
  real :: g = 9.81                      !Erdbeschleunigung [m/s²]
  double precision alpha                !Abwurfwinkel      [rad]    
  integer :: q, n, i, j, m                 !Laufvariablen
  real :: Pi = 3.1415926535898          !Ludolphsche Zahl 
  double precision, dimension(4,6) :: K !K(j,i)=(K(1,1),...,K(1,6),...,...,K(4,6)) Matrix
  double precision, dimension(9) :: W   !W(i)=(W(1)=x,W(2)=y,...,W(4)=v_x,...,W(6)=v_z) Vektor  
  double precision Mj, Mk               !Hilfsgroessen: Mj = 3 bzw. 6 & Mk = 2 bzw. 1
  double precision A                    !Hilfsgr.: eliminiert nichtvorhandene Matrixelemente 
  integer,parameter :: unit = 70

  !ERGEBNISSE SOLLEN ALS TEXTDATEI IM FOLGENDEM VERZEICHNIS GESPEICHERT WERDEN:
  open(unit, File='/afs/.geo.uni-koeln.de/usr/rizzo/my/f95/NumerischeMethoden/RungeKutta/RuKu4terOrdn/ParameterStudien/0k6XY.txt')





  !SETZE PARAMETER:
  alpha = m * (Pi/180.0)
  v_0 = 20.0
  dt = 0.6
  n = 4


  !STARTWERTE: 
  t = 0.0 
  W = 0.0
  x = 0.0
  y = 0.0
  z = 0.0                                !z = 0, da Problem 2dimensional
  v_x = v_0 *cos(alpha)


  v_y = v_0 *sin(alpha)
  v_z = 0.0                              !v_z = 0, da Problem 2dimensional 



  !DEFINITION DES VEKTORS W (ORTS- & GESCHWINDIGKEITSKOMPONENTEN):  
  W(1) = x                               !x
  W(2) = y                               !y
  W(3) = 0.0                             !z
  W(4) = v_0 * cos(alpha)                !v_x = W(4) bleibt waehrend der Flugzeit konstant
  W(5) = v_0 * sin(alpha)                !v_y = W(5) aendert sich waehrend der Flugzeit  
  W(6) = 0.0                             !v_z
  W(7) = 0.0                             !a_x 
  W(8) = (-1) * g                        !a_y
  W(9) = 0.0                             !a_z



  !AUSGABE AUF KONSOLE:
  write(*,*) "---------------------------------------------------------------------"
  write(*,*) "Programm zur Berechnung der Bahn des schiefen Wurfes"
  write(*,*) "Loesungsmethode: Runge-Kutta-4ter-Ordnung" 
  write(*,*) "Programm wurde erstellt am: 01/09/2010"
  write(*,*) "---------------------------------------------------------------------"
  write(*,*) " dt[s]     x [m]     y [m]      z[m]   v_x[m/s]  v_y[m/s]  v_z[m/s]  " 
  write(*,*) "---------------------------------------------------------------------"
  write(*,522) t, x, y, z, v_x, v_y, v_z                !Ausgabe der Anfangswerte
522 format(F7.3, 3X, F7.3, 3X, F7.3, 3X, F7.3, 3X, F7.3, 3X, F7.3, 3X, F7.3, 3X)



  !AUSGABE ALS TEXTDATEI:
  !write(unit,*) " dt[s]     x [m]     y [m]      z[m]   v_x[m/s]  v_y[m/s]  v_z[m/s] " 
  !write(unit,*) "--------------------------------------------------------------------"
  !write(unit,*) t, y  
  write(unit,*) x, y

  !BERECHNUNG:

  do m = 45, 45 !Winkel

     do q = 0, n                                         !Grosse do-Zeitschleife



        do j = 1, 4
           do i = 1, 6 


              if ((i > 3) .or. (j == 1)) then            !Beseitigt nichtvorhandene Matrixelem.                   
                 A = 0                                   !  K(0,4),K(0,5),...,K(0,6)                                  
              else                                       !& K(1,7),K(1,8),...,K(1,10)
                 A = 1
              end if


              if ((j == 2) .or. (j == 3)) then           !Bedingung: Nenner Mk = 2 bzw. 1 
                 Mk = 2
              else
                 Mk = 1
              end if



              K(j,i) = dt * (W(i+3) + A * K(j-1,i+3)/Mk) !BERECHNET DIE EINZELNEN MATRIXELEMENTE



           end do
        end do




        do i = 1, 6                                      !Schleife der Koord. x,y,z,v_x,v_y,v_z 
           do j = 1, 4                                   !Schleife fuer Matrixelem. der Spalte



              if ((j == 1) .or. (j == 4)) then           !Schleife fuer den Nenner Mj = 6 bzw. 3
                 Mj = 6
              else
                 Mj = 3
              endif



              W(i) = W(i) + K(j,i)/Mj                    !RUNGE-KUTTA-4TER-ORDNUNG-ALGORITHMUS



              t = (q+1) * dt                             !q+1 vermeidet Offset-Probleme

           end do                                        !Ende der i-Schleife

        end do                                           !Ende der j-Schleife





        write(*,523) t, W(1), W(2), W(3), W(4), W(5), W(6) !Ausgeben der Anfangswerte
523     format(F7.3, 3X, F7.3, 3X, F7.3, 3X, F7.3, 3X, F7.3, 3X, F7.3, 3X, F7.3, 3X)
        !write(unit,*) t, W(2)                               !Ausgabe als Textdatei
        write(unit,*) W!(1), W(2)

     end do                                              !Ende der grossen do-Zeit-Schleife
  end do!Winkel
  close(unit)

end program RungeKuttaUltraKurz
