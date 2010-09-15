
program TeilchenTrajektorie

  !PROGRAMM: RUNGE-KUTTA-4TER-ORDNUNG

  implicit none   

  !DEFINITION DER VARIABLEN:
  integer, parameter :: rk = SELECTED_REAL_KIND(10,30)
  double precision xr, v_xr, a_xr       !x-Koord. [m] & Geschw.[m/s] im Rhea-Ruhesystem  
  double precision yr, v_yr, a_yr       !y-Koord. [m] & Geschw.[m/s] im Rhea-Ruhesystem
  double precision zr, v_zr, a_zr       !z-Koord. [m] & Geschw.[m/s] im Rhea-Ruhesystem 
  double precision xs, v_xs, a_xs       !x-Koord. [m] & Geschw.[m/s] im Saturn-Ruhesystem  
  double precision ys, v_ys, a_ys       !y-Koord. [m] & Geschw.[m/s] im Saturn-Ruhesystem  
  double precision zs, v_zs, a_zs       !z-Koord. [m] & Geschw.[m/s] im Saturn-Ruhesystem   
  double precision t, dt                !Flugzeit [s] & Zeitintervall [s]
  double precision v_0, L               !Anfangsgeschw.[m/s], Lagrangepunkt
  double precision alpha                !Abwurfwinkel  [rad]     
  double precision Mj, Mk               !Hilfsgroessen: Mj = 3 bzw. 6 & Mk = 2 bzw. 1
  double precision omegaz               !Winkelgeschwingigkeit in z-Richtung [1/s]
  double precision gamma                !Gravitationskonstante [m³/kg*s²]
  double precision Ms, Mr               !Masse Saturn, Masse Rhea [kg]
  double precision v_r                  !Geschw. von Rhea um Saturn [m/s]
  double precision A                    !Hilfsgr.: eliminiert nichtvorhandene Matrixelemente 
  double precision, dimension(4,6) :: K !K(j,i)=(K(1,1),...,K(1,6),...,...,K(4,6)) Matrix
  double precision, dimension(9) :: W   !W(i)=(W(1)=x,W(2)=y,...,W(4)=v_x,...,W(6)=v_z) Vektor  
  integer :: q, n, i, j                 !Laufvariablen
  real :: Pi = 3.14159265358979323846   !Ludolphsche Zahl 
  real :: d                             !Abstand Saturn-Rhea



  !SETZE KONSTANTEN & PARAMETER: 

  d = 527040000.0                ![m] Abstand Saturn-Rhea  
  Mr = 2.3166e21                 ![kg]
  Ms = 5.685e24                  ![kg]
  omegaz = 1.609607175e-5        ![1/s]
  gamma = 6.67428e-11            ![m³/kg*s²]

  !alpha= 0.0 !45.0 *(Pi/180.0)  ![rad] Das Teilchen wird in einem Winkel von 45° herausgeschlagen
  v_0  = 0.0                     ![m/s]
  dt   = 0.001                   ![s]
  n    = 50                      ![Anzahl der Rechnungen]
  t = 0.0 




  !STARTWERTE IM RUHESYSTEM DES SATURN:   
 !xs = 526852322.4  ![m] Lagrangepunkt 1  
  xs = 527254851.1  ![m] Lagrangepunkt 2
  ys = 0.0          !0.0
  zs = 0.0          !1528000.0




  !STARTWERTE IM RUHESYSTEM VON RHEA:
  xr = d - xs                          ![m] Teilchen befinde sich bei t=0s genau am Lagrangepunkt
  yr = 0.0                             !0.0                
  zr = 0.0                             !1528000.0  !Radius Rhea r= 1528000m                             
  v_xr = 0.0!v_0 *cos(alpha)           !Geschwindigkeit des Teilchen bei t= 0s (Sputtering)
  v_yr = 0.0
  v_zr = 0.0!v_0 *sin(alpha) 
  a_xr = -(gamma * Ms * xs)/(xs**2 + ys**2 + zs**2)**(1.5) - (gamma * Mr * xr)/(xr**2 + yr**2 + zr**2)**(1.5) &
       + xr * omegaz**2 + 2 * omegaz * v_yr
  a_yr = -(gamma * Ms * ys)/(xs**2 + ys**2 + zs**2)**(1.5) - (gamma * Mr * yr)/(xr**2 + yr**2 + zr**2)**(1.5) &
       + yr * omegaz**2 - 2 * omegaz * v_xr 
  a_zr = -(gamma * Ms * zs)/(xs**2 + ys**2 + zs**2)**(1.5) - (gamma * Mr * zr)/(xs**2 + ys**2 + zs**2)**(1.5) 





  !DEFINITION DES VEKTORS W (ORTS- & GESCHWINDIGKEITSKOMPONENTEN):  
  W(1) = xr                    !x
  W(2) = yr                    !y
  W(3) = zr                    !z
  W(4) = v_xr                  !v_x 
  W(5) = v_yr                  !v_y   
  W(6) = v_zr                  !v_z
  W(7) = a_xr                  !a_x  
  W(8) = a_yr                  !a_y
  W(9) = a_zr                  !a_z




  !AUSGABE AUF KONSOLE:
  write(*,*) "---------------------------------------------------------------------"
  write(*,*) "Programm zur Berechnung der Bahn des schiefen Wurfes"
  write(*,*) "Loesungsmethode: Runge-Kutta-4ter-Ordnung" 
  write(*,*) "Programm wurde erstellt am: 01/09/2010"

  !write(*,*) t, W(4), W(5), W(6)

  write(*,*) "   t        x[m]           y[m]      z[m]    v_x[m/s]   v_y[m/s]  a_x[m/s]"

  write(*,*) "--------------------------------------------------------------------------"


  !BERECHNUNG:

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


           !############################################################################################
           K(j,i) = dt * (W(i+3) + A * K(j-1,i+3)/Mk) !BERECHNET DIE EINZELNEN MATRIXELEMENTE
           !############################################################################################


        end do
     end do




     do i = 1, 6                                      !Schleife der Koord. x,y,z,v_x,v_y,v_z 
        do j = 1, 4                                   !Schleife fuer Matrixelem. der Spalte



           if ((j == 1) .or. (j == 4)) then           !Schleife fuer den Nenner Mj = 6 bzw. 3
              Mj = 6
           else
              Mj = 3
           endif


           !############################################################################################
           W(i) = W(i) + K(j,i)/Mj                    !RUNGE-KUTTA-4TER-ORDNUNG-ALGORITHMUS
           !############################################################################################


           t = (q+1) * dt                             !q+1 vermeidet Offset-Probleme

        end do                                        !Ende der i-Schleife

     end do                                           !Ende der j-Schleife






     write(*,523) t, W(1), W(2), W(3), W(4), W(5), W(7)
523  format(F7.3, 3X, F13.3, 3X, F7.3, 3X, F7.3, 3X, F7.3, 3X, F7.3, 3X, F7.3, 3X)


  end do                                              !Ende der grossen do-Zeit-Schleife


end program TeilchenTrajektorie

