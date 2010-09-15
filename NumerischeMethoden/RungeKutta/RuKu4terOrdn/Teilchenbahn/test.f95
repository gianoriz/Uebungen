program test

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
  double precision v_0                  !Anfangsgeschw.[m/s]
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
  real :: Pi = 3.1415926535898          !Ludolphsche Zahl 



  !SETZE KONSTANTEN & PARAMETER: 

  Ms = 5685.0e23            ![kg]
  Mr = Ms!23166.0e7            ![kg]
  omegaz = 1.6096071e-5     ![1/s]
  gamma = 6.67428e-11       ![m³/kg*s²]
  v_r = 8483.27             ![m/s]
  alpha= 45.0 *(Pi/180.0)   ![rad] Das Teilchen wird in einem Winkel von 45° herausgeschlagen
  v_0  = 0.0 !20.0               ![m/s]
  dt   = 0.2                ![s]
  n    = 14                 ![Anzahl der Rechnungen]!
  t = 0.0 

 write(*,*) t


  !STARTWERTE IM RUHESYSTEM DES SATURNS:   
  xs = 263520000.0 !527040000.0         ![m]
  ys = 0.0         !0.0                 ![m]
  zs = 0.0         !1528000.0           ![m]




  !STARTWERTE IM RUHESYSTEM VON RHEA:
  xr = 263520000.0 !                    !Teilchen befinde sich im Lagrangepunkt
  yr = 0.0         !                
  zr = 0.0         !                             
  v_xr = v_0 *cos(alpha)                !Geschwindigkeit des Teilchen bei t= 0s (Sputtering)
  v_yr = 0.0
  v_zr = v_0 *sin(alpha) 

  a_xr = -(gamma * Ms * xs)/(xs**2 + ys**2 + zs**2)**(1.5) - (gamma * Mr * xr)/(xr**2 + yr**2 + zr**2)**(1.5) &
       + xr * omegaz**2 + 2 * omegaz * v_yr

  a_yr = -(gamma * Ms * ys)/(xs**2 + ys**2 + zs**2)**(1.5) - (gamma * Mr * yr)/(xr**2 + yr**2 + zr**2)**(1.5) &
       + yr * omegaz**2 - 2 * omegaz * v_xr 

  a_zr = -(gamma * Ms * zs)/(xs**2 + ys**2 + zs**2)**(1.5) - (gamma * Mr * zr)/(xs**2 + ys**2 + zs**2)**(1.5) 


  !write(*,*) dt, xr, yr, zr
  !write(*,*) dt, v_xr, v_yr, v_zr
  write(*,*) dt, a_xr, a_yr, a_zr 



  !do q = 0, n                                         !Grosse do-Zeitschleife



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

   t = (q+1) * dt  


  write(*,*) t, K(1,1), K(1,2), K(1,3), K(1,4), K(1,20)


  !write(*,*) t, xr, yr, zr
  !write(*,*) t, v_xr, v_yr, v_zr
  !write(*,*) t, a_xr, a_yr, a_zr 

  !end do


end program test
