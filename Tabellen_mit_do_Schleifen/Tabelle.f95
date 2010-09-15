program Wertetabelle



real ::    x,delta_x
integer :: Werteanzahl, j, ios
intrinsic  SIN, EXP

print *, "Berechnung einer Wertetabelle"



Ausgangswert: do

  write (*,*) "Erster x-Wert:"
  read (*,*,IOSTAT= ios) x
  if (ios == 0) exit 
  
end do Ausgangswert


Intervall: do

  write (*,*) "x-Intervall:"
  read (*,*,IOSTAT= ios) delta_x
  if (ios == 0) exit

end do Intervall


Zeilen: do 

  write(*,*) "Anzahl der Werte:"
  read (*,*,IOSTAT= ios)  Werteanzahl
  if (ios == 0) exit

end do Zeilen


!Berechnung der Funktion und Ausgabe der Wertetabele



write(*,*) "   x             f(x)          f'(x)"
write(*,*) '-----------------------------------------'



do j = 1, Werteanzahl  !j soll von 1 bis zu einer beliebigen Werteanzahl laufen 
  write (*,*) , j, x,  2.5 * SIN (2 * x) * EXP (- x ** 2 / 4)
  x = x + delta_x
end do 
 

end program Wertetabelle
