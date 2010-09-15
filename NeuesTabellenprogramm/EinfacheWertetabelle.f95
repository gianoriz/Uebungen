program EinfacheWertetabelle 

!Dieses Programm berechnet eine Wertetabelle der Funktion f(x)=x^2


integer :: i 
real :: x



! Der Offset liegt bei x=-11, Wichtig: der Offset liegt immer außerhalb von der do-Schleife
x=-11



do i = -11, 9  !i läuft von -11 bis 9 damit wir eine Wertetabelle von -10 bis 10 bekommen, da x=i+1 ist 

   x = i + 1   
   y = x**2

print *, x, y

end do



!print *, x, x**2


end program EinfacheWertetabelle
