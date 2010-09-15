program IfSchleife

real :: x
integer :: i,n


do i = 0, 9
   x = i + 1 
   y = x**2

print *, x, y

if (i == 9) then    !Wenn i bei 10 angekommen ist
do  n = 0,10        !dann Zähle weiter von 0 bis 10 jedoch umgekehrt
x = i - n           !die Umkehrung wird mit x = i -n erzwungen
y = x**2

print *,  x, y

end do

end if 

end do 



end program IfSchleife

