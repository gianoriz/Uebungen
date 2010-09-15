program FreierFall

implicit none 

real :: y ! Hoehenwert 
real :: t ! Zeit
real :: Anfangshoehe
real :: dt
integer :: i

! Lege fest, wo die Ergebnisse ausgegeben werden sollen
open(7, File='/afs/.geo.uni-koeln.de/usr/rizzo/Desktop/SenkrechterWurf.txt')

print *, "Anfangshoehe angeben"
read *, Anfangshoehe


print *, "Bitte Wert für die Schrittweite angeben"
read *, dt


!write (7,*)  "   x             f(x)         "
!write (7,*)  '-----------------------------------------'



t = 0 !Offset

do i = 0, 2 * int (1/dt)
   t = i * dt
  
   y = (- 9.81 / 2) * (t**2) + (Anfangshoehe)

write (7,*) t, y


!write(7,*) " Text " 



end do 

close(7)

end program
