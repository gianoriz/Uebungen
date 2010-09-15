program und

implicit none

integer :: m, n, i, j   
integer  :: x, y



x = 0! Offset 

do i = 0, 10 

y = x

x = x + 1 


!print *, x  

if ( x > 5 .and. y - 1 > 4 ) then 
print *, x, y

end if


end do

 

end program
