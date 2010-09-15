program Matrix

implicit none

real, dimension(2,2) :: A = reshape( (/1, 3, 2, 4/), (/2, 2/) )
real, dimension(2,2) :: B = reshape( (/5, 7, 6, 8/), (/2, 2/) )
real, dimension(2,2) :: C

C = matmul(A, B)

write(*,*) "Matrix C ="
write(*,*) C

end program Matrix 




