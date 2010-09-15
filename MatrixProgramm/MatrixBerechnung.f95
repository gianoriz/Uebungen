program matrizen 

  implicit none




  integer :: i,j
  real , dimension (3 ,3) :: A



  do i=1 ,3
     do j=1 ,3
        A(i,j)=(i -1)*3+ j
     end do
  end do



  write (* ,21) A
  21 format (3(3 f5 .1/))










end program matrizen
