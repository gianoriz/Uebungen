PROGRAM test_sum

!Neu

  implicit none

  real :: summe
  integer :: i



  do i = 1, 7
    ! summe = summe + 1/ real(i)
    Sum(i)
  end do




  write(*,*) summe 





END PROGRAM test_sum
     
