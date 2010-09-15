PROGRAM test_sum


  INTEGER :: x(5) = (/ 1, 2, 3, 4 ,5 /) ! 5-dimensionaler Vector

  print *, SUM(x)                       ! all elements, sum = 15

  print *, SUM(x, MASK=MOD(x, 2)==1)    ! odd elements, sum = 9



END PROGRAM test_sum
     
