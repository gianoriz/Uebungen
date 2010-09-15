program Restdivision

  integer :: x
  integer :: y
  integer :: z

  print *, "Zaehler eingeben"
  read *, x

  print *, "Nenner eingeben"
  read *, y


  z = mod (x,y)

  print *, "Der Teiler lautet"
  print *, (x-z)/y


  print *, "Der Rest lautet"
  print *, z 




end program Restdivision
