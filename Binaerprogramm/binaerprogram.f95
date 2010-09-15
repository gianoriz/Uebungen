program binaerprogram

! Dies ist ein Programm zur Umwandlung von Dezimalzahlen in Dualzahlen
! Aufagabenstellung: Schreibe ein Programm, dass z.B. die Zahl 14 in eine 5 Bit Dualzahl umwandelt. 5 Bit heisst die Groesste Zahl die eingegeben werden kann ist die 16+8+4+2+1 = 31. Wird eine hoehere Zahl gewaehlt z.B. die 33, dann diese Zahl nicht mehr binaer (also in 0 und 1 ) umgeschrieben werden. Jedoch kann dass Programm erweitert werden inden man die Bitzahl erhoeht.   



integer :: x !Divident 
integer :: y !Divisor
integer :: i !Laufparameter von 0 bis 4
integer :: j
integer :: Rest
integer :: GZ



print *, "Geben Sie bitte eine Zahl von 1 bis 31 ein"
read *, x

do i = 0, 4

           

           j = 2**(4-i)
           Rest = mod (x,j) 
           GZ = (x-Rest)/j
           print *, GZ
           x = Rest
 


end do





end program binaerprogram
