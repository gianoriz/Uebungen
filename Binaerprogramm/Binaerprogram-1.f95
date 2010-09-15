program binaerprogram

implicit none

! Dies ist ein Programm zur Umwandlung von Dezimalzahlen in Dualzahlen
! Aufagabenstellung: Schreibe ein Programm, dass z.B. die Zahl 14 in eine 5 Bit Dualzahl umwandelt. 5 Bit heisst die Groesste Zahl die eingegeben werden kann ist die 16+8+4+2+1 = 31. Wird eine hoehere Zahl gewaehlt z.B. die 33, dann diese Zahl nicht mehr binaer (also in 0 und 1 ) umgeschrieben werden. Jedoch kann dass Programm erweitert werden inden man die Bitzahl erhoeht.   



integer :: Eingabezahl !Wandelt die Eingabezahl in Binaerzahlen um 
integer :: i !Laufparameter von 0 bis b mit b= Bitzahl
integer :: j ! Zweierpotenz
integer :: Rest !Gibt mir beim berechnen des Quotienten (zwischen Eingabezahl & Zweierpotenz) den Rest an 
integer :: GZ ! Ist die Zahl, die ausgegeben wird
integer :: ja = 1
integer :: nein = 0
integer :: GabEsSchonNeEins = 0
integer :: b !Anzahl meiner Bits 


print *, "Geben Sie bitte eine Zahl von 1 bis 31 ein"
read *, Eingabezahl 

b = 25


do i = 0, b
           
           j = 2**(b-i)
           Rest = mod (Eingabezahl,j) 
           GZ = (Eingabezahl-Rest)/j
           Eingabezahl = Rest !D.h., rechne mit dem Rest weiter
           


if (GZ == 1) then !Wenn mein GZ gleich 1 ist dann wird GabEsSchonNeEins auf 1 gesetzt
GabEsSchonNeEins = 1
end if     



if (GabEsSchonNeEins == 1) then ! und erst wenn nachdem GabEsSchonNeEins auf 1 gesetzt wurde, dann werden die Binaerzahlen ausgegeben  
print *, GZ
end if




end do


end program binaerprogram
