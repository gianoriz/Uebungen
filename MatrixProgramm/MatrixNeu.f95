program MatrixNeu

  implicit none

  integer :: i,j
  real a(4) , b(4) , x(2 ,4) , y(4 ,2) , z(2 ,2)
  a = (/ (i,i=1 ,4) /)
  b = a+4
  x = reshape ( (/1 ,5 ,2 ,6 ,3 ,7 ,4 ,8 /), (/2 ,4/) )
  y= transpose (x)
  z= matmul (x,y)



  write (* ,4) ' vektor a = ',a        d des Vektors a
  write (* ,4) ' sum(a) = ',sum(a)  !berechnet die Summe der Vektorkomponenten
  write (* ,4) ' product (a) = ',product (a) !berechnet das Produkt der Vektorkomponeneten 
  write (* ,4) ' maxval (a) = ',maxval (a) !(Maximal Value:: der hoechste Wert im Vektor)
  write (* ,1) ' maxloc (a) = ',maxloc (a) !zeigt an auf welcher Stelle der hoechste Wert sich befindet
  write (* ,4) ' minval (a) = ',minval (a) !(Minimal Value:: der niedrigste  Wert im Vektor)
  write (* ,1) ' minloc (a) = ',minloc (a) !zeigt an auf welcher Stelle der niedrigste befindet
  write (* ,4) ' cshift (a, -1) = ',cshift (a , -1) !Um einen Index verschieben
  write (* ,4) ' eoshift (a, -1) = ',eoshift (a , -1) !erniedrigt jeden Eintrag um eine Stelle
  write (* ,3) ' all(a <3) = ',all(a <3) 
  write (* ,3) ' any(a <3) = ',any(a <3)
  write (* ,1) ' count (a <3) = ',count (a <3) !berechne wieviele Werte kleiner 3 sind
  write (* ,4) ' vektor b = ',b 
  write (* ,2) ' dot_product (a,b) = ',dot_product (a,b) !multipliezier Vektor a mit b (Skalarprodukt)
  write (* ,4) ' matrix x = ' ,((x(i,j),j=1 ,4) ,i=1 ,2) !Definition einer 2X4 Matrix
  write (* ,2) ' transpose (x) = y = ' ,((y(i,j),j=1 ,2) ,i=1 ,4) !Transponieren
  write (* ,2) ' matmul (x,y) = z = ' ,((z(i,j),j=1 ,2) ,i=1 ,2) !Matrixmultiplikation
1 format (1x, A, I7) !Ausgabeformat 
2 format (1x, A, 2f7 .1:/ , (21x ,2 f7 .1))!Ausgabeformat
3 format (1x, A, L7)!Ausgabeformat
4 format (1x, A, 4f7 .1:/ , (21x ,4 f7 .1))!Ausgabeformat

end program MatrixNeu
