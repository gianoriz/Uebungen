program Vertauschen 




real :: x, y
real :: a !Zwischenspeicher

print *, "Geben Sie bitte x ein:"
read *, x

print *, "Geben Sie bitte y ein:"
read *, y


a = y !merke das a = y war

y = x !vertausche y mit x

x = a !nun setze x gleich dem alten Wert von y welches mit a vermerkt wurde
 

print *, x, y  

end program Vertauschen
