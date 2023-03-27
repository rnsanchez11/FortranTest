program operaciones

! estos son algunos operadores aritmeticos
! ** exponente
! * / + - multiplicacion suma resta normales

    real :: pi
    real :: radius
    real :: height
    real :: area
    real :: volume

    pi = 3.14159

    print *, 'enter cylinder base radius'
    read (*,*) radius

    print *, 'enter cylinder base radius'
    read (*,*) height

    area = pi * radius**2
    volume = area * height

    print *, 'Cylinder radius is: ', radius
    print *, 'Cylinder height is: ', height
    print *, 'Cylinder base area is: ', area
    print *, 'Cylinder volume is: ', volume

end program operaciones

