program main
    use types, only: dp
    implicit none
    integer :: n
    real(dp) :: a, b, c
    real*8 :: d, e, r
    real*8, external :: f
    double precision :: g, h, i
    
    a = 2 ; b = 3 ; c = a + b
    !print*, c
    !d = 2 ; e = 3 ; f = d + e
    !print*, f
    g = 2 ; h = 3 ; i = g + h
    !print*, i

    call RANDOM_NUMBER(r)
    !print*, r

    !gnuplot
    !plot 'data.dat' with lines

    call create_data()

    !leer y guardar datos
    call read_data()

    !hacer inntegral numerica
    a = 0.0 ; b = 1.0 ; n = 10 ; r = 0.0
    !call integral(f,a,b,n,r)
    print*, r

end program main


!------------------------------------------------------------------------------------------------------------------------------
subroutine create_data()

    implicit none
    
    integer :: unidad_archivo, error, i
    real :: numero
    
    ! Abrir el archivo
    unidad_archivo = 11
    open(unit=unidad_archivo, file='datos.dat', status='unknown', action='write', iostat=error)
    
    if (error /= 0) then
      print *, "Error al abrir el archivo"
      stop
    endif
    
    ! Escribir en el archivo
    !write(unidad_archivo, *) 1.0
    !write(unidad_archivo, *) 2.0
    !write(unidad_archivo, *) 3.0

    do i = 1, 100
        call RANDOM_NUMBER(numero)
        write(unidad_archivo,*) i, numero
    enddo
    
    ! Cerrar el archivo
    close(unit=unidad_archivo, iostat=error)
    
    if (error /= 0) then
      print *, "Error al cerrar el archivo"
      stop
    endif
    
    return
end subroutine create_data


!--------------------------------------------------------------------------------------------------------------------
subroutine read_data()

    implicit none
    real*8 :: datos(100,2)
    integer :: i, j, error, unidad_archivo

    unidad_archivo = 11
    !-----
    open(unit=unidad_archivo, file='datos.dat', status='old', action='read', iostat=error)
    if (error /= 0) then
      print *, "Error al abrir el archivo"
      stop
    endif

    !------
    read(unidad_archivo,*)((datos(i,j), j = 1, 2), i = 1, 100)

    !------
    close(unit=unidad_archivo, iostat=error)
    if (error /= 0) then
      print *, "Error al cerrar el archivo"
      stop
    endif

    !--------
    do i = 1, 100
        !print*, datos(i,:)
    enddo

    return
end subroutine read_data


