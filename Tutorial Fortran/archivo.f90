program archivo

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

    end program archivo
    