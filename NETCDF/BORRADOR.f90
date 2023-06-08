program main
    character(len=12) :: nn1
    character(len=3) :: nn2
    character(len=15) :: field
    integer :: ii,k

    nn1 = 'field.data_0'
    do ii = 400, 408, 2
      write(nn2,'(i3)')ii
      field = nn1//nn2
      print*, field

      open(unit=k,file=field,status='old',action='read')
        write(*,*) 'hola'
      close (unit=k)

    enddo

end program