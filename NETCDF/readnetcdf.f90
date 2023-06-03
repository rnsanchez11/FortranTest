program read_netcdf
    use netcdf
    implicit none

    !Variables
    integer, parameter :: nx = 192, ny = 128, nz = 160
    real*8 :: data_in(nx, ny, nz)
    integer :: varid, ncid, x, y, z
    real*8 :: u, v, w, q1, Umean
    real*8, dimension(ny) :: vprofile, ydistance
    character(len=*), parameter :: OUT_FILE = 'data.txt' ! Output file.
    character(len=*), parameter :: PLT_FILE = 'plot.plt' ! Gnuplot file.
    integer :: i, fu !gnuplot var


    ! Open file netCDF
    call check(nf90_open("field.data_0400", nf90_nowrite, ncid))
    ! Get the varid of the data variable, based on its name.
    call check(nf90_inq_varid(ncid, "q1", varid))
    ! Read the data.
    call check(nf90_get_var(ncid, varid, data_in))

    ! Mean velocity field
    ! U(x,t) = <U(x,t)> + u(x,t)
    v = (data_in(192,128,160) + data_in(191,127,159))/2
    print*, "Velocity in x: ", v

    ! <U(x,t)>
    Umean = 0
    do x = 1, nx
      do y = 1, ny
        do z = 1, nz
          Umean = Umean + (data_in(x,ny/2,nz/2))/(nx*ny*nz)
        enddo
      enddo
    enddo
    print*, "Velocity Umean: ", Umean

    ! Fluctuations
    do y = 1, ny
          u = data_in(90,y,80)
          vprofile(y) = u
          ydistance(y) = (y-64)
    enddo
    print*, "Velocity Fluctuations: ", vprofile

    ! Open a .txt file for plot
    open (action='write', file=OUT_FILE, newunit=fu, status='replace')
    do i = 1, ny
        write (fu, *) vprofile(i), ydistance(i)
    end do
    close (fu)

    call execute_command_line('gnuplot -p ' // PLT_FILE)

    call check( nf90_close(ncid))

    contains
    subroutine check(status)
      integer, intent (in) :: status
      
      if(status /= nf90_noerr) then 
        print *, trim(nf90_strerror(status))
        stop "Stopped"
      end if
    end subroutine check 

end program 


