! compiling
! gfortran -o field.out PPfielddata.f90 -I/opt/homebrew/Cellar/netcdf-fortran/4.6.0_1/include -L/opt/homebrew/Cellar/netcdf-fortran/4.6.0_1/lib -lnetcdff

! This program do the postprocecing of a DNS simulation file

program PPfielddata
    use netcdf
    implicit none

    !Variables
    character(len=*), parameter :: OUT_FILE = 'data.txt'    ! Output file.
    character(len=*), parameter :: PLT_FILE = 'plot.plt'    ! Gnuplot file.
    integer, parameter :: nx = 192, ny = 128, nz = 160      ! Dimensions
    integer :: varid, ncid, x, y, z                         ! id variable netcdf, id file netcdf, contadores x,y,z
    integer :: i, fu, fu2                                   ! contador, var for open files
    real*8 :: q1(nx, ny, nz)                                ! data 
    real*8 :: u, v, w, Umean, Ux                            ! work variables 
    real*8, dimension(ny,1) :: vprofile, ydistance, y1, y3
    


    ! Open file netCDF
    call check(nf90_open("field.data_0400", nf90_nowrite, ncid))
    ! Get the varid of the data variable, based on its name.
    call check(nf90_inq_varid(ncid, "q1", varid))
    ! Read the data.
    call check(nf90_get_var(ncid, varid, q1))

    ! Mean velocity field
    ! U(x,t) = <U(x,t)> + u(x,t)
    ! <U(x,t)>

    Umean = 0
    do x = 1, nx
      do y = 1, ny
        do z = 1, nz
          Umean = Umean + (q1(x,ny/2,nz/2))/(nx*ny*nz)
        enddo
      enddo
    enddo
    print*, "Velocity Umean: ", Umean
    
    ! Grafica de un punto de la simulacion
    do y = 1, ny
          u = q1(10,y,10)
          vprofile(y,1) = u
    enddo
    !print*, "Velocity Fluctuations: ", vprofile

    ! Read the y grid for plot
    open (action='read', file='ygrid.plo', unit=fu2, status='old')
      do y = 1, ny
        read(fu2,*) y1(y,1), ydistance(y,1), y3(y,1)
      enddo
      !print*, ydistance
    close (fu2)

    ! Open a .txt file for plot
    open (action='write', file=OUT_FILE, newunit=fu, status='replace')
    do i = 1, ny
        write (fu, *) vprofile(i,1), ydistance(i,1)
    end do
    close (fu)
  
    !ejecuta el comando para graficar en gnuplot
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


