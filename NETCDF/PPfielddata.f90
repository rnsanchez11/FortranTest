! compiling
! gfortran -o field.out PPfielddata.f90 -I/opt/homebrew/Cellar/netcdf-fortran/4.6.0_1/include -L/opt/homebrew/Cellar/netcdf-fortran/4.6.0_1/lib -lnetcdff

! This program do the postprocecing of a DNS simulation file

program PPfielddata
  use netcdf
  implicit none

  !Variables
  character(len=*), parameter :: OUT_FILE = 'data.txt'    ! Output file.
  character(len=*), parameter :: PLT_FILE = 'plot.plt'    ! Gnuplot file.
  character(len=12) :: nn1                                ! primera parte nombre del los archivos multiples
  character(len=3) :: nn2                                 ! # nombre del los archivos multiples
  character(len=15) :: field                              ! nombre de los archivos
  integer, parameter :: nx = 192, ny = 128, nz = 160      ! Dimensions
  integer :: varid, ncid, x, y, z                         ! id variable netcdf, id file netcdf, contadores x,y,z
  integer :: i, ii, fu, fu2, fu3                          ! contador, var for open files
  real*8 :: q1(nx, ny, nz)                                ! data 
  real*8 :: u, v, w, Umean, Ux                            ! work variables 
  real*8, dimension(ny,1) :: vprofile, Uxz, Ulinear
  real*8, dimension(ny,1) :: ydistance, y1, y3
  real*8, dimension(nx,1) :: xdistance, x1, x3
  real*8, dimension(nx,1) :: xprofile
  real, dimension(ny,1) :: x11, y11

  ! GRID ----------------------------------------------------------------------------------------------------------
  ! Read the y grid for plot
  open (action='read', file='ygrid.plo', unit=fu2, status='old')
    do y = 1, ny
      read(fu2,*) y1(y,1), ydistance(y,1), y3(y,1)
    enddo
  close (fu2)

  ! Read the X grid for plot
  open (action='read', file='Xgrid.plo', unit=fu2, status='old')
    do x = 1, nx
      read(fu2,*) x1(x,1), xdistance(x,1), x3(x,1)
    enddo
  close (fu2)
  
  ! Read the laminar profile
  open (action='read', file='datalaminar.txt', unit=fu3, status='old')
    read(fu3,*) x11, y11
  close (fu3)

! open various files
nn1 = "field.data_0"
  do ii = 400, 400, 2
  write(nn2,"(i3)")ii ! (i3) numero de cifras
  field = nn1//nn2
  print*, field

  ! Open file netCDF
  call check(nf90_open(field, nf90_nowrite, ncid))
  ! Get the varid of the data variable, based on its name.
  call check(nf90_inq_varid(ncid, "q1", varid))
  ! Read the data.
  call check(nf90_get_var(ncid, varid, q1))

  ! Mean velocity field
  ! U(x,t) = <U(x,t)> + u(x,t)
  ! <U(x,t)>

  ! mean velocity x-z plane
  Umean = 0
  do y = 1, ny
    do x = 1, nx
      do z = 1, nz
        Umean = Umean + (q1(x,y,z))/(nx*nz)
      enddo
    enddo
    Uxz(y,1) = Umean
    !print*, "Velocity Uxz: ", Uxz
    Umean = 0
  enddo

  
  ! Grafica de un punto de la simulacion
  do y = 1, ny
        u = q1(10,y,10)
        vprofile(y,1) = u
  enddo

  ! Open a .txt file for plot
  open (action='write', file=OUT_FILE, newunit=fu, status='replace')
  do i = 1, ny
      write (fu, *) Uxz(i,1), ydistance(i,1)
  end do
  close (fu)

  ! Turbulence intensitive
  
  
  !ejecuta el comando para graficar en gnuplot
  call execute_command_line('gnuplot -p ' // PLT_FILE)

  call check(nf90_close(ncid))

enddo

contains
subroutine check(status)
  integer, intent (in) :: status
    
  if(status /= nf90_noerr) then 
    print *, trim(nf90_strerror(status))
    stop "Stopped"
  end if
end subroutine check 

end program 


