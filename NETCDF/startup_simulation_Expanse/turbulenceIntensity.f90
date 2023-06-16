! compiling
! gfortran -o field.out turbulenceIntensity.f90 -I/opt/homebrew/Cellar/netcdf-fortran/4.6.0_1/include -L/opt/homebrew/Cellar/netcdf-fortran/4.6.0_1/lib -lnetcdff

! This program do the postprocecing of a DNS simulation file
! using 1:2 with linespoints  , (-(x)**2 - 1) with lines
program meanprofile
    use netcdf
    implicit none

  !Variables
    character(len=*), parameter :: OUT_FILE = 'data.txt'    ! Output file.
    character(len=*), parameter :: PLT_FILE = 'plot2.plt'    ! Gnuplot file.
    character(len=12) :: nn1                                ! primera parte nombre del los archivos multiples
    character(len=3) :: nn2                                 ! # nombre del los archivos multiples
    character(len=15) :: field                              ! nombre de los archivos
    integer, parameter :: nx = 192, ny = 128, nz = 160      ! Dimensions
    integer, parameter :: s = 249, ti = 400, tf = 400, tdiff = (tf - ti)/2
    integer :: varid, ncid, x, y, z                         ! id variable netcdf, id file netcdf, contadores x,y,z
    integer :: i, t, fu, fu2, fu3                           ! contador, var for open files
    real*8 :: q1(nx, ny, nz)                                ! data 
    real*8 :: u, v, w, Um, Ux, Umean                            ! work variables 
    real*8, dimension(ny,1) :: vprofile, Uxz, Ulinear
    real*8, dimension(ny,1) :: ydistance, y1, y3
    real*8, dimension(nx,1) :: xdistance, x1, x3
    real*8, dimension(nx,1) :: xprofile
    real*8, dimension(ny,1) :: x11, y11, Umeantotal
    real*8, dimension(nx, ny, nz, s) :: Up
    real*8, dimension(ny*(tdiff+1),1) :: Uxztotal, ydistancetotal

    real*8, parameter :: Upared = 1
    real*8, parameter :: hwc = 1
    real*8, parameter :: Uc = 1
    real*8, parameter :: rho = 1
    real*8, parameter :: Reynoldsc = 3300
    real*8, parameter :: Reynoldst = 180
  
    real*8 :: viscinematica, tw
    real*8, dimension(ny,1) :: ty

  ! TURBULENCE INTENSITY -----------------------------------------------------------------------------------------------

  ! open various files
  nn1 = 'field.data_0'
  do t = ti, tf, 2
  write(nn2,"(i3)")t ! (i3) numero de cifras
  field = nn1//nn2
  print*, field

  ! Open file netCDF
  call check(nf90_open(field, nf90_nowrite, ncid))
  ! Get the varid of the data variable, based on its name.
  call check(nf90_inq_varid(ncid, "q1", varid))
  ! Read the data.
  call check(nf90_get_var(ncid, varid, q1))

  viscinematica = 1/Reynoldsc
  !print*, viscinematica

  tw = rho*viscinematica*((Uxz(1,1) - Upared)/ydistance(1,1))

  do y = 1, ny
  ty(y,1) = tw*(1-(y/hwc))
  enddo


  !uf = 0
  do y = 1, ny
    do x = 1, nx
      do z = 1, nz
        !uf(x,y,z) = q1(x,y,z) - Uxz(y,1)
      enddo
    enddo
  enddo


  ! Read the y grid for plot
  open (action='read', file='ygrid.plo', unit=fu2, status='old')
    do y = 1, ny
     read(fu2,*) y1(y,1), ydistance(y,1), y3(y,1)
   enddo
  close (fu2)

  ! Open a .txt file for plot
  open (action='write', file=OUT_FILE, newunit=fu, status='replace')
    do i = 1, ny
    !write (fu, *) uf(1,i,1), ydistance(i,1)
    end do
  close (fu)

  ! Open a .txt file for plot
  open (action='write', file=OUT_FILE, newunit=fu, status='replace')
    do i = 1, ny
        write (fu, *) ty(i,1), ydistance(i,1)
    end do
  close (fu)

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