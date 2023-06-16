! compiling
! gfortran -o field.out meanprofile.f90 -I/opt/homebrew/Cellar/netcdf-fortran/4.6.0_1/include -L/opt/homebrew/Cellar/netcdf-fortran/4.6.0_1/lib -lnetcdff

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
  integer, parameter :: ti = 400, tf = 400, tdiff = (tf - ti)/2
  integer :: varid, ncid, x, y, z                         ! id variable netcdf, id file netcdf, contadores x,y,z
  integer :: i, t, fu, fu2, fu3                           ! contador, var for open files
  real*8 :: q1(nx, ny, nz)                                ! data 
  real*8 :: u, v, w, Um, Umean                            ! work variables 
  real*8, dimension(ny,1) :: vprofile, Uxz, Ulinear
  real*8, dimension(ny,1) :: ydistance, y1, y3
  real*8, dimension(nx,1) :: xdistance, x1, x3
  real*8, dimension(nx,1) :: xprofile
  real*8, dimension(ny,1) :: x11, y11, Umeantotal
  real*8, dimension(ny*(tdiff+1),1) :: Uxztotal, ydistancetotal



  ! GRID ----------------------------------------------------------------------------------------------------------
  ! Read the y grid for plot
  open (action='read', file='ygrid.plo', unit=fu2, status='old')
    do y = 1, ny
      read(fu2,*) y1(y,1), ydistance(y,1), y3(y,1)
    enddo
  close (fu2)

  ! Read the X grid for plot
  open (action='read', file='xgrid.plo', unit=fu2, status='old')
    do x = 1, nx
      read(fu2,*) x1(x,1), xdistance(x,1), x3(x,1)
    enddo
  close (fu2)
    
  ! Read the laminar profile
  open (action='read', file='datalaminar.txt', unit=fu3, status='old')
    read(fu3,*) x11, y11
  close (fu3)


  ! MEAN VELOCITY PROFILE -----------------------------------------------------------------------------------------------
  ! Open a .txt file for plot
  open (action='write', file="Umean.txt", newunit=fu, status='replace')
    
    ! open various files
    nn1 = "field.data_0"
    do t = ti, tf, 2
    write(nn2,"(i3)")t ! (i3) numero de cifras
    field = nn1//nn2

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
    Um = 0
    do y = 1, ny
      do x = 1, nx
        do z = 1, nz
          Um = Um + (q1(x,y,z))/(nx*nz)
        enddo
      enddo
      Uxz(y,1) = Um
      !print*, "Velocity Uxz: ", Uxz
      Um = 0
    enddo
    
    ! Grafica de un punto de la simulacion
    do y = 1, ny
          u = q1(10,y,10)
          vprofile(y,1) = u
    enddo

    do i = 1, ny
        write (fu, *) Uxz(i,1), ydistance(i,1), t
    end do

    call check(nf90_close(ncid))
    enddo
  close (fu)

  open (action='read', file='Umean.txt', unit=fu2, status='old')
    do y = 1, ny*(tdiff+1)
    read(fu2,*) Uxztotal(y,1), ydistancetotal(y,1)
    enddo

    Umean = 0
    do y = 1, ny
    do i = 0, tdiff
      Umean = Umean + Uxztotal(y+i*128,1)/(tdiff+1)
    enddo
    Umeantotal(y,1) = Umean
    Umean = 0
    enddo
  close (fu2)

  ! Open a .txt file for plot
  open (action='write', file=OUT_FILE, newunit=fu, status='replace')
  do i = 1, ny
    write (fu, *) Umeantotal(i,1), ydistance(i,1)
  end do 
  close (fu)


!ejecuta el comando para graficar en gnuplot
call execute_command_line('gnuplot -p ' // PLT_FILE)


  contains
  subroutine check(status)
    integer, intent (in) :: status
      
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check 
  
end program 