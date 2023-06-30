
! gfortran -o field.out meanvelocityonetime.f90 -I/usr/include -I/usr/include -L/usr/lib/x86_64-linux-gnu -lnetcdff -Wl,-Bsymbolic-functions -flto=auto -ffat-lto-objects -flto=auto -Wl,-z,relro -Wl,-z,now -lnetcdf -lnetcdf -lm

program meanvelocity
    use netcdf
    implicit none
  
    !Variables
    character(len=12) :: nn1                                                  ! primera parte nombre del los archivos multiples
    character(len=3) :: nn2                                                   ! # nombre del los archivos multiples
    character(len=15) :: field                                                ! nombre de los archivos
  
    integer, parameter :: nx = 192, ny = 128, nz = 160, nt = 249              ! Dimensions
    integer, parameter :: s = 249, ti = 0, tf = 0, tdiff = (tf - ti)/2
    integer :: varid, ncid                                                    ! id variable netcdf, id file netcdf, contadores x,y,z
    integer :: x, y, z, i, t, iii
    integer :: fu, fu1, fu2, fu3, fu4, fu5, fu6                                         ! contador, var for open files
    integer :: fu7, fu8, fu9, f1, f2, f3, iostat
    real*8 :: q1(nx, ny, nz), q2(nx, ny, nz), q3(nx, ny, nz)                  ! data 
    real*8 :: flucx(nx,ny,nz), flucy(nx,ny,nz), flucz(nx,ny,nz), flucuv(nx,ny,nz)
    real*8 :: urms1, vrms1, wrms1, uvrms1, ut, Reynoldsc
    real*8 :: u, v, w
    real*8 :: Uxm, Ux, Uxmean                                          ! work variables 
    real*8 :: Uym, Uy, Uymean
    real*8 :: Uzm, Uz, Uzmean, UV
    real*8, dimension(nx,1) :: xdistance, x1, x3
    real*8, dimension(nx,1) :: xprofile
    real*8, dimension(ny,1) :: Uxxz, vxprofile
    real*8, dimension(ny,1) :: Uyxz, vyprofile
    real*8, dimension(ny,1) :: Uzxz, vzprofile
    real*8, dimension(ny,1) :: ydistance, y1, y3
    real*8, dimension(ny,1) :: x11, y11
    real*8, dimension(ny,1) :: Ulaminar
    real*8, dimension(ny,1) :: Uxmeantotal, Uymeantotal, Uzmeantotal
    real*8, dimension(ny,1) :: urmsfinal, vrmsfinal, wrmsfinal, uvrmsfinal
    real*8, dimension(ny,1) :: urms, vrms, wrms, uvrms
    real*8, dimension(ny,1) :: yplus
    real*8, dimension(ny*(tdiff+1),1) :: ydistancetotal, yplustotal
    real*8, dimension(ny*(tdiff+1),1) :: urmstotal, vrmstotal, wrmstotal, uvrmstotal
    real*8, dimension(ny*(tdiff+1),1) :: Uxxztotal, Uyxztotal, Uzxztotal


    open (action='read', file='ygrid.plo', unit=fu2, status='old')
        do y = 1, ny
          read(fu2,*) y1(y,1), ydistance(y,1), y3(y,1)
        enddo
    close (fu2)

    open (action='write', file="Uxmean_0050.txt", newunit=fu, status='replace')
 
    call check(nf90_open("field.data_0050", nf90_nowrite, ncid)) ! Open file netCDF
    call check(nf90_inq_varid(ncid, "q1", varid))    ! Get the varid of the data variable, based on its name.
    call check(nf90_get_var(ncid, varid, q1))        ! Read the data.

    Ux = 0
    do y = 1, ny
      do x = 1, nx
        do z = 1, nz
          Ux = Ux + (q1(x,y,z))/(nx*nz)
        enddo
      enddo
      Uxxz(y,1) = Ux
      Ux = 0
    enddo
    

    do i = 1, ny
        write (fu, *) Uxxz(i,1), ydistance(i,1), t    !Write the file to plot
    end do

    close (fu)
    call check(nf90_close(ncid))
    call execute_command_line('gnuplot -p ' // 'plotmeanonetime.plt') !ejecuta el comando para graficar en gnuplot
  


    contains
    subroutine check(status)
      integer, intent (in) :: status
        
      if(status /= nf90_noerr) then 
        print *, trim(nf90_strerror(status))
        stop "Stopped"
      end if
    end subroutine check 

end program