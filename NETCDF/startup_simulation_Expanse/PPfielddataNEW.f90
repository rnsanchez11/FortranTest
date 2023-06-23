! compiling
! gfortran -o field.out PPfielddata.f90 -I/opt/homebrew/Cellar/netcdf-fortran/4.6.0_1/include -L/opt/homebrew/Cellar/netcdf-fortran/4.6.0_1/lib -lnetcdff
!LINUX
!gfortran -o field.out PPfielddata.f90 -I/usr/include -I/usr/include -L/usr/lib/x86_64-linux-gnu -lnetcdff -Wl,-Bsymbolic-functions -flto=auto -ffat-lto-objects -flto=auto -Wl,-z,relro -Wl,-z,now -lnetcdf -lnetcdf -lm

! This program do the postprocecing of a DNS simulation file

program PPfielddata
  use netcdf
  implicit none

  !Variables
  character(len=12) :: nn1                                                  ! primera parte nombre del los archivos multiples
  character(len=3) :: nn2                                                   ! # nombre del los archivos multiples
  character(len=15) :: field                                                ! nombre de los archivos

  integer, parameter :: nx = 192, ny = 128, nz = 160, nt = 249              ! Dimensions
  integer, parameter :: s = 249, ti = 400, tf = 400, tdiff = (tf - ti)/2
  integer :: varid, ncid                                                    ! id variable netcdf, id file netcdf, contadores x,y,z
  integer :: x, y, z, i, t
  integer :: fu, fu1, fu2, fu3, fu4, fu5, fu6                                         ! contador, var for open files
  integer :: fu7, fu8, fu9, f1, f2, f3, iostat
  real*8 :: q1(nx, ny, nz), q2(nx, ny, nz), q3(nx, ny, nz)                  ! data 
  real*8 :: flucx(nx,ny,nz), flucy(nx,ny,nz), flucz(nx,ny,nz), flucuv(nx,ny,nz)
  real*8 :: urms1, vrms1, wrms1, uvrms1, ut, Reynoldsc
  real*8 :: u, v, w
  real*8 :: Uxm, Ux, Uxmean                                          ! work variables 
  real*8 :: Uym, Uy, Uymean
  real*8 :: Uzm, Uz, Uzmean
  real*8, dimension(nx,1) :: xdistance, x1, x3
  real*8, dimension(nx,1) :: xprofile
  real*8, dimension(ny,1) :: Uxxz, vxprofile
  real*8, dimension(ny,1) :: Uyxz, vyprofile
  real*8, dimension(ny,1) :: Uzxz, vzprofile
  real*8, dimension(ny,1) :: ydistance, y1, y3
  real*8, dimension(ny,1) :: x11, y11
  real*8, dimension(ny,1) :: Ulaminar
  real*8, dimension(ny,1) :: Uxmeantotal, Uymeantotal, Uzmeantotal
  real*8, dimension(ny,1) :: urmsfinal, vrmsfinal, wrmsfinal
  real*8, dimension(ny,1) :: urms, vrms, wrms, uvrms
  real*8, dimension(ny,1) :: yplus
  real*8, dimension(ny*(tdiff+1),1) :: ydistancetotal, yplustotal
  real*8, dimension(ny*(tdiff+1),1) :: urmstotal, vrmstotal, wrmstotal
  real*8, dimension(ny*(tdiff+1),1) :: Uxxztotal, Uyxztotal, Uzxztotal

  real*8, parameter :: Upared = 0
  real*8, parameter :: hwc = 1
  real*8, parameter :: Uc = 1
  real*8, parameter :: rho = 1
  !real*8, parameter :: Reynoldsc = 4200
  real*8 :: viscinematica, tw
  real*8, dimension(ny,1) :: ty, viscoustress, Reynoldstress


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
  open (action='read', file='datalaminar.txt', unit=fu2, status='old')
    read(fu2,*) x11, y11
  close (fu2)


! MEAN VELOCITY PROFILE -----------------------------------------------------------------------------------------------


  ! X velocity ----------------------------------------------------------------
  open (action='write', file="Uxmean.txt", newunit=fu1, status='replace')
  ! Y velocity ----------------------------------------------------------------
  open (action='write', file="Uymean.txt", newunit=fu2, status='replace')
  ! Z velocity ----------------------------------------------------------------
  open (action='write', file="Uzmean.txt", newunit=fu3, status='replace')
    
    ! open various files
    nn1 = "field.data_0"
    do t = ti, tf, 2
      write(nn2,"(i3)")t ! (i3) numero de cifras
      field = nn1//nn2
      
      call check(nf90_open(field, nf90_nowrite, ncid)) ! Open file netCDF
      call check(nf90_inq_varid(ncid, "q1", varid))    ! Get the varid of the data variable, based on its name.
      call check(nf90_get_var(ncid, varid, q1))        ! Read the data.

      call check(nf90_open(field, nf90_nowrite, ncid)) ! Open file netCDF
      call check(nf90_inq_varid(ncid, "q2", varid))    ! Get the varid of the data variable, based on its name.
      call check(nf90_get_var(ncid, varid, q2))        ! Read the data.

      call check(nf90_open(field, nf90_nowrite, ncid)) ! Open file netCDF
      call check(nf90_inq_varid(ncid, "q3", varid))    ! Get the varid of the data variable, based on its name.
      call check(nf90_get_var(ncid, varid, q3))        ! Read the data.

      ! Grafica de un punto de la simulacion
      do y = 1, ny
        u = q1(10,y,10)
        vxprofile(y,1) = u
        v = q2(10,y,10)
        vyprofile(y,1) = v
        w = q3(10,y,10)
        vzprofile(y,1) = w
      enddo

      !PLOT ONE POINT ********************************************************************************************************
      open (action='write', file="Onepoint.txt", newunit=fu4, status='replace')
      do i = 1, ny
      write (fu4, *) vxprofile(i,1), ydistance(i,1)
      enddo 
      close (fu4)
      !call execute_command_line('gnuplot -p ' // 'plotonepoint.plt') !ejecuta el comando para graficar en gnuplot
      open (action='write', file="Onepointy.txt", newunit=fu5, status='replace')
      do i = 1, ny
      write (fu5, *) vyprofile(i,1), ydistance(i,1)
      enddo 
      close (fu5)
      !call execute_command_line('gnuplot -p ' // 'plotonepoint.plt') !ejecuta el comando para graficar en gnuplot
      open (action='write', file="Onepointw.txt", newunit=fu6, status='replace')
      do i = 1, ny
      write (fu6, *) vzprofile(i,1), ydistance(i,1)
      enddo 
      close (fu6)
      !call execute_command_line('gnuplot -p ' // 'plotonepoint.plt') !ejecuta el comando para graficar en gnuplot

      !************************************************************************************************************************



      ! MEAN VELOCITY FIELD
      ! U(x,t) = <U(x,t)> + u(x,t)

      Uxm = 0
      Uym = 0
      Uzm = 0
      do y = 1, ny
        do x = 1, nx
          do z = 1, nz
            Uxm = Uxm + (q1(x,y,z))/(nx*nz)
            Uym = Uym + (q2(x,y,z))/(nx*nz)
            Uzm = Uzm + (q3(x,y,z))/(nx*nz)
          enddo
        enddo
        Uxxz(y,1) = Uxm
        Uxm = 0
        Uyxz(y,1) = Uym
        Uym = 0
        Uzxz(y,1) = Uzm
        Uzm = 0
      enddo
      

      do i = 1, ny
          write (fu1, *) Uxxz(i,1), ydistance(i,1), t    !Write the file to plot
      end do
      do i = 1, ny
          write (fu2, *) Uyxz(i,1), ydistance(i,1), t    !Write the file to plot
      end do
      do i = 1, ny
          write (fu3, *) UZxz(i,1), ydistance(i,1), t    !Write the file to plot
      end do

    enddo
  close (fu1)
  close (fu2)
  close (fu3)


  open (action='read', file='Uxmean.txt', unit=fu7, status='old')
  do y = 1, ny*(tdiff+1)
    read(fu7,*) Uxxztotal(y,1), ydistancetotal(y,1)
  enddo
  Uxmean = 0

  Uzmean = 0
  do y = 1, ny
  do i = 0, tdiff
    Uxmean = Uxmean + Uxxztotal(y+i*128,1)/(tdiff+1)

    Uzmean = Uzmean + Uzxztotal(y+i*128,1)/(tdiff+1)
  enddo
  Uxmeantotal(y,1) = Uxmean
  Uxmean = 0

  Uzmeantotal(y,1) = Uzmean
  Uzmean = 0
  enddo

  close (fu7)
  
  open (action='read', file='Uymean.txt', unit=fu8, status='old')
  do y = 1, ny*(tdiff+1)
    read(fu8,*) Uyxztotal(y,1), ydistancetotal(y,1)
  enddo
  Uymean = 0
  do y = 1, ny
    do i = 0, tdiff
      Uymean = Uymean + Uyxztotal(y+i*128,1)/(tdiff+1)
    enddo
    Uymeantotal(y,1) = Uymean
    Uymean = 0
  enddo
  close (fu8)

  open (action='read', file='Uzmean.txt', unit=fu9, status='old')
  do y = 1, ny*(tdiff+1)
    read(fu9,*) Uzxztotal(y,1), ydistancetotal(y,1)
  enddo
  do y = 1, ny
    do i = 0, tdiff
      Uzmean = Uzmean + Uzxztotal(y+i*128,1)/(tdiff+1)
    enddo
    Uzmeantotal(y,1) = Uzmean
    Uzmean = 0
  enddo
  close (fu9)


  ! Open a .txt file for plot
  open (action='write', file='plotmeanvelx.txt', newunit=fu1, status='replace')
  do i = 1, ny
    write (fu1, *) Uxmeantotal(i,1), ydistance(i,1)
  end do 
  close (fu1)
  open (action='write', file='plotmeanvely.txt', newunit=fu2, status='replace')
  do i = 1, ny
    write (fu2, *) Uymeantotal(i,1), ydistance(i,1)
  end do 
  close (fu2)
  open (action='write', file='plotmeanvelz.txt', newunit=fu3, status='replace')
  do i = 1, ny
    write (fu3, *) Uzmeantotal(i,1), ydistance(i,1)
  end do 
  close (fu3)

  call check(nf90_close(ncid))
  call execute_command_line('gnuplot -p ' // 'plotmeanvelx.plt') !ejecuta el comando para graficar en gnuplot
  !call execute_command_line('gnuplot -p ' // 'plotmeanvely.plt') !ejecuta el comando para graficar en gnuplot
  !call execute_command_line('gnuplot -p ' // 'plotmeanvelz.plt') !ejecuta el comando para graficar en gnuplot









! TURBULENCE INTENSITY -----------------------------------------------------------------------------------------------

  Reynoldsc = 3300/Uxxz(64,1)
  viscinematica = 1/Reynoldsc
  tw = rho*viscinematica*((Uxxz(1,1))/((ydistance(1,1)+1)))

  print*, "viscinematica is ", viscinematica
  print*, "Uxz is ", Uxxz(1,1)
  print*, "ydistance is ", ydistance(128,1)
  print*, "U centerline is ", Uxxz(64,1)
  print*, "Rec is ", Reynoldsc
  print*, "tw is ",tw


  ut = sqrt(tw/rho)
  print*, "ut is ",ut
  print*, "Ret is ", ut/viscinematica
  
  
  !Distance y+
  do y = 1, ny
    yplus(y,1) = ut*(ydistance(y,1)+1)/viscinematica
  enddo
  !print*, yplus


  ! total shear stress ***************************
  do y = 1, ny
  ty(y,1) = (1-((ydistance(y,1)+1)/hwc))
  enddo
  open (action='write', file="totalshear.txt", newunit=fu, status='replace')
  do i = 1, ny
      write (fu, *) ydistance(i,1), ty(i,1)
  end do
  close (fu)

  ! Viscous stress ***********************************
  viscoustress(1,1) = tw
  do y = 2, ny
    viscoustress(y,1) = rho*viscinematica/tw*((Uxxz(y,1)-Uxxz(y-1,1))/((ydistance(y,1)+1)-(ydistance(y-1,1)+1)))
  enddo

  open (action='write', file="viscoustress.txt", newunit=fu, status='replace')
  do i = 1, ny
      write (fu, *) ydistance(i,1), viscoustress(i,1)
  end do
  close (fu)











  open (action='write', file="fluctuationsx.txt", newunit=fu1, status='replace')
  open (action='write', file="fluctuationsy.txt", newunit=fu2, status='replace')
  open (action='write', file="fluctuationsz.txt", newunit=fu3, status='replace')
  open (action='write', file="fluctuationsuv.txt", newunit=fu8, status='replace')   

  do t = ti, tf, 2
  
      ! Fluctuations in x, y, z
      flucx = 0
      flucy = 0
      flucz = 0
      flucuv = 0
      do y = 1, ny
        do x = 1, nx
          do z = 1, nz
            flucx(x,y,z) = (q1(x,y,z) - Uxxz(y,1))**2
            flucy(x,y,z) = (q2(x,y,z) - Uyxz(y,1))**2
            flucz(x,y,z) = (q3(x,y,z) - Uzxz(y,1))**2
            flucuv(x,y,z) = (q1(x,y,z) - Uxxz(y,1))*(q2(x,y,z) - Uyxz(y,1))
          enddo
        enddo
      enddo

      urms1 = 0
      vrms1 = 0
      wrms1 = 0
      uvrms1 = 0
      do y = 1, ny
        do x = 1, nx
          do z = 1, nz
            urms1 = urms1 + (sqrt(flucx(x,y,z)))/(nx*nz)
            vrms1 = vrms1 + (sqrt(flucy(x,y,z)))/(nx*nz)
            wrms1 = wrms1 + (sqrt(flucz(x,y,z)))/(nx*nz)
            uvrms1 = uvrms1 + ((flucuv(x,y,z)))/(nx*nz)
          enddo
        enddo
        urms(y,1) = urms1/ut
        urms1 = 0
        vrms(y,1) = vrms1/ut
        vrms1 = 0
        wrms(y,1) = wrms1/ut
        wrms1 = 0
        uvrms(y,1) = uvrms1
        uvrms1 = 0
      enddo
      !print*, urms

      do i = 1, ny
        write (fu1, *) urms(i,1), yplus(i,1), t    !Write the file to plot
      end do
      do i = 1, ny
        write (fu2, *) vrms(i,1), yplus(i,1), t    !Write the file to plot
      end do
      do i = 1, ny
        write (fu3, *) wrms(i,1), yplus(i,1), t    !Write the file to plot
      end do
      do i = 1, ny
        write (fu8, *) uvrms(i,1), yplus(i,1), t    !Write the file to plot
      end do
    enddo
  close (fu1)
  close (fu2)
  close (fu3)
  close (fu8)
  
  open (action='read', file='fluctuationsx.txt', unit=f1, status='old')
    do y = 1, ny*(tdiff+1)
      read(f1,*,IOSTAT=iostat) urmstotal(y,1), yplustotal(y,1)
    enddo

    Ux = 0
    do y = 1, ny
      do i = 0, tdiff
        Ux = Ux + urmstotal(y+i*128,1)/(tdiff+1)
      enddo
      urmsfinal(y,1) = Ux
      Ux = 0
    enddo
  close (f1)

  open (action='read', file='fluctuationsy.txt', unit=f2, status='old')
    do y = 1, ny*(tdiff+1)
      read(f2,*,IOSTAT=iostat) vrmstotal(y,1), yplustotal(y,1)
    enddo

    Uy = 0
    do y = 1, ny
      do i = 0, tdiff
        Uy = Uy + vrmstotal(y+i*128,1)/(tdiff+1)
      enddo
      vrmsfinal(y,1) = Uy
      Uy = 0
    enddo
  close (f2)

  open (action='read', file='fluctuationsz.txt', unit=f3, status='old')
    do y = 1, ny*(tdiff+1)
      read(f3,*,IOSTAT=iostat) wrmstotal(y,1), yplustotal(y,1)
    enddo

    Uz = 0
    do y = 1, ny
      do i = 0, tdiff
        Uz = Uz + wrmstotal(y+i*128,1)/(tdiff+1)
      enddo
      wrmsfinal(y,1) = Uz
      Uz = 0
    enddo
  close (f3)


  open (action='write', file="dataTurbIntx.txt", newunit=fu, status='replace')
  do i = 1, ny
      write (fu, *) ydistance(i,1), urmsfinal(i,1)
  end do
  close (fu)

  open (action='write', file="dataTurbInty.txt", newunit=fu1, status='replace')
  do i = 1, ny
      write (fu1, *) ydistance(i,1), vrmsfinal(i,1)
  end do
  close (fu1)

  open (action='write', file="dataTurbIntz.txt", newunit=fu2, status='replace')
  do i = 1, ny
      write (fu2, *) ydistance(i,1), wrmsfinal(i,1)
  end do
  close (fu2)

  call execute_command_line('gnuplot -p ' // "plotTurbInt.plt")

  open (action='write', file="dataTurbIntypx.txt", newunit=fu, status='replace')
  do i = 1, ny/2
      write (fu, *) yplus(i,1), urmsfinal(i,1)
  end do
  close (fu)
  call execute_command_line('gnuplot -p ' // "plotTurbIntyp.plt")



  !print*, "uv", uvrms
  ! Reynolds stress
  do y = 1, ny
    Reynoldstress(y,1) = -rho/tw*uvrms(y,1)
  enddo

  open (action='write', file="Restress.txt", newunit=fu, status='replace')
  do i = 1, ny
      write (fu, *) ydistance(i,1), Reynoldstress(i,1)
  end do
  close (fu)

  call execute_command_line('gnuplot -p ' // "plotTotalstress.plt")









  contains
  subroutine check(status)
    integer, intent (in) :: status
      
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check 

end program 


