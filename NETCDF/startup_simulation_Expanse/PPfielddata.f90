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
  integer, parameter :: s = 249, ti = 300, tf = 400, tdiff = (tf - ti)/2
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
  ! Open a .txt file for plot
  open (action='write', file="Uxmean.txt", newunit=fu, status='replace')
    
    ! open various files
    nn1 = "field.data_0"
    do t = ti, tf, 2
      write(nn2,"(i3)")t ! (i3) numero de cifras
      field = nn1//nn2
      !print*, field
      
      call check(nf90_open(field, nf90_nowrite, ncid)) ! Open file netCDF
      call check(nf90_inq_varid(ncid, "q1", varid))    ! Get the varid of the data variable, based on its name.
      call check(nf90_get_var(ncid, varid, q1))        ! Read the data.

      ! Grafica de un punto de la simulacion
      do y = 1, ny
        u = q1(10,y,10)
        vxprofile(y,1) = u
      enddo

      open (action='write', file="Onepointx.txt", newunit=fu2, status='replace')
      do i = 1, ny
      write (fu2, *) vxprofile(i,1), ydistance(i,1)
      enddo 
      close (fu2)
      !call execute_command_line('gnuplot -p ' // 'plotonepoint.plt') !ejecuta el comando para graficar en gnuplot

      ! Mean velocity field: U(x,t) = <U(x,t)> + u(x,t)
      ! mean velocity x-z plane
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

    enddo
  close (fu)

  open (action='read', file='Uxmean.txt', unit=fu3, status='old')
    do y = 1, ny*(tdiff+1)
    read(fu3,*,IOSTAT=iostat) Uxxztotal(y,1), ydistancetotal(y,1)
    enddo

    Uxmean = 0
    do y = 1, ny
    do i = 0, tdiff
      Uxmean = Uxmean + Uxxztotal(y+i*128,1)/(tdiff+1)
    enddo
    Uxmeantotal(y,1) = Uxmean
    Uxmean = 0
    enddo
  close (fu3)

  ! Open a .txt file for plot
  open (action='write', file='plotmeanvelx.txt', newunit=fu, status='replace')
  do i = 1, ny
    write (fu, *) Uxmeantotal(i,1), ydistance(i,1)
  end do 
  close (fu)
  call check(nf90_close(ncid))
  call execute_command_line('gnuplot -p ' // 'plotmeanvelx.plt') !ejecuta el comando para graficar en gnuplot








  ! Y velocity ----------------------------------------------------------------
  ! Open a .txt file for plot
  open (action='write', file="Uymean.txt", newunit=fu, status='replace')
    
    ! open various files
    nn1 = "field.data_0"
    do t = ti, tf, 2
      write(nn2,"(i3)")t ! (i3) numero de cifras
      field = nn1//nn2

      
      call check(nf90_open(field, nf90_nowrite, ncid)) ! Open file netCDF
      call check(nf90_inq_varid(ncid, "q2", varid))    ! Get the varid of the data variable, based on its name.
      call check(nf90_get_var(ncid, varid, q2))        ! Read the data.

      ! Grafica de un punto de la simulacion
      do y = 1, ny
        v = q2(10,y,10)
        vyprofile(y,1) = v
      enddo

      open (action='write', file="Onepointy.txt", newunit=fu2, status='replace')
      do i = 1, ny
      write (fu2, *) vyprofile(i,1), ydistance(i,1)
      enddo 
      close (fu2)
      !call execute_command_line('gnuplot -p ' // 'plotonepoint.plt') !ejecuta el comando para graficar en gnuplot

      ! Mean velocity field: U(x,t) = <U(x,t)> + u(x,t)
      ! mean velocity x-z plane
      Uy = 0
      do y = 1, ny
        do x = 1, nx
          do z = 1, nz
            Uy = Uy + (q2(x,y,z))/(nx*nz)
          enddo
        enddo
        Uyxz(y,1) = Uy
        Uy = 0
      enddo
      

      do i = 1, ny
          write (fu, *) Uyxz(i,1), ydistance(i,1), t    !Write the file to plot
      end do

    enddo
  close (fu)

  open (action='read', file='Uymean.txt', unit=fu3, status='old')
    do y = 1, ny*(tdiff+1)
    read(fu3,*) Uyxztotal(y,1), ydistancetotal(y,1)
    enddo

    Uymean = 0
    do y = 1, ny
    do i = 0, tdiff
      Uymean = Uymean + Uyxztotal(y+i*128,1)/(tdiff+1)
    enddo
    Uymeantotal(y,1) = Uymean
    Uymean = 0
    enddo
  close (fu3)

  ! Open a .txt file for plot
  open (action='write', file='plotmeanvely.txt', newunit=fu, status='replace')
  do i = 1, ny
    write (fu, *) Uymeantotal(i,1), ydistance(i,1)
  end do 
  close (fu)
  call check(nf90_close(ncid))
  !call execute_command_line('gnuplot -p ' // 'plotmeanvely.plt') !ejecuta el comando para graficar en gnuplot

  






    ! Z velocity ----------------------------------------------------------------
  ! Open a .txt file for plot
  open (action='write', file="Uzmean.txt", newunit=fu, status='replace')
    
    ! open various files
    nn1 = "field.data_0"
    do t = ti, tf, 2
      write(nn2,"(i3)")t ! (i3) numero de cifras
      field = nn1//nn2

      
      call check(nf90_open(field, nf90_nowrite, ncid)) ! Open file netCDF
      call check(nf90_inq_varid(ncid, "q3", varid))    ! Get the varid of the data variable, based on its name.
      call check(nf90_get_var(ncid, varid, q3))        ! Read the data.

      ! Grafica de un punto de la simulacion
      do y = 1, ny
        w = q3(10,y,10)
        vzprofile(y,1) = w
      enddo

      open (action='write', file="Onepointz.txt", newunit=fu2, status='replace')
      do i = 1, ny
      write (fu2, *) vzprofile(i,1), ydistance(i,1)
      enddo 
      close (fu2)
      !call execute_command_line('gnuplot -p ' // 'plotonepoint.plt') !ejecuta el comando para graficar en gnuplot

      ! Mean velocity field: U(x,t) = <U(x,t)> + u(x,t)
      ! mean velocity x-z plane
      Uz = 0
      do y = 1, ny
        do x = 1, nx
          do z = 1, nz
            Uz = Uz + (q3(x,y,z))/(nx*nz)
          enddo
        enddo
        Uzxz(y,1) = Uz
        Uz = 0
      enddo
      

      do i = 1, ny
          write (fu, *) Uzxz(i,1), ydistance(i,1), t    !Write the file to plot
      end do

    enddo
  close (fu)

  open (action='read', file='Uzmean.txt', unit=fu3, status='old')
    do y = 1, ny*(tdiff+1)
    read(fu3,*) Uzxztotal(y,1), ydistancetotal(y,1)
    enddo

    Uzmean = 0
    do y = 1, ny
    do i = 0, tdiff
      Uzmean = Uzmean + Uzxztotal(y+i*128,1)/(tdiff+1)
    enddo
    Uzmeantotal(y,1) = Uzmean
    Uzmean = 0
    enddo
  close (fu3)

  ! Open a .txt file for plot
  open (action='write', file='plotmeanvelz.txt', newunit=fu, status='replace')
  do i = 1, ny
    write (fu, *) Uzmeantotal(i,1), ydistance(i,1)
  end do 
  close (fu)
  call check(nf90_close(ncid))
  !call execute_command_line('gnuplot -p ' // 'plotmeanvely.plt') !ejecuta el comando para graficar en gnuplot







! TURBULENCE INTENSITY -----------------------------------------------------------------------------------------------

  Reynoldsc = 3300/Uxxz(64,1)
  viscinematica = 1/Reynoldsc
  print*, "viscinematica is ", viscinematica
  print*, "Uxz is ", Uxxz(1,1)
  print*, "ydistance is ", ydistance(128,1)
  print*, "U centerline is ", Uxxz(64,1)
  print*, "Rec is ", Reynoldsc
 
  tw = rho*viscinematica*((Uxxz(1,1))/((ydistance(1,1)+1)))
  !tw = rho*viscinematica*((((Uxz(1,1) + Uxz(2,1))/2) - Upared)/((ydistance(1,1)+ydistance(2,1))/2))
  !tw = 0.00175
  print*, "tw is ",tw

  ! total shear stress
  do y = 1, ny
  ty(y,1) = (1-((ydistance(y,1)+1)/hwc))
  enddo

  viscoustress(1,1) = tw
  ! viscous stress
  do y = 2, ny
    viscoustress(y,1) = rho*viscinematica/tw*((Uxxz(y,1)-Uxxz(y-1,1))/((ydistance(y,1)+1)-(ydistance(y-1,1)+1)))
  enddo

  open (action='write', file="viscoustress.txt", newunit=fu, status='replace')
  do i = 1, ny
      write (fu, *) ydistance(i,1), viscoustress(i,1)
  end do
  close (fu)

  ut = sqrt(tw/rho)
  print*, "ut is ",ut
  print*, "Ret is ", ut/viscinematica
  !Distance y+
  do y = 1, ny
    yplus(y,1) = ut*(ydistance(y,1)+1)/viscinematica
  enddo
  !print*, yplus

  ! Open a .txt file for plot
  open (action='write', file="totalshear.txt", newunit=fu, status='replace')
  do i = 1, ny
      write (fu, *) ydistance(i,1), ty(i,1)
  end do
  close (fu)









  open (action='write', file="fluctuationsx.txt", newunit=fu, status='replace')
    
    ! open various files
    nn1 = "field.data_0"
    do t = ti, tf, 2
      write(nn2,"(i3)")t ! (i3) numero de cifras
      field = nn1//nn2
      
      call check(nf90_open(field, nf90_nowrite, ncid)) ! Open file netCDF
      call check(nf90_inq_varid(ncid, "q1", varid))    ! Get the varid of the data variable, based on its name.
      call check(nf90_get_var(ncid, varid, q1))        ! Read the data.

      ! Mean velocity field: U(x,t) = <U(x,t)> + u(x,t)
      ! mean velocity x-z plane
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
      
      ! Fluctuations in x
      flucx = 0
      do y = 1, ny
        do x = 1, nx
          do z = 1, nz
            flucx(x,y,z) = (q1(x,y,z) - Uxxz(y,1))**2
          enddo
        enddo
      enddo

      urms1 = 0
      do y = 1, ny
        do x = 1, nx
          do z = 1, nz
            urms1 = urms1 + (sqrt(flucx(x,y,z)))/(nx*nz)
            !urms1 = urms1 + (flucx(x,y,z))/(nx*nz)
          enddo
        enddo
        urms(y,1) = urms1/ut
        urms1 = 0
      enddo
      !print*, urms

      do i = 1, ny
          write (fu, *) urms(i,1), yplus(i,1), t    !Write the file to plot
      end do

    enddo
  close (fu)
  
  open (action='read', file='fluctuationsx.txt', unit=fu3, status='old')
    do y = 1, ny*(tdiff+1)
      read(fu3,*) urmstotal(y,1), yplustotal(y,1)
    enddo

    Ux = 0
    do y = 1, ny
      do i = 0, tdiff
        Ux = Ux + urmstotal(y+i*128,1)/(tdiff+1)
      enddo
      urmsfinal(y,1) = Ux
      Ux = 0
    enddo
  close (fu3)

  open (action='write', file="dataTurbIntx.txt", newunit=fu, status='replace')
  do i = 1, ny
      write (fu, *) ydistance(i,1), urmsfinal(i,1)
  end do
  close (fu)
  !call execute_command_line('gnuplot -p ' // "plotTurbInt.plt")

  open (action='write', file="dataTurbIntypx.txt", newunit=fu, status='replace')
  do i = 1, ny/2
      write (fu, *) yplus(i,1), urmsfinal(i,1)
  end do
  close (fu)
  !call execute_command_line('gnuplot -p ' // "plotTurbIntyp.plt")

  call check(nf90_close(ncid))






  open (action='write', file="fluctuationsy.txt", newunit=fu, status='replace')
    
  ! open various files
  nn1 = "field.data_0"
  do t = ti, tf, 2
    write(nn2,"(i3)")t ! (i3) numero de cifras
    field = nn1//nn2
    
    call check(nf90_open(field, nf90_nowrite, ncid)) ! Open file netCDF
    call check(nf90_inq_varid(ncid, "q2", varid))    ! Get the varid of the data variable, based on its name.
    call check(nf90_get_var(ncid, varid, q2))        ! Read the data.

    ! Mean velocity field: U(x,t) = <U(x,t)> + u(x,t)
    ! mean velocity x-z plane
    Uy = 0
    do y = 1, ny
      do x = 1, nx
        do z = 1, nz
          Uy = Uy + (q2(x,y,z))/(nx*nz)
        enddo
      enddo
      Uyxz(y,1) = Uy
      Uy = 0
    enddo
    
    ! Fluctuations in x
    flucy = 0
    do y = 1, ny
      do x = 1, nx
        do z = 1, nz
          flucy(x,y,z) = (q2(x,y,z) - Uyxz(y,1))**2
        enddo
      enddo
    enddo

    vrms1 = 0
    do y = 1, ny
      do x = 1, nx
        do z = 1, nz
          vrms1 = vrms1 + (sqrt(flucy(x,y,z)))/(nx*nz)
          !vrms1 = vrms1 + (flucy(x,y,z))/(nx*nz)
        enddo
      enddo
      vrms(y,1) = vrms1/ut
      vrms1 = 0
    enddo
    !print*, urms

    do i = 1, ny
        write (fu, *) vrms(i,1), yplus(i,1), t    !Write the file to plot
    end do

  enddo
close (fu)

open (action='read', file='fluctuationsy.txt', unit=fu3, status='old')
  do y = 1, ny*(tdiff+1)
    read(fu3,*) vrmstotal(y,1), yplustotal(y,1)
  enddo

  Uy = 0
  do y = 1, ny
    do i = 0, tdiff
      Uy = Uy + vrmstotal(y+i*128,1)/(tdiff+1)
    enddo
    vrmsfinal(y,1) = Uy
    Uy = 0
  enddo
close (fu3)

open (action='write', file="dataTurbInty.txt", newunit=fu, status='replace')
do i = 1, ny
    write (fu, *) ydistance(i,1), vrmsfinal(i,1)
end do
close (fu)
!call execute_command_line('gnuplot -p ' // "plotTurbInt.plt")

open (action='write', file="dataTurbIntypy.txt", newunit=fu, status='replace')
do i = 1, ny/2
    write (fu, *) yplus(i,1), vrmsfinal(i,1)
end do
close (fu)
!call execute_command_line('gnuplot -p ' // "plotTurbIntyp.plt")

call check(nf90_close(ncid))







open (action='write', file="fluctuationsz.txt", newunit=fu, status='replace')
    
  ! open various files
  nn1 = "field.data_0"
  do t = ti, tf, 2
    write(nn2,"(i3)")t ! (i3) numero de cifras
    field = nn1//nn2
    
    call check(nf90_open(field, nf90_nowrite, ncid)) ! Open file netCDF
    call check(nf90_inq_varid(ncid, "q3", varid))    ! Get the varid of the data variable, based on its name.
    call check(nf90_get_var(ncid, varid, q3))        ! Read the data.

    ! Mean velocity field: U(x,t) = <Ux,t)> + u(x,t)
    ! mean velocity x-z plane
    Uz = 0
    do y = 1, ny
      do x = 1, nx
        do z = 1, nz
          Uz = Uz + (q3(x,y,z))/(nx*nz)
        enddo
      enddo
      Uzxz(y,1) = Uz
      Uz = 0
    enddo
    
    ! Fluctuations in x
    flucz = 0
    do y = 1, ny
      do x = 1, nx
        do z = 1, nz
          flucz(x,y,z) = (q3(x,y,z) - Uzxz(y,1))**2
        enddo
      enddo
    enddo

    wrms1 = 0
    do y = 1, ny
      do x = 1, nx
        do z = 1, nz
          wrms1 = wrms1 + (sqrt(flucz(x,y,z)))/(nx*nz)
          !vrms1 = vrms1 + (flucy(x,y,z))/(nx*nz)
        enddo
      enddo
      wrms(y,1) = wrms1/ut
      wrms1 = 0
    enddo
    !print*, urms

    do i = 1, ny
        write (fu, *) wrms(i,1), yplus(i,1), t    !Write the file to plot
    end do

  enddo
close (fu)

open (action='read', file='fluctuationsz.txt', unit=fu3, status='old')
  do y = 1, ny*(tdiff+1)
    read(fu3,*) wrmstotal(y,1), yplustotal(y,1)
  enddo

  Uz = 0
  do y = 1, ny
    do i = 0, tdiff
      Uz = Uz + wrmstotal(y+i*128,1)/(tdiff+1)
    enddo
    wrmsfinal(y,1) = Uz
    Uz = 0
  enddo
close (fu3)

open (action='write', file="dataTurbIntz.txt", newunit=fu, status='replace')
do i = 1, ny
    write (fu, *) ydistance(i,1), wrmsfinal(i,1)
end do
close (fu)
call execute_command_line('gnuplot -p ' // "plotTurbInt.plt")

open (action='write', file="dataTurbIntypz.txt", newunit=fu, status='replace')
do i = 1, ny/2
    write (fu, *) yplus(i,1), wrmsfinal(i,1)
end do
close (fu)
call execute_command_line('gnuplot -p ' // "plotTurbIntyp.plt")

call check(nf90_close(ncid))





! Reynolds stress

open (action='write', file="fluctuationsuv.txt", newunit=fu8, status='replace')   

do t = ti, tf, 2

  flucuv = 0
  do y = 1, ny
    do x = 1, nx
      do z = 1, nz
        flucuv(x,y,z) = (q1(x,y,z) - Uxxz(y,1))*(q2(x,y,z) - Uyxz(y,1))
      enddo
    enddo
  enddo

  uvrms1 = 0
  do y = 1, ny
    do x = 1, nx
      do z = 1, nz
        uvrms1 = uvrms1 + ((flucuv(x,y,z)))/(nx*nz)
      enddo
    enddo
    uvrms(y,1) = uvrms1
    uvrms1 = 0
  enddo

  do i = 1, ny
    write (fu8, *) uvrms(i,1), yplus(i,1), t    !Write the file to plot
  end do

enddo
close (fu8)

open (action='read', file='fluctuationsuv.txt', unit=fu3, status='old')
  do y = 1, ny*(tdiff+1)
    read(fu3,*,IOSTAT=iostat) uvrmstotal(y,1), yplustotal(y,1)
  enddo

  UV = 0
  do y = 1, ny
    do i = 0, tdiff
      UV = UV + uvrmstotal(y+i*128,1)/(tdiff+1)
    enddo
    uvrmsfinal(y,1) = UV
    UV = 0
  enddo
close (fu3)

do y = 1, ny
  Reynoldstress(y,1) = -rho/tw*uvrmsfinal(y,1)
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


