program four_dimensional_array
  implicit none
  integer, parameter :: nx = 192, ny = 128, nz = 160, nt = 60
  real*8 ::  prueba(nx, ny, nz, nt)
  integer :: x, y, z, t

  do t = 1, nt
    do x = 1, nx
      do y = 1, ny
        do z = 1, nz
          prueba(x, y, z, t) = 2.5 +t
        end do
      end do
    end do
  end do

  print*, prueba(1,2,3,4)
  
end program four_dimensional_array

      ! Fluctuations in x, y, z

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


