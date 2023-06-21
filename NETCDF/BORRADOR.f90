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


