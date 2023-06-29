program ejemplo_arreglo_4d
  use netcdf
  implicit none
    
    character(len=12) :: nn1                                                  ! primera parte nombre del los archivos multiples
    character(len=3) :: nn2                                                   ! # nombre del los archivos multiples
    character(len=15) :: field  
    integer, parameter :: Nx = 192, Ny = 128, Nz = 160, Nt = 249
    integer, parameter :: s = 249, ti = 200, tf = 400, tdiff = (tf - ti)/2
    integer :: varid, ncid  
    real*8 :: q1(nx, ny, nz), q2(nx, ny, nz), q3(nx, ny, nz) 

    integer :: array(Nx, Ny, Nz, Nt)
    integer :: i, j, k, t
    
    do t = ti, tf, 2
      nn1 = "field.data_0"
      write(nn2,"(i3)")t ! (i3) numero de cifras
      field = nn1//nn2

      call check(nf90_open(field, nf90_nowrite, ncid)) ! Open file netCDF
      call check(nf90_inq_varid(ncid, "q1", varid))    ! Get the varid of the data variable, based on its name.
      call check(nf90_get_var(ncid, varid, q1))        ! Read the data.

      do i = 1, Nx
        do j = 1, Ny
          do k = 1, Nz
            array(i, j, k, t) = q1(i,j,k)
          end do
        end do
      end do

    end do
    
    ! Accediendo a los elementos del arreglo
    do t = ti, tf, 2
      do i = 1, Nx
        do j = 1, Ny
          do k = 1, Nz
            write(*, *) "array(", i, ",", j, ",", k, ",", t, ") =", array(i, j, k, t)
          end do
        end do
      end do
    end do
    

    call check(nf90_close(ncid))
    contains
    subroutine check(status)
      integer, intent (in) :: status
        
      if(status /= nf90_noerr) then 
        print *, trim(nf90_strerror(status))
        stop "Stopped"
      end if
    end subroutine check 
  end program ejemplo_arreglo_4d