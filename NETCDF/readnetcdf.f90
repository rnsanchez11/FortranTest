! compiling
! gfortran -o read.out readnetcdf.f90 -I/opt/homebrew/Cellar/netcdf-fortran/4.6.0_1/include -L/opt/homebrew/Cellar/netcdf-fortran/4.6.0_1/lib -lnetcdff
program read_netcdf
    use netcdf
    implicit none

    !Variables
    integer, parameter :: n1m = 192, n2m = 128, n3m = 160  ! Dimensions
    real*8 :: q1(n1m, n2m, n3m)
    real*8 :: q2(n1m, n2m, n3m)
    real*8 :: q3(n1m, n2m, n3m)
    real*8 :: pr(n1m, n2m, n3m)
    integer :: varid, ncid
    real*8 :: v

    ! Open file netCDF
    call check(nf90_open("field.data_0400", nf90_nowrite, ncid))
    ! Get the varid of the data variable, based on its name.
    call check(nf90_inq_varid(ncid, "q1", varid))
    call check(nf90_inq_varid(ncid, "q2", varid))
    call check(nf90_inq_varid(ncid, "q3", varid))
    call check(nf90_inq_varid(ncid, "pr", varid))
    ! Read the data.
    call check(nf90_get_var(ncid, varid, q1))
    call check(nf90_get_var(ncid, varid, q2))
    call check(nf90_get_var(ncid, varid, q3))
    call check(nf90_get_var(ncid, varid, pr))

    ! Realizar cualquier procedimiento

    v = q1(192,128,160) + q2(192,128,160) + q3(192,128,160) + pr(192,128,160)
    print*, "Suma q1 + q2 + q3 + pr: ", v
 

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


