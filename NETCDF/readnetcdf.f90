program read_netcdf
    use netcdf
    implicit none

    !Variables
    integer, parameter :: nx = 192, ny = 128, nz = 160
    real*8 :: data_in(nx, ny, nz)
    integer :: varid, ncid, x, y, z
    real*8 :: u, v, w, q1, Umean
    
    ! Open file netCDF
    call check(nf90_open("field.data_0400", nf90_nowrite, ncid))
    ! Get the varid of the data variable, based on its name.
    call check(nf90_inq_varid(ncid, "q1", varid))
    ! Read the data.
    call check(nf90_get_var(ncid, varid, data_in))

    ! Mean velocity field
    ! U(x,t) = <U(x,t)> + u(x,t)
    v = (data_in(192,128,160) + data_in(191,127,159))/2
    print*, "Velocity in x: ", v

    ! <U(x,t)>
    Umean = 0
    do x = 1, nx
      do y = 1, ny
        do z = 1, nz
          Umean = Umean + (data_in(x,y,z))/(nx*ny*nz)
        enddo
      enddo
    enddo
    print*, "Velocity Umean: ", Umean
`
    ! Fluctuations
    do z = 1, nz-1
      do y = 1, ny-1
        do x = 1, nx-1
          u = (data_in(x,y,z) + data_in(x+1,y+1,z+1))/2 - Umean
          !print*, "Velocity Fluctuations: ", u
        enddo
      enddo
    enddo
    


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


