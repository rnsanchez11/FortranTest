program createNetcdfFile

use netcdf
implicit none

!Name file
character(len=*), parameter :: filename = "test.nc"
 
!Grid
integer, parameter :: Ndims = 3
integer, parameter :: Nx = 8, Ny = 8, Nz = 8

!ID for variables and dimentions
integer :: ncid, varid, dimids(Ndims)
integer :: xdimid, ydimid, zdimid

!Data array to write
integer :: dataOut(Nx,Ny,Nz)

! Loop indexes, and error handling.
integer :: x, y, z

!write data
do x = 1, Nx
    do y = 1, Ny
        do z = 1, Nz
            dataOut(x, y, z) = (x - 1) * Ny + (y - 1) + z
        enddo
    enddo
enddo

! Check the return code
!The nf90_clobber parameter tells netCDF to overwrite this file, if it already exists.
call check(nf90_create(filename, nf90_clobber,ncid))

!Define the dimention. hand back an ID for each
call check(nf90_def_dim(ncid, "Grid_x", Nx, xdimid))
call check(nf90_def_dim(ncid, "Grid_y", Ny, ydimid))
call check(nf90_def_dim(ncid, "Grid_z", Nz, zdimid))

!The dimids array is used to pass the IDs of the dimensions of the variables.
dimids = (/ xdimid, ydimid, zdimid/)
print*, dimids

!Define the variable. The type of the variable in this case is NF90_INT (4-byte integer).
call check(nf90_def_var(ncid, "data", nf90_double, dimids, varid))

call check(nf90_enddef(ncid))

!Write the pretend data to the file
call check(nf90_put_var(ncid, varid, dataOut))

print *, "*** SUCCESS writing example file ***"

contains
subroutine check(status)
    integer, intent (in) :: status

    if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
end subroutine check

end program
