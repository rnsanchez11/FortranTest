program inq_libvers

use netcdf
print *, (nf90_inq_libvers('field.data_0400'))

end program
