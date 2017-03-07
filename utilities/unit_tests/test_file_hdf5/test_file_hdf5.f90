program test_file_hdf5

  use utilities
  implicit none

# include <kind_parameter.h>

  type(file_hdf5_type) :: test
  intknd :: funit
  charwd :: name
  charwd :: ext
  charwd :: path
  logknd :: is_read
  logknd :: is_write

  funit = 200
  name = 'subsc'
  ext = '.h5'
  path = './'
  is_read = .true.
  is_write = .true.

  call test%init(funit,name,ext,path,is_read,is_write)



endprogram test_file_hdf5
