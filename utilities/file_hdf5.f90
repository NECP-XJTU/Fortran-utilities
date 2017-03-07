!###############################################################################
!> Module for specifying hdf5 file type
!>
!> @author Jun Chen
!>   @date 10/3/2016
!>
!> @todo
!>   - Test: Added some comments.
!###############################################################################
module file_hdf5

  use ISO_FORTRAN_ENV
  use error_warning
  use string
  use file_base, only : file_base_type
  use, intrinsic  :: ISO_C_BINDING
#ifdef WITH_HDF5
  use hdf5
#endif

  implicit none
  private

# include <kind_parameter.h>

  public :: file_hdf5_type,write_dataset,read_dataset

  !-----------------------------------------------------------------------------
  !Type to define a hdf5 file
  type,extends(file_base_type) :: file_hdf5_type

    intknd :: i_line = 0
    character(200) :: fullname = ''   !< Full name of file object
#ifdef WITH_HDF5
    integer(HID_T) :: file_id
#endif

    ! The element is extended from base time
  contains
    procedure,pass :: init   => init_file_hdf5
    procedure,pass :: open   => open_file_hdf5
    procedure,pass :: close  => close_file_hdf5
    procedure,pass :: clear  => clear_file_hdf5
    procedure,pass :: get_one_line
  endtype
  
  interface write_dataset
    module procedure write_str_0d
    module procedure write_str_1d
    module procedure write_int_0d
    module procedure write_int_1d
    module procedure write_int_2d
    module procedure write_int_3d
    module procedure write_dbl_0d
    module procedure write_dbl_1d
    module procedure write_dbl_2d
    module procedure write_dbl_3d
  endinterface write_dataset
  
  interface read_dataset
    module procedure read_str_0d
    module procedure read_str_1d
    module procedure read_int_0d
    module procedure read_int_1d
    module procedure read_int_2d
    module procedure read_int_3d
    module procedure read_dbl_0d
    module procedure read_dbl_1d
    module procedure read_dbl_2d
    module procedure read_dbl_3d    
  endinterface read_dataset

!===============================================================================

contains
!-------------------------------------------------------------------------------
!> Initializes a hdf5 file
!-------------------------------------------------------------------------------
  subroutine init_file_hdf5(file,unit,name,ext,path,is_read,is_write)

    class(file_hdf5_type),intent(inout) :: file
    intknd,intent(in) :: unit
    charSt,intent(in) :: name
    charSt,intent(in) :: ext
    charSt,intent(in) :: path
    logknd,intent(in) :: is_read
    logknd,intent(in) :: is_write


#ifdef WITH_HDF5
    ! local
    integer :: hdf5_err ! HDF5 error code
    char10 :: str
    if(.not. file%is_init) then
      file%i_line = 0
      file%unit = unit
      file%name = name
      file%ext  = ext
      file%path = path
      file%fullname = trim(path)//trim(name)//trim(ext)
      file%is_read  = is_read
      file%is_write = is_write
      file%is_init = .true.
      
      ! Initialize the Fortran interface
      call h5open_f(hdf5_err)
      
      ! create a new file using the default properties
      call h5fcreate_f(trim(file%fullname),H5F_ACC_TRUNC_F,file%file_id,hdf5_err)

      call file%open()

    else
      call raise_Error('init_file_hdf5 :: the hdf5 file object'//              &
        ' has been initialized')
    endif
#endif
  endsubroutine init_file_hdf5

!-------------------------------------------------------------------------------
!> open a hdf5 file
!-------------------------------------------------------------------------------
  subroutine open_file_hdf5(file)
    charSt,parameter :: myName = 'open_file_hdf5 -- '
    class(file_hdf5_type),intent(inout) :: file

#ifdef WITH_HDF5
    !local
    intknd :: iostat
    logknd :: is_exist
    char10 :: status
    integer(HID_T) :: action
    integer :: hdf5_err ! HDF5 error flag

    if(file%is_init .and. .not.(file%is_open)) then
      !Set value for status
      if(file%is_read) then
        status = 'OLD'
      else
        status = 'REPLACE'
      endif

      !Set value for action
      if(file%is_read .and. .not.(file%is_write)) then
        action = H5F_ACC_RDONLY_F
      elseif(.not.file%is_read .and. file%is_write) then
        action = H5F_ACC_RDWR_F
      else
        action = H5F_ACC_RDWR_F
      endif

      if(status == 'OLD') then
        inquire(file=trim(str_join_path(file%path,file%name,file%ext)), &
         exist=is_exist)
        if(.not.is_exist) then
          file%is_init = .false.
          call raise_Error(myName//'Could not open file '//  &
            trim(str_join_path(file%path,file%name,file%ext))//', it does not exist.')
        endif
      endif

      if(file%is_init) then
        call h5fopen_f(trim(file%fullname),action,file%file_id,hdf5_err) ! h5f

        if(hdf5_err == 0) then
          file%is_open = .true.
          file%is_end = .false.
        else
          file%is_init = .false.
          call raise_Error(myName//'Failed to open '//trim(adjustl(file%path))//&
            trim(adjustl(file%name))//trim(adjustl(file%ext)))
        endif
      endif

    endif
#endif
  endsubroutine open_file_hdf5

!-------------------------------------------------------------------------------
!> close a hdf5 file
!-------------------------------------------------------------------------------
  subroutine close_file_hdf5(file)
    class(file_hdf5_type),intent(inout) :: file
#ifdef WITH_HDF5

    intknd :: ioerr

    if(file%is_open) then
      call h5fclose_f(file%file_id, ioerr)
      if(ioerr == 0) then
        file%is_open = .false.
      else
        call raise_Error("close_file_hdf5 -- Cannot close file '"//            &
          trim(adjustl(file%path))//trim(adjustl(file%fullname))// &
            trim(adjustl(file%ext))//"'")
      endif
    endif

    file%is_end = .true.
    file%is_open = .false.
#endif
  endsubroutine close_file_hdf5

!-------------------------------------------------------------------------------
!> clear a hdf5 file
!-------------------------------------------------------------------------------
  subroutine clear_file_hdf5(file)
    class(file_hdf5_type),intent(inout) :: file
#ifdef WITH_HDF5
    if(file%is_init) then
      if(file%is_open) call file%close()
      file%i_line = 0
      file%unit = -1
      file%path = ''
      file%name = ''
      file%ext  = ''
      file%is_open = .false.
      file%is_end  = .false.
      file%is_read = .false.
      file%is_write= .false.
      file%is_init = .false.
    endif
#endif
  endsubroutine clear_file_hdf5

!-------------------------------------------------------------------------------
!>
!-------------------------------------------------------------------------------
  subroutine get_one_line(file,strline)

    class(file_hdf5_type),intent(inout) :: file
    charSt,intent(out) :: strline

	! Local
    character(256) :: buffer
    intknd :: buffer_size, iostat

    buffer = ''
    iostat = 0
    strline = ''
    if(file%is_init .and. file%is_open .and. .not.(file%is_end)) then
      do while(iostat /= IOSTAT_EOR .and. iostat /= IOSTAT_END)

        !read one buffer
        read(unit=file%unit, FMT='(a)',size=buffer_size,advance='NO',          &
          iostat=iostat) buffer

        !At the end of file
        if(iostat == IOSTAT_END) then
          file%is_end = .true.

        !At the end of record
        elseif(iostat == IOSTAT_EOR) then
          strline = trim(strline)//trim(adjustl(buffer))
          file%i_line = file%i_line + 1

        !Error reading this line
        elseif(iostat < IOSTAT_EOR) then
          call raise_Error('get_one_line: Error reading one line'//            &
            ' from file (unit = '//to_str(file%unit)//'), IOSTAT = '//         &
            to_str(iostat))

        !Add buffer to strline, and continue reading
        else
          strline = trim(strline)//trim(adjustl(buffer))

        endif
      enddo
    endif
  endsubroutine get_one_line
  
!-------------------------------------------------------------------------------
!> reads string data
!-------------------------------------------------------------------------------
  subroutine read_str_0d(file,key,value)
    class(file_hdf5_type),intent(inout) :: file
    character(*)  ,intent(in)           :: key        ! name of data
    character(*)  ,intent(inout),target :: value      ! data to read
#ifdef WITH_HDF5
    ! local
    integer :: hdf5_err
    integer(HID_T)                      :: dset_id    ! dataset identifier
    integer(HID_T)                      :: dspace_id  ! dataspace identifier
    integer(HID_T)                      :: filetype
    integer(HID_T)                      :: memtype
    integer(SIZE_T)                     :: size
    integer(SIZE_T)                     :: n
    type(c_ptr)                         :: f_ptr      ! C data pointer type

    ! open the hdf5 file
    call file%open()
    
    ! open an existing dataset
    call h5dopen_f(file%file_id, trim(key), dset_id, hdf5_err)
    call h5dget_space_f(dset_id, dspace_id, hdf5_err)

    ! Make sure value is large enough
    call h5dget_type_f(dset_id, filetype, hdf5_err)
    call h5tget_size_f(filetype, size, hdf5_err)
    if (size > len(value) + 1) then
      call raise_fatal('Character value is not long enough to &
           &read HDF5 string.')
    end if

    ! Get datatype in memory based on Fortran character
    n = len(value)
    call h5tcopy_f(H5T_FORTRAN_S1, memtype, hdf5_err)
    call h5tset_size_f(memtype, n, hdf5_err)

    ! Get pointer to start of string
    f_ptr = c_loc(value(1:1))

    ! read the dataset, Fortran 2003 interface
    call h5dread_f(dset_id, memtype, f_ptr, hdf5_err)

    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

    ! close the dataspace
    call h5sclose_f(dspace_id,hdf5_err)

    ! release the datatype
    call h5tclose_f(memtype,hdf5_err)

    ! release the datatype
    call h5tclose_f(filetype,hdf5_err)

#endif
  endsubroutine read_str_0d

!-------------------------------------------------------------------------------
!> reads string data
!-------------------------------------------------------------------------------
  subroutine read_str_1d(file,key,value)
    class(file_hdf5_type),intent(inout) :: file
    character(*)  ,intent(in)           :: key        ! name of data
    character(*)  ,intent(inout),target :: value(:)   ! data to read
#ifdef WITH_HDF5
    ! local
    integer                             :: hdf5_err
    integer(HSIZE_T)                    :: dims(1)
    integer(HID_T)                      :: dset_id    ! dataset identifier
    integer(HID_T)                      :: dspace_id  ! dataspace identifier
    integer(HID_T)                      :: filetype
    integer(HID_T)                      :: memtype
    integer(SIZE_T)                     :: size
    integer(SIZE_T)                     :: n
    type(c_ptr)                         :: f_ptr      ! C data pointer type

    dims(:) = shape(value)
    
    ! open the hdf5 file
    call file%open()

    ! open an existing dataset
    call h5dopen_f(file%file_id, trim(key), dset_id, hdf5_err)
    call h5dget_space_f(dset_id, dspace_id, hdf5_err)

    ! Make sure value is large enough
    call h5dget_type_f(dset_id, filetype, hdf5_err)
    call h5tget_size_f(filetype, size, hdf5_err)
    if (size > len(value) + 1) then
      call raise_fatal('Character value is not long enough to &
           &read HDF5 string.')
    end if

    ! Get datatype in memory based on Fortran character
    n = len(value)
    call h5tcopy_f(H5T_FORTRAN_S1, memtype, hdf5_err)
    call h5tset_size_f(memtype, n, hdf5_err)

    ! Get pointer to start of string
    f_ptr = c_loc(value(1:1))

    ! read the dataset, Fortran 2003 interface
    call h5dread_f(dset_id, memtype, f_ptr, hdf5_err)

    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

    ! close the dataspace
    call h5sclose_f(dspace_id,hdf5_err)

    ! release the datatype
    call h5tclose_f(memtype,hdf5_err)

    ! release the datatype
    call h5tclose_f(filetype,hdf5_err)

#endif
  endsubroutine read_str_1d

  
!-------------------------------------------------------------------------------
!> reads integer precision scalar data
!-------------------------------------------------------------------------------
  subroutine read_int_0d(file,key,value)
    class(file_hdf5_type),intent(inout) :: file
    character(*)  ,intent(in)           :: key        ! name of data
    integer       ,intent(inout),target :: value      ! data to read
#ifdef WITH_HDF5
    ! local
    integer :: hdf5_err
    integer(HID_T)                      :: dset_id    ! dataset identifier
    type(c_ptr)                         :: f_ptr      ! C data pointer type

    ! open the hdf5 file
    call file%open()

    ! open an existing dataset
    call h5dopen_f(file%file_id, trim(key), dset_id, hdf5_err)

    f_ptr = c_loc(value)

    ! read the dataset, Fortran 2003 interface
    call h5dread_f(dset_id,H5T_NATIVE_INTEGER,f_ptr,hdf5_err)

    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

#endif
  endsubroutine read_int_0d

!-------------------------------------------------------------------------------
!> reads integer precision 1-D array data
!-------------------------------------------------------------------------------
  subroutine read_int_1d(file,key,value)
    class(file_hdf5_type),intent(inout) :: file
    character(*)  ,intent(in)           :: key        ! name of data
    integer       ,intent(inout),target :: value(:)   ! data to read
#ifdef WITH_HDF5
    ! local
    integer                             :: hdf5_err
    integer(HSIZE_T)                    :: dims(1)
    integer(HID_T)                      :: dset_id    ! dataset identifier
    type(c_ptr)                         :: f_ptr      ! C data pointer type

    dims(:) = shape(value)

    ! open the hdf5 file
    call file%open()

    ! open an existing dataset
    call h5dopen_f(file%file_id,trim(key),dset_id,hdf5_err)

    f_ptr = c_loc(value)

    ! read the dataset, Fortran 2003 interface
    call h5dread_f(dset_id,H5T_NATIVE_INTEGER,f_ptr,hdf5_err)

    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

#endif
  endsubroutine read_int_1d

!-------------------------------------------------------------------------------
!> reads integer precision 2-D array data
!-------------------------------------------------------------------------------
  subroutine read_int_2d(file,key,value)
    class(file_hdf5_type),intent(inout) :: file
    character(*)  ,intent(in)           :: key        ! name of data
    integer       ,intent(inout),target :: value(:,:) ! data to read
#ifdef WITH_HDF5
    ! local
    integer                             :: hdf5_err
    integer(HSIZE_T)                    :: dims(2)
    integer(HID_T)                      :: dset_id    ! dataset identifier
    type(c_ptr)                         :: f_ptr      ! C data pointer type

    dims(:) = shape(value)
    
    ! open the hdf5 file
    call file%open()

    ! open an existing dataset
    call h5dopen_f(file%file_id,trim(key),dset_id,hdf5_err)

    f_ptr = c_loc(value)

    ! read the dataset, Fortran 2003 interface
    call h5dread_f(dset_id,H5T_NATIVE_INTEGER,f_ptr,hdf5_err)

    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

#endif
  endsubroutine read_int_2d
  
!-------------------------------------------------------------------------------
!> reads integer precision 3-D array data
!-------------------------------------------------------------------------------
  subroutine read_int_3d(file,key,value)
    class(file_hdf5_type),intent(inout) :: file
    character(*)  ,intent(in)           :: key          ! name of data
    integer       ,intent(inout),target :: value(:,:,:) ! data to read
#ifdef WITH_HDF5
    ! local
    integer :: hdf5_err
    integer(HSIZE_T)                    :: dims(3)
    integer(HID_T)                      :: dset_id      ! dataset identifier
    type(c_ptr)                         :: f_ptr        ! C data pointer type

    dims(:) = shape(value)

    ! open the hdf5 file
    call file%open()

    ! open an existing dataset
    call h5dopen_f(file%file_id,trim(key),dset_id,hdf5_err)

    f_ptr = c_loc(value)

    ! read the dataset, Fortran 2003 interface
    call h5dread_f(dset_id,H5T_NATIVE_INTEGER,f_ptr,hdf5_err)

    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

#endif
  endsubroutine read_int_3d
  
!-------------------------------------------------------------------------------
!> reads double precision scalar data
!-------------------------------------------------------------------------------
  subroutine read_dbl_0d(file,key,value)
    class(file_hdf5_type),intent(inout) :: file
    character(*),intent(in)             :: key        ! name of data
    realkd,intent(inout),target         :: value      ! data to read
#ifdef WITH_HDF5
    ! local
    integer                           :: hdf5_err
    integer(HID_T)                    :: dset_id    ! dataset identifier
    type(c_ptr)                       :: f_ptr      ! C data pointer type

    ! open the hdf5 file
    call file%open()

    ! open an existing dataset
    call h5dopen_f(file%file_id,trim(key),dset_id,hdf5_err)

    f_ptr = c_loc(value)

    ! read the dataset, Fortran 2003 interface
    call h5dread_f(dset_id,H5T_NATIVE_DOUBLE,f_ptr,hdf5_err)

    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

#endif
  endsubroutine read_dbl_0d

!-------------------------------------------------------------------------------
!> reads double precision scalar data
!-------------------------------------------------------------------------------
  subroutine read_dbl_1d(file,key,value)
    class(file_hdf5_type),intent(inout) :: file
    character(*),intent(in)             :: key        ! name of data
    realkd,intent(inout),target         :: value(:)   ! data to read
#ifdef WITH_HDF5
    ! local
    integer                             :: hdf5_err
    integer(HSIZE_T)                    :: dims(1)
    integer(HID_T)                      :: dset_id    ! dataset identifier
    type(c_ptr)                         :: f_ptr      ! C data pointer type

    dims(:) = shape(value)
    
    ! open the hdf5 file
    call file%open()
    
    ! open an existing dataset
    call h5dopen_f(file%file_id,trim(key),dset_id,hdf5_err)

    f_ptr = c_loc(value)

    ! read the dataset, Fortran 2003 interface
    call h5dread_f(dset_id,H5T_NATIVE_DOUBLE,f_ptr,hdf5_err)

    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

#endif
  endsubroutine read_dbl_1d

!-------------------------------------------------------------------------------
!> reads double precision 2-D array data
!-------------------------------------------------------------------------------
  subroutine read_dbl_2d(file,key,value)
    class(file_hdf5_type),intent(inout) :: file
    character(*),intent(in)             :: key        ! name of data
    realkd,intent(inout),target         :: value(:,:) ! data to read
#ifdef WITH_HDF5
    ! local
    integer                             :: hdf5_err
    integer(HSIZE_T)                    :: dims(2)
    integer(HID_T)                      :: dset_id    ! dataset identifier
    type(c_ptr)                         :: f_ptr      ! C data pointer type

    dims(:) = shape(value)
    
    ! open the hdf5 file
    call file%open()

    ! open an existing dataset
    call h5dopen_f(file%file_id,trim(key),dset_id,hdf5_err)

    f_ptr = c_loc(value)

    ! read the dataset, Fortran 2003 interface
    call h5dread_f(dset_id,H5T_NATIVE_DOUBLE,f_ptr,hdf5_err)

    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

#endif
  endsubroutine read_dbl_2d

!-------------------------------------------------------------------------------
!> reads double precision 3-D array data
!-------------------------------------------------------------------------------
  subroutine read_dbl_3d(file,key,value)
    class(file_hdf5_type),intent(inout) :: file
    character(*),intent(in)             :: key          ! name of data
    realkd,intent(inout),target         :: value(:,:,:) ! data to read
#ifdef WITH_HDF5
    ! local
    integer                             :: hdf5_err
    integer(HSIZE_T)                    :: dims(3)
    integer(HID_T)                      :: dset_id      ! dataset identifier
    type(c_ptr)                         :: f_ptr        ! C data pointer type

    dims(:) = shape(value)
    
    ! open the hdf5 file
    call file%open()

    ! open an existing dataset
    call h5dopen_f(file%file_id,trim(key),dset_id,hdf5_err)

    f_ptr = c_loc(value)

    ! read the dataset, Fortran 2003 interface
    call h5dread_f(dset_id,H5T_NATIVE_DOUBLE,f_ptr,hdf5_err)

    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

#endif
  endsubroutine read_dbl_3d

!-------------------------------------------------------------------------------
!> writes string data
!-------------------------------------------------------------------------------
  subroutine write_str_0d(file,key,value)
    class(file_hdf5_type),intent(inout)  :: file
    character(*),intent(in)     :: key          ! name of data
    character(*),intent(in)     :: value        ! data to write
#ifdef WITH_HDF5
    ! local
    integer                     :: hdf5_err
    integer(HID_T)              :: dset_id      ! dataset identifier
    integer(HID_T)              :: dspace_id    ! dataspace identifier
    integer(HID_T)              :: filetype
    integer(HID_T)              :: memtype
    integer(HID_T)              :: n
    type(c_ptr)                 :: f_ptr        ! C data pointer type

    ! create datatype for HDF5 file based on C char
    n = len_trim(value)
    call h5tcopy_f(H5T_C_S1, filetype, hdf5_err)
    call h5tset_size_f(filetype, n + 1, hdf5_err)

    ! create datatype in memory based on Fortran character
    call h5tcopy_f(H5T_FORTRAN_S1, memtype, hdf5_err)
    if(n > 0) call h5tset_size_f(memtype, n, hdf5_err)

    ! create dataspace
    call h5screate_f(H5S_SCALAR_F,dspace_id,hdf5_err)

    ! create dataset
    call h5dcreate_f(file%file_id, trim(key), filetype, dspace_id, dset_id, hdf5_err)
    
    ! get pointer to start of string
    f_ptr = c_loc(value(1:1))
    
    ! write the dataset
    if(n > 0) &
      call h5dwrite_f(dset_id,memtype,f_ptr,hdf5_err)
    
    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

    ! close the dataspace
    call h5sclose_f(dspace_id,hdf5_err)

    ! release the datatype
    call h5tclose_f(memtype,hdf5_err)

    ! release the datatype
    call h5tclose_f(filetype,hdf5_err)
    
#endif
  endsubroutine write_str_0d

!-------------------------------------------------------------------------------
!> writes string 1-D array data
!-------------------------------------------------------------------------------
  subroutine write_str_1d(file,key,value)
    class(file_hdf5_type),intent(inout)   :: file
    character(*),intent(in)            :: key          ! name of data
    character(*),intent(in),target     :: value(:)     ! data to write
#ifdef WITH_HDF5
    ! local
    integer                            :: hdf5_err
    integer(HID_T)                     :: dset_id      ! dataset identifier
    integer(HID_T)                     :: dspace_id    ! dataspace identifier
    integer(HSIZE_T)                   :: dims(1)
    integer(HID_T)                     :: filetype
    integer(HID_T)                     :: memtype
    integer(HID_T)                     :: n
    type(c_ptr)                        :: f_ptr        ! C data pointer type
    
    dims(:) = shape(value)
    ! create datatype for HDF5 file based on C char
    n = maxval(len_trim(value))
    call h5tcopy_f(H5T_C_S1, filetype, hdf5_err)
    call h5tset_size_f(filetype, n + 1, hdf5_err)

    ! create datatype in memory based on Fortran character
    call h5tcopy_f(H5T_FORTRAN_S1, memtype, hdf5_err)
    call h5tset_size_f(memtype, int(len(value(1)), SIZE_T), hdf5_err)

    ! create dataspace
    call h5screate_simple_f(1, dims, dspace_id, hdf5_err)

    ! create dataset
    call h5dcreate_f(file%file_id, trim(key), filetype, dspace_id, dset_id, hdf5_err)
    
    ! get pointer to start of string
    f_ptr = c_loc(value(1)(1:1))
    
    ! write the dataset
    if(n > 0) &
      call h5dwrite_f(dset_id,memtype,f_ptr,hdf5_err)
    
    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

    ! close the dataspace
    call h5sclose_f(dspace_id,hdf5_err)

    ! release the datatype
    call h5tclose_f(memtype,hdf5_err)

    ! release the datatype
    call h5tclose_f(filetype,hdf5_err)

    
#endif
  endsubroutine write_str_1d
  
!-------------------------------------------------------------------------------
!> writes integer precision scalar data
!-------------------------------------------------------------------------------
  subroutine write_int_0d(file,key,value)
    class(file_hdf5_type),intent(inout)  :: file
    character(*),intent(in)          :: key        ! name of data
    integer,     intent(in),target   :: value      ! data to write
#ifdef WITH_HDF5
    ! local
    integer                          :: hdf5_err
    integer(HID_T)                   :: dset_id    ! dataset identifier
    integer(HID_T)                   :: dspace_id  ! dataspace identifier
    type(c_ptr)                      :: f_ptr      ! C data pointer type

    ! create dataspace
    call h5screate_f(H5S_SCALAR_F,dspace_id,hdf5_err)

    ! create dataset
    call h5dcreate_f(file%file_id, trim(key), H5T_NATIVE_INTEGER, dspace_id, &
       dset_id, hdf5_err)
    f_ptr = c_loc(value)

    ! write the dataset, Fortran 2003 interface
    call h5dwrite_f(dset_id,H5T_NATIVE_INTEGER,f_ptr,hdf5_err)

    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

    ! close the dataspace
    call h5sclose_f(dspace_id,hdf5_err)

#endif
  endsubroutine write_int_0d
  
!-------------------------------------------------------------------------------
!> writes integer precision 1-D array data
!-------------------------------------------------------------------------------
  subroutine write_int_1d(file,key,value)
    class(file_hdf5_type),intent(inout)  :: file
    character(*),  intent(in)          :: key       ! name of data
    integer,       intent(in),target   :: value(:)  ! data to write
#ifdef WITH_HDF5
    ! local
    integer(HSIZE_T)                   :: dims(1)
    integer                            :: hdf5_err                
    integer(HID_T)                     :: dset_id   ! dataset identifier
    integer(HID_T)                     :: dspace_id ! dataspace identifier
    type(c_ptr)                        :: f_ptr     ! C data pointer type

    dims(:) = shape(value)
    ! create dataspace
    call h5screate_simple_f(1, dims, dspace_id, hdf5_err)

    ! create dataset
    call h5dcreate_f(file%file_id, trim(key), H5T_NATIVE_INTEGER, dspace_id, &
       dset_id, hdf5_err)
    f_ptr = c_loc(value)

    ! write the dataset, Fortran 2003 interface
    call h5dwrite_f(dset_id,H5T_NATIVE_INTEGER,f_ptr,hdf5_err)

    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

    ! close the dataspace
    call h5sclose_f(dspace_id,hdf5_err)

#endif
  endsubroutine write_int_1d
  
!-------------------------------------------------------------------------------
!> writes integer precision 2-D array data
!-------------------------------------------------------------------------------
  subroutine write_int_2d(file,key,value)
    class(file_hdf5_type),intent(inout)  :: file
    character(*),  intent(in)          :: key        ! name of data
    integer,       intent(in),target   :: value(:,:) ! data to write
#ifdef WITH_HDF5
    ! local
    integer(HSIZE_T)                   :: dims(2)
    integer                            :: hdf5_err                
    integer(HID_T)                     :: dset_id    ! dataset identifier
    integer(HID_T)                     :: dspace_id  ! dataspace identifier
    type(c_ptr)                        :: f_ptr      ! C data pointer type

    dims(:) = shape(value)
    ! create dataspace
    call h5screate_simple_f(2, dims, dspace_id, hdf5_err)

    ! create dataset
    call h5dcreate_f(file%file_id, trim(key), H5T_NATIVE_INTEGER, dspace_id, &
       dset_id, hdf5_err)
    f_ptr = c_loc(value)

    ! write the dataset, Fortran 2003 interface
    call h5dwrite_f(dset_id,H5T_NATIVE_INTEGER,f_ptr,hdf5_err)

    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

    ! close the dataspace
    call h5sclose_f(dspace_id,hdf5_err)

#endif
  endsubroutine write_int_2d
  
!-------------------------------------------------------------------------------
!> writes integer precision 3-D array data
!-------------------------------------------------------------------------------
  subroutine write_int_3d(file,key,value)
    class(file_hdf5_type),intent(inout)  :: file
    character(*),  intent(in)          :: key          ! name of data
    integer,       intent(in),target   :: value(:,:,:) ! data to write
#ifdef WITH_HDF5
    ! local
    integer(HSIZE_T)                   :: dims(3)
    integer                            :: hdf5_err                
    integer(HID_T)                     :: dset_id      ! dataset identifier
    integer(HID_T)                     :: dspace_id    ! dataspace identifier
    type(c_ptr)                        :: f_ptr        ! C data pointer type

    dims(:) = shape(value)
    ! create dataspace
    call h5screate_simple_f(3, dims, dspace_id, hdf5_err)

    ! create dataset
    call h5dcreate_f(file%file_id, trim(key), H5T_NATIVE_INTEGER, dspace_id, &
       dset_id, hdf5_err)
    f_ptr = c_loc(value)

    ! write the dataset, Fortran 2003 interface
    call h5dwrite_f(dset_id,H5T_NATIVE_INTEGER,f_ptr,hdf5_err)

    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

    ! close the dataspace
    call h5sclose_f(dspace_id,hdf5_err)
    
#endif
  endsubroutine write_int_3d
  
!-------------------------------------------------------------------------------
!> writes double precision scalar data
!-------------------------------------------------------------------------------
  subroutine write_dbl_0d(file,key,value)
    class(file_hdf5_type),intent(inout)  :: file
    character(*),intent(in)          :: key       ! name of data
    realkd,intent(in),target   :: value     ! data to write
#ifdef WITH_HDF5
    ! local
    integer                          :: hdf5_err
    integer(HID_T)                   :: dset_id   ! dataset identifier
    integer(HID_T)                   :: dspace_id ! dataspace identifier
    type(c_ptr)                      :: f_ptr     ! C data pointer type

    ! create dataspace
    call h5screate_f(H5S_SCALAR_F, dspace_id, hdf5_err)

    ! create dataset
    call h5dcreate_f(file%file_id, trim(key), H5T_NATIVE_DOUBLE, dspace_id, &
       dset_id, hdf5_err)
    f_ptr = c_loc(value)
    
    ! write the dataset, Fortran 2003 interface
    call h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,f_ptr,hdf5_err)

    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)

    ! close the dataspace
    call h5sclose_f(dspace_id,hdf5_err)

#endif
  endsubroutine write_dbl_0d
  
!-------------------------------------------------------------------------------
!> writes double precision 1-D array data
!-------------------------------------------------------------------------------
  subroutine write_dbl_1d(file,key,value)
    class(file_hdf5_type),intent(inout)  :: file
    character(*),intent(in)          :: key       ! name of data
    realkd,intent(in),target   :: value(:)  ! data to write
#ifdef WITH_HDF5
    !local
    integer(HSIZE_T)                 :: dims(1)
    integer                          :: hdf5_err
    integer(HID_T)                   :: dset_id   ! dataset identifier
    integer(HID_T)                   :: dspace_id ! dataspace identifier
    type(c_ptr)                      :: f_ptr     ! C data pointer type

    dims(:) = shape(value)
    ! create dataspace
    call h5screate_simple_f(1, dims, dspace_id, hdf5_err)

    ! create dataset
    call h5dcreate_f(file%file_id, trim(key), H5T_NATIVE_DOUBLE, dspace_id, &
       dset_id, hdf5_err)
    f_ptr = c_loc(value)
    
    ! write the dataset, Fortran 2003 interface
    call h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,f_ptr,hdf5_err)
    
    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)
    
    ! close the dataspace
    call h5sclose_f(dspace_id,hdf5_err)
    
#endif
  endsubroutine write_dbl_1d

!-------------------------------------------------------------------------------
!> writes double precision 2-D array data
!-------------------------------------------------------------------------------
  subroutine write_dbl_2d(file,key,value)
    class(file_hdf5_type),intent(inout)  :: file
    character(*),intent(in)          :: key       ! name of data
    realkd,intent(in),target   :: value(:,:)! data to write
#ifdef WITH_HDF5
    !local
    integer(HSIZE_T)                 :: dims(2)
    integer                          :: hdf5_err
    integer(HID_T)                   :: dset_id   ! dataset identifier
    integer(HID_T)                   :: dspace_id ! dataspace identifier
    type(c_ptr)                      :: f_ptr     ! C data pointer type

    dims(:) = shape(value)
    ! create dataspace
    call h5screate_simple_f(2, dims, dspace_id, hdf5_err)

    ! create dataset
    call h5dcreate_f(file%file_id, trim(key), H5T_NATIVE_DOUBLE, dspace_id, &
       dset_id, hdf5_err)
    f_ptr = c_loc(value)
    
    ! write the dataset, Fortran 2003 interface
    call h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,f_ptr,hdf5_err)
    
    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)
    
    ! close the dataspace
    call h5sclose_f(dspace_id,hdf5_err)
    
#endif
  endsubroutine write_dbl_2d

!-------------------------------------------------------------------------------
!> writes double precision 3-D array data
!-------------------------------------------------------------------------------
  subroutine write_dbl_3d(file,key,value)
    class(file_hdf5_type),intent(inout)  :: file
    character(*),intent(in)              :: key          ! name of data
    realkd,intent(in),target             :: value(:,:,:) ! data to write
#ifdef WITH_HDF5
    !local
    integer(HSIZE_T)                 :: dims(3)
    integer                          :: hdf5_err
    integer(HID_T)                   :: dset_id      ! dataset identifier
    integer(HID_T)                   :: dspace_id    ! dataspace identifier
    type(c_ptr)                      :: f_ptr        ! C data pointer type

    dims(:) = shape(value)
    ! create dataspace
    call h5screate_simple_f(3, dims, dspace_id, hdf5_err)

    ! create dataset
    call h5dcreate_f(file%file_id, trim(key), H5T_NATIVE_DOUBLE, dspace_id, &
       dset_id, hdf5_err)
    f_ptr = c_loc(value)
    
    ! write the dataset, Fortran 2003 interface
    call h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,f_ptr,hdf5_err)
    
    ! close the dataset
    call h5dclose_f(dset_id,hdf5_err)
    
    ! close the dataspace
    call h5sclose_f(dspace_id,hdf5_err)
    
#endif
  endsubroutine write_dbl_3d

!-------------------------------------------------------------------------------
endmodule file_hdf5
