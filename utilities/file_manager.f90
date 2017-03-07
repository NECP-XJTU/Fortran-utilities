!###############################################################################
!> Module for specifying file manager type
!>
!> @author Zhouyu Liu
!>   @date 04/21/2015
!>
!> @todo
!>   - Test: Added some comments.
!###############################################################################
module file_manager

  use ISO_FORTRAN_ENV
  use error_warning
  use string
  use file_base,      only: file_base_type,file_container_type
  use file_text,      only: file_text_type
  use file_binary,    only: file_binary_type
  use hash_table,     only: int_int_hash_table
  use file_hdf5,      only: file_hdf5_type
  use parallel_env,   only: parallel_env_type

  implicit none
  private

# include <kind_parameter.h>

  public :: file_manager_type

  !-----------------------------------------------------------------------------
  !> file manager type to manager all inp, out, log and XS files
  type :: file_manager_type

    intknd :: n_file = 0                                  !< # of files
    type(file_container_type),allocatable   :: files(:)   !< All files

  contains

    procedure,pass :: clear
    procedure,pass :: add                                 !< file adding method for file manager
    procedure,pass :: get_base_file_with_unit             !< get a base file with "name", ".ext" or "name.ext"
    procedure,pass :: get_base_file_with_str              !< get a base file with the file unit
    generic :: get_base_file => get_base_file_with_unit, &!< get a base file with the unit or str
                                get_base_file_with_str

    procedure,pass :: get_text_file_with_str              !< get a text file with "name", ".ext" or "name.ext"
    procedure,pass :: get_text_file_with_unit             !< get a text file with the file unit
    generic :: get_text_file => get_text_file_with_str, & !< get a text file with the unit or str
                                get_text_file_with_unit

    procedure,pass :: get_hdf5_file_with_str              !< get an hdf5 file with "name", ".ext" or "name.ext"
    procedure,pass :: get_hdf5_file_with_unit             !< get an hdf5 file with the file unit
    generic :: get_hdf5_file => get_hdf5_file_with_str, & !< get an hdf5 file with the unit or str
                                get_hdf5_file_with_unit

    procedure,pass :: get_binary_file_with_unit                !< get a text file with "name", ".ext" or "name.ext"
    procedure,pass :: get_binary_file_with_str                 !< get a text file with the file unit
    generic :: get_binary_file => get_binary_file_with_unit, & !< get a text file with the unit or str
                                  get_binary_file_with_str

    procedure,pass :: get_unit => get_unit_with_str       !< get the file unit with "name", ".ext" or "name.ext"

  endtype

!===============================================================================

contains
!-------------------------------------------------------------------------------
!> add a file to the file manager
!> file_m,   - in,          the file manager object
!> file_type - in,          type of file, suport 'text', 'binary' now.
!> file      - in,          full path of the file, like  "path/name.ext"
!> readwrite - optional in, access permission of the file, suport 'read',
!>            'write', 'readwrite'
!> unit      - optional in, the specified unit number of the file.
!-------------------------------------------------------------------------------
  subroutine add(file_m,file_type,file,readwrite,unit)

    charSt,parameter :: myName = 'file_manager_type%add :: '
    class(file_manager_type),intent(inout) :: file_m
    charSt,         intent(in) :: file_type
    charSt,         intent(in) :: file
    charSt,optional,intent(in) :: readwrite
    intknd,optional,intent(in) :: unit

    class(file_base_type),pointer :: file_temp => null()
    type(file_container_type),allocatable :: file_temps(:)

    logknd :: is_read,is_write
    logknd :: is_exist,bool
    intknd :: nerror,funit,file_number,i
    char50 :: strtem,str_filetype
    character(500) :: path
    character(500)  :: name,ext
    !Check input values
    nerror = get_nError()

    !check itype
    str_filetype = str_to_low(trim(adjustl(file_type)))
    if(trim(str_filetype) /= 'text' .and. trim(str_filetype) /= 'binary'       &
      .and. trim(str_filetype) /= 'hdf5') &
      call raise_error(myName//'- file type = '//trim(file_type)//         &
        ' is not recognized.')

    !check unit
    if(present(unit)) then
      bool = check_unit(file_m,unit)
      if(bool) funit = unit
    else
      funit = file_m%n_file + 100
      if(.not. check_unit(file_m,funit)) &
        call raise_error(myName//'- Automatically generated unit is USED.')
    endif

    !check readwrite
    if(present(readwrite)) then
      is_write = .false.
      is_read = .false.
      strtem = str_to_cap(adjustl(readwrite))
      if(trim(strtem) == 'READ') then
        is_read = .true.
      elseif(trim(strtem) == 'WRITE') then
        is_write = .true.
      elseif(trim(strtem) == 'READWRITE') then
        is_read = .true.
        is_write = .true.
      else
        call raise_error(myName//' - readwrite NOT recognized')
      endif
    else
      call raise_warning(myName//' - READ/WRITE not specified, '// &
        'default file is readwriteable!')
      is_read = .true.
      is_write = .true.
    endif

    if(is_read) then
      inquire(file=file,EXIST=is_exist)
      if(.not. is_exist) &
        call raise_error(myName//' - Readable file: '//trim(file)// &
          ' does not exist.')
    endif

    inquire(file=file,opened=bool)
    if(bool) call raise_error(myName//' - file: '//trim(file)// &
        ' already opened.')

    !Input is correct
    if(nerror == get_nError()) then
      !Get the path, name and ext of the file.
      !To be done here. it is waiting for the string module.
      path = str_path_of_file(file)
      name = str_name_of_file(file)
      ext  = str_ext_of_file(file)

      if(trim(str_filetype) == 'text') then
        allocate(file_text_type :: file_temp)
      elseif(trim(str_filetype) == 'binary') then
        allocate(file_binary_type :: file_temp)
      elseif(trim(str_filetype) == 'hdf5') then
        allocate(file_hdf5_type :: file_temp)
      endif

      call file_temp%init(funit,name,ext,path,is_read,is_write)

      ! If succeeded add it to the manager
      if(.not. allocated(file_m%files)) then
        file_m%n_file = 1
        allocate(file_m%files(1))
        file_m%files(1)%file => file_temp
      else
        ! One more file types
        file_m%n_file = file_m%n_file + 1

        ! Add the new file object to the container list
        allocate(file_temps(file_m%n_file))
        file_temps(1:file_m%n_file - 1) = file_m%files
        call MOVE_ALLOC(FROM = file_temps, TO = file_m%files)
        file_m%files(file_m%n_file)%file => file_temp
      endif

      nullify(file_temp)

    endif
  endsubroutine add

!-------------------------------------------------------------------------------
!> clear all files from file manager
!-------------------------------------------------------------------------------
  subroutine clear(file_m)

    class(file_manager_type),intent(inout) :: file_m
    intknd :: i

    if (allocated(file_m%files)) then
      do i = 1, file_m%n_file
        call file_m%files(i)%file%clear()
        deallocate(file_m%files(i)%file)
      enddo
      file_m%n_file = 0
    endif

  endsubroutine clear

!-------------------------------------------------------------------------------
!> check if the user input unit of the file is leagle
!-------------------------------------------------------------------------------
  function check_unit(file_m,unit)  result(bool)
    charSt,parameter :: myName = 'file_manager_type%check_unit :: '

    class(file_manager_type),intent(inout) :: file_m
    intknd,                  intent(in)    :: unit

    intknd :: i,nerror
    logknd :: bool

    bool = .true.

    nerror = get_nerror()
    !check unit
    if(unit == OUTPUT_UNIT) then
      call raise_error(myName//' - Illegal value for input argument UNIT! '//  &
        'Value is equal to default OUTPUT_UNIT.')
    elseif(unit == ERROR_UNIT) then
      call raise_error(myName//' - Illegal value for input argument UNIT! '//  &
        'Value is equal to default ERROR_UNIT.')
    elseif(unit == INPUT_UNIT) then
      call raise_error(myName//' - Illegal value for input argument UNIT! '//  &
        'Value is equal to efault INPUT_UNIT.')
    else
      do i = 1, file_m%n_file
        if(unit == file_m%files(i)%file%unit) then
          call raise_warning(myName//' - Illegal value for input argument '//  &
            'UNIT! Value has been used for other files.')
        endif
      enddo
    endif

    if(nerror /= get_nerror()) bool = .false.

  endfunction check_unit



!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  function get_binary_file_with_str(file_m,str) result(file)
    charSt,parameter :: myName = 'file_manager_type%get_binary_file_with_str :: '

    class(file_manager_type),intent(inout) :: file_m
    charSt,                  intent(in)    :: str

    type(file_binary_type),pointer :: file
    class(file_base_type),pointer :: file_base => null()

    file => null()
    file_base => get_base_file_with_str(file_m,str)

    if(associated(file_base)) then
      selecttype(file_base)
      type is(file_binary_type)
        file => file_base
      class default
        call raise_warning(myName//' - No binary file with"'//trim(str)// &
          '" was fould')
      endselect
    else
      call raise_warning(myName//' - No binary file with"'//trim(str)// &
        '" was fould')
    endif

  endfunction get_binary_file_with_str

!-------------------------------------------------------------------------------
  function get_binary_file_with_unit(file_m,unit) result(file)
    charSt,parameter :: myName = 'file_manager_type%get_binary_file_with_unit :: '

    class(file_manager_type),intent(inout) :: file_m
    intknd,                  intent(in)    :: unit

    type(file_binary_type),pointer :: file
    class(file_base_type),pointer :: file_base => null()


    file => null()
    file_base => get_base_file_with_unit(file_m,unit)
    if(associated(file_base)) then
      selecttype(file_base )
      type is(file_binary_type)
        file => file_base
      class default
        call raise_warning(myName//' - No binary file with"'//trim(to_str(unit))// &
          '" unit was fould')
      endselect
    else
      call raise_warning(myName//' - No binary file with"'//trim(to_str(unit))// &
        '" unit was fould')
    endif

  endfunction get_binary_file_with_unit


!-------------------------------------------------------------------------------
  function get_text_file_with_str(file_m,str) result(file)
    charSt,parameter :: myName = 'file_manager_type%get_text_file_with_str :: '

    class(file_manager_type),intent(inout) :: file_m
    charSt,                  intent(in)    :: str

    type(file_text_type),pointer :: file
    class(file_base_type),pointer :: file_base => null()


    file => null()
    file_base => get_base_file_with_str(file_m,str)
    if(associated(file_base)) then
      selecttype(file_base)
      type is(file_text_type)
        file => file_base
      class default
        call raise_warning(myName//' - No text file with"'//trim(str)// &
          '" was fould')
      endselect
    else
        call raise_warning(myName//' - No text file with"'//trim(str)// &
          '" was fould')
    endif

  endfunction get_text_file_with_str

!-------------------------------------------------------------------------------
  function get_text_file_with_unit(file_m,unit) result(file)
    charSt,parameter :: myName = 'file_manager_type%get_text_file_with_unit :: '

    class(file_manager_type),intent(inout) :: file_m
    intknd,                  intent(in)    :: unit

    type(file_text_type),pointer :: file
    class(file_base_type),pointer :: file_base => null()


    file => null()
    file_base => get_base_file_with_unit(file_m,unit)
    if(associated(file_base)) then
      selecttype(file_base)
      type is(file_text_type)
        file => file_base
      class default
        call raise_warning(myName//' - No text file with"'//trim(to_str(unit))// &
          '" unit was fould')
      endselect
    else
      call raise_warning(myName//' - No text file with"'//trim(to_str(unit))// &
        '" unit was fould')
    endif

  endfunction get_text_file_with_unit

!-------------------------------------------------------------------------------
  function get_hdf5_file_with_str(file_m,str) result(file)
    charSt,parameter :: myName = 'file_manager_type%get_hdf5_file_with_str :: '

    class(file_manager_type),intent(inout) :: file_m
    charSt,                  intent(in)    :: str

    type(file_hdf5_type),pointer :: file
    class(file_base_type),pointer :: file_base => null()

    file => null()
    file_base => get_base_file_with_str(file_m,str)

    if(associated(file_base)) then
      selecttype(file_base)
        type is(file_hdf5_type)
          file => file_base
        class default
          call raise_warning(myName//' - No hdf5 file with"'//trim(str)// &
            '" was fould')
      endselect
    else
      call raise_warning(myName//' - No hdf5 file with"'//trim(str)// &
            '" was fould')
    endif

  endfunction get_hdf5_file_with_str

!-------------------------------------------------------------------------------
  function get_hdf5_file_with_unit(file_m,unit) result(file)
    charSt,parameter :: myName = 'file_manager_type%get_hdf5_file_with_unit :: '

    class(file_manager_type),intent(inout) :: file_m
    intknd,                  intent(in)    :: unit

    type(file_hdf5_type),pointer :: file
    class(file_base_type),pointer :: file_base => null()


    file => null()
    file_base => get_base_file_with_unit(file_m,unit)
    if(associated(file_base)) then
      selecttype(file_base)
      type is(file_hdf5_type)
        file => file_base
      class default
        call raise_warning(myName//' - No hdf5 file with"'//trim(to_str(unit))// &
          '" unit was fould')
      endselect
    else
      call raise_warning(myName//' - No hdf5 file with"'//trim(to_str(unit))// &
        '" unit was fould')
    endif

  endfunction get_hdf5_file_with_unit

!-------------------------------------------------------------------------------
  function get_base_file_with_unit(file_m,unit) result(file)

    class(file_manager_type),intent(inout) :: file_m
    intknd,                  intent(in)    :: unit

    class(file_base_type),pointer :: file
    intknd :: i

    file => null()
    do i = 1, file_m%n_file
      if(file_m%files(i)%file%unit == unit) then
        file => file_m%files(i)%file
        exit
      endif
    enddo

  endfunction get_base_file_with_unit

!-------------------------------------------------------------------------------
  function get_base_file_with_str(file_m,str) result(file)
    charSt,parameter :: myName = 'file_manager_type%get_base_file_with_str :: '

    class(file_manager_type),intent(inout) :: file_m
    charSt,                  intent(in)    :: str
    class(file_base_type),pointer :: file

    char50 :: name, ext
    intknd :: len_name,len_ext

    file => null()
    name = str_name_of_file(str)
    ext  = str_ext_of_file(str)

    len_name = len_trim(adjustl(name))
    len_ext = len_trim(adjustl(ext))

    if(len_name /= 0 .and. len_ext /= 0) then
      file => get_base_file_with_name_ext(file_m,name,ext)
    elseif(len_name /= 0) then
      file => get_base_file_with_name(file_m,name)
    elseif(len_ext /= 0) then
      file => get_base_file_with_ext(file_m,ext)
    else
      call raise_warning(myName// ' - input of string is empty!')
    endif

  endfunction get_base_file_with_str


!-------------------------------------------------------------------------------
  function get_base_file_with_name_ext(file_m,name,ext) result(file)
    charSt,parameter :: myName = 'file_manager_type%get_base_file_with_name_ext :: '

    class(file_manager_type),intent(inout) :: file_m
    charSt,                  intent(in)    :: name
    charSt,                  intent(in)    :: ext

    class(file_base_type),pointer :: file
    intknd :: ith, n, i

    file => null()
    ith = 0
    n = 0
    do i = 1, file_m%n_file
      if(trim(file_m%files(i)%file%name) == trim(adjustl(name)) .and. &
        trim(file_m%files(i)%file%ext) == trim(adjustl(ext))) then
        n = n + 1
        ith = i
      endif
    enddo
    if(n == 0) then
      call raise_warning(myName//' - NO '//trim(adjustl(name))// &
        trim(adjustl(ext))//' file in the manager, null() is returned.')
    elseif(n > 1) then
      call raise_warning(myName//' - More than one '//trim(adjustl(name))// &
        trim(adjustl(ext))//' files in the manager, the last one is returned.')
    endif
    if(ith > 0) file => file_m%files(ith)%file

  endfunction get_base_file_with_name_ext

!-------------------------------------------------------------------------------
  function get_base_file_with_name(file_m,name) result(file)
    charSt,parameter :: myName = 'file_manager_type%get_base_file_with_name :: '

    class(file_manager_type),intent(inout) :: file_m
    charSt,                  intent(in)    :: name

    class(file_base_type),pointer :: file
    intknd :: ith, n, i

    file => null()
    ith = 0
    n = 0
    do i = 1, file_m%n_file
      if(trim(file_m%files(i)%file%name) == trim(adjustl(name))) then
        n = n + 1
        ith = i
      endif
    enddo
    if(n == 0) then
      call raise_warning(myName//' - NO '//trim(adjustl(name))// &
      ' file in the manager, null() is returned.')
    elseif(n > 1) then
      call raise_warning(myName//' - More than one '//trim(adjustl(name))// &
      ' files in the manager, the last one is returned.')
    endif
    if(ith > 0) file => file_m%files(ith)%file
  endfunction get_base_file_with_name

!-------------------------------------------------------------------------------
  function get_base_file_with_ext(file_m,ext) result(file)
    charSt,parameter :: myName = 'file_manager_type%get_base_file_with_ext :: '

    class(file_manager_type),intent(inout) :: file_m
    charSt,                  intent(in)    :: ext

    class(file_base_type),pointer :: file
    intknd :: ith, n, i

    file => null()
    ith = 0

    n = 0
    do i = 1, file_m%n_file
      if(trim(file_m%files(i)%file%ext) == trim(adjustl(ext))) then
        n = n + 1
        ith = i
      endif
    enddo
    if(n == 0) then
      call raise_warning(myName//' - NO '//trim(adjustl(ext))// &
      ' file in the manager, null() is returned.')
    elseif(n > 1) then
      call raise_warning(myName//' - More than one '//trim(adjustl(ext))// &
      ' files in the manager, the last one is returned.')
    endif
    if(ith > 0) file => file_m%files(ith)%file
  endfunction get_base_file_with_ext


!-------------------------------------------------------------------------------
  function get_unit_with_str(file_m,str) result(unit)
    charSt,parameter :: myName = 'file_manager_type%get_unit_with_str :: '

    class(file_manager_type),intent(inout) :: file_m
    charSt,                  intent(in)    :: str
    intknd                                 :: unit

    class(file_base_type),pointer :: file => null()

    unit = -1
    file => null()
    file => get_base_file_with_str(file_m,str)
    if(associated(file)) then
      unit = file%unit
    else
      call raise_warning(myName//' - NO '//trim(str)// &
        ' file in the manager, -1 is returned.')
    endif
  endfunction get_unit_with_str

!-------------------------------------------------------------------------------
endmodule file_manager
