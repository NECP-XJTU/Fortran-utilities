!###############################################################################
!> Module for specifying text file type
!>
!> @author Zhouyu Liu
!>   @date 04/21/2015
!>
!> @todo
!>   - Test: Added some comments.
!###############################################################################
module file_text

  use ISO_FORTRAN_ENV
  use error_warning
  use string
  use file_base, only : file_base_type

  implicit none
  private

# include <kind_parameter.h>

  public :: file_text_type

  !-----------------------------------------------------------------------------
  !Type to define a text file
  type,extends(file_base_type) :: file_text_type

    intknd :: i_line = 0

    !The element is extended from base time
  contains
    procedure,pass :: init   => init_file_text
    procedure,pass :: open   => open_file_text
    procedure,pass :: close  => close_file_text
    procedure,pass :: clear  => clear_file_text
    procedure,pass :: rewind => rewind_file_text
    procedure,pass :: backspace => backspace_file_text
    procedure,pass :: get_one_line
    procedure,pass :: get_all_lines
  endtype

!===============================================================================

contains

!-------------------------------------------------------------------------------
!> Initializes a text file
!-------------------------------------------------------------------------------
  subroutine init_file_text(file,unit,name,ext,path,is_read,is_write)

    class(file_text_type),intent(inout) :: file
    intknd,intent(in) :: unit
    charSt,intent(in) :: name
    charSt,intent(in) :: ext
    charSt,intent(in) :: path
    logknd,intent(in) :: is_read
    logknd,intent(in) :: is_write

    ! Local
    char10 :: str
    if(.not. file%is_init) then
      file%i_line = 0
      file%unit = unit
      file%name = name
      file%ext  = ext
      file%path = path
      file%is_read  = is_read
      file%is_write = is_write
      file%is_init = .true.

      call file%open()

    else
      call raise_Error('init_file_text :: the text file object'//              &
        ' has been initialized')
    endif

  endsubroutine init_file_text

!-------------------------------------------------------------------------------
!> open a text file
!-------------------------------------------------------------------------------
  subroutine open_file_text(file)

    charSt,parameter :: myName = 'open_file_text -- '
    class(file_text_type),intent(inout) :: file

    intknd :: iostat
    logknd :: is_exist
    char10 :: status,action

    if(file%is_init .and. .not.(file%is_open)) then
      !Set value for status
      if(file%is_read) then
        status = 'OLD'
      else
        status = 'REPLACE'
      endif

      !Set value for action
      if(file%is_read .and. .not.(file%is_write)) then
        action = 'READ'
      elseif(.not.file%is_read .and. file%is_write) then
        action = 'WRITE'
      else
        action = 'READWRITE'
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
        open(unit = file%unit, form = 'FORMATTED', position = 'REWIND',          &
          action=action,file = trim(str_join_path(file%path,file%name,file%ext)),      &
          status = status, iostat = iostat)

        if(iostat == 0) then
          file%is_open = .true.
          file%is_end = .false.
        else
          file%is_init = .false.
          call raise_Error(myName//'Failed to open '//trim(adjustl(file%path))//  &
            trim(adjustl(file%name))//trim(adjustl(file%ext)))
        endif
      endif

    endif

  endsubroutine open_file_text

!-------------------------------------------------------------------------------
!> close a text file
!-------------------------------------------------------------------------------
  subroutine close_file_text(file)
    class(file_text_type),intent(inout) :: file

    intknd :: ioerr

    if(file%is_open) then
      close(unit = file%unit, iostat = ioerr)
      if(ioerr == 0) then
        file%is_open = .false.
      else
        call raise_Error("close_file_text -- Cannot close file '"//            &
          trim(adjustl(file%path))//trim(adjustl(file%name))// &
            trim(adjustl(file%ext))//"'")
      endif
    endif

    file%is_end = .true.
    file%is_open = .false.

  endsubroutine close_file_text

!-------------------------------------------------------------------------------
!> clear a text file
!-------------------------------------------------------------------------------
  subroutine clear_file_text(file)
    class(file_text_type),intent(inout) :: file
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

  endsubroutine clear_file_text

!-------------------------------------------------------------------------------
!> rewind method for the text file type
!-------------------------------------------------------------------------------
  subroutine rewind_file_text(file)
    class(file_text_type),intent(inout) :: file

    intknd :: iostat

    if(file%is_init) then
      if(file%is_open) then
        rewind(unit = file%unit, iostat = iostat)
        if(iostat /= 0) &
          call raise_Error('rewind_file_text: Error rewinding' //              &
            ' file(unit = '//to_str(file%unit)//' ) iostat ='//to_str(iostat))
        file%i_line = 0
        file%is_end = .false.
      else
        call raise_Error('rewind_file_text -- file is opened.')
      endif
    else
      call raise_Error('rewind_file_text -- file is initialized.')
    endif

  endsubroutine rewind_file_text

!-------------------------------------------------------------------------------
!> backspace for a text file
!-------------------------------------------------------------------------------
  subroutine backspace_file_text(file)

    class(file_text_type),intent(inout) :: file

    intknd :: iostat

    if(file%is_init) then
      if(file%is_open) then
        backspace(unit = file%unit, iostat = iostat)
        if(iostat /= 0)                                                        &
          call raise_Error('backspace_file_text: Error '//                     &
            'backspacing file(unit = '//to_str(file%unit)//' ) iostat ='//     &
            to_str(iostat))
        if(.not.file%is_end) file%i_line = file%i_line -1
        file%is_end = .false.
      else
        call raise_Error('backspace_file_text -- file is not opened.')
      endif
    else
      call raise_Error('backspace_file_text -- file is not initialized.')
    endif

  endsubroutine backspace_file_text

!-------------------------------------------------------------------------------
!> read one line
!-------------------------------------------------------------------------------
  subroutine get_one_line(file,strline)

    class(file_text_type),intent(inout) :: file
    charln,intent(out) :: strline

	  ! Local
    charln :: buffer
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
          strline = trim(strline)//trim(buffer)
          file%i_line = file%i_line + 1

        !Error reading this line
        elseif(iostat < IOSTAT_EOR) then
          call raise_Error('get_one_line: Error reading one line'//            &
            ' from file (unit = '//to_str(file%unit)//'), IOSTAT = '//         &
            to_str(iostat))

        !Add buffer to strline, and continue reading
        else
          strline = trim(strline)//trim(buffer)

        endif
      enddo
    endif
  endsubroutine get_one_line
  
!-------------------------------------------------------------------------------
!> read all lines
!-------------------------------------------------------------------------------
  subroutine get_all_lines(file,strline)
    class(file_text_type),intent(inout) :: file
    character(len=256),intent(out) :: strline(:)

	  ! Local
    character(256) :: buffer
    intknd :: buffer_size, iostat,i_line

    buffer = ''
    iostat = 0
    strline = ''
    i_line = 1
    if(file%is_init .and. file%is_open .and. .not.(file%is_end)) then
      do while(iostat /= IOSTAT_END)

        !read one buffer
        read(unit=file%unit, FMT='(a)',size=buffer_size,advance='NO',          &
          iostat=iostat) buffer

        !At the end of file
        if(iostat == IOSTAT_END) then
          file%is_end = .true.

        !At the end of record
        elseif(iostat == IOSTAT_EOR) then
          strline(i_line) = trim(strline(i_line))//trim(buffer)
          file%i_line = file%i_line + 1
          i_line = i_line+1

        !Error reading this line
        elseif(iostat < IOSTAT_EOR) then
          call raise_Error('get_one_line: Error reading one line'//            &
            ' from file (unit = '//to_str(file%unit)//'), IOSTAT = '//         &
            to_str(iostat))

        !Add buffer to strline, and continue reading
        else
          strline(i_line) = trim(strline(i_line))//trim(buffer)

        endif
      enddo
    endif

  endsubroutine get_all_lines

!-------------------------------------------------------------------------------
endmodule file_text
