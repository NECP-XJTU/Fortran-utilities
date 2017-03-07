!###############################################################################
!> Module for specifying base file type
!>
!> @author Zhouyu Liu
!>   @date 04/21/2015
!>
!> @todo
!>   - Test: Added some comments.
!###############################################################################
module file_base

  use iso_fortran_env, only: error_unit, output_unit, input_unit

  implicit none
  private

# include <kind_parameter.h>

  !public files
  public :: file_base_type, file_container_type,get_new_unit, delete_file

  !-----------------------------------------------------------------------------
  !>Abstrac type for files
  type,abstract :: file_base_type
    logknd :: is_init = .false.   !< Whether the object is initialized
    intknd :: unit    = -1        !< The unit of the file
    character(200) :: path = ''   !< Full path of file object
    character(50 ) :: name = ''   !< Name of the file
    character(50 ) :: ext  = ''   !< extention of the file
    logknd :: is_open = .false.   !< Whether the file is opened
    logknd :: is_end  = .false.   !< Whether it is the end of the file
    logknd :: is_read = .false.   !< Whether the file is readable
    logknd :: is_write= .false.   !< Whether the file is writeable
  contains
    procedure(file_base_init_abstract),deferred,pass :: init
    procedure(file_base_open_abstract),deferred,pass :: open
    procedure(file_base_close_abstract),deferred,pass :: close
    procedure(file_base_clear_abstract),deferred,pass :: clear
  endtype file_base_type

  !-----------------------------------------------------------------------------
  !>the container type of file
  type :: file_container_type
    class(file_base_type),pointer :: file => null()
  endtype

  !-----------------------------------------------------------------------------
  !>the interface of init method for base file type
  abstract interface
    subroutine file_base_init_abstract(file,unit,name,ext,path,is_read,is_write)

      import :: file_base_type
# include <kind_parameter.h>

      class(file_base_type),intent(inout) :: file
      intknd,intent(in) :: unit
      charSt,intent(in) :: name
      charSt,intent(in) :: ext
      charSt,intent(in) :: path
      logknd,intent(in) :: is_read
      logknd,intent(in) :: is_write

    endsubroutine file_base_init_abstract
  endinterface

  !-----------------------------------------------------------------------------
  !>the interface of open method for base file type
  abstract interface
    subroutine file_base_open_abstract(file)

      import :: file_base_type

      class(file_base_type),intent(inout) :: file

    endsubroutine file_base_open_abstract
  endinterface

  !-----------------------------------------------------------------------------
  !>the interface of close method for base file type
  abstract interface
    subroutine file_base_close_abstract(file)

      import :: file_base_type

      class(file_base_type),intent(inout) :: file

    endsubroutine file_base_close_abstract
  endinterface

  !-----------------------------------------------------------------------------
  !>the interface of clear method for base file type
  abstract interface
    subroutine file_base_clear_abstract(file)

      import :: file_base_type

      class(file_base_type),intent(inout) :: file

    endsubroutine file_base_clear_abstract
  endinterface

contains

  !-------------------------------------------------------------------------------
  !> get a new file unit that is not occupied.
  !-------------------------------------------------------------------------------
  function get_new_unit() result(unit)

    intknd :: unit
    logknd :: opened

    unit = 1
    do
      inquire(unit=unit, opened=opened)
      if ((.not. opened) .and. unit /= error_unit .and. unit /= output_unit&
        & .and. unit /= input_unit) then
        exit
      end if
      unit = unit + 1
    end do

  end function get_new_unit

  !-------------------------------------------------------------------------------
  !> delete file
  !-------------------------------------------------------------------------------
  subroutine delete_file(fname)
    charSt, intent(in) :: fname

    logknd :: file_exists
    logknd :: file_opened
    intknd :: file_unit

    inquire(file=trim(adjustl(fname)), exist=file_exists)
    if (file_exists) then
      inquire(file=trim(adjustl(fname)), opened=file_opened)
      if (file_opened) then
        inquire(file=trim(adjustl(fname)), number=file_unit)
      else
        open(newunit=file_unit, file=trim(adjustl(fname)))
      end if
      close(unit=file_unit, status='delete')
    end if

  end subroutine delete_file

!-------------------------------------------------------------------------------
endmodule file_base
