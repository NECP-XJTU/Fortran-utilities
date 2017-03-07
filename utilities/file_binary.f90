!###############################################################################
!> Module for specifying binary file type
!>
!> @author Zhouyu Liu
!>   @date 04/21/2015
!>
!> @todo
!>   - Test: Added some comments.
!###############################################################################
module file_binary

  use file_base, only : file_base_type
  use error_warning

  implicit none
  private

# include <kind_parameter.h>

  !public list
  public :: file_binary_type


  !-----------------------------------------------------------------------------
  !Type to difine a binary file
  type,extends(file_base_type) :: file_binary_type

    !The element is extended from base time
  contains
    procedure,pass :: init  => init_file_binary
    procedure,pass :: open  => open_file_binary
    procedure,pass :: close => close_file_binary
    procedure,pass :: clear => clear_file_binary
  endtype

!===============================================================================

contains

!-------------------------------------------------------------------------------
!> Initializes a binary file
!-------------------------------------------------------------------------------
  subroutine init_file_binary(file,unit,name,ext,path,is_read,is_write)

      class(file_binary_type),intent(inout) :: file

      intknd,intent(in) :: unit
      charSt,intent(in) :: name
      charSt,intent(in) :: ext
      charSt,intent(in) :: path
      logknd,intent(in) :: is_read
      logknd,intent(in) :: is_write
      call raise_error('init_file_binary NOT supported')

  endsubroutine init_file_binary

!-------------------------------------------------------------------------------
!> open a binary file
!-------------------------------------------------------------------------------
  subroutine open_file_binary(file)

    class(file_binary_type),intent(inout) :: file

    char10 :: status,action
    if(file%is_init) then

      !Set value for status
      if(file%is_read .and. .not.(file%is_write)) then
        status = 'OLD'
      else
        status = 'REPLACE'
      endif

      !Set value for action
      if(file%is_read .and. (.not.(file%is_write))) then
        action = 'READ'
      elseif(file%is_write .and. (.not.(file%is_read))) then
        action = 'WRITE'
      elseif(file%is_write .and. file%is_read) then
        action = 'READWRITE'
      endif



      !OPEN(UNIT=file%unitno,STATUS=status, &
      !  ACCESS='SEQUENTIAL',FORM=UNFORMATTED, &
      !    ACTION=TRIM(actionvar),FILE=TRIM(file%path)// &
      !      TRIM(file%name)//TRIM(file%Ext), &
      !        IOSTAT=ioerr)
      !IF(ioerr /= 0) THEN
      !  WRITE(emesg,'(a,i4,a,i4)') 'Error opening file "'// &
      !    TRIM(file%path)//TRIM(file%name)//TRIM(file%ext)// &
      !    '" (UNIT=',file%unitno,') IOSTAT=',ioerr
      !  CALL file%acd%raiseError('open_binary_file'//' - '//emesg)
      !else
      !  file%is_open = .true.
      !  file%is_end = .true.
      !endif
      !  CALL file%acd%raiseError('open_binary_file'// &
      !    ' - binary file has not been initialized!')
    endif
  endsubroutine open_file_binary


!-------------------------------------------------------------------------------
!> close a binary file
!-------------------------------------------------------------------------------
  subroutine close_file_binary(file)
    class(file_binary_type),intent(inout) :: file

  endsubroutine close_file_binary

!-------------------------------------------------------------------------------
!> clear a binary file
!-------------------------------------------------------------------------------
  subroutine clear_file_binary(file)
    class(file_binary_type),intent(inout) :: file

  endsubroutine clear_file_binary

!-------------------------------------------------------------------------------
endmodule file_binary
