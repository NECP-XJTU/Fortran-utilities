!###############################################################################
!> Module for specifying base file type
!>
!> @author Zhouyu Liu
!>   @date 04/21/2015
!>
!> @todo
!>   - Test: Added some comments.
!###############################################################################
module error_warning

  use ISO_FORTRAN_ENV

  implicit none
  private

# include <kind_parameter.h>

  intknd :: t_error_unit = ERROR_UNIT

  public :: raise_error
  public :: raise_fatal
  public :: raise_warning
  public :: raise_message

  public :: get_nWarning
  public :: get_nError
  public :: set_quiet
  public :: get_is_quiet
  public :: set_stop_on_error
  public :: get_is_stop_on_error
  public :: set_log_unit

  intknd,save,private :: nWarning         = 0
  intknd,save,private :: nError           = 0
  intknd,save,private :: nMsg             = 0
  intknd,save,private :: log_unit         = 886
  logknd,save,private :: is_quiet         = .true.
  logknd,save,private :: is_log_open      = .false.
  logknd,save,private :: is_stop_on_error = .true.

!===============================================================================

contains

!-------------------------------------------------------------------------------
! Set symble of whether to print warnings and errors to ERROR_UNIT
!-------------------------------------------------------------------------------
  subroutine set_stop_on_error(bool)
    logknd,intent(in) :: bool

    is_stop_on_error = bool

  endsubroutine set_stop_on_error

!-------------------------------------------------------------------------------
! Set symble of whether to print warnings and errors to ERROR_UNIT
!-------------------------------------------------------------------------------
  subroutine set_quiet(bool)
    logknd,intent(in) :: bool

    is_quiet = bool

  endsubroutine set_quiet

!-------------------------------------------------------------------------------
! Set symble of whether to print warnings and errors to ERROR_UNIT
!-------------------------------------------------------------------------------
  function get_nError() result(ne)

    intknd :: ne

    ne = nError

  endfunction get_nError

!-------------------------------------------------------------------------------
! Set symble of whether to print warnings and errors to ERROR_UNIT
!-------------------------------------------------------------------------------
  function get_nWarning() result(nw)

    intknd :: nw

    nw = nWarning

  endfunction get_nWarning

!-------------------------------------------------------------------------------
! Get the is_quiet value
!-------------------------------------------------------------------------------
  function get_is_quiet() result(isQuiet)

    logknd :: isQuiet

    isQuiet = is_quiet

  endfunction get_is_quiet

!-------------------------------------------------------------------------------
! Get the is_stop_on_error value
!-------------------------------------------------------------------------------
  function get_is_stop_on_error() result(stop_on_error)

    logknd :: stop_on_error

    stop_on_error = is_stop_on_error

  endfunction get_is_stop_on_error

!-------------------------------------------------------------------------------
! Set the unit of log file
!-------------------------------------------------------------------------------
  subroutine set_log_unit(unit)

    intknd,intent(in) :: unit

    logknd :: is_ok

    is_OK = check_log_file(unit)
    if(is_OK) then
      log_unit = unit
      is_log_open = .true.
    else
      is_log_open = .false.
      call raise_warning('set_log_unit -- Failed setting log unit, No warnings &
        & and erros will be printed to log file.')
    endif

  endsubroutine


!-------------------------------------------------------------------------------
! Set symble of whether to print errors
!-------------------------------------------------------------------------------
  subroutine raise_message(msg)
    charSt,intent(in) :: msg

    charkd(len=len_trim(msg)+7) :: message

    !Print to the ERROR_UNIT
    call print_message(T_ERROR_UNIT, message, 7_kip)

    !Print to the log file
    if(is_log_open) then
      if(check_log_file(log_unit)) then
        call print_message(log_unit, message, 7_kip)
        flush(log_unit)
      endif
    endif
  endsubroutine raise_message


!-------------------------------------------------------------------------------
! Set symble of whether to print errors
!-------------------------------------------------------------------------------
  subroutine raise_fatal(msg)
    charSt,intent(in) :: msg

    charkd(len=len_trim(msg)+7) :: message

    message = 'FATAL: '//msg

    !Print to the ERROR_UNIT
    if(.not. is_quiet) then
      call print_message(T_ERROR_UNIT, message, 7_kip)
      call print_message(T_ERROR_UNIT, 'Please check the log file', 7_kip)
    endif

    !Print to the log file
    if(is_log_open) then
      if(check_log_file(log_unit)) then
        call print_message(log_unit, message, 7_kip)
        flush(log_unit)
      endif
    endif

    nError = nError + 1
    stop 886

  endsubroutine raise_fatal


!-------------------------------------------------------------------------------
! Set symble of whether to print errors
!-------------------------------------------------------------------------------
  subroutine raise_error(msg)
    charSt,intent(in) :: msg

    charkd(len=len_trim(msg)+7) :: message

    !Print to the ERROR_UNIT
    message = 'ERROR: '//msg

    if(.not. is_quiet) then
      call print_message(T_ERROR_UNIT, message, 7_kip)
    endif

    !Print to the log file
    if(is_log_open) then
      if(check_log_file(log_unit)) then
        call print_message(log_unit, message, 7_kip)
        flush(log_unit)
      endif
    endif

    nError = nError + 1
    if(is_stop_on_error) stop 886

  endsubroutine raise_error

!-------------------------------------------------------------------------------
! Set symble of whether to print errors
!-------------------------------------------------------------------------------
  subroutine raise_warning(msg)
    charSt,intent(in) :: msg

    charkd(len=len_trim(msg)+9) :: message

    message = 'WARNING: '//msg
   !Print to the ERROR_UNIT
    if(.not. is_quiet) then
      call print_message(T_ERROR_UNIT, message, 9_kip)
    endif

    !Print to the log file
    if(is_log_open) then
      if(check_log_file(log_unit)) then
        call print_message(log_unit, message, 9_kip)
        flush(log_unit)
      endif
    endif

    nWarning = nWarning + 1

  endsubroutine raise_warning

!-------------------------------------------------------------------------------
! print message to specified file unit
!-------------------------------------------------------------------------------
  subroutine print_message(unit,msg,indent)

    intknd,intent(in) :: unit
    charSt,intent(in) :: msg
    intknd,intent(in) :: indent

    intknd :: i_stt
    intknd :: i_stp
    intknd :: line_wrap
    intknd :: length

    nMsg = nMsg + 1

    ! Set line wrapping and indentation
    line_wrap= 80

    !The length of message
    length = len_trim(msg)

    i_stt = 0
    do
      if (length - i_stt < line_wrap - indent + 1) then

        ! Remainder of message will fit on line
        write(unit, fmt='(A)') msg(i_stt+1:length)
        exit

      !Multi-line output
      else

        ! Determine last space in current line
        i_stp = i_stt + index(msg(i_stt+1:i_stt+line_wrap-indent+1), &
             ' ', BACK=.true.)

        ! This is a special case where there is no space
        if (i_stp == i_stt) then
          i_stp = i_stt + line_wrap - indent + 1
          write(unit, fmt='(A/A)', advance='no') &
               msg(i_stt+1:i_stp-1), repeat(' ', indent)
          i_stp = i_stp - 1

        ! Write up to last space
        else
          write(unit, fmt='(A/A)', advance='no') &
               msg(i_stt+1:i_stp-1), repeat(' ', indent)
        end if

        ! Advance starting position
        i_stt = i_stp
        if (i_stt > length) exit
      end if
    end do

  endsubroutine print_message

!-------------------------------------------------------------------------------
! check if the log file is ready for print message
!-------------------------------------------------------------------------------
  function check_log_file(unit) result(is_ok)

    charSt,parameter :: myName = 'set_log_unit -- '
    intknd,intent(in) :: unit

    logknd :: is_ok

    logknd :: is_open
    char10 :: str_stat
    intknd :: nw

    is_ok = .false.
    nw = nMsg
    !check unit
    if(unit == OUTPUT_UNIT) then
      call print_message(T_ERROR_UNIT, 'WARNING: '//&
        myName//'unit value is equal to default OUTPUT_UNIT.', 9_kip)
    elseif(unit == ERROR_UNIT) then
      call print_message(T_ERROR_UNIT, 'WARNING: '//&
        myName//'unit value is equal to default ERROR_UNIT.', 9_kip)
    elseif(unit == INPUT_UNIT) then
      call print_message(T_ERROR_UNIT, 'WARNING: '//&
        myName//'unit value is equal to default INPUT_UNIT.', 9_kip)
    endif
    if(nMsg == nw) then
      !if the file is open
      inquire(unit = unit, opened = is_open)
      if(.not. is_open) call print_message(T_ERROR_UNIT, &
        'WARNING: '//myName//'The log file is not open.', 9_kip)

      !if the file is formatted filed
      inquire(unit = unit, form = str_stat)
      if(trim(str_stat) /= 'FORMATTED') &
        call print_message(T_ERROR_UNIT, &
          'WARNING: '//myName//'The log file is not FORMATTED.', 9_kip)

      !if the file is SEQUENTIAL filed
      inquire(unit = unit, access = str_stat)
      if(trim(str_stat) /= 'SEQUENTIAL') &
        call print_message(T_ERROR_UNIT, &
          'WARNING: '//myName//'The log file is not SEQUENTIAL.', 9_kip)

      !if the file is WRITABLE filed
      inquire(unit = unit, action = str_stat)
      if(.not.(trim(str_stat) == 'WRITE' .or. trim(str_stat) == 'READWRITE'))  &
        call print_message(T_ERROR_UNIT, &
          'WARNING: '//myName//'The log file is not WRITABLE.', 9_kip)

    endif

    nWarning = nWarning + nMsg - nW
    if(nMsg == nw) is_ok = .true.

  endfunction check_log_file

!-------------------------------------------------------------------------------
endmodule error_warning
