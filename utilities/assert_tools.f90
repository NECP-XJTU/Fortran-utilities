module assert_tools
  implicit none
  private

# include <kind_parameter.h>

  public :: procedure_assert_start
  public :: procedure_assert_component_begin
  public :: procedure_assert
  public :: procedure_assert_component_summary
  public :: procedure_assert_finish

  intknd,save :: n_error_component = 0  !< Number of errors of current component
  intknd,save :: n_assert_component = 0 !< Number of assertions of current comp
  intknd,save :: n_error = 0            !< Total number of errors
  intknd,save :: n_assert = 0           !< Total number of assertions

!===============================================================================
contains

!-------------------------------------------------------------------------------
!> Start of assertion: initialize the number of errors and assertions
!-------------------------------------------------------------------------------
  subroutine procedure_assert_start()

    ! Every time it is called, it increases
    n_error = 0
    n_assert = 0
    n_error_component = 0
    n_assert_component = 0

    write(*,*) '      ###### ASSERTION ###### '
    write(*,*) '---------------------------------------------------------------'
  endsubroutine procedure_assert_start

!-------------------------------------------------------------------------------
!> Start of assertion of one component: initialize the number of errors and
!> assertions of current component
!-------------------------------------------------------------------------------
  subroutine procedure_assert_component_begin(string)
    charSt,intent(in) :: string  !< Message to print

    ! Every time it is called, it increases
    n_error_component = 0
    n_assert_component = 0

    write(*,'(a)') 'Assertion begin: '//trim(adjustl(string))
  endsubroutine procedure_assert_component_begin

!-------------------------------------------------------------------------------
!> Start of assertion of one component: summary the number of errors and
!> assertions of current component
!-------------------------------------------------------------------------------
  subroutine procedure_assert_component_summary(string)

    charSt,intent(in) :: string  !< Message to print

    char10 :: str_n_error_component  = ''
    char10 :: str_n_assert_component = ''

    !Body of the subroutine
    write(str_n_error_component,'(i0)') n_error_component
    write(str_n_assert_component,'(i0)') n_assert_component

    if(n_error_component /= 0) then
      write(*,'(a)') '  FAILED, '//trim(adjustl(string))//' summary: '//       &
        trim(adjustl(str_n_error_component))//'/'//                            &
        trim(adjustl(str_n_assert_component))//' assertion failed.'
    else
      write(*,*) '  PASSED: '// trim(adjustl(string))
    endif
    write(*,*)
  endsubroutine procedure_assert_component_summary

!-------------------------------------------------------------------------------
!> Assert method: assert the input bool and write out the error message
!-------------------------------------------------------------------------------
  subroutine procedure_assert(bool,i_line,string)
    logknd,intent(in) :: bool    !< Logical to write error message
    intknd,intent(in) :: i_line  !< line number of error
    charSt,intent(in) :: string  !< error message

    !Every time it is called, it increases
    n_assert = n_assert + 1
    n_assert_component = n_assert_component + 1

    !If failed, it will not stop,
    if(.not. bool) then
      n_error = n_error + 1
      n_error_component = n_error_component + 1

      write(*,'(a,i0,a)') '  ASSERTION FAILED on line ',i_line,':'
      write(*,*)          '      - '//trim(adjustl(string))
    endif
  endsubroutine procedure_assert

!-------------------------------------------------------------------------------
!> Finish method, write out the summary message of the assertion
!-------------------------------------------------------------------------------
  subroutine procedure_assert_finish()

    char10 :: str_n_error  = ''
    char10 :: str_n_assert = ''

    write(str_n_error,'(i0)') n_error
    write(str_n_assert,'(i0)') n_assert
    write(*,*) '---------------------------------------------------------------'

    if(n_error /= 0) then
      write(*,'(a)') 'ASSERTION FINISHED: '//trim(adjustl(str_n_error))//'/'// &
      trim(adjustl(str_n_assert))//' assertions failed.'
      stop 886
    else
      write(*,'(a)') 'ASSERTION FINISHED: all '//trim(adjustl(str_n_assert))// &
        ' assertions succeed.'
    endif
  endsubroutine procedure_assert_finish

!-------------------------------------------------------------------------------
endmodule assert_tools
