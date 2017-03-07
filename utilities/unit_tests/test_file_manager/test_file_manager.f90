program test_file_manager

  use utilities
  implicit none

# include <kind_parameter.h>

  type(file_manager_type) :: test
  character(len=512) :: msg
  character(len=512) :: str

  call set_stop_on_error(.false.)
  call set_quiet(.false.)

  assert_start()
! test add
  assert_component_begin('%add')

  call test_add()
  assert_component_summary('%add')
  assert_finish()


contains
  subroutine test_add()
    intknd :: unit,i
    logknd :: bool
    !bad input
    call test%add('text','test.inp',readwrite='read')
    call test%add('text','test.out',readwrite='write')
    call test%add('text','test.out',readwrite='write')

    !correct input
    call test%add('text','test.out',readwrite='write')
    call test%add('text','test.vld',readwrite='write')
    call test%add('text','test.log',readwrite='write')
    bool = associated(test%get_base_file('.out'))
    assert(bool, '%get_file_with_ext failed')
    bool = associated(test%get_base_file('.vld'))
    assert(bool, '%get_file_with_ext failed')
    bool = associated(test%get_base_file('.log'))
    assert(bool, '%get_file_with_ext failed')

    unit = test%get_unit('.out')
    bool = associated(test%get_base_file(unit))
    assert(bool, '%get_file_with_unit failed')

    unit = test%get_unit('.vld')
    bool = associated(test%get_base_file(unit))
    assert(bool, '%get_file_with_unit failed')

    unit = test%get_unit('.log')
    bool = associated(test%get_base_file(unit))
    assert(bool, '%get_file_with_unit failed')

    unit = test%get_unit('test')
    bool = associated(test%get_base_file(unit))
    assert(bool, '%get_file_with_unit failed')

    unit = test%get_unit('test.log')
    bool = associated(test%get_base_file(unit))
    assert(bool, '%get_file_with_unit failed')

    unit = test%get_unit('texxt')
    write(*,*) "unit = test%get_unit('texxt')",unit
    bool = associated(test%get_base_file(unit))
    assert(.not.bool, '%get_file_with_unit failed')

    !%clear
    call test%clear()

    stop

  endsubroutine test_add
endprogram test_file_manager
