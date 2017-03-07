program test_linkedList
  use utilities
  implicit none

# include <kind_parameter.h>

  type(int_list_type) :: int_list

  assert_start()
  call set_stop_on_error(.false.)

!Test append(...)

  assert_component_begin('append(...)')

  call int_list%append(1)
  call int_list%append(1)
  call int_list%append(2)
  call int_list%append(3)

  assert(int_list%nElement == 4, 'Wrong of nElement')
  assert(int_list%last_index == 1, 'Wrong of last_index')
  assert(int_list%last_element%value == 1, 'Wrong of last_element')
  assert(int_list%start%value == 1, 'Wrong of start')
  assert(int_list%end%value == 3, 'Wrong of end')
  assert(int_list%start%next%value == 1, 'Wrong of start%next')
  assert(int_list%start%next%next%value == 2, 'Wrong of start%next%next')
  assert(int_list%start%next%next%next%value == 3, 'Wrong of start%next%next%next')
  assert_component_summary('append(...)')

  assert_component_begin('get_index(...)')
  assert(int_list%get_index(1) == 1, 'Wrong of get_index')
  assert(int_list%get_index(2) == 3, 'Wrong of get_index')
  assert(int_list%get_index(3) == 4, 'Wrong of get_index')
  assert(int_list%get_index(4) == -1, 'Wrong of get_index')
  assert_component_summary('get_index(...)')

  assert_component_begin('get_value(...)')
  assert(int_list % get_value(4) == 3, "Wrong of get_value")
  assert(int_list % get_value(3) == 2, "Wrong of get_value")
  assert(int_list % get_value(2) == 1, "Wrong of get_value")
  assert(int_list % get_value(1) == 1, "Wrong of get_value")
  assert_component_summary('get_value(...)')

  assert_component_begin('set_value(...)')
  call int_list % set_value(4, 4)
  call int_list % set_value(3, 3)
  call int_list % set_value(2, 2)
  call int_list % set_value(1, 1)
  assert(int_list % get_value(4) == 4, "Wrong of set_value")
  assert(int_list % get_value(3) == 3, "Wrong of set_value")
  assert(int_list % get_value(2) == 2, "Wrong of set_value")
  assert(int_list % get_value(1) == 1, "Wrong of set_value")
  assert_component_summary('set_value(...)')

  assert_component_begin('insert(...)')
  call int_list % insert(2, 10)
  assert((int_list % get_value(1) == 1), 'Error of insert')
  assert((int_list % get_value(2) == 10), 'Error of insert')
  assert((int_list % get_value(3) == 2), 'Error of insert')
  assert((int_list % get_value(4) == 3), 'Error of insert')
  assert((int_list % get_value(5) == 4), 'Error of insert')
  assert_component_summary('insert(...)')

  assert_component_begin('clear(...)')
  call int_list%clear()
  assert(int_list%last_index == huge(0), 'Wrong of clear')
  assert(.not. associated(int_list%last_element), 'Wrong of clear')
  assert(.not. associated(int_list%start), 'Wrong of clear')
  assert(.not. associated(int_list%end), 'Wrong of clear')
  assert_component_summary('clear(...)')

  assert_finish()

endprogram test_linkedList
