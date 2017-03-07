program test_hash_table
  use utilities
  implicit none

# include <kind_parameter.h>

  intknd :: keys(4) = [4, 100, 65, -8]
  intknd :: values(4) = [99, 98, 97, 0]
  intknd, allocatable :: hash_keys(:)
  intknd :: i
  type(int_int_hash_table) :: int_htable
  type(int_int_hash_table) :: int_htable_

  call set_stop_on_error(.false.)
  call set_quiet(.false.)

  ! add pairs
  do i = 1, size(keys)
    call int_htable % add(keys(i), values(i))
  end do

  ! test get value
  assert_component_begin("%int_int_add(...)")
  assert((int_htable % get_value(keys(1)) == values(1)), "Error of get value 1")
  assert((int_htable % get_value(keys(2)) == values(2)), "Error of get value 2")
  assert((int_htable % get_value(keys(3)) == values(3)), "Error of get value 3")
  assert((int_htable % get_value(keys(4)) == values(4)), "Error of get value 4")
  assert_component_summary("%int_int_add(...)")

  ! test has key
  assert_component_begin("%int_int_has_key(...)")
  assert((int_htable % has_key(keys(1))), "Error of has key 1")
  assert((int_htable % has_key(keys(2))), "Error of has key 2")
  assert((int_htable % has_key(keys(3))), "Error of has key 3")
  assert((int_htable % has_key(keys(4))), "Error of has key 4")
  assert_component_summary("%int_int_has_key(...)")

  ! test get keys
  assert_component_begin("%int_int_get_keys(...)")
  call int_htable % get_keys(hash_keys)
  assert((size(hash_keys) == 4), "Error of get keys")
  assert_component_summary("%int_int_get_keys(...)")

  ! test copy hash table
  assert_component_begin("%int_int_copy(...)")
  call int_htable_ % copy(int_htable)
  assert((int_htable_ % get_value(keys(1)) == values(1)), "Error of copy 1")
  assert((int_htable_ % get_value(keys(2)) == values(2)), "Error of copy 2")
  assert((int_htable_ % get_value(keys(3)) == values(3)), "Error of copy 3")
  assert((int_htable_ % get_value(keys(4)) == values(4)), "Error of copy 4")
  assert_component_summary("%int_int_copy(...)")

  ! test clear
  assert_component_begin("%int_int_clear(...)")
  call int_htable_ % clear()
  call int_htable % clear()
  call int_htable % get_keys(hash_keys)
  assert((.NOT. allocated(hash_keys)), "Error of clear")
  assert_component_summary("%int_int_clear(...)")

  assert_finish()

end program test_hash_table
