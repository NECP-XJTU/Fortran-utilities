module hash_table

  implicit none
  private

# include <kind_parameter.h>

  integer, parameter :: HASH_SIZE = 4993
  integer, parameter :: HASH_NONE = -huge(0)

  public :: int_int_hash_table, HASH_NONE

  !-----------------------------------------------------------------------------
  !> a (key, value) pair
  type :: int_int_hash_pair
    type(int_int_hash_pair),pointer :: next => null()
    intknd :: key
    intknd :: value
  end type int_int_hash_pair

  !-----------------------------------------------------------------------------
  !> a pointer to (key, value) pair
  type :: int_int_hash_list
    type(int_int_hash_pair), pointer :: hlist => null()
  end type int_int_hash_list

  !-----------------------------------------------------------------------------
  !> a pointer to (key, value) pair
  type :: int_int_hash_table
    intknd :: n_pair = 0
    type(int_int_hash_list), pointer :: htable(:) => null()
  contains
    procedure :: get_pair => int_int_get_pair
    procedure :: get_value => int_int_get_value
    procedure :: get_keys => int_int_get_keys
    procedure :: has_key => int_int_has_key
    procedure :: add => int_int_add
    procedure :: clear => int_int_clear
    procedure :: copy => int_int_copy
  end type int_int_hash_table

contains

  !-------------------------------------------------------------------------------
  ! copy a hash table
  !-------------------------------------------------------------------------------
  subroutine int_int_copy(this, from)
    class(int_int_hash_table) :: this
    class(int_int_hash_table) :: from

    intknd :: i
    intknd, allocatable :: keys(:)

    call this % clear()
    call from % get_keys(keys)
    if (allocated(keys)) then
      do i = 1, size(keys)
        call this % add(keys(i), from % get_value(keys(i)))
      end do
    end if

  end subroutine int_int_copy

  !-------------------------------------------------------------------------------
  ! determine whether a hash table has a key
  !-------------------------------------------------------------------------------
  function int_int_has_key(hash_table, key) result(bool)
    class(int_int_hash_table) :: hash_table
    intknd, intent(in) :: key
    logknd :: bool

    if (associated(hash_table % get_pair(key))) then
      bool = .TRUE.
    else
      bool = .FALSE.
    end if

  end function int_int_has_key

  !-------------------------------------------------------------------------------
  ! get all the keys of a hash table
  !-------------------------------------------------------------------------------
  subroutine int_int_get_keys(this, keys)
    class(int_int_hash_table) :: this
    intknd, allocatable, intent(inout) :: keys(:)

    intknd :: i
    intknd :: j
    type(int_int_hash_pair), pointer :: pair

    pair => null()
    j = 0
    if (allocated(keys)) deallocate(keys)
    if (associated(this % htable)) then
      allocate(keys(this % n_pair))
      do i = 1, size(this % htable)
        pair => this % htable(i) % hlist
        do
          if (.NOT. associated(pair)) exit
          j = j + 1
          keys(j) = pair % key
          pair => pair % next
        end do
      end do
    end if

  end subroutine int_int_get_keys

  !-------------------------------------------------------------------------------
  ! clear a hash table
  !-------------------------------------------------------------------------------
  subroutine int_int_clear(this)
    class(int_int_hash_table) :: this

    intknd :: i
    type(int_int_hash_pair), pointer :: pair
    type(int_int_hash_pair), pointer :: next

    if (associated(this % htable)) then
      do i = 1, size(this % htable)
        pair => this % htable(i) % hlist
        do
          if (.NOT. associated(pair)) exit
          next => pair % next
          deallocate(pair)
          pair => next
        end do
        nullify(this % htable(i) % hlist)
      end do
      deallocate(this % htable)
    end if

  end subroutine int_int_clear

  !-------------------------------------------------------------------------------
  ! get a value in hash table with key
  !-------------------------------------------------------------------------------
  function int_int_get_value(this, key) result(value)
    class(int_int_hash_table) :: this
    intknd, intent(in) :: key
    intknd :: value

    type(int_int_hash_pair), pointer :: pair

    pair => this % get_pair(key)
    if (associated(pair)) then
      value = pair % value
    else
      value = HASH_NONE
    end if

  end function int_int_get_value

  !-------------------------------------------------------------------------------
  ! add a (key, value) pair to a hash table
  !-------------------------------------------------------------------------------
  subroutine int_int_add(this, key, value)
    class(int_int_hash_table) :: this
    intknd, intent(in) :: key
    intknd, intent(in) :: value

    intknd :: hash
    type(int_int_hash_pair), pointer :: pair
    type(int_int_hash_pair), pointer :: new_pair

    pair => null()
    new_pair => null()
    pair => this % get_pair(key)
    if (associated(pair)) then
      pair % value = value
    else
      if (.NOT. associated(this % htable)) then
        allocate(this % htable(HASH_SIZE))
      end if
      hash = hash_value_int(key)
      allocate(new_pair)
      new_pair % key = key
      new_pair % value = value
      new_pair % next => this % htable(hash) % hlist
      this % htable(hash) % hlist => new_pair
      this % n_pair = this % n_pair + 1
    end if

  end subroutine int_int_add

  !-------------------------------------------------------------------------------
  ! get a pair in hash table with key
  !-------------------------------------------------------------------------------
  function int_int_get_pair(this, key) result(pair)
    class(int_int_hash_table) :: this
    intknd, intent(in) :: key
    type(int_int_hash_pair), pointer :: pair

    intknd :: hash

    pair => null()
    if (associated(this % htable)) then
      hash = hash_value_int(key)
      pair => this % htable(hash) % hlist
      do
        if (.NOT. associated(pair)) exit
        if (pair % key == key) exit
        pair => pair % next
      end do
    end if

  end function int_int_get_pair

  !-------------------------------------------------------------------------------
  ! get hash value for a given key
  !-------------------------------------------------------------------------------
  function hash_value_int(key) result(hash)
    intknd, intent(in) :: key
    intknd :: hash

    hash = 0
    hash = 1 + mod(abs(key-1), int(HASH_SIZE, kip))

  end function hash_value_int

endmodule hash_table
