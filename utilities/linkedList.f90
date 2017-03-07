!###############################################################################
!> Module for specifying kind parameters for intrinsic data types
!>
!> @author Zhouyu Liu
!>   @date 03/18/2015
!>
!> @todo
!>   - Test: Added some comments.
!###############################################################################
module linkedList

  use math, only: operator(.abseq.)

  implicit none
  private

# include <kind_parameter.h>

  public :: char_list_type
  public :: int_list_type
  public :: real_list_type
  public :: real_list_element_type
  !-----------------------------------------------------------------------------
  !
  type :: char_list_element_type

    charwd :: value = ''
    type(char_list_element_type),pointer :: prev  => null()
    type(char_list_element_type),pointer :: next => null()

  endtype char_List_element_type

  !-----------------------------------------------------------------------------
  !
  type :: char_list_type

    integer(kip) :: nElement = 0
    integer(kip) :: last_index = huge(0)
    type(char_list_element_type),pointer :: last_element => NULL()
    type(char_list_element_type),pointer :: start => NULL()
    type(char_list_element_type),pointer :: end   => NULL()

  contains

    procedure,pass :: append   => char_list_append
    procedure,pass :: clear     => char_list_clear
    procedure,pass :: get_index => char_list_get_index
    procedure,pass :: get_value => char_list_get_value
    procedure,pass :: set_value => char_list_set_value
    procedure,pass :: insert => char_list_insert

  endtype char_list_type

  !-----------------------------------------------------------------------------
  !
  type :: int_list_element_type

    intknd :: value = 0
    type(int_list_element_type),pointer :: prev  => null()
    type(int_list_element_type),pointer :: next => null()

  endtype int_list_element_type

  !-----------------------------------------------------------------------------
  !
  type :: int_list_type

    intknd :: nElement = 0
    intknd :: last_index = huge(0)
    type(int_list_element_type),pointer :: last_element => NULL()
    type(int_list_element_type),pointer :: start => NULL()
    type(int_list_element_type),pointer :: end   => NULL()

  contains

    procedure,pass :: append     => int_list_append
    procedure,pass :: clear      => int_list_clear
    procedure,pass :: get_index  => int_list_get_index
    procedure,pass :: get_value  => int_list_get_value
    procedure,pass :: set_value  => int_list_set_value
    procedure,pass :: insert  => int_list_insert

  endtype int_list_type

  !-----------------------------------------------------------------------------
  !
  type :: real_list_element_type

    realkd :: value = -huge(0.0_krp)
    type(real_list_element_type),pointer :: prev  => null()
    type(real_list_element_type),pointer :: next => null()

  endtype real_list_element_type

  !-----------------------------------------------------------------------------
  !
  type :: real_list_type

    intknd :: nElement = 0
    intknd :: last_index = huge(0)
    type(real_list_element_type),pointer :: last_element => NULL()
    type(real_list_element_type),pointer :: start => NULL()
    type(real_list_element_type),pointer :: end   => NULL()

  contains

    procedure,pass :: append    => real_list_append
    procedure,pass :: append_s  => real_list_append_sort
    procedure,pass :: append_p  => real_list_append_position
    procedure,pass :: clear     => real_list_clear
    procedure,pass :: get_index => real_list_get_index
    procedure,pass :: get_value => real_list_get_value
    procedure,pass :: set_value => real_list_set_value
    procedure,pass :: insert => real_list_insert

  endtype real_list_type

!===============================================================================

contains
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
  subroutine char_list_append(this,value)

    class(char_list_type),intent(inout) :: this
    character(LEN=*),intent(in) :: value

    !Local parameters
    type(char_list_element_type),pointer :: elem

    !Create element and set dat
    elem => null()
    allocate(elem)
    elem%value = value

    if (.not. associated(this % start)) then
      this%start => elem
      this%end => elem
      this%last_element => elem
      this%last_index = 1
    else
      this%end%next => elem
      elem%prev => this%end
      this%end => this%end%next
    endif
    nullify(elem)

    this%nElement = this%nElement + 1
  endsubroutine char_list_append

!-------------------------------------------------------------------------------
!>
!-------------------------------------------------------------------------------
  subroutine char_list_clear(this)
    class(char_list_type),intent(inout) :: this

    type(char_list_element_type),pointer :: current
    type(char_list_element_type),pointer :: next

    current => null()
    next => null()
    if(this%nElement > 0) then
      current => this % start
      do while (associated(current))

        next => current % next

        ! Deallocate memory
        deallocate(current)

        ! The next element
        current => next
      end do

      nullify(this % start)
      nullify(this % end)
      nullify(this % last_element)
      this%nElement = 0
      this%last_index = huge(0)
    end if

  endsubroutine char_list_clear

!-------------------------------------------------------------------------------
!>
!-------------------------------------------------------------------------------
  function char_list_get_index(this, value) result(ith)
    class(char_list_type),intent(inout) :: this
    charSt,intent(in) :: value
    intknd :: ith

    type(char_list_element_type), pointer :: elem

    elem => null()
    ith = 0
    elem => this % start
    do while (associated(elem))
      ith = ith + 1
      if (trim(value) == trim(elem % value)) exit
      elem => elem % next
    end do

    if (.not. associated(elem)) ith = -1


  endfunction char_list_get_index

  !-------------------------------------------------------------------------------
  !>
  !-------------------------------------------------------------------------------
  function char_list_get_value(this, index) result(val)
    class(char_list_type), intent(inout) :: this
    intknd, intent(in) :: index
    character(150) :: val

    intknd :: i
    type(char_list_element_type), pointer :: elem

    elem => null()
    if (index > this % nElement) then
      val = ""
    else
      elem => this % start
      do i = 1, index - 1
        elem => elem % next
      end do
      val = elem % value
    end if

  end function char_list_get_value

  !-------------------------------------------------------------------------------
  !>
  !-------------------------------------------------------------------------------
  subroutine char_list_set_value(this, index, val)
    class(char_list_type), intent(inout) :: this
    intknd, intent(in) :: index
    charSt, intent(in) :: val

    intknd :: i
    type(char_list_element_type), pointer :: elem

    elem => null()
    if (index <= this % nElement) then
      elem => this % start
      do i = 1, index - 1
        elem => elem % next
      end do
      elem % value = val
    end if

  end subroutine char_list_set_value

  !-------------------------------------------------------------------------------
  !>
  !-------------------------------------------------------------------------------
  subroutine char_list_insert(this, index, val)
    class(char_list_type), intent(inout) :: this
    intknd, intent(in) :: index
    charSt, intent(in) :: val

    intknd :: i
    type(char_list_element_type), pointer :: elem
    type(char_list_element_type), pointer :: newelem

    elem => null()
    newelem => null()
    if (index <= this % nElement) then
      allocate(newelem)
      newelem % value = val
      newelem % next => null()
      elem => this % start
      do i = 1, index - 2
        elem => elem % next
      end do
      newelem % next => elem % next
      elem % next => newelem
      this % nElement = this % nElement + 1
    end if

  end subroutine char_list_insert

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
  subroutine int_list_append(this,value)

    class(int_list_type),intent(inout) :: this
    intknd,intent(in) :: value

    !Local parameters
    type(int_list_element_type),pointer :: elem

    !Create element and set dat
    elem => null()
    allocate(elem)
    elem%value = value

    if (.not. associated(this % start)) then
      this%start => elem
      this%end => elem
      this%last_element => elem
      this%last_index = 1
    else

      this%end%next => elem
      elem%prev => this%end
      this%end => this%end%next
    endif

    this%nElement = this%nElement + 1
  endsubroutine int_list_append

!-------------------------------------------------------------------------------
!>
!-------------------------------------------------------------------------------
  subroutine int_list_clear(this)
    class(int_list_type),intent(inout) :: this

    type(int_list_element_type),pointer :: current
    type(int_list_element_type),pointer :: next

    current => null()
    next => null()
    if(this%nElement > 0) then
      current => this % start
      do while (associated(current))

        next => current % next

        ! Deallocate memory
        deallocate(current)

        ! The next element
        current => next
      end do

      nullify(this % start)
      nullify(this % end)
      nullify(this % last_element)
      this%nElement = 0
      this%last_index = huge(0)
    end if

  endsubroutine int_list_clear

!-------------------------------------------------------------------------------
!>
!-------------------------------------------------------------------------------
  function int_list_get_index(this, value) result(ith)
    class(int_list_type),intent(inout) :: this
    intknd,intent(in) :: value
    intknd :: ith

    type(int_list_element_type), pointer :: elem

    elem => null()
    ith = 0
    elem => this % start
    do while (associated(elem))
      ith = ith + 1
      if (value == elem % value) exit
      elem => elem % next
    end do

    if (.not. associated(elem)) ith = -1


  endfunction int_list_get_index

  !-------------------------------------------------------------------------------
  !>
  !-------------------------------------------------------------------------------
  function int_list_get_value(this, index) result(val)
    class(int_list_type), intent(inout) :: this
    intknd, intent(in) :: index
    intknd :: val

    intknd :: i
    type(int_list_element_type), pointer :: elem

    elem => null()
    if (index > this % nElement) then
      val = -huge(0)
    else
      elem => this % start
      do i = 1, index - 1
        elem => elem % next
      end do
      val = elem % value
    end if

  end function int_list_get_value

  !-------------------------------------------------------------------------------
  !>
  !-------------------------------------------------------------------------------
  subroutine int_list_set_value(this, index, val)
    class(int_list_type), intent(inout) :: this
    intknd, intent(in) :: index
    intknd, intent(in) :: val

    intknd :: i
    type(int_list_element_type), pointer :: elem

    elem => null()
    if (index <= this % nElement) then
      elem => this % start
      do i = 1, index - 1
        elem => elem % next
      end do
      elem % value = val
    end if

  end subroutine int_list_set_value

  !-------------------------------------------------------------------------------
  !>
  !-------------------------------------------------------------------------------
  subroutine int_list_insert(this, index, val)
    class(int_list_type), intent(inout) :: this
    intknd, intent(in) :: index
    intknd, intent(in) :: val

    intknd :: i
    type(int_list_element_type), pointer :: elem
    type(int_list_element_type), pointer :: newelem

    elem => null()
    newelem => null()
    if (index <= this % nElement) then
      allocate(newelem)
      newelem % value = val
      newelem % next => null()
      elem => this % start
      do i = 1, index - 2
        elem => elem % next
      end do
      newelem % next => elem % next
      elem % next => newelem
      this % nElement = this % nElement + 1
    end if

  end subroutine int_list_insert

  !-------------------------------------------------------------------------------
  !
  !-------------------------------------------------------------------------------
  subroutine real_list_append(this,value)

    class(real_list_type),intent(inout) :: this
    realkd,intent(in) :: value

    !Local parameters
    type(real_list_element_type),pointer :: elem

    !Create element and set dat
    elem => null()
    allocate(elem)
    elem%value = value

    if (.not. associated(this % start)) then
      this%start => elem
      this%end => elem
      this%last_element => elem
      this%last_index = 1
    else
      this%end%next => elem
      elem%prev => this%end
      this%end => this%end%next
    endif

    this%nElement = this%nElement + 1
  endsubroutine real_list_append

  !-------------------------------------------------------------------------------
  ! It will not append the value which is equal to linkedlist element.
  !-------------------------------------------------------------------------------
  subroutine real_list_append_sort(this,value)

    class(real_list_type),intent(inout) :: this
    realkd,intent(in) :: value

    !Local parameters
    type(real_list_element_type),pointer :: elem
    type(real_list_element_type),pointer :: elem1
    type(real_list_element_type),pointer :: elem2

    !Create element and set dat
    elem => null()
    elem1 => null()
    elem2 => null()
    allocate(elem)
    elem%value = value

    if (.not. associated(this % start)) then
      this%start => elem
      this%end => elem
      this%start%next => this%end
      this%end%prev => this%start
      this%last_element => elem
      this%last_index = 1
    else
      if (elem%value < this%start%value)then
        !> it will change the list head
        this%start%prev => elem
        elem%next => this%start
        this%start => this%start%prev
        !elem%next => this%start
        !this%start%prev => elem
        !this%start => elem
      elseif (elem%value > this%end%value)then
        this%end%next => elem
        elem%prev => this%end
        this%end => this%end%next
      elseif (this%start%value < elem%value .and. elem%value < this%end%value)  &
                                                                            then
        elem1 => this%start
        elem2 => elem1%next
        search : do
          if (elem%value == elem1%value .or. elem%value == elem2%value)return
          if((elem%value > elem1%value) .and. (elem%value < elem2%value))then
            elem1%next => elem
            elem%prev => elem1
            elem%next => elem2
            elem2%prev => elem
            exit
          endif
          elem1 => elem2
          elem2 => elem2%next
        enddo search
      elseif (elem%value == this%start%value .or. elem%value == this%end%value)then
        return
      endif
    endif
    this%nElement = this%nElement + 1
  endsubroutine real_list_append_sort

!>------------------------------------------------------------------------------
!> Append the value to the position specified.
!>
!> @param value, in - value to be appended
!> @param position, in - appending position in the this linkedlist
!>
!> Note:
!>      1) It will append the value to the top/bottom when position < 1 or
!>         > this%nElement.
!>      2) Variable position means to append the value behind the position in
!>         this linkedlist.
!>------------------------------------------------------------------------------
  subroutine real_list_append_position(this,value,position)

    class(real_list_type),intent(inout) :: this
    realkd,intent(in) :: value
    intknd, intent(in) :: position
    intknd :: loop_position

    !Local parameters
    type(real_list_element_type),pointer :: elem
    type(real_list_element_type),pointer :: elem1
    type(real_list_element_type),pointer :: elem2

    !Create element and set dat
    elem => null()
    elem1 => null()
    elem2 => null()
    allocate(elem)
    elem%value = value

    if (.not. associated(this % start)) then
      this%start => elem
      this%end => elem
      this%start%next => this%end
      this%end%prev => this%start
      this%last_element => elem
      this%last_index = 1
    else
      if (position < 1)then
        this%start%prev => elem
        elem%next => this%start
        this%start => this%start%prev
      elseif (position > this%nElement)then
        this%end%next => elem
        elem%prev => this%end
        this%end => this%end%next
      else
        elem1 => this%start
        elem2 => elem1%next
        search : do loop_position = 1,position-1
          elem1 => elem2
          elem2 => elem2%next
        enddo search
        elem1%next => elem
        elem%prev => elem1
        elem%next => elem2
        elem2%prev => elem
      endif
    endif
    this%nElement = this%nElement + 1
  endsubroutine real_list_append_position

  !-------------------------------------------------------------------------------
  !>
  !-------------------------------------------------------------------------------
  subroutine real_list_clear(this)
    class(real_list_type),intent(inout) :: this

    type(real_list_element_type),pointer :: current
    type(real_list_element_type),pointer :: next
    realkd :: temp

    current => null()
    next => null()
    if(this%nElement > 0) then
      current => this % start
      do while (associated(current))

        next => current % next

        ! Deallocate memory
        deallocate(current)

        ! The next element
        current => next
      end do

      temp = this%get_value(-1)
      nullify(this % start)
      nullify(this % end)
      !nullify(this % last_element)
      this%last_element => null()
      this%nElement = 0
      this%last_index = huge(0)
    end if
  endsubroutine real_list_clear

  !-------------------------------------------------------------------------------
  !>
  !-------------------------------------------------------------------------------
  function real_list_get_index(this, value) result(ith)
    class(real_list_type),intent(inout) :: this
    realkd,intent(in) :: value
    intknd :: ith

    type(real_list_element_type), pointer :: elem

    elem => null()
    ith = 0
    elem => this % start
    do while (associated(elem))
      ith = ith + 1
      if (value .abseq. elem % value) exit
      elem => elem % next
    end do

    if (.not. associated(elem)) ith = -1
    !> nullify the save elem in get_value subroutine

  endfunction real_list_get_index

  !-------------------------------------------------------------------------------
  !>
  !-------------------------------------------------------------------------------
  function real_list_get_value(this, index) result(val)
    class(real_list_type), intent(inout) :: this
    intknd, intent(in) :: index
    realkd :: val

    !intknd :: i
    !type(real_list_element_type), pointer :: elem

    !elem => null()
    !if (index > this % nElement) then
    !  val = -huge(rzero)
    !else
    !  elem => this % start
    !  do i = 1, index - 1
    !    elem => elem % next
    !  end do
    !  val = elem % value
    !end if

     intknd :: i
     intknd, save :: last_i = 1
     intknd, save :: last_nElement = 0
     type(real_list_element_type), pointer, save :: last_end => null()
     type(real_list_element_type), pointer, save :: elem => null()
     type(real_list_element_type), pointer, save :: last_element => null()
     if (index == -1) then
       last_element => null()
       last_end => null()
       last_nElement = 0
       last_i = 1
       val = -huge(0.0_krp)
       return
     endif
     if (.not. associated(last_element,this%last_element))then
       !> new linked list
       elem => null()
       last_end => this%end
       last_element => this%last_element
       last_nElement = this%nElement
       last_i = 1
     else
       !> old linked list but with more elements
       !> refresh the elem
       if (last_nElement /= this%nElement .and. associated(last_end,this%end)) &
         then
         last_end => this%end
         elem => this%start
         last_i = 1
         last_nElement = this%nElement
       endif
     endif
     if (index > this % nElement) then
       val = -huge(0.0_krp)
     else
       if(.not. associated(elem)) elem => this%start
       if (index > last_i)then
         do i = last_i,index-1
           elem => elem % next
         enddo
         val = elem % value
         last_i = index
       elseif (index < last_i)then
         do i = last_i,index+1,-1
           elem => elem % prev
         enddo
         val = elem % value
         last_i = index
       else
         val = elem % value
         last_i = index
       endif
     end if


  end function real_list_get_value

  !-------------------------------------------------------------------------------
  !>
  !-------------------------------------------------------------------------------
  subroutine real_list_set_value(this, index, val)
    class(real_list_type), intent(inout) :: this
    intknd, intent(in) :: index
    realkd, intent(in) :: val

    intknd :: i
    type(real_list_element_type), pointer :: elem

    elem => null()
    if (index <= this % nElement) then
      elem => this % start
      do i = 1, index - 1
        elem => elem % next
      end do
      elem % value = val
    end if

  end subroutine real_list_set_value

  !-------------------------------------------------------------------------------
  !>
  !-------------------------------------------------------------------------------
  subroutine real_list_insert(this, index, val)
    class(real_list_type), intent(inout) :: this
    intknd, intent(in) :: index
    realkd, intent(in) :: val

    intknd :: i
    type(real_list_element_type), pointer :: elem
    type(real_list_element_type), pointer :: newelem

    elem => null()
    newelem => null()
    if (index <= this % nElement) then
      allocate(newelem)
      newelem % value = val
      newelem % next => null()
      elem => this % start
      do i = 1, index - 2
        elem => elem % next
      end do
      newelem % next => elem % next
      elem % next => newelem
      this % nElement = this % nElement + 1
    end if

  end subroutine real_list_insert

!-------------------------------------------------------------------------------
 endmodule linkedList
