module timer
  use error_warning
  use string
  use linkedlist
  implicit none
  private

#include <kind_parameter.h>
  public :: timer_type
  public :: timer_manager_type

  !-----------------------------------------------------------------------------
  ! TIMER represents a timer that can be started and stopped to measure how long
  ! different routines run. The intrinsic routine system_clock is used to measure
  ! time rather than cpu_time.
  type Timer_type
      charwd :: name = ''
      charln :: discription = ''
      logical,private :: actived      = .false. ! is timer running
      integer,private :: start_counts = 0       ! counts when started
      intknd,private  :: n_calls = 0
      real(8),private :: runtime = rzero        ! total time runtime in seconds
  contains
      procedure :: start     => timer_start
      procedure :: stop      => timer_stop
      procedure :: reset     => timer_seset
      procedure :: ncalls    => timer_get_ncalls
      procedure :: get_runtime  => timer_get_runtime
      procedure,nopass :: stamp => timer_stamp
  endtype Timer_type

  !>----------------------------------------------------------------------------
  !> A timer manager to allocate more timers. It also supplies functions to
  !> operate the timer, which make the timer more friendly.
  type timer_manager_type

    intknd :: nt = 0                         !< number of timers
    type(char_list_type) :: name_list        !<Name list to quickly search timer
    type(timer_type),allocatable :: timers(:)
  contains
    procedure :: add     => timer_manager_add
    procedure :: start   => timer_manager_start
    procedure :: stop    => timer_manager_stop
    procedure :: reset   => timer_manager_reset
    procedure :: name    => timer_manager_name
    procedure :: ncalls  => timer_manager_ncalls
    procedure :: get_runtime  => timer_manager_get_runtime
    procedure :: discription  => timer_manager_discription
    procedure,nopass :: stamp => timer_stamp
  endtype

!===============================================================================

contains

!==============================================================================

!>------------------------------------------------------------------------------
!> it starts running a timer and measures the current time
!>------------------------------------------------------------------------------
  subroutine timer_start(this)

    class(Timer_type), intent(inout) :: this

    ! Turn timer on and measure starting time
    this % actived = .true.
    call system_clock(this % start_counts)
    this % n_calls = this % n_calls + 1

  end subroutine timer_start

!>------------------------------------------------------------------------------
!> ncalls returns the number of calls of the timer
!>
!>------------------------------------------------------------------------------
  function timer_get_ncalls(this) result(ncalls)

    class(Timer_type), intent(in) :: this   ! the timer
    intknd                 :: ncalls ! total runtime time

    ncalls = this%n_calls

  end function timer_get_ncalls

!>------------------------------------------------------------------------------
!> get_runtime returns the current value of the timer
!>
!> note:
!>       if timer%actived is true, it will return runtime from start untill
!>     the subroutine is called.
!>       if timer%actived is false, it will return runtime untill last stop
!>------------------------------------------------------------------------------
  function timer_get_runtime(this) result(runtime)

    class(Timer_type), intent(in) :: this   ! the timer
    real(8)                 :: runtime ! total runtime time

    integer :: end_counts   ! current number of counts
    integer :: count_rate   ! system-dependent counting rate
    real(8) :: elapsed_time ! runtime time since last start

    if (this % actived) then
      call system_clock(end_counts, count_rate)
      elapsed_time = real(end_counts - this % start_counts,krp)/real(count_rate,krp)
      runtime = this % runtime + elapsed_time
    else
      runtime = this % runtime
    end if

  end function timer_get_runtime

!>------------------------------------------------------------------------------
!>  TIMER_STOP stops the timer and sets the runtime time, set the timer inactive
!>------------------------------------------------------------------------------
  subroutine timer_stop(this)

    class(Timer_type), intent(inout) :: this

    ! Check to make sure timer was running
    if (.not. this % actived) return

    ! Stop timer and add time
    this % runtime = timer_get_runtime(this)
    this % actived = .false.

  end subroutine timer_stop

!>------------------------------------------------------------------------------
  ! TIMER_RESET resets a timer to have a zero value, and number of call to be 0.
!>------------------------------------------------------------------------------
  subroutine timer_seset(this)

    class(Timer_type), intent(inout) :: this

    this % actived      = .false.
    this % start_counts = 0
    this % runtime      = rzero
    this % n_calls = 0

  end subroutine timer_seset

!>------------------------------------------------------------------------------
!> timestamp print current date and time.
!>------------------------------------------------------------------------------
  function timer_stamp() result(str_date_time)

    character ( len = 8 ) :: ampm
    integer(kind=4) :: d
    integer(kind=4) :: h
    integer(kind=4) :: m
    integer(kind=4) :: mm
    character ( len = 9 ), parameter, dimension(12) :: month = (/ &
      'January  ', 'February ', 'March    ', 'April    ', &
      'May      ', 'June     ', 'July     ', 'August   ', &
      'September', 'October  ', 'November ', 'December ' /)
    integer(kind=4) :: n
    integer(kind=4) :: s
    integer(kind=4) :: values(8)
    integer(kind=4) :: y
    character(len=40) str_date_time

    call date_and_time ( values = values )

    y = values(1)
    m = values(2)
    d = values(3)
    h = values(5)
    n = values(6)
    s = values(7)
    mm = values(8)

    if ( h < 12 ) then
      ampm = 'AM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Noon'
      else
        ampm = 'PM'
      end if
    else
      h = h - 12
      if ( h < 12 ) then
        ampm = 'PM'
      else if ( h == 12 ) then
        if ( n == 0 .and. s == 0 ) then
          ampm = 'Midnight'
        else
          ampm = 'AM'
        end if
      end if
    end if
    write(str_date_time,'(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)')&
      d,trim ( month(m) ),y, h, ':', n, ':', s, '.', mm, trim ( ampm )
  endfunction timer_stamp

!>------------------------------------------------------------------------------
!> Method to add a timer to the timer manager. the discription is optional
!> duplication will be checked to ignore the defined timer.
!> @param name - in, the name of timer (length < 20)
!> @discription - in, optional, the detailed discription of timer.
!<
!> Note: the timer name is case sensitive.
!>------------------------------------------------------------------------------
subroutine timer_manager_add(this,name,discription)
  class(timer_manager_type),intent(inout) :: this
  charSt,intent(in) :: name
  character(len=*),optional,intent(in) :: discription

  intknd :: ith
  type(timer_type),allocatable :: t_timers(:)

  ith = this%name_list%get_index(name)
  if(ith < 0) then
    this%nt = this%nt + 1
    call this%name_list%append(name)
    if(this%nt > 1) then
      allocate(t_timers(this%nt))
      t_timers(1:this%nt-1) = this%timers
      t_timers(this%nt)%name = trim(adjustl(name))
      if(present(discription)) &
        t_timers(this%nt)%discription =  trim(adjustl(discription))
      call move_alloc(t_timers,this%timers)
    else
      allocate(this%timers(this%nt))
      this%timers(this%nt)%name = name
      if(present(discription)) &
        this%timers(this%nt)%discription = discription

    endif
  else
    call raise_warning('timer_manager%add - timer "'//trim(name)// &
      '" has been added!')
  endif

endsubroutine timer_manager_add

!>------------------------------------------------------------------------------
!> Method to start a timer or all timers. if timer name is given. only the
!> specified timer is starting.

!> @param name - in, the name of timer (length < 20)
!<
!> Note: the timer name is case sensitive.
!>------------------------------------------------------------------------------
subroutine timer_manager_start(this,name)
  class(timer_manager_type),intent(inout) :: this
  character(len=*),optional,intent(in) :: name
  intknd :: ith
  if(present(name)) then
    ith = this%name_list%get_index(name)
    if(ith > 0) then
      call this%timers(ith)%start()
    else
      call raise_warning('timer_manager%start - timer "'//trim(name)// &
        '" not found')
    endif
  else
    do ith = 1, this%nt
      call this%timers(ith)%start()
    enddo
  endif
endsubroutine timer_manager_start

!>------------------------------------------------------------------------------
!> Method to stop a timer or all timers. if timer name is given. only the
!> specified timer is stop.

!> @param name - in, the name of timer (length < 20)
!<
!> Note: the timer name is case sensitive.
!>------------------------------------------------------------------------------
subroutine timer_manager_stop(this,name)
  class(timer_manager_type),intent(inout) :: this
  character(len=*),optional,intent(in) :: name
  intknd :: ith
  if(present(name)) then
    ith = this%name_list%get_index(name)
    if(ith > 0) then
      call this%timers(ith)%stop()
    else
      call raise_warning('timer_manager%stop - timer "'//trim(name)// &
        '" not found')
    endif
  else
    do ith = 1, this%nt
      call this%timers(ith)%stop()
    enddo
  endif
endsubroutine timer_manager_stop

!>------------------------------------------------------------------------------
!> Method to reset a timer or all timers. if timer name is given. only the
!> specified timer is reset.

!> @param name - in, the name of timer (length < 20)
!<
!> Note: the timer name is case sensitive.
!>------------------------------------------------------------------------------
subroutine timer_manager_reset(this,name)
  class(timer_manager_type),intent(inout) :: this
  character(len=*),optional,intent(in) :: name
  intknd :: ith
  if(present(name)) then
    ith = this%name_list%get_index(name)
    if(ith > 0) then
      call this%timers(ith)%reset()
    else
     call raise_warning('timer_manager%reset - timer "'//trim(name)// &
      '" not found')
    endif
  else
    do ith = 1, this%nt
      call this%timers(ith)%reset()
    enddo
  endif
endsubroutine timer_manager_reset

!>------------------------------------------------------------------------------
!> Method to get run time of one timer or all timers. if timer name is given.
!> only the specified timer's run time is return.

!> @param name - in, the name of timer (length < 20)
!<
!> Note: the timer name is case sensitive.
!>------------------------------------------------------------------------------
function timer_manager_get_runtime(this,name) result(runtime)
  class(timer_manager_type),intent(inout) :: this
  character(len=*),optional,intent(in) :: name

  realkd :: runtime
  intknd :: ith

  runtime = 0.0_krp
  ith = this%name_list%get_index(name)
  if(ith > 0) then
    runtime = this%timers(ith)%get_runtime()
  else
   call raise_warning('timer_manager%get_runtime - timer "'//trim(name)// &
    '" not found')
  endif
endfunction timer_manager_get_runtime

!>------------------------------------------------------------------------------
!> Method to get discription of one timer. The timer name must be given.

!> @param name - in, the name of timer (length < 20)
!<
!> Note: the timer name is case sensitive.
!>------------------------------------------------------------------------------
function timer_manager_discription(this,name) result(discriptions)
  class(timer_manager_type),intent(inout) :: this
  character(len=*),intent(in) :: name

  charln :: discriptions
  intknd :: ith

  discriptions = ''
  ith = this%name_list%get_index(name)
  if(ith > 0) then
    discriptions = this%timers(ith)%discription
  else
   call raise_warning('timer_manager%discription - timer "'//trim(name)// &
    '" not found')
  endif
endfunction timer_manager_discription

!>------------------------------------------------------------------------------
!> Method to get name of one timer. The timer name must be given.

!> @param name - in, the name of timer (length < 20)
!<
!> Note: the timer name is case sensitive.
!>------------------------------------------------------------------------------
function timer_manager_name(this,name) result(names)
  class(timer_manager_type),intent(inout) :: this
  character(len=*),intent(in) :: name
  charwd :: names

  intknd :: ith
  names = ''
  ith = this%name_list%get_index(name)
  if(ith > 0) then
    names = this%timers(ith)%name
  else
   call raise_warning('timer_manager%name - timer "'//trim(name)// &
    '" not found!')
  endif
endfunction timer_manager_name

!>------------------------------------------------------------------------------
!> Method to get number of calling start method of specified timer.
!> A timer name must be given.

!> @param name - in, the name of timer (length < 20)
!<
!> Note: the timer name is case sensitive.
!>------------------------------------------------------------------------------
function timer_manager_ncalls(this,name) result(ncalls)
  class(timer_manager_type),intent(inout) :: this
  character(len=*),intent(in) :: name
  intknd :: ncalls

  intknd :: ith
  ncalls = 0
  ith = this%name_list%get_index(name)
  if(ith > 0) then
    ncalls = this%timers(ith)%n_calls
  else
   call raise_warning('timer_manager%ncalls - timer "'//trim(name)// &
    '" not found!')
  endif
endfunction timer_manager_ncalls

!>------------------------------------------------------------------------------
!>------------------------------------------------------------------------------

!===============================================================================
end module timer
