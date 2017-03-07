  program test_timer
  use utilities
  implicit none

# include <kind_parameter.h>

  logknd :: bool
  intknd :: ith,ith2
  type(timer_type) :: ttimer
  type(timer_manager_type) :: ttimer_m
  charln,allocatable :: discriptions(:)


  assert_start()
  call set_stop_on_error(.false.)
  call set_quiet(.false.)
  call test_timer_type()
  call test_timer_manager()

contains

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
  subroutine test_timer_type()


    call ttimer%start()
    call cost_some_time(1)
    call ttimer%stop()
    write(*,*) ttimer%get_runtime()

    call ttimer%reset()
    call cost_some_time(1)
    call ttimer%stop()
    write(*,*) ttimer%get_runtime()

    call ttimer%start()
    call cost_some_time(1)
    call ttimer%stop()
    write(*,*) ttimer%get_runtime()

    write(*,*) ttimer%stamp()

  endsubroutine test_timer_type

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
  subroutine test_timer_manager()


    write(*,*) 'test timer_manager'
    call ttimer_m%add('moc')
    call ttimer_m%add('communication')
    call ttimer_m%add('cmfd')
    do ith = 1, 10
    call ttimer_m%start('moc2d')
    call ttimer_m%start('moc')
    call cost_some_time(10)
    do ith2 = 1, 5
      call ttimer_m%start('cmfd')
      call cost_some_time(5)
      call ttimer_m%stop('cmfd')
    enddo
    call ttimer_m%stop('moc2d')
    call ttimer_m%stop('moc')
    enddo

    allocate(discriptions(ttimer_m%nt))
    write(*,*) trim(ttimer_m%name('moc2d'))
    write(*,*) ttimer_m%ncalls('moc2d')
    write(*,*) ttimer_m%get_runtime('moc2d')
    write(*,*) trim(ttimer_m%discription('moc2d'))
!    write(*,*) trim(discriptions(1))

    write(*,*) ttimer_m%name('moc')
    write(*,*) "ttimer_m%ncalls('moc2d')",ttimer_m%ncalls('moc')
    write(*,*) ttimer_m%get_runtime('moc')
    write(*,*) trim(ttimer_m%discription('moc'))
!    write(*,*) trim(discriptions(1))

    write(*,*) trim(ttimer_m%name('cmfd'))
    write(*,*) "ttimer_m%ncalls('cmfd')",ttimer_m%ncalls('cmfd')
    write(*,*) ttimer_m%get_runtime('cmfd')
    write(*,*) trim(ttimer_m%discription('cmfd'))

    write(*,*) ttimer_m%stamp()

  endsubroutine test_timer_manager

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
  subroutine cost_some_time(n)
    intknd,intent(in) :: n
    intknd :: i
    realkd :: t

    t = 2.99_krp
    do i = 1, 10000*n
      t = exp(t * 6.1_krp)
    enddo

  endsubroutine cost_some_time



endprogram test_timer