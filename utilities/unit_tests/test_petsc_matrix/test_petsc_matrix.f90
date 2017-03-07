!
!   test linear solver module
!
!  -------------------------------------------------------------------------

program test_linear_solver_petsc_matrix
  use utilities
  implicit none

# include <kind_parameter.h>

  type(linear_solver_petsc_matrix_type) :: test
  type(parallel_env_type),pointer   :: para_env
!-------------------------------------------------------------------------------
!!> case:  A={1,2,0;0,1,5;0,0,1} b={-1,9,2} => x={1,-1,2}
!-------------------------------------------------------------------------------
  intknd :: i,j,nrow,ncol
  realkd :: num

  allocate(para_env)
  call para_env%init()
  call para_env%cart_creat(1,1,1,1,1)
#ifdef WITH_PETSC

  assert_start()

  assert_component_begin('%init')

  nrow = 3
  ncol = 3
  call test%init(para_env,nrow,ncol)


  if(test%n_proc == 1) then

    assert_component_summary('%init')

    call test%a%set_value(0,0,1.0_krp)
    call test%a%set_value(0,1,2.0_krp)
    call test%a%set_value(1,1,1.0_krp)
    call test%a%set_value(1,2,5.0_krp)
    call test%a%set_value(2,2,1.0_krp)

    call test%b%set_value(0,-1.0_krp)
    call test%b%set_value(1,9.0_krp)
    call test%b%set_value(2,2.0_krp)

    call test%solve()

    call test%a%printout(' matrix A: ')
    call test%b%printout(' right  b: ')
    call test%x%printout(' resolution x: ')

    call test%get_result()

    assert(abs(test%results(1) - 1.0_krp) < 1.0e-6_krp, '%solve() error')
    assert(abs(test%results(2) + 1.0_krp) < 1.0e-6_krp, '%solve() error')
    assert(abs(test%results(3) - 2.0_krp) < 1.0e-6_krp, '%solve() error')

  elseif(test%n_proc == 3) then
    !> save for parallel calculation
    !> don't change index number
    if(test%rank == 0) then
      call test%a%set_value(0,0,1.0_krp)
      call test%a%set_value(0,1,2.0_krp)

      call test%b%set_value(0,-1.0_krp)
    elseif(test%rank == 1) then
      call test%a%set_value(1,1,1.0_krp)
      call test%a%set_value(1,2,5.0_krp)

      call test%b%set_value(1,9.0_krp)
    elseif(test%rank == 2) then
      call test%a%set_value(2,2,1.0_krp)

      call test%b%set_value(2,2.0_krp)
    endif

    call test%solve()

    call test%a%printout(' matrix A: ')
    call test%b%printout(' right  b: ')
    call test%x%printout(' resolution x: ')

    call test%get_result()

    write(*,*) test%rank,"   ", test%results

  endif

  assert_finish()

#endif

endprogram test_linear_solver_petsc_matrix

