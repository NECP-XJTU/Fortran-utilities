!
!   test linear solver module with parallel PETSc free-matrix solver
!
!  -------------------------------------------------------------------------
program test_petsc_sweep
  use utilities
  implicit none

# include <kind_parameter.h>

  type(linear_solver_petsc_sweep_type) :: test
  type(parallel_env_type),pointer   :: para_env

  intknd :: nrow,ncol

  nrow = 2
  ncol = 2

  allocate(para_env)
  call para_env%init()
  call para_env%cart_creat(1,1,1,1,1)
#ifdef WITH_PETSC

  assert_start()
  assert_component_begin('%init')

  call test%init(para_env,nrow,ncol)

  assert_component_summary('%init')

  call test%b%set_value(0,4.0_krp)
  call test%b%set_value(1,3.0_krp)

  test%indata%a = 2
  test%indata%b = -1
  test%indata%c = 1
  test%indata%is_init = .true.

write(*,*) '!!!!!!!!  solve strat  !!!!!!!!!!!!!!!!!!'
  call test%solve()
write(*,*) '!!!!!!!!  solve finish  !!!!!!!!!!!!!!!!!!'

  call test%b%printout(' right  b: ')
  call test%x%printout(' resolution x: ')
  call test%get_result()

  write(*,*) test%results

  !assert(abs(test%results(1) - 3.5_krp) < 1.0e-6_krp, '%solve() error')
  !assert(abs(test%results(2) - 3.0_krp) < 1.0e-6_krp, '%solve() error')
  assert_finish()
#endif

endprogram test_petsc_sweep





