program test_parallel_env
  use utilities
  implicit none

# include <kind_parameter.h>

  logknd :: bool
  type(parallel_env_type) :: para_env

  assert_start()
  call set_stop_on_error(.false.)

  !Test str_to_cap(...)
  call para_env%init()
  call para_env%cart_creat(1,1,1,1,1)
  call para_env%clear()
  assert_finish()

endprogram test_parallel_env
