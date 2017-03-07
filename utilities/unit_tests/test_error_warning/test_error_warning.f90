program test_error_warning

  use utilities
  implicit none

# include <kind_parameter.h>

  intknd :: nerror,nw

  assert_start()
! test raise_error
  assert_component_begin('raise_error')
  !test raise_error -- quiet
  call set_quiet(.true.)
  call set_stop_on_error(.false.)
  call raise_error('There should be no errors printed')
  nerror = get_nError()
  assert(nerror == 1, 'raise_error is wrong.')

  !test raise_error -- not quiet, no log
  call set_quiet(.false.)
  call raise_error('There should be some errors printed')
  nerror = get_nError()
  assert(nerror == 2, 'raise_error is wrong.')

  !test raise_error -- not quiet, have log file
  open(unit=1,file='errors.log')
  call set_log_unit(1)

  call raise_error('There should be some errors printed in log file.')
  nerror = get_nError()
  assert(nerror == 3, 'raise_error is wrong.')
  close(1)

  !test raise_error -- not quiet, stop on error
  call set_stop_on_error(.false.)
  call raise_error('There should be some errors printed in log file.')
  nerror = get_nError()
  assert(nerror == 4, 'raise_error is wrong.')

  assert_component_summary('raise_error')

!test get_nError
  assert_component_begin('get_nError')

  assert(get_nError() == 4, 'get_nError() is wrong.')

  assert_component_summary('get_nError')

!Test raise_warning
  assert_component_begin('raise_warning')

  !test raise_warning -- quiet
  call set_quiet(.true.)
  call raise_warning('There should be no warnings printed')

  !test raise_warning -- not quiet, no log
  call set_quiet(.false.)
  call raise_warning('There should be some warnings printed')

  !test raise_warning -- not quiet, have log file
  open(unit=1,file='warnings.log')
  call set_log_unit(1)
  call raise_warning('There should be some warnings printed in log file.')
  close(1)

  assert_component_summary('raise_warning')

!Test set_log_unit
  nw = get_nWarning()

  ! ERROR_UNIT
  call set_log_unit(ERROR_UNIT)
  assert(get_nWarning() == nw+ 2, 'Error when setting log unit = ERROR_UNIT.')

  ! INPUT_UNIT
  call set_log_unit(INPUT_UNIT)
  assert(get_nWarning() == nw + 4, 'Error when setting log unit = INPUT_UNIT.')

  ! OUTPUT_UNIT
  call set_log_unit(OUTPUT_UNIT)
  assert(get_nWarning() == nw + 6, 'Error when setting log unit = OUTPUT_UNIT.')

  nw = get_nWarning()
  ! Not open
  close(1)
  call set_log_unit(1)
  assert(get_nWarning() == nw + 5, 'Error when setting log unit = 1.')

  ! UNFORMATTED
  open(1,file='test.log',form='UNFORMATTED')
  call set_log_unit(1)
  assert(get_nWarning() == nw + 7, 'Error when setting log of UNFORMATTED')
  close(1)

  ! SEQUENTIAL
  open(1,file='test.log',access='STREAM')
  call set_log_unit(1)
  assert(get_nWarning() == nw + 10, 'Error when setting log of STREAM')
  close(1)

  ! SEQUENTIAL
  open(1,file='test.log',action='READ')
  call set_log_unit(1)
  assert(get_nWarning() == nw + 12, 'Error when setting log of READ')
  close(1)

!Stop on error has been tested. it will be shown here, because it will fail
!The test.
  assert_finish()

endprogram test_error_warning
