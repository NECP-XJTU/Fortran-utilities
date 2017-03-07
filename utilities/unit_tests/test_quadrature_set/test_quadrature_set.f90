program test_quadrature_set
  use utilities
  implicit none

# include <kind_parameter.h>

  type(quadrature_set_type) :: test

  realkd,allocatable :: angles(:)
  realkd,allocatable :: weights(:)
  logknd :: is_true

  call set_stop_on_error(.false.)
  call set_quiet(.false.)

  call test%clear()

!Start test
  assert_start()

! Test %init(...)
  assert_component_begin('%init(...)')
  call test_init_bad_input()
  call test_init_44()
  call test_init_22()
  call test_init_11()
  assert_component_summary('%init(...)')

!Test %updates_azimuthal_weights()
  assert_component_begin('%updates_azimuthal_weights(...)')
  call test_updates_weight_expand_to_half()
  assert_component_summary('%updates_azimuthal_weights(...)')

!Test %clear()
  assert_component_begin('%clear()')
  call test_clear()
  assert_component_summary('%clear()')

!Finish tests
  assert_finish()

!===============================================================================

contains

!>------------------------------------------------------------------------------
!>
  subroutine test_init_bad_input()

    call test%init(2,-1,-1)
    assert(test%n_alpha == 2, '%init(...) is wrong')
    assert(test%n_theta == 1, '%init(...) is wrong')
    call test%clear()

    call test%init(2,3,-1)
    assert(test%n_alpha == 4, '%init(...) is wrong')
    assert(test%n_theta == 1, '%init(...) is wrong')

    call test%clear()

  endsubroutine test_init_bad_input

!>------------------------------------------------------------------------------
!>
  subroutine test_init_44()
    !(4,4)
    call test%init(3,4,4)
    assert(test%n_theta == 4, '%n_theta is wrong')
    assert(test%n_alpha == 4, '%n_alpha is wrong')

    allocate(angles(4))
    allocate(weights(4))
    angles(1) =0.282757063593796_krp
    angles(2) =0.649036580460780_krp
    angles(3) =1.017455539490153_krp
    angles(4) =1.386317078892131_krp
    weights(1)=0.101228536290377_krp
    weights(2)=0.222381034453374_krp
    weights(3)=0.313706645877887_krp
    weights(4)=0.362683783378362_krp
    assert(all(test%theta == angles), '%theta is wrong')
    assert(all(test%sin_theta .abseq. sin(angles)), '%sin_theta is wrong')
    assert(all(test%weight_theta == weights), '%weight_theta is wrong')

    angles(1) = halfpi/8._krp
    angles(2) = angles(1)*3
    angles(3) = angles(1)*5
    angles(4) = angles(1)*7
    assert(all(test%alpha == angles), '%n_alpha is wrong')
    assert(all(test%weight_alpha == 0.25_krp*0.25_krp),'%weight_alpha is wrong')

    call test%clear()

  endsubroutine test_init_44

!>------------------------------------------------------------------------------
!>
  subroutine test_init_22()

    !(2,2)
    deallocate(angles); allocate(angles(2))
    deallocate(weights); allocate(weights(2))
    angles(1)  = 0.277194163666_krp
    angles(2)  = 1.046575079148_krp
    weights(1) = 0.139473_krp
    weights(2) = 0.860527_krp

    call test%init(2,2,2)
    assert(all(test%theta == angles), '%theta is wrong')
    assert(all(test%sin_theta .abseq. sin(angles)), '%sin_theta is wrong')
    assert(all(test%weight_theta == weights), '%weight_theta is wrong')

    call test%clear()
  endsubroutine test_init_22

!>------------------------------------------------------------------------------
!>
  subroutine test_init_11()

    !(1,1)
    deallocate(angles); allocate(angles(1))
    deallocate(weights); allocate(weights(1))
    angles(1)  = 0.851461245321_krp
    weights(1) = 1.0_krp

    call test%init(2,1,1)
    write(*,*) 'test%theta',test%theta
    assert(all(test%theta == angles), '%theta is wrong')
    assert(all(test%sin_theta .abseq. sin(angles)), '%sin_theta is wrong')
    assert(all(test%weight_theta == weights), '%weight_theta is wrong')

    call test%clear()
  endsubroutine test_init_11

!>------------------------------------------------------------------------------
!>
  subroutine test_update_weights()

    call test%init(2,8,8)
    call test%updates_weight_expand_to_half()
    is_true = all(test%weight_alpha .abseq. 0.125_krp*0.25_krp)
    assert(is_true,'updates_azimuthal_weights wrong')
    call test%clear

  endsubroutine test_update_weights

!>------------------------------------------------------------------------------
!>
  subroutine test_updates_weight_expand_to_half()

    call test%init(2,8,8)
    call test%updates_weight_expand_to_half()
    is_true = all(test%weight_alpha .abseq. 0.125_krp*0.25_krp)
    assert(is_true,'updates_azimuthal_weights wrong')
    call test%clear

  endsubroutine test_updates_weight_expand_to_half

!>------------------------------------------------------------------------------
!>
  subroutine test_clear()

    call test%clear()
    call test%init(2,8,8)
    call test%clear()
    call test%clear()
    assert(.not.(test%is_init),' %clear failed!')
    assert((test%i_type == 0),' %clear failed!')
    assert((test%n_alpha == 0),' %clear failed!')
    assert((test%n_theta == 0),' %clear failed!')

    assert(.not.allocated(test%alpha),' %clear failed!')
    assert(.not.allocated(test%weight_alpha),' %clear failed!')
    assert(.not.allocated(test%theta),' %clear failed!')
    assert(.not.allocated(test%sin_theta),' %clear failed!')
    assert(.not.allocated(test%r_sin_theta),' %clear failed!')
    assert(.not.allocated(test%weight_theta),' %clear failed!')

  endsubroutine test_clear

!-------------------------------------------------------------------------------
endprogram