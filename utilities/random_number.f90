module random_number
  implicit none
  private

  public :: sub_random

  integer(4),save      :: random_stt = 1
  integer(8),save      :: n_random   = 0
  integer(4),parameter :: lambda     = 65539
  real(4),parameter    :: m        = 2147483647
  real(4),parameter    :: m_1      = 0.4656613e-9

contains

  function proc_random() result(random_real)
    real(4)      :: random_real

!    random_stt = random_stt * 65539 + m_1
!    random_real = random_stt * m
!    n_random = n_random + 1
  endfunction proc_random

  subroutine sub_random()

 !   random_stt = random_stt * lambda
 !   write(*,*) 'random_stt:',random_stt
 !   random_stt = random_stt + m
 !   write(*,*) 'random_stt:',random_stt
 !   random_real = random_stt * m_1
 !   write(*,*) 'random_real:',random_real
 !   write(*,*)
 !   n_random = n_random + 1
  endsubroutine sub_random

endmodule random_number