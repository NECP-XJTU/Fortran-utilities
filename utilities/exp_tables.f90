module exp_table
  implicit none
  private

# include <kind_parameter.h>
  public :: exp_table_type

  type :: exp_table_type
    logknd :: is_init = .false.
    intknd :: ninterval = 0
    realkd :: stt_val = 0.0D0
    realkd :: stp_val = 0.0D0
    realkd :: rdx = 0.0D0
    realkd :: dx = 0.0D0
    realkd,allocatable :: table(:,:)
  contains
    procedure,pass :: init => init_exp_table
    procedure,pass :: expcalc
    
  endtype exp_table_type
contains
  
  elemental function expcalc(this,x) result(ans)
    class(exp_table_type),intent(in) :: this
    realkd,intent(in) :: x
    realkd :: ans
    
    integer :: i
    
    i = floor(x*this%rdx)
    ans = this%table(1,i)*x + this%table(2,i)
    
  endfunction
  
  subroutine init_exp_table(this,stt_val,stp_val,ninterval)
    class(exp_table_type),intent(inout) :: this
    realkd,intent(in) :: stt_val
    realkd,intent(in) :: stp_val
    intknd,intent(in) :: ninterval
    
    intknd :: i
    intknd :: stt_table
    intknd :: stp_table
    realkd :: x1,x2,y1,y2
    
    stt_table = stt_val*ninterval
    stp_table = stp_val*ninterval
    
    this%stt_val = stt_val
    this%stp_val = stp_val
    this%ninterval = ninterval
    this%rdx = 1.0D0*ninterval
    this%dx = 1.0D0/ninterval
    allocate(this%table(2,stt_table:stp_table))
    
    x1 = stt_val
    y1 = 1.0D0 - exp(x1)
    do i = stt_table,stp_table
      x2 = x1 + this%dx
      y2 = 1.0D0 - exp(x2)
      this%table(1,i) = (y2-y1) * this%rdx
      this%table(2,i) = y1 - this%table(1,i)*x1
      x1 = x2
      y1 = y2
    enddo
    
    this%is_init = .true.
  
  endsubroutine

  endmodule exp_table
  