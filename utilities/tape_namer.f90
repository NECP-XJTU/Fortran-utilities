module tape_namer


  implicit none
  private

    
# include <kind_parameter.h>
  
  public :: namer


  contains

  subroutine namer(unit,name)
    intknd, intent(in) :: unit
    charSt, intent(inout) :: name
    charkd(len=3) :: num
    write(num,'(i3)')unit
    name = './'//'tape'//'.'//trim(adjustl(num))
  endsubroutine

endmodule