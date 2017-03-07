module endf_format_output

  use linkedlist
  use error_warning
  implicit none
  private

# include <kind_parameter.h>

  public :: tab1_out
  public :: tab2_out
  public :: list_out
  public :: cont_out
  public :: dir_out
  public :: head_out
  public :: text_out
  public :: end_out
  public :: title_define
  public :: column_format_type

!-------------------------------------------------------------------------------
  type column_format_type
    realkd :: float                              !< effective part
    intknd :: int                                !< exponent part
    character(len=1) :: operater = ''            !< operater part

  contains

    procedure, pass :: get_value

  endtype column_format_type

  !> Set some digits control parameter
  realkd, parameter :: zero  = 0.0_krp
  realkd, parameter :: tenth = 0.1_krp
  realkd, parameter :: onem  = 0.999999999_krp
  realkd, parameter :: one   = 1.0_krp
  realkd, parameter :: ten   = 10._krp
  realkd, parameter :: amil  = 0.999999999e6_krp
  realkd, parameter :: up    = 0.000000001_krp
  realkd, parameter :: top9  = 9.999999995_krp
  realkd, parameter :: bot9  = 9.99999995_krp
  realkd, parameter :: top7  = 9.9999995_krp
  realkd, parameter :: top6  = 9.999995_krp
  realkd, parameter :: top5  = 9.99995_krp

!===============================================================================

contains

!-------------------------------------------------------------------------------
!> output the HEAD format to file
!>
!> @param unit - in, unit of output file
!> @param c - in, c value in HEAD format
!> @param d - in, d value in HEAD format
!> @param mat,mf,mt - in, standard ENDF identifier
!>
!> Note :
!>       1) The output file must be opened.
!-------------------------------------------------------------------------------
  subroutine head_out(unit,c,d,mat,mf,mt,line_num)
    intknd, intent(in) :: unit
    realkd, intent(in) :: c(2)
    intknd, intent(in) :: d(4)
    intknd, intent(in) :: mat,mf,mt
    intknd, intent(inout) :: line_num
    intknd :: loop_c
    chr100 :: errmsg

    !> output parameter c
    do loop_c = 1,2
      call write_float(unit,c(loop_c))
    enddo
    !> output parameter d, standard ENDF identifier, line #
    write(unit,'(4i11,i4,i2,i3,i5)',advance='yes')d(1:4),mat,mf,mt,line_num
    line_num = line_num+1
  endsubroutine

!-------------------------------------------------------------------------------
!> output the CONT format to file
!>
!> @param unit - in, unit of output file
!> @param c - in, c value in CONT format
!> @param d - in, d value in CONT format
!> @param mat,mf,mt - in, standard ENDF identifier
!>
!> Note :
!>       1) The output file must be opened.
!-------------------------------------------------------------------------------
  subroutine cont_out(unit,c,d,mat,mf,mt,line_num)
    intknd, intent(in) :: unit
    realkd, intent(in) :: c(2)
    intknd, intent(in) :: d(4)
    intknd, intent(in) :: mat,mf,mt
    intknd, intent(inout) :: line_num
    intknd :: loop_c
    chr100 :: errmsg

    !> output parameter c
    do loop_c = 1,2
      call write_float(unit,c(loop_c))
    enddo
    !> output parameter d, standard ENDF identifier, line #
    write(unit,'(4i11,i4,i2,i3,i5)',advance='yes')d(1:4),mat,mf,mt,line_num
    line_num = line_num+1
  endsubroutine

!-------------------------------------------------------------------------------
!> output the DIR format to file
!>
!> @param unit - in, unit of output file
!> @param mfn,mtn,ncn,modn - in, standard ENDF identifier
!> @param mat,mf,mt - in, standard ENDF identifier
!>
!> Note :
!>       1) The output file must be opened.
!-------------------------------------------------------------------------------
  subroutine dir_out(unit,mfn,mtn,ncn,modn,mat,mf,mt,line_num)
    intknd, intent(in) :: unit
    intknd, intent(in) :: mfn,mtn,ncn,modn
    intknd, intent(in) :: mat,mf,mt
    intknd, intent(inout) :: line_num
    chr100 :: errmsg

    !> two blank, mfn, mtn, ncn, standard ENDF identifier, line #
    write(unit,'(22x,4i11,i4,i2,i3,i5)',advance='yes')mfn,mtn,ncn,modn,mat,mf, &
    mt,line_num
    line_num = line_num+1
  endsubroutine

!-------------------------------------------------------------------------------
!> output the TAB1 format to file
!>
!> @param unit - in, unit of output file
!> @param c - in, c value in TAB1 format
!> @param d - inout, d value in TAB1 format
!> @param a - inout, TAB1 value in TAB1 format with allocatable array
!> @param list - in, TAB1 value in TAB1 format with linkedlist type
!> @param mat,mf,mt - in, standard ENDF identifier
!>
!> Note :
!>       1) The output file must be opened.
!>       2) There are two option to output the data, array and linkedlist.
!>       3) The allocatable a,d array and linkedlist list will be clear after
!>          this subroutine.
!-------------------------------------------------------------------------------
  subroutine tab1_out(unit,c,d,mat,mf,mt,line_num,a,list)
    intknd, intent(in) :: unit
    realkd, intent(in) :: c(2)
    intknd, intent(inout), allocatable :: d(:)
    realkd, intent(inout), allocatable, optional :: a(:)
    type(real_list_type), intent(inout), optional :: list
    intknd, intent(in) :: mat,mf,mt
    intknd, intent(inout) :: line_num
    intknd :: loop_c,loop_line,loop_d,loop_a
    chr100 :: errmsg

    !> output control parameter
    !> output parameter c
    do loop_c = 1,2
      call write_float(unit,c(loop_c))
    enddo
    !> output parameter d, standard ENDF identifier, line #
    write(unit,'(4i11,i4,i2,i3,i5)',advance='yes')d(1:4),mat,mf,mt,line_num
    line_num = line_num+1
    !> output the rest of parameter d
    if (mod(size(d)-4,6) /= 0)then
      do loop_line = 0,int((size(d)-4)/6.0_krp)
        do loop_d = 5+loop_line*6,min(size(d),10+loop_line*6)
          write(unit,'(i11)',advance='no')d(loop_d)
        enddo
        if (loop_line == int((size(d)-4)/6.0_krp))then
          !> write more blank
          select case(6-mod(size(d)-4,6))
          case(2)
            write(unit,'(22x)',advance='no')
          case(4)
            write(unit,'(44x)',advance='no')
          case default
            write(errmsg,'("wrong array size of D")')
            call raise_error(errmsg)
          end select
          write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
          line_num = line_num+1
        else
          write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
          line_num = line_num+1
        endif
      enddo
    else
      do loop_line = 0,int((size(d)-4)/6.0_krp)-1
        do loop_d = 5+loop_line*6,min(size(d),10+loop_line*6)
          write(unit,'(i11)',advance='no')d(loop_d)
        enddo
          write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
          line_num = line_num+1
      enddo
    endif
    !> output the tab1 record
    if (present(a))then
      if (mod(size(a),6) /= 0)then
        do loop_line = 0,int(size(a)/6.0_krp)
          do loop_a = 1+loop_line*6,min(size(a),6+loop_line*6)
            call write_float(unit,a(loop_a))
          enddo
          if (loop_line == int(size(a)/6.0_krp))then
            !> write more blank
            select case(6-mod(size(a),6))
            case(2)
              write(unit,'(22x)',advance='no')
            case(4)
              write(unit,'(44x)',advance='no')
            case default
              write(errmsg,'("wrong array size of the tab1 record")')
              call raise_error(errmsg)
            end select
            write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
            line_num = line_num+1
          else
            write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
            line_num = line_num+1
          endif
        enddo
      else
        do loop_line = 0,int(size(a)/6.0_krp)-1
          do loop_a = 1+loop_line*6,min(size(a),6+loop_line*6)
            call write_float(unit,a(loop_a))
          enddo
            write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
            line_num = line_num+1
        enddo
      endif
    elseif (present(list))then
      if (mod(list%nElement,6) /= 0)then
        do loop_line = 0,int(list%nElement/6.0_krp)
          do loop_a = 1+loop_line*6,min(list%nElement,6+loop_line*6)
            call write_float(unit,list%get_value(loop_a))
          enddo
          if (loop_line == int(list%nElement/6.0_krp))then
            !> write more blank
            select case(6-mod(list%nElement,6))
            case(2)
              write(unit,'(22x)',advance='no')
            case(4)
              write(unit,'(44x)',advance='no')
            case default
              write(errmsg,'("wrong array size of the tab1 record")')
              call raise_error(errmsg)
            end select
            write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
            line_num = line_num+1
          else
            write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
            line_num = line_num+1
          endif
        enddo
      else
        do loop_line = 0,int(list%nElement/6.0_krp)-1
          do loop_a = 1+loop_line*6,min(list%nElement,6+loop_line*6)
            call write_float(unit,list%get_value(loop_a))
          enddo
            write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
            line_num = line_num+1
        enddo
      endif
    else
      write(errmsg,'("no TAB1 record to output")')
      call raise_error(errmsg)
    endif
    deallocate(d)
    if (present(a))then
      deallocate(a)
    elseif (present(list))then
      call list%clear()
    endif
  endsubroutine

!-------------------------------------------------------------------------------
!> output the LIST format to file
!>
!> @param unit - in, unit of output file
!> @param c - in, c value in LIST format
!> @param d - in, d value in LIST format
!> @param a - inout, tab1 value in LIST format with allocatable array
!> @param list - in, LIST value in LIST format with linkedlist type
!> @param mat,mf,mt - in, standard ENDF identifier
!>
!> Note :
!>       1) The output file must be opened.
!>       2) There are two option to output the data, array and linkedlist.
!>       3) The allocatable array a and linkedlist list will be clear after this
!>          subroutine.
!-------------------------------------------------------------------------------
  subroutine list_out(unit,c,d,mat,mf,mt,line_num,a,list)
    intknd, intent(in) :: unit
    realkd, intent(in) :: c(2)
    intknd, intent(in) :: d(4)
    intknd, intent(in) :: mat,mf,mt
    realkd, intent(inout), allocatable, optional :: a(:)
    type(real_list_type), intent(inout), optional :: list
    intknd, intent(inout) :: line_num
    intknd :: loop_c,loop_line,loop_d,loop_a
    intknd :: six = 6
    chr100 :: errmsg

    !> output control parameter
    !> output parameter c
    do loop_c = 1,2
      call write_float(unit,c(loop_c))
    enddo
    !> output parameter d, standard ENDF identifier, line #
    write(unit,'(4i11,i4,i2,i3,i5)',advance='yes')d(1:4),mat,mf,mt,line_num
    line_num = line_num+1
    !> output the list record
    if (present(a))then
      if (mod(size(a),six) /= 0)then
        do loop_line = 0,size(a)/six
          do loop_a = 1+loop_line*six,min(size(a),six+loop_line*six)
            call write_float(unit,a(loop_a))
          enddo
          if (loop_line == size(a)/six)then
            !> write more blank
            select case(six-mod(size(a),six))
            case(1)
              write(unit,'(11x)',advance='no')
            case(2)
              write(unit,'(22x)',advance='no')
            case(3)
              write(unit,'(33x)',advance='no')
            case(4)
              write(unit,'(44x)',advance='no')
            case(5)
              write(unit,'(55x)',advance='no')
            end select
            write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
            line_num = line_num+1
          else
            write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
            line_num = line_num+1
          endif
        enddo
      else
        do loop_line = 0,size(a)/six-1
          do loop_a = 1+loop_line*six,min(size(a),six+loop_line*six)
            call write_float(unit,a(loop_a))
          enddo
            write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
            line_num = line_num+1
        enddo
      endif
    elseif (present(list))then
      if (mod(list%nElement,six) /= 0)then
        do loop_line = 0,list%nElement/six
          do loop_a = 1+loop_line*six,min(list%nElement,six+loop_line*six)
            call write_float(unit,list%get_value(loop_a))
          enddo
          if (loop_line == list%nElement/six)then
            !> write more blank
            select case(six-mod(list%nElement,six))
            case(1)
              write(unit,'(11x)',advance='no')
            case(2)
              write(unit,'(22x)',advance='no')
            case(3)
              write(unit,'(33x)',advance='no')
            case(4)
              write(unit,'(44x)',advance='no')
            case(5)
              write(unit,'(55x)',advance='no')
            end select
            write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
            line_num = line_num+1
          else
            write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
            line_num = line_num+1
          endif
        enddo
      else
        do loop_line = 0,list%nElement/six-1
          do loop_a = 1+loop_line*six,min(list%nElement,six+loop_line*six)
            call write_float(unit,list%get_value(loop_a))
          enddo
            write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
            line_num = line_num+1
        enddo
      endif
    else
      write(errmsg,'("no TAB1 record to output")')
      call raise_error(errmsg)
    endif
    if (present(a))then
      deallocate(a)
    elseif (present(list))then
      call list%clear()
    endif
  endsubroutine

!-------------------------------------------------------------------------------
!> output the TAB2 format to file
!>
!> @param unit - in, unit of output file
!> @param c - in, c value in TAB2 format
!> @param d - inout, d value in TAB2 format
!> @param mat,mf,mt - in, standard ENDF identifier
!>
!> Note :
!>       1) The output file must be opened.
!>       2) The allocatable array d will be deallocated after this subroutine.
!-------------------------------------------------------------------------------
  subroutine tab2_out(unit,c,d,mat,mf,mt,line_num)
    intknd, intent(in) :: unit
    realkd, intent(in) :: c(2)
    intknd, intent(inout), allocatable :: d(:)
    intknd, intent(in) :: mat,mf,mt
    intknd, intent(inout) :: line_num
    intknd :: loop_c,loop_line,loop_d
    chr100 :: errmsg

    !> output parameter c
    do loop_c = 1,2
      call write_float(unit,c(loop_c))
    enddo
    !> output parameter d, standard ENDF identifier, line #
    write(unit,'(4i11,i4,i2,i3,i5)',advance='yes')d(1:4),mat,mf,mt,line_num
    line_num = line_num+1
    !> output the rest of parameter d
    if (mod(size(d)-4,6) /= 0)then
      do loop_line = 0,int((size(d)-4)/6.0_krp)
        do loop_d = 5+loop_line*6,min(size(d),10+loop_line*6)
          write(unit,'(i11)',advance='no')d(loop_d)
        enddo
        if (loop_line == int((size(d)-4)/6.0_krp))then
          !> write more blank
          select case(6-mod(size(d)-4,6))
          case(2)
            write(unit,'(22x)',advance='no')
          case(4)
            write(unit,'(44x)',advance='no')
          case default
            write(errmsg,'("wrong array size of D")')
            call raise_error(errmsg)
          end select
          write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
          line_num = line_num+1
        else
          write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
          line_num = line_num+1
        endif
      enddo
    else
      do loop_line = 0,int((size(d)-4)/6.0_krp)-1
        do loop_d = 5+loop_line*6,min(size(d),10+loop_line*6)
          write(unit,'(i11)',advance='no')d(loop_d)
        enddo
          write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
          line_num = line_num+1
      enddo
    endif
    deallocate(d)
  endsubroutine

!-------------------------------------------------------------------------------
!> output the END format to file
!>
!> @param unit - in, unit of output file
!> @param mat,mf,mt - in, standard ENDF identifier
!> @param flag - in, flag to define END format type
!>                   1 - SEND, 2 - FEND, 3 - MEND, 4 - TEND
!>
!> Note :
!>       1) The output file must be opened.
!>       2) 4 options represent the end of a section, file, material, or tape,
!>          respectively.
!>       3) This subroutine will reset the line_num to 1
!-------------------------------------------------------------------------------
  subroutine end_out(unit,mat,mf,control,line_num)
    intknd, intent(in) :: unit,mat,mf,control
    intknd :: int_zero = 0,five_nine = 99999,neg_one = -1
    intknd, intent(inout) :: line_num

    select case(control)
    case(1)
      !> SEND
      write(unit,'(66x,i4,i2,i3,i5)',advance = 'yes')mat,mf,int_zero,five_nine
    case(2)
      !> FEND
      write(unit,'(66x,i4,i2,i3,i5)',advance = 'yes')mat,int_zero,int_zero,int_zero
    case(3)
      !> MEND
      write(unit,'(66x,i4,i2,i3,i5)',advance = 'yes')int_zero,int_zero,int_zero,int_zero
    case(4)
      !> TEND
      write(unit,'(66x,i4,i2,i3,i5)',advance = 'yes')neg_one,int_zero,int_zero,int_zero
    end select
    line_num = 1
  endsubroutine

!-------------------------------------------------------------------------------
!> output the TEXT format to file
!>
!> @param unit - in, unit of output file
!> @param text - in, text data in TEXT format
!> @param mat,mf,mt - in, standard ENDF identifier
!>
!> Note :
!>       1) The output file must be opened.
!-------------------------------------------------------------------------------
  subroutine text_out(unit,text,mat,mf,mt,line_num)
    intknd, intent(in) :: unit,mat,mf,mt
    intknd, intent(inout) :: line_num
    character(*), intent(in) :: text
    intknd :: loop_line,loop_blank
    character(len = 1) :: blank = " "

    if (mod(len_trim(text),66) /= 0)then
      do loop_line = 0,int(len_trim(text)/66.0_krp)
        write(unit,'(a)',advance='no')text(loop_line*66+1:                   &
        min(len_trim(text),66+loop_line*66))
        if (loop_line == int(len_trim(text)/66.0_krp))then
          !> write more blank
          do loop_blank = 1,66-mod(len_trim(text),66)
            write(unit,'(a1)',advance='no')blank
          enddo
          write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
          line_num = line_num+1
        else
          write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
          line_num = line_num+1
        endif
      enddo
    else
      do loop_line = 0,int(mod(len_trim(text),66))-1
        write(unit,'(a66)',advance='no')text(loop_line*66+1:                   &
          min(len_trim(text),66+loop_line*66))
          write(unit,'(i4,i2,i3,i5)',advance ='yes')mat,mf,mt,line_num
          line_num = line_num+1
      enddo
    endif
  endsubroutine

!-------------------------------------------------------------------------------
!> output the float number to file with 7/9 digits
!>
!> @param unit - in, unit of output file
!> @param val - in, specified value
!>
!> Note :
!>       1) to be conntinued.. (method)
!-------------------------------------------------------------------------------
  subroutine write_float(unit,val)
    intknd, intent(in) :: unit
    realkd, intent(in) :: val
    realkd :: absval
    chr100 :: errmsg
    type(column_format_type) :: x

    absval = abs(val)
    call x%get_value(val)
    if (absval >= amil .or. absval < one) then
      !> 7-digits to output
      if (x%int < 10)then
        write(unit,'(f9.6,a1,i1)',advance='no')x%float,x%operater,x%int
      elseif (x%int < 100)then
        write(unit,'(f8.5,a1,i2)',advance='no')x%float,x%operater,x%int
      else
        !write(errmsg,'("value is to large to output:",es)')val
        !call raise_error("errmsg")
      endif
    else
      !> judging the effective value
      if (abs(1-anint(x%float*1.e6_krp)/x%float/1.e6_krp) < 1.e-13_krp)then
        !> 7-digits output
        if (x%int < 10)then
          write(unit,'(f9.6,a1,i1)',advance='no')x%float,x%operater,x%int
        elseif (x%int < 100)then
          write(unit,'(f8.5,a1,i2)',advance='no')x%float,x%operater,x%int
        else
          !write(errmsg,'("value is to large to output:",es)')val
          !call raise_error("errmsg")
        endif
      else
        !> 9-digits output
        select case (x%int)
        case(0)
          write(unit,'(f11.8)',advance='no')val
        case(1)
          write(unit,'(f11.7)',advance='no')val
        case(2)
          write(unit,'(f11.6)',advance='no')val
        case(3)
          write(unit,'(f11.5)',advance='no')val
        case(4)
          write(unit,'(f11.4)',advance='no')val
        case(5)
          write(unit,'(f11.3)',advance='no')val
        end select
      endif
    endif
  endsubroutine

!-------------------------------------------------------------------------------
!> get the float number, operater, exponent number of x.
!>
!> @param x - in, specified value
!-------------------------------------------------------------------------------
  subroutine get_value(this,x)
    class(column_format_type) :: this
    realkd, intent(in) :: x

    !> check zero
    if (x == zero)then
      this%float = zero
      this%operater = '+'
      this%int = 0
    elseif (x > tenth .and. x < amil)then
      !> extended nine sig-fig form
      this%int = int(log10(abs(x)))
      if (abs(x) < onem)then
        this%float = 10._krp*x
        this%operater = '-'
        this%int = 1
        return
      endif
      this%float = x/ten**this%int
      this%operater = '+'
      if(abs(this%int) < 10 .and. abs(this%float) < top9) return
      if(abs(this%int) >= 10 .and. abs(this%float) < bot9) return
      this%float = this%float/ten
      this%int = this%int+1
    else
      !> normal seven, six, and five-fig form
      this%int = int(log10(abs(x)))
      if (abs(x) < one)then
        this%int = 1-this%int
        this%float = x*ten**this%int
        this%operater = '-'
        if(abs(this%int) < 10 .and. abs(this%float) < top7) return
        if(abs(this%int) < 100 .and. abs(this%int) >= 10 .and.                 &
                                                  abs(this%float) < top6) return
        if(abs(this%int) >= 100 .and. abs(this%float) < top5) return
        this%float = this%float/ten
        this%float = this%float+up
        this%int = this%int-1
        if (this%int > 0)return
        this%operater = '+'
      endif
      this%float = x/ten**this%int
      this%operater = '+'
      if(abs(this%int) < 10 .and. abs(this%float) < top7) return
      if(abs(this%int) < 100 .and. abs(this%int) >= 10 .and.                   &
                                                  abs(this%float) < top6) return
      if(abs(this%int) >= 100 .and. abs(this%float) < top5) return
      this%float = this%float/ten
      this%float = this%float+up
      this%int = this%int +1
    endif
  endsubroutine

!-------------------------------------------------------------------------------
!> generate the title for the endf format tape.
!>
!> @param mat - in, standard standard ENDF identifier
!> @param nlib - in, ENDF format library identifier
!-------------------------------------------------------------------------------
  function title_define(mat,nlib) result(title)
    intknd, intent(in) :: mat,nlib
    character(len = 66) :: title
    character(len = 20) :: lib

    select case(nlib)
    case(0)
      lib = 'ENDF/B'
    case(1)
      lib = 'ENDF/A'
    case(2)
      lib = 'JEFF'
    case(3)
      lib = 'EFF'
    case(4)
      lib = 'ENDF/B - high energy'
    case(5)
      lib = 'CENDL'
    case(6)
      lib = 'JENDL'
    case(21)
      lib = 'SG-23'
    case(31)
      lib = 'INDL/V'
    case(32)
      lib = 'INDL/A'
    case(33)
      lib = 'FENDL'
    case(34)
      lib = 'IRDF'
    case(35)
      lib = 'BROND - IAEA version'
    case(36)
      lib = 'INGDB-90'
    case(37)
      lib = 'FENDL/A'
    case(41)
      lib = 'BROND - original version'
    case default
      lib = 'Unknown ENDF library'
    end select
    write(title,'("MAT",1x,i4,1x,"from",1x,a,1x,"processed by NECP-ATLAS")')   &
    mat,trim(lib)

  endfunction

!-------------------------------------------------------------------------------
endmodule
