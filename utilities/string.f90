module string
  use error_warning
  implicit none

  private

# include <kind_parameter.h>

  !public list
  public :: str_to_cap
  public :: str_to_low
  public :: str_to_log
  public :: str_slash_replace
  public :: str_replace
  public :: str_find
  public :: str_cout
  public :: str_match
  public :: to_str
  public :: str_to_int
  public :: str_to_real
  public :: str_split
  public :: str_isInt
  public :: str_isReal
  public :: str_isLog
  public :: str_name_of_file
  public :: str_ext_of_file
  public :: str_path_of_file
  public :: str_join_path

  public :: slash
  public :: forward_slash
  public :: backward_slash
  public :: colon_mark
  public :: bang_mark
  public :: equal_mark
  public :: blank_mark
  public :: dash_mark
  public :: ddash_mark



  charSt,parameter :: forward_slash  = '/'       !< A forward slash parameter
  charSt,parameter :: backward_slash = '\'       !< A backword slash parameter
  charSt,parameter :: colon_mark     = ':'       !< A colon parameter
  charSt,parameter :: bang_mark      = '!'       !< A colon parameter
  charSt,parameter :: equal_mark     = '='       !< A colon parameter
  charSt,parameter :: blank_mark     = ' '       !< A colon parameter
  charSt,parameter :: dash_mark      = '-'       !< A colon parameter
  charSt,parameter :: ddash_mark     = '--'      !< A colon parameter
  charSt,parameter :: pot_mark       = '.'       !< A colon parameter

#ifdef SYS_WIN32
  charSt,parameter :: slash = backward_slash !< backward slash for windows
#else
  charSt,parameter :: slash = forward_slash  !< forward slash for other systems
#endif

  !-----------------------------------------------------------------------------
  !> Interface to combine int_to_str and real_to_str to one function to_str
  interface to_str
    module procedure int_to_str, real_to_str
  endinterface

  !-----------------------------------------------------------------------------
  !> Interface to combine str_split_space, str_split_one and str_split_more
  interface str_split
    module procedure str_split_space, str_split_one, str_split_more
  endinterface str_split

!==============================================================================

contains

!-------------------------------------------------------------------------------
!> Change the str to capital case
!-------------------------------------------------------------------------------
  elemental function str_to_cap(str) result(str_cap)

    charSt, intent(in)      :: str     !< The str to be changed
    character(len=len(str)) :: str_cap !< capital case of str

    intknd :: i

    do i = 1, len(str)
      if('a' <= str(i:i) .and. str(i:i) <= 'z') then
        str_cap(i:i) = achar(iachar(str(i:i)) - 32)
      else
        str_cap(i:i) = str(i:i)
      endif
    enddo

  endfunction str_to_cap

!-------------------------------------------------------------------------------
!> Change the str to low case
!-------------------------------------------------------------------------------
  elemental function str_to_low(str) result(str_low)

    charSt, intent(in)      :: str      !< The str to be changed
    character(len=len(str)) :: str_low  !< lower case of str

    intknd :: i

    do i = 1, len(str)
      if('A' <= str(i:i) .and. str(i:i) <= 'Z') then
        str_low(i:i) = achar(iachar(str(i:i)) + 32)
      else
        str_low(i:i) = str(i:i)
      endif
    enddo

  endfunction str_to_low

!-------------------------------------------------------------------------------
!> Convert string to logical
!-------------------------------------------------------------------------------
  function str_to_log(str) result(str_log)
    charSt, intent(in) :: str
    logknd :: str_log

    str_log = .false.
    select case (str_to_low(trim(adjustl(str))))
    case ('yes', 'on', 'true')
      str_log = .true.
    case ('no', 'off', 'false')
      str_log = .false.
    case default
      str_log = .false.
      call raise_warning('String "'//trim(adjustl(str))//'"is not a logical identification!')
    end select

  end function str_to_log

!-------------------------------------------------------------------------------
!> Replace the slash
!-------------------------------------------------------------------------------
  elemental function str_slash_replace(str) result(str_slash_rep)

    charSt, intent(in)        :: str           !< The str to be changed
    character(len = len(str)) :: str_slash_rep !< str after replacement

    intknd :: i


#ifdef SYS_WIN32
  charSt,parameter :: to_replace_slash = forward_slash
#else
  charSt,parameter :: to_replace_slash = backward_slash
#endif

    do i = 1, len(str)
        if(str(i:i) == to_replace_slash) then
          str_slash_rep(i:i) = slash
        else
            str_slash_rep(i:i) = str(i:i)
        endif
    enddo

  endfunction str_slash_replace

!-------------------------------------------------------------------------------
!> Replace part of the str
!-------------------------------------------------------------------------------
 elemental function str_replace(str,str_ori,str_rep) result(str_out)

   charSt,intent(in)          :: str          !< The original str
   charSt,intent(in)          :: str_ori      !< The part to be replaced
   charSt,intent(in)          :: str_rep      !< The part we using to repalce
   character(len = &
   (len(str) + len(str_rep)                                                    &
   - len(str_ori)))           :: str_out      !< The str after replacement

   character(len = len(str))  :: str_1        !< Preceding str
   character(len = len(str))  :: str_2        !< Subsequent str

   intknd :: i

   str_out = str
   do i =1, len(str) - len(str_ori) + 1
     if(str_out(i:(i+len(str_ori)-1)) == str_ori)then
       str_1 = str_out(1:(i-1))
       str_2 = str_out((i+len(str_ori)):len(str))
       str_out = str_1(1:(i-1)) // str_rep // str_2(1:len_trim(str_2))
     endif
   enddo

 endfunction str_replace

!-------------------------------------------------------------------------------
!> Find the index a string appearing in  another string
!-------------------------------------------------------------------------------
  elemental function str_find(str,str_to_find,ith) result (n)

    charSt, intent(in)  :: str          !< The origal str
    charSt, intent(in)  :: str_to_find  !< The str to find
    intknd, intent(in)  :: ith          !< i-th str_to_find
    intknd              :: n            !< The location of ith

    intknd              :: index_n
    intknd              :: iend         !< Off location
    intknd  :: i

    n = 0
    if(len(str) > 0) then
      index_n = 0
      iend = len(str_to_find) - 1
      do i =1, len(str) - len(str_to_find) + 1
        if(str(i:i+iend) == str_to_find)then
          index_n = index_n + 1
          if(index_n == ith)then
            n = i
          endif
          if(index_n == ith)exit
        endif
      enddo
    endif

  endfunction str_find

!-------------------------------------------------------------------------------
!> Cout the frequency a str appeared in another str
!-------------------------------------------------------------------------------
  elemental function str_cout(str,str_to_cout) result(n)

    charSt, intent(in) :: str              !< The origal str
    charSt, intent(in) :: str_to_cout      !< The str to cout
    intknd             :: n                !< The total frequency

    intknd :: iend                         !< Off location
    intknd :: i

    n= 0
    if(len(str) > 0) then
      iend = len(str_to_cout) - 1
      do i =1, len(str) - len(str_to_cout) + 1
        if(str(i:i+iend) == str_to_cout)then
          n = n + 1
        endif
      enddo
    endif

  endfunction str_cout

!-------------------------------------------------------------------------------
!> Change integer to string
!-------------------------------------------------------------------------------
  function int_to_str(int) result(str)

    intknd,         intent(in)  :: int         !< Value to convert to char form
    charkd(len = 21) :: str         !< Output character string

    write (str,*) int
    str =trim(adjustl(str))

  endfunction int_to_str

!-------------------------------------------------------------------------------
!> Change string to integer
!-------------------------------------------------------------------------------
  function str_to_int(str) result(int)

    charSt,intent(in) :: str         !< String to convert to int form
    intknd            :: int         !< Output integer

    if(str_isInt(str)) then
      read(str,*) int
    else
      int = -huge(0)
      call raise_error('String "'//str//'"is not a integer')
    endif

  endfunction str_to_int

!-------------------------------------------------------------------------------
!> Change real to string
!-------------------------------------------------------------------------------
   function real_to_str(real_in,digits_in) result(str)

    realkd, intent(in)         :: real_in    !< Real to convert to char form
    intknd,optional,intent(in) :: digits_in

    character(len = 15)         :: str        !< Output character string
    character(9)                :: fmt        !< ¸ñÊ½

    intknd :: digits,width
    realkd :: rtemp

    digits = 6
    if(present(digits_in)) digits = digits_in

    width = 15

    !clear string before use
    str = ''

    rtemp = abs(real_in)

    !select proper format
    if (rtemp >= 0.0e-1_krp .and. rtemp < 1.0_krp) then
      write(fmt, '("(ES",I2,".",I2,")")') width, digits-1
    elseif (rtemp >= 1.0_krp .and. rtemp < 10.0_krp) then
      write(fmt, '("(F",I2,".",I2,")")') width, max(digits-1, 0_kip)
    elseif (rtemp >= 10.0_krp .and. rtemp < 100.0_krp) then
      write(fmt, '("(F",I2,".",I2,")")') width, max(digits-2, 0_kip)
    elseif (rtemp >= 100.0_krp .and. rtemp < 1000.0_krp) then
      write(fmt, '("(F",I2,".",I2,")")') width, max(digits-3, 0_kip)
    elseif (rtemp >= 100.0_krp .and. rtemp < 10000.0_krp) then
      write(fmt, '("(F",I2,".",I2,")")') width, max(digits-4, 0_kip)
    elseif (rtemp >= 10000.0_krp .and. rtemp < 100000.0_krp) then
      write(fmt, '("(F",I2,".",I2,")")') width, max(digits-5, 0_kip)
    else
      write(fmt, '("(ES",I2,".",I2,")")') width, digits - 1
    end if

    write(str,fmt) real_in
    str = trim(adjustl(str))
  endfunction real_to_str

!-------------------------------------------------------------------------------
!> Change string to real
!-------------------------------------------------------------------------------
  function str_to_real(str) result(real_out)

    charSt,intent(in)       :: str      !< String to convert real form
    realkd                  :: real_out !< Output real

    if(str_isReal(str)) then
      read(str,*) real_out
    else
      real_out = -huge(0.0)
      call raise_error('String "'//str//'"is not a real number')
    endif

  endfunction str_to_real

!-------------------------------------------------------------------------------
!>Split the str to parts
!-------------------------------------------------------------------------------
  subroutine str_split_more(str,words,split_1,split_2,split_3,split_4,split_5)

    charSt,intent(in)   :: str
    charSt,allocatable,intent(out)  :: words(:)
    character(1), intent(in) :: split_1
    character(1), intent(in) :: split_2
    character(1), intent(in),optional :: split_3
    character(1), intent(in),optional :: split_4
    character(1), intent(in),optional :: split_5

    character(len = len(str)) :: str_temp

    str_temp = str

    str_temp = str_replace(str_temp,split_2,split_1)

    if(present(split_3))then
      str_temp = str_replace(str_temp,split_3,split_1)
    endif

    if(present(split_4))then
      str_temp = str_replace(str_temp,split_4,split_1)
    endif

    if(present(split_5))then
      str_temp = str_replace(str_temp,split_5,split_1)
    endif
    call str_split_one(str_temp,words,split_1)
  endsubroutine str_split_more

!-------------------------------------------------------------------------------
!>Split the str to parts with space
!-------------------------------------------------------------------------------
  subroutine str_split_space(str,words)

    charSt,intent(in)              :: str
    charSt,allocatable,intent(out) :: words(:)

    call str_split_one(str,words,' ')

  endsubroutine str_split_space

!-------------------------------------------------------------------------------
!>Split the str to parts with
!-------------------------------------------------------------------------------
  subroutine str_split_one(str,words,split)

    charSt,intent(in)               :: str
    character(1), intent(in)        :: split
    charSt,allocatable,intent(out)  :: words(:)

    character(len=256) :: words_temp(80)

    character(1)  :: chr     ! current character
    intknd        :: n
    intknd        :: i       ! current index
    intknd        :: i_start ! starting index of word
    intknd        :: i_end   ! ending index of word
    character(len = len(str)) :: str_temp

    logknd        :: is_quiet,is_stop_on_error

    is_quiet = get_is_quiet()
    is_stop_on_error = get_is_stop_on_error()

    call set_quiet(.false.)
    call set_stop_on_error(.true.)

    words_temp = 'test'
    str_temp = str
    i_start = 0
    i_end = 0
    n = 0
    !write(*,*) str_temp
    !write(*,*) len_trim(str_temp)
    do i = 1, len_trim(str_temp)
      chr = str_temp(i:i)
      if ((i_start == 0) .and. (chr /= split) .and. (chr /= achar(9))) then
        i_start = i
      end if
      if (i_start > 0) then
        if ((chr == split) .or. (chr == achar(9))) then
          i_end = i - 1
        elseif (i == len_trim(str_temp)) then
          i_end = i
        endif
        !write(*,*) i_start,i_end
        if (i_end > 0) then
          n = n + 1
          words_temp(n) = str_temp(i_start:i_end)
          if(i_end-i_start+1 > 256) &
            call raise_error('str_split_one :: '//                             &
              'a word in str has more 256 characters')
          if(n > 80) &
            call raise_error('str_split_one :: '//                             &
              'str has more 80 words')

          ! reset indices
          i_start = 0
          i_end = 0
        end if
      end if
    enddo

    call set_quiet(is_quiet)
    call set_stop_on_error(is_stop_on_error)

    allocate(words(n))
    do i = 1, n
      words(i) = words_temp(i)(1:len_trim(words_temp(i)))
    enddo

  endsubroutine str_split_one

!-------------------------------------------------------------------------------
!> Justify a str is a integer number or not
!-------------------------------------------------------------------------------
  elemental function str_isLog(str) result(is_log)

    charSt,intent(in)        :: str      !< The str to be justified
    logknd :: is_log

    is_log = .false.
    select case (str_to_low(trim(adjustl(str))))
    case ('yes', 'on', 'true', 'no', 'off', 'false')
      is_log = .true.
    end select

  endfunction str_isLog

!-------------------------------------------------------------------------------
!> Justify a str is a integer number or not
!-------------------------------------------------------------------------------
  elemental function str_isInt(str) result(is_real)

    charSt,intent(in)        :: str      !< The str to be justified

    intknd                   :: status   !< The status of I/O
    intknd                   :: int      !< The corresponding integer
    logknd                   :: is_real  !< The corresponding real

    is_real = .false.
    read(str, * ,IOSTAT = status) int
    if(status == 0) is_real = .true.

  endfunction str_isInt

!-------------------------------------------------------------------------------
!> Justify a str is a real number or not
!-------------------------------------------------------------------------------
  elemental function str_isReal(str) result(is_real)

    charSt, intent(in)       :: str         !< The str to be justified

    intknd                   :: status      !< The status of I/O
    logknd                   :: is_real     !< The corresponding real
    realkd                   :: real_out

    is_real = .false.
    read(str, * ,IOSTAT = status) real_out
    if(status == 0) is_real = .true.

  endfunction str_isReal

!-------------------------------------------------------------------------------
!> Match two strings
!-------------------------------------------------------------------------------
 elemental function str_match(str,str_to_match) result(bool)

   charSt, intent(in)         :: str          !< The matched str
   charSt, intent(in)         :: str_to_match !< The str to match
   intknd                     :: n            !< First occurrence of the str
   logknd                     :: bool         !< Match successfully or failed

   n = index(str,str_to_match)
   if (n > 0)then
       bool = .true.    !< Match successfully
   else
       bool = .false.   !< Match failed
   endif

  endfunction str_match

!-------------------------------------------------------------------------------
!> Abstract the name of a file
!-------------------------------------------------------------------------------
 elemental function str_name_of_file(str) result(name)

   charSt, intent(in)     :: str         !< The origal file name
   character(len = len(str)) :: str_temp !< The temp str
   char50                 :: name        !< The name of file
   intknd                 :: n           !< The sum of slash
   intknd                 :: n_end       !< The position of the last slash
   intknd                 :: m           !< The sum of '.'
   intknd                 :: m_end       !< The position of the last .
   intknd                 :: i_point     !< The position of the only .

  str_temp = str_slash_replace(str)
  m = str_cout(str_temp, pot_mark)
  n = str_cout(str_temp, slash)
  if(n == 0)then
    if(m == 0)then
      name = adjustl(trim(str_temp))
    else
      i_point = str_find(str_temp, pot_mark, m)
      name = adjustl(trim(str_temp(1:(i_point - 1))))
    endif
  else
    if(m == 0)then
      n_end = str_find(str_temp, slash, n)
      name = adjustl(trim(str_temp((n_end+1):len(str_temp))))
    else
      n_end = str_find(str_temp, slash, n)
      m_end = str_find(str_temp,pot_mark, m)
      if(m_end > n_end)then
        name = adjustl(trim(str_temp((n_end+1):(m_end-1))))
      else
        name = adjustl(trim(str_temp((n_end+1):len(str_temp))))
      endif
    endif
  endif

 endfunction

!-------------------------------------------------------------------------------
!> Abstract the ext of a file
!-------------------------------------------------------------------------------
  elemental function str_ext_of_file(str) result(ext)

    charSt, intent(in)     :: str      !< The origal file name

    charln                 :: str_temp
    charwd                 :: ext      !< The ext of file
    intknd                 :: n        !< The location of '.'
    intknd                 :: n_end    !< The end of the file name

    str_temp = str_slash_replace(str)
    n = str_cout(str_temp, pot_mark)
    n_end = str_find(str_temp, pot_mark, n)
    if(n == 0)then
      ext = ''
    else
      ext = adjustl(trim(str_temp((n_end):(len(str_temp)))))
    endif

  endfunction

!-------------------------------------------------------------------------------
!> Abstract the path of a file
!-------------------------------------------------------------------------------
  elemental function str_path_of_file(str) result(path)

    charSt, intent(in)      :: str    !< The name of file
    charln                  :: path   !< The name of path

    intknd                    :: n
    intknd                    :: n_end
    character(len = len(str)) :: str_temp

    str_temp = str_slash_replace(str)
    n = str_cout(str_temp, slash)
    if(n > 0) then
      n_end = str_find(str_temp, slash, n)
      path = adjustl(trim(str_temp(1:n_end)))
    else
      path = '.'//slash
    endif

  endfunction str_path_of_file

!-------------------------------------------------------------------------------
!> Join path and name and ext together
!-------------------------------------------------------------------------------
  elemental function str_join_path(path, name, ext) result(str)

    charSt, intent(in) :: path
    charSt, intent(in) :: name
    charSt, intent(in) :: ext

    charln :: str
    intknd :: n     !< The sum of the slash
    intknd :: n_end !< The position of the last slash

    character(len=len(path)) :: path_temp
    path_temp = trim(str_slash_replace(path))
    n = str_cout(path_temp, slash)
    n_end = str_find(path_temp,slash,n)
    if(n_end == len_trim(path_temp))then
      if(trim(ext) /= '')then
        str = trim(adjustl(path_temp))//trim(adjustl(name))//        &
              trim(adjustl(ext))
      else
        str = trim(adjustl(path_temp))//trim(adjustl(name))
      endif
    else
      if(trim(adjustl(ext)) /= '')then
        str = trim(adjustl(path_temp))//slash//trim(adjustl(name))// &
              trim(adjustl(ext))
      else
        str = trim(adjustl(path_temp))//slash//trim(adjustl(name))
      endif
    endif
    str = trim(str)
  endfunction str_join_path

!-------------------------------------------------------------------------------
endmodule string
