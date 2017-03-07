!###############################################################################
!> Module for specifying kind parameters for intrinsic data types
!>
!> @author Zhouyu Liu
!>   @date 03/18/2015
!>
!> @todo
!>   - Test: Added some comments.
!###############################################################################
module math
  use error_warning
  use get_eigen_values, only: rg
  implicit none

  private

# include <kind_parameter.h>

  public :: operator(.abseq.)
  public :: operator(.absge.)
  public :: operator(.absle.)
  public :: operator(.absgt.)
  public :: operator(.abslt.)

  public :: binary_search_str
  public :: binary_search_intknd
  public :: binary_search_real
  public :: binary_search_real1
  ! public :: linear_solver_gauss_eliminate
  public :: bubble_sort
  public :: interp_sqli_1d
  public :: interp_sqli_2d
  public :: gauss_legendre_integral_define
  public :: gauss_hermite_integral_define
  public :: gauss_laguerre_integral_define
  public :: lu_linsol
  public :: ludcmp
  public :: lubksb
  public :: eigen_matrix
  public :: get_roots
  public :: spherical_harmonic
  public :: factorial
  !----------------------------------------------------------------------------
  !> math function used in Atlas
  public :: interp_n
  public :: f_e1
  public :: gami
  public :: heav
  public :: hnab
  public :: legndr
  public :: conjugate_grad
  !----------------------------------------------------------------------------
  public :: output_dim2,gaussian,gauss
  public :: pmgmres_ilu_cr
  public :: lagrange_interp
  public :: qsort
  public :: ax_cr

  !> types and parameters for qsort
  type limits
    intknd :: ileft, iright
  end type limits
  intknd, parameter :: isw = 10

  !-----------------------------------------------------------------------------
  !> Interface for the operator for "approximately equals"
  interface operator(.abseq.)
   module procedure abseq_single
   module procedure abseq_double
  endinterface

  !-----------------------------------------------------------------------------
  !> Interface for tge operator "greater than or equal"
  interface operator(.absge.)
   module procedure absge_single
   module procedure absge_double
  endinterface

  !-----------------------------------------------------------------------------
  !> Interface for the operator for "less or equal"
  interface operator(.absle.)
   module procedure absle_single
   module procedure absle_double
  endinterface

  !-----------------------------------------------------------------------------
  !> Interface for tge operator "greater"
  interface operator(.absgt.)
   module procedure absgt_single
   module procedure absgt_double
  endinterface

  !-----------------------------------------------------------------------------
  !> Interface for the operator for "less"
  interface operator(.abslt.)
   module procedure abslt_single
   module procedure abslt_double
  endinterface

  interface bubble_sort
    module procedure sort_intknd
    module procedure sort_single
    module procedure sort_double
  endinterface

  contains
!-------------------------------------------------------------------------------
!> Defines the operation when comparing two single precision reals
!-------------------------------------------------------------------------------
  elemental function abseq_single(r1,r2) result(bool)

    real(kfp),intent(in) :: r1 !< a single precision real number
    real(kfp),intent(in) :: r2 !< a single precision real number
    logical :: bool

    bool = (abs(r1 - r2) <= epss)

  endfunction abseq_single

!-------------------------------------------------------------------------------
!> Defines the operation when comparing two double precision reals
!-------------------------------------------------------------------------------

  elemental function abseq_double(r1,r2) result(bool)

    real(kdp),intent(in) :: r1 !< a double precision real number
    real(kdp),intent(in) :: r2 !< a double precision real number
    logical :: bool

    bool = (abs(r1 - r2) <= epsd)

  endfunction abseq_double

!-------------------------------------------------------------------------------
!> Defines the operation when comparing two single precision reals
!-------------------------------------------------------------------------------
  elemental function absge_single(r1,r2) result(bool)

    real(kfp),intent(in) :: r1  !< a single precision real number
    real(kfp),intent(in) :: r2  !< a single precision real number
    logical :: bool

    bool = (r1 > r2 - epss)

  endfunction absge_single

!-------------------------------------------------------------------------------
!> Defines the operation when comparing two double precision reals
!-------------------------------------------------------------------------------
  elemental function absge_double(r1,r2) result(bool)

    real(kdp),intent(in) :: r1 !< a double precision real number
    real(kdp),intent(in) :: r2 !< a double precision real number
    logical :: bool

    bool = (r1 > r2 - epsd)

  endfunction absge_double

!-------------------------------------------------------------------------------
!> Defines the operation when comparing two single precision reals
!-------------------------------------------------------------------------------
  elemental function absle_single(r1,r2) result(bool)

    real(kfp),intent(in) :: r1  !< a single precision real number
    real(kfp),intent(in) :: r2  !< a single precision real number
    logical :: bool

    bool = (r1 < r2 - epss)

  endfunction absle_single

!-------------------------------------------------------------------------------
!> Defines the operation when comparing two double precision reals
!-------------------------------------------------------------------------------
  elemental function absle_double(r1,r2) result(bool)

    real(kdp),intent(in) :: r1 !< a double precision real number
    real(kdp),intent(in) :: r2 !< a double precision real number
    logical :: bool

    bool = (r1 < r2 - epsd)

  endfunction absle_double

!-------------------------------------------------------------------------------
!> Defines the operation when comparing two single precision reals
!-------------------------------------------------------------------------------
  elemental function absgt_single(r1,r2) result(bool)

    real(kfp),intent(in) :: r1  !< a single precision real number
    real(kfp),intent(in) :: r2  !< a single precision real number
    logical :: bool

    bool = (r1-r2) > epss

  endfunction absgt_single

!-------------------------------------------------------------------------------
!> Defines the operation when comparing two double precision reals
!-------------------------------------------------------------------------------
  elemental function absgt_double(r1,r2) result(bool)

    real(kdp),intent(in) :: r1 !< a double precision real number
    real(kdp),intent(in) :: r2 !< a double precision real number
    logical :: bool

    bool = (r1-r2) > epsd

  endfunction absgt_double

!-------------------------------------------------------------------------------
!> Defines the operation when comparing two single precision reals
!-------------------------------------------------------------------------------
  elemental function abslt_single(r1,r2) result(bool)

    real(kfp),intent(in) :: r1 !< a single precision real number
    real(kfp),intent(in) :: r2 !< a single precision real number
    logical :: bool

    bool = (r2-r1) > epss

  endfunction abslt_single

!-------------------------------------------------------------------------------
!> Defines the operation when comparing two double precision reals
!-------------------------------------------------------------------------------
  elemental function abslt_double(r1,r2) result(bool)

    real(kdp),intent(in) :: r1 !< a double precision real number
    real(kdp),intent(in) :: r2 !< a double precision real number
    logical :: bool

   bool = (r2-r1) > epsd

  endfunction abslt_double


!-------------------------------------------------------------------------------
!> Defines the operation of binary search of string arry
!-------------------------------------------------------------------------------
  function binary_search_str(str_arry,str) result(i_stt)
    charSt,intent(in) :: str_arry(:)
    charSt,intent(in) :: str

    intknd :: i_stt,i_mid,i_stp
    i_stt = 1
    i_stp = size(str_arry)
    i_mid = i_stp
    if(trim(str) < trim(str_arry(1))) then
      i_stt = -1
    elseif(trim(str) > trim(str_arry(i_stp))) then
      i_stt = i_stp + 1
    elseif(trim(str) == trim(str_arry(i_stp))) then
      i_stt = i_stp
    else
      do
        if(i_stp - i_stt == 1) exit
        i_mid = (i_stp + i_stt)/2
        if(trim(str) >= trim(str_arry(i_mid))) then
          i_stt = i_mid
        else
          i_stp = i_mid
        endif
      enddo
    endif

  endfunction binary_search_str

!-------------------------------------------------------------------------------
!> Defines the operation of binary search of intknd arry
!> Please see the example to the return value and usage
!-------------------------------------------------------------------------------
  function binary_search_intknd(int_arry,int) result(i_stt)
    intknd,intent(in) :: int_arry(:)
    intknd,intent(in) :: int

    intknd :: i_stt,i_mid,i_stp
    i_stt = 1
    i_stp = size(int_arry)
    i_mid = i_stp
    if(int < int_arry(1)) then
      i_stt = -1
    elseif(int > int_arry(i_stp)) then
      i_stt = i_stp + 1
    elseif(int == int_arry(i_stp)) then
      i_stt = i_stp
    else
      do
        if(i_stp - i_stt == 1) exit
        i_mid = (i_stp + i_stt)/2
        if(int >= int_arry(i_mid)) then
          i_stt = i_mid
        else
          i_stp = i_mid
        endif
      enddo
    endif

  endfunction binary_search_intknd


!-------------------------------------------------------------------------------
!> Defines the operation of binary search of real arry
!-------------------------------------------------------------------------------
  function binary_search_real(real_arry,real) result(i_stt)
    realkd,intent(in) :: real_arry(:)
    realkd,intent(in) :: real

    intknd :: i_stt,i_mid,i_stp
    i_stt = 1
    i_stp = size(real_arry)
    i_mid = i_stp
    if(real < real_arry(1)) then
      i_stt = -1
    elseif(real > real_arry(i_stp)) then
      i_stt = i_stp + 1
    elseif(real == real_arry(i_stp)) then
      i_stt = i_stp
    else
      do
        if(i_stp - i_stt == 1) exit
        i_mid = (i_stp + i_stt)/2
        if(real >= real_arry(i_mid)) then
          i_stt = i_mid
        else
          i_stp = i_mid
        endif
      enddo
    endif

  endfunction binary_search_real

  !-------------------------------------------------------------------------------
  !> Defines the operation of binary search of real array. array should be
  !! decreasing.
  !-------------------------------------------------------------------------------
  function binary_search_real1(real_arry,real) result(i_stt)
    realkd,intent(in) :: real_arry(:)
    realkd,intent(in) :: real

    intknd :: i_stt,i_mid,i_stp
    i_stt = 1
    i_stp = size(real_arry)
    i_mid = i_stp
    if(real > real_arry(1)) then
      i_stt = -1
    elseif(real < real_arry(i_stp)) then
      i_stt = i_stp + 1
    elseif(real == real_arry(i_stp)) then
      i_stt = i_stp
    else
      do
        if(i_stp - i_stt == 1) exit
        i_mid = (i_stp + i_stt)/2
        if(real <= real_arry(i_mid)) then
          i_stt = i_mid
        else
          i_stp = i_mid
        endif
      enddo
    endif

  endfunction binary_search_real1
  
!-------------------------------------------------------------------------------
!> get spherical harmonic function
!-------------------------------------------------------------------------------
  function spherical_harmonic(n,theta,phi) result(sph_har)
    intknd,intent(in) :: n           !< order of spherical harmonic function
    realkd,intent(in) :: theta, phi  !< azimuthal and polar angle
              
    realkd,allocatable :: aso_p(:,:) !< associated Legendre polynomials
    realkd,allocatable :: sph_har(:) !< the 2n+1 spherical harmonic polynomials

    intknd :: p, q, i,j, m

    !calculate associated legendre polynomials
    allocate(aso_p(0:n,0:n))
    allocate(sph_har((-1)*n:n))

    !do p=0,n
    !  if (n==0) then
    !    aso_p(0,0)=1
    !  else if (p==0) then
    !    aso_p(0,0) =1
    !    aso_p(1,0) =cos(theta)
    !  else if (p==n) then
    !    aso_p(p,p)=                                                            &
    !      (aso_p(p,p-1)-(2*p-1)*aso_p(p-1,p-1)/cos(theta))*sin(theta)          &
    !      /(cos(theta)-1/cos(theta))                                           
    !  else                                                                     
    !    aso_p(p,p)=                                                            &
    !      (aso_p(p,p-1)-(2*p-1)*aso_p(p-1,p-1)/cos(theta))*sin(theta)          &
    !      /(cos(theta)-1/cos(theta))                                           
    !    aso_p(p+1,p)=                                                          &
    !      (2*aso_p(p+1,p-1)-2*p*aso_p(p,p-1)/cos(theta))*sin(theta)            &
    !      /(cos(theta)-1/cos(theta))                                           
    !  endif                                                                    
    !  do q=p,n-2                                                               
    !    aso_p(q+2,p)=                                                          &
    !      ((2*q+3)*cos(theta)*aso_p(q+1,p)-(p+q+1)*aso_p(q,p))                 &
    !      /(q-p+2)
    !  enddo
    !enddo
    
    if(n == 0) then
      aso_p(0,0)=1
    elseif(n == 1) then
      aso_p(0,0)=1
      aso_p(1,0)=cos(theta)
      i = 1
      j = 1
      aso_p(i,j) = 1.0_krp/abs(sin(theta))*((i-(j-1))*cos(theta)*aso_p(i,j-1)- &
        (i+(j-1))*aso_p(i-1,j-1))
    else
      !> get P(0,0),P(1,0),P(2,0)......P(n,0) from Legendre polynomials
      aso_p(0,0)=1
      aso_p(1,0)=cos(theta)
      do i = 2,n
        aso_p(i,0) = 1.0_krp/i*((2*(i-1)+1)*cos(theta)*aso_p(i-1,0)-(i-1)*     &
          aso_p(i-2,0))
      enddo
      !> get P(1,1),P(2,1)...P(n,1) from associated Legendre polynomials
      do j = 1,n
        do i = j,n
          aso_p(i,j) = 1.0_krp/abs(sin(theta))*((i-(j-1))*cos(theta)*aso_p(i,j-1)- &
            (i+(j-1))*aso_p(i-1,j-1))
        enddo
      enddo
    endif
  
    !calculate spherical harmonic function
    m=-n
    do i=1,2*n+1
      if (i<n+1) then
        sph_har(m)=aso_p(n,(-1)*m)*sin((-1)*m*phi)
      else
        sph_har(m)=aso_p(n,m)*cos(m*phi)
      endif
      m=m+1
    enddo

  endfunction spherical_harmonic

!-------------------------------------------------------------------------------
!> Defines the gauss linear solver
!-------------------------------------------------------------------------------

  ! subroutine linear_solver_gauss_eliminate(ndir,a,b,x)

  !   ! assumed shape array, and the subdescriptor starts with intknd 1!
  !   intknd, intent(in)    :: ndir
  !   realkd, intent(inout) :: a(ndir,ndir)
  !   realkd, intent(inout) :: b(ndir)
  !   realkd, intent(out)   :: x(ndir)


  !   intknd :: l
  !   realkd :: d,t
  !   intknd :: i,j,k,is,js(ndir)

  !     !Solve the linear system
  !     l=1
  !     do k=1,ndir-1
  !       d=0._krp
  !       do i=k,ndir
  !         do j=k,ndir
  !           if(abs(A(i,j)) > d) then
  !             d=abs(A(i,j))
  !             JS(k)=j
  !             is=i
  !           endif
  !         enddo
  !       enddo
  !       if(d+1._krp == 1._krp) then
  !         l=0
  !       else
  !         if(JS(k) /= k) then
  !           do i=1,ndir
  !             t=A(i,k)
  !             A(i,k)=A(i,JS(k))
  !             A(i,JS(k))=t
  !           enddo
  !         endif
  !         if(is /= k) then
  !           do j=k,ndir
  !             t=A(k,j)
  !             A(k,j)=A(is,j)
  !             A(is,j)=t
  !           enddo
  !           t=B(k)
  !           B(k)=B(is)
  !           B(is)=t
  !         endif
  !       endif
  !       if(l == 0) then
  !         call raise_fatal('Failed to solve the linear equation.')
  !       endif
  !       do j=k+1,ndir
  !         A(k,j)=A(k,j)/A(k,k)
  !       enddo
  !       B(k)=B(k)/A(k,k)
  !       do i=k+1,ndir
  !         do j=k+1,ndir
  !           A(i,j)=A(i,j)-A(i,k)*A(k,j)
  !         enddo
  !         B(i)=B(i)-A(i,k)*B(k)
  !       enddo
  !     enddo
  !     if(abs(A(ndir,ndir)+1._krp) == 1._krp) then
  !       call raise_fatal('Failed to solve the linear equation.')
  !     endif
  !     X(ndir)=B(ndir)/A(ndir,ndir)
  !     do i=ndir-1,1,-1
  !       t=0._krp
  !       do j=i+1,ndir
  !         t=t+A(i,j)*X(j)
  !       enddo
  !       X(i)=B(i)-t
  !     enddo
  !     JS(ndir)=ndir
  !     do k=ndir,1,-1
  !       if(JS(k) /= k) then
  !         t=X(k)
  !         X(k)=X(JS(k))
  !         X(JS(k))=t
  !       endif
  !     enddo
  ! endsubroutine linear_solver_gauss_eliminate

  !-------------------------------------------------------------------------------
  !> solve linear system by LU decomposition method. A will be changed.
  !-------------------------------------------------------------------------------
  subroutine lu_linsol(n, A, b, x)
    charSt,parameter :: myName = 'math => lu_linsol - '
    intknd, intent(in) :: n
    realkd, intent(inout) :: A(n, n)
    realkd, intent(in) :: b(n)
    realkd, intent(inout) :: x(n)

    intknd :: indx(n)
    intknd :: d, code

    x = b
    call ludcmp(n, A, indx, d, code)
    if (code == 1) then
      call raise_error(myname // 'lu_linsol failed')
    else
      call lubksb(n, A, indx, x)
    end if

  end subroutine lu_linsol

  !-------------------------------------------------------------------------------
  !> Given an N x N matrix A, this routine replaces it by the LU
  !> decomposition of a rowwise permutation of itself. A and N
  !> are input. INDX is an output vector which records the row
  !> permutation effected by the partial pivoting; D is output
  !> as -1 or 1, depending on whether the number of row inter-
  !> changes was even or odd, respectively. This routine is used
  !> in combination with LUBKSB to solve linear equations or to
  !> invert a matrix. Return code is 1, if matrix is singular.
  !-------------------------------------------------------------------------------
  subroutine ludcmp(n, A, indx, d, code)
    intknd, parameter :: nmax = 100
    realkd, parameter :: tiny = 1.5e-16_krp

    intknd, intent(in) :: n
    realkd, intent(inout) :: A(n, n)
    intknd, intent(out) :: d, code
    intknd, intent(out) :: indx(n)

    realkd  :: amax, dum, summ, vv(nmax)
    intknd :: i, j, k, imax

    d=1; code=0

    do i=1,n
      amax=0.0_krp
      do j=1,n
        if (abs(A(i,j)).gt.amax) amax=abs(A(i,j))
      end do
      if(amax.lt.tiny) then
        code = 1
        return
      end if
      vv(i) = 1.0_krp / amax
    end do

    do j=1,n
      do i=1,j-1
        summ = A(i,j)
        do k=1,i-1
          summ = summ - A(i,k)*A(k,j)
        end do
        A(i,j) = summ
      end do
      amax = 0.0_krp
      do i=j,n
        summ = A(i,j)
        do k=1,j-1
          summ = summ - A(i,k)*A(k,j)
        end do
        A(i,j) = summ
        dum = vv(i)*abs(summ)
        if(dum.ge.amax) then
          imax = i
          amax = dum
        end if
      end do

      if(j.ne.imax) then
        do k=1,n
          dum = A(imax,k)
          A(imax,k) = A(j,k)
          A(j,k) = dum
        end do
        d = -d
        vv(imax) = vv(j)
      end if

      indx(j) = imax
      if(abs(A(j,j)) < tiny) A(j,j) = tiny

      if(j.ne.n) then
        dum = 1.0_krp / A(j,j)
        do i=j+1,n
          A(i,j) = A(i,j)*dum
        end do
      end if
    end do

  end subroutine ludcmp

  !-------------------------------------------------------------------------------
  !> Solves the set of N linear equations A . X = B.  Here A is
  !> input, not as the matrix A but rather as its LU decomposition,
  !> determined by the routine LUDCMP. INDX is input as the permuta-
  !> tion vector returned by LUDCMP. B is input as the right-hand
  !> side vector B, and returns with the solution vector X. A, N and
  !> INDX are not modified by this routine and can be used for suc-
  !> cessive calls with different right-hand sides. This routine is
  !> also efficient for plain matrix inversion.
  !-------------------------------------------------------------------------------
  subroutine lubksb(n, A, indx, b)
    intknd, intent(in) :: n
    realkd, intent(in) :: A(n,n)
    intknd, intent(in) :: indx(n)
    realkd, intent(inout) :: b(n)

    intknd :: ii, i, j, ll
    realkd :: summ

    ii = 0

    do i=1,n
      ll = indx(i)
      summ = b(ll)
      b(ll) = b(i)
      if(ii.ne.0) then
        do j=ii,i-1
          summ = summ - A(i,j)*b(j)
        end do
      else if(summ.ne.0.0_krp) then
        ii = i
      end if
      b(i) = summ
    end do

    do i=n,1,-1
      summ = b(i)
      if(i < n) then
        do j=i+1,n
          summ = summ - A(i,j)*b(j)
        end do
      end if
      b(i) = summ / A(i,i)
    end do

  end subroutine lubksb


!-------------------------------------------------------------------------------
!> Defines the operation of sort method of intknd arry
!-------------------------------------------------------------------------------
  subroutine sort_intknd(n,array)
    implicit none
    intknd,intent(in) :: n
    intknd,intent(inout),dimension(n) :: array

    ! Local
    intknd :: i,j,temp
    do i = n-1,1,-1
      do j = 1,i
        if(array(j) > array(j+1)) then
          temp = array(j)
          array(j) = array(j+1)
          array(j+1) = temp
        endif
      enddo
    enddo

  endsubroutine

!-------------------------------------------------------------------------------
!> Defines the operation of sort method of single precision arry
!-------------------------------------------------------------------------------
  subroutine sort_single(n,array)
    implicit none
    intknd,intent(in) :: n
    real(kfp),intent(inout),dimension(n) :: array

    ! Local
    intknd :: i,j
    real(kfp) :: temp
    do i = n-1,1,-1
      do j = 1,i
        if(array(j) > array(j+1)) then
          temp = array(j)
          array(j) = array(j+1)
          array(j+1) = temp
        endif
      enddo
    enddo
  endsubroutine

!-------------------------------------------------------------------------------
!> Defines the operation of sort method of double precision arry
!-------------------------------------------------------------------------------
  subroutine sort_double(n,array)
    implicit none
    intknd,intent(in) :: n
    real(kdp),intent(inout),dimension(n) :: array

    ! Local
    intknd :: i,j
    real(kdp) :: temp
    do i = n-1,1,-1
      do j = 1,i
        if(array(j) > array(j+1)) then
          temp = array(j)
          array(j) = array(j+1)
          array(j+1) = temp
        endif
      enddo
    enddo
  endsubroutine

  !-----------------------------------------------------------------------------
  !> interpolate one dimension array in sqrt-linear relationship
  !-----------------------------------------------------------------------------
  function interp_sqli_1d(x0, n, x, y) result(y0)
    realkd, intent(in) :: x0
    intknd, intent(in) :: n
    realkd, intent(in) :: x(n)
    realkd, intent(in) :: y(n)
    realkd :: y0

    intknd :: i0
    intknd :: i1
    realkd :: r

    i0 = binary_search_real(x, x0)
    if (i0 == -1) i0 = 1
    if (i0 >= n) i0 = n - 1
    i1 = i0 + 1
    r = (sqrt(x(i1)) - sqrt(x0)) / (sqrt(x(i1)) - sqrt(x(i0)))
    y0 = r * y(i0) + (1.0_krp - r) * y(i1)

  end function interp_sqli_1d

  !-----------------------------------------------------------------------------
  !> interpolate two dimension array in sqrt-linear relationship
  !-----------------------------------------------------------------------------
  function interp_sqli_2d(x0, y0, nx, ny, x, y, z) result(z0)
    realkd, intent(in) :: x0
    realkd, intent(in) :: y0
    intknd, intent(in) :: nx
    intknd, intent(in) :: ny
    realkd, intent(in) :: x(nx)
    realkd, intent(in) :: y(ny)
    realkd, intent(in) :: z(nx, ny)
    realkd :: z0

    intknd :: ix0
    intknd :: ix1
    intknd :: iy0
    intknd :: iy1
    realkd :: r
    realkd :: zz(2)

    ix0 = binary_search_real(x, x0)
    if (ix0 == -1) ix0 = 1
    if (ix0 >= nx) ix0 = ny - 1
    ix1 = ix0 + 1
    iy0 = binary_search_real(y, y0)
    if (iy0 == -1) iy0 = 1
    if (iy0 >= ny) iy0 = ny - 1
    iy1 = iy0 + 1
    r = (sqrt(x(ix1)) - sqrt(x0)) / (sqrt(x(ix1)) - sqrt(x(ix0)))
    zz(1) = r * z(ix0, iy0) + (1.0_krp - r) * z(ix1, iy0)
    zz(2) = r * z(ix0, iy1) + (1.0_krp - r) * z(ix1, iy1)
    r = (sqrt(y(iy1)) - sqrt(y0)) / (sqrt(y(iy1)) - sqrt(y(iy0)))
    z0 = r * zz(1) + (1.0_krp - r) * zz(2)

  end function interp_sqli_2d

  !-----------------------------------------------------------------------------
  !> gauss legendre integration.
  !>
  !> @param n - in, # of gauss points
  !> @param x - inout, absciassaes
  !> @param wtt - inout, weights
  !> @param a,b - in, lower and upper integral limits
  !>
  !> Note:
  !>      1) do not use n > 40 for that the error is large.
  !-----------------------------------------------------------------------------
  subroutine gauss_legendre_integral_define(n, x, wtt, a, b)
    charSt,parameter :: myName = &
      'math => gauss_legendre_integral_define - '
    intknd, intent(in) :: n
    realkd, intent(inout) :: x(n)
    realkd, intent(inout) :: wtt(n)
    realkd, intent(in) :: a
    realkd, intent(in) :: b

    select case(n)
    case(1)
      x=(/0.0_krp/)
      wtt=(/2.0_krp/)
    case(2)
      x=(/-5.773502691896258E-1_krp,5.773502691896258E-1_krp/)
      wtt=(/1.0_krp,1.0_krp/)
    case(3)
      x=(/0.0_krp,7.745966692414834E-1_krp,-7.745966692414834E-1_krp/)
      wtt=(/8.888888888888889E-1_krp,5.555555555555556E-1_krp,5.555555555555556E-1_krp/)
    case(4)
      x=(/-8.611363115940526E-1_krp,-3.399810435848563E-1_krp, 3.399810435848563E-1_krp,&
          8.611363115940526E-1_krp/)
      wtt=(/3.478548451374539E-1_krp,6.521451548625461E-1_krp,6.521451548625461E-1_krp,&
          3.478548451374539E-1_krp/)
    case(5)
      x=(/-9.061798459386640E-1_krp,-5.384693101056831E-1_krp,                  0.0_krp,&
          5.384693101056831E-1_krp, 9.061798459386640E-1_krp/)
      wtt=(/2.369268850561891E-1_krp,4.786286704993665E-1_krp,5.688888888888889E-1_krp,&
            4.786286704993665E-1_krp,2.369268850561891E-1_krp/)
    case(6)
      x=(/-9.324695142031520E-1_krp,-6.612093864662645E-1_krp,-2.386191860831969E-1_krp,&
          2.386191860831969E-1_krp, 6.612093864662645E-1_krp, 9.324695142031520E-1_krp/)
      wtt=(/1.713244923791703E-1_krp,3.607615730481386E-1_krp,4.679139345726910E-1_krp,&
            4.679139345726910E-1_krp,3.607615730481386E-1_krp,1.713244923791703E-1_krp/)
    case(7)
      x=(/-9.491079123427585E-1_krp,-7.415311855993944E-1_krp,-4.058451513773972E-1_krp,0.0_krp,&
        4.058451513773972E-1_krp, 7.415311855993944E-1_krp, 9.491079123427585E-1_krp/)
      wtt=(/1.294849661688697E-1_krp,2.797053914892767E-1_krp, 3.818300505051189E-1_krp,&
        & 4.179591836734694E-1_krp, 3.818300505051189E-1_krp, 2.797053914892767E-1_krp,&
        & 1.294849661688697E-1_krp/)
    case(8)
      x=(/-9.602898564975362E-1_krp,-7.966664774136267E-1_krp,-5.255324099163290E-1_krp,&
          -1.834346424956498E-1_krp,1.834346424956498E-1_krp,5.255324099163290E-1_krp,&
           7.966664774136267E-1_krp,9.602898564975362E-1_krp/)
      wtt=(/1.012285362903763E-1_krp,2.223810344533745E-1_krp,3.137066458778873E-1_krp,&
            3.626837833783620E-1_krp,3.626837833783620E-1_krp,3.137066458778873E-1_krp,&
            2.223810344533745E-1_krp,1.012285362903763E-1_krp/)
    case(9)
      x=(/-9.681602395076261E-1_krp,-8.360311073266358E-1_krp,-6.133714327005904E-1_krp,&
          -3.242534234038089E-1_krp,0.0_krp, 3.242534234038089E-1_krp,&
           6.133714327005904E-1_krp, 8.360311073266358E-1_krp, 9.681602395076261E-1_krp/)
      wtt=(/8.127438836157441E-2_krp,1.806481606948574E-1_krp,2.606106964029355E-1_krp,&
            3.123470770400028E-1_krp,3.302393550012598E-1_krp,3.123470770400028E-1_krp,&
            2.606106964029355E-1_krp,1.806481606948574E-1_krp,8.127438836157441E-2_krp/)
    case(10)
      x=(/-9.739065285171717E-1_krp,-8.650633666889845E-1_krp,-6.794095682990244E-1_krp,&
          -4.333953941292472E-1_krp,-1.488743389816312E-1_krp, 1.488743389816312E-1_krp,&
           4.333953941292472E-1_krp, 6.794095682990244E-1_krp, 8.650633666889845E-1_krp,&
           9.739065285171717E-1_krp/)
      wtt=(/6.667134430868814E-2_krp,1.494513491505806E-1_krp,2.190863625159820E-1_krp,&
            2.692667193099964E-1_krp,2.955242247147529E-1_krp,2.955242247147529E-1_krp,&
            2.692667193099964E-1_krp,2.190863625159820E-1_krp,1.494513491505806E-1_krp,&
            6.667134430868814E-2_krp/)
    case(11)
      x=(/-9.782286581460570E-1_krp,-8.870625997680953E-1_krp,-7.301520055740493E-1_krp,&
          -5.190961292068118E-1_krp,-2.695431559523450E-1_krp,0.0_krp,&
           2.695431559523450E-1_krp, 5.190961292068118E-1_krp, 7.301520055740493E-1_krp,&
           8.870625997680953E-1_krp, 9.782286581460570E-1_krp/)
      wtt=(/5.566856711617367E-2_krp,1.255803694649046E-1_krp,1.862902109277343E-1_krp,&
            2.331937645919905E-1_krp,2.628045445102467E-1_krp,2.729250867779006E-1_krp,&
            2.628045445102467E-1_krp,2.331937645919905E-1_krp,1.862902109277343E-1_krp,&
            1.255803694649046E-1_krp,5.566856711617367E-2_krp/)
    case(12)
      x=(/-9.815606342467193E-1_krp,-9.041172563704749E-1_krp,-7.699026741943047E-1_krp,&
          -5.873179542866174E-1_krp,-3.678314989981802E-1_krp,-1.252334085114689E-1_krp,&
           1.252334085114689E-1_krp, 3.678314989981802E-1_krp, 5.873179542866174E-1_krp,&
           7.699026741943047E-1_krp, 9.041172563704749E-1_krp, 9.815606342467193E-1_krp/)
      wtt=(/4.717533638651183E-2_krp,1.069393259953184E-1_krp,1.600783285433462E-1_krp,&
            2.031674267230659E-1_krp,2.334925365383548E-1_krp,2.491470458134028E-1_krp,&
            2.491470458134028E-1_krp,2.334925365383548E-1_krp,2.031674267230659E-1_krp,&
            1.600783285433462E-1_krp,1.069393259953184E-1_krp,4.717533638651183E-2_krp/)
    case(13)
      x=(/-9.841830547185881E-1_krp,-9.175983992229780E-1_krp,-8.015780907333099E-1_krp,&
          -6.423493394403402E-1_krp,-4.484927510364469E-1_krp,-2.304583159551348E-1_krp,&
          0.0_krp, 2.304583159551348E-1_krp, 4.484927510364469E-1_krp,&
           6.423493394403402E-1_krp, 8.015780907333099E-1_krp, 9.175983992229780E-1_krp,&
           9.841830547185881E-1_krp/)
      wtt=(/4.048400476531588E-2_krp,9.212149983772845E-2_krp,1.388735102197872E-1_krp,&
            1.781459807619457E-1_krp,2.078160475368885E-1_krp,2.262831802628972E-1_krp,&
            2.325515532308739E-1_krp,2.262831802628972E-1_krp,2.078160475368885E-1_krp,&
            1.781459807619457E-1_krp,1.388735102197872E-1_krp,9.212149983772845E-2_krp,&
            4.048400476531588E-2_krp/)
    case(14)
      x=(/-9.862838086968123E-1_krp,-9.284348836635735E-1_krp,-8.272013150697650E-1_krp,&
          -6.872929048116855E-1_krp,-5.152486363581541E-1_krp,-3.191123689278898E-1_krp,&
          -1.080549487073437E-1_krp, 1.080549487073437E-1_krp, 3.191123689278898E-1_krp,&
           5.152486363581541E-1_krp, 6.872929048116855E-1_krp, 8.272013150697650E-1_krp,&
           9.284348836635735E-1_krp, 9.862838086968123E-1_krp/)
      wtt=(/3.511946033175186E-2_krp,8.015808715976021E-2_krp,1.215185706879032E-1_krp,&
            1.572031671581935E-1_krp,1.855383974779378E-1_krp,2.051984637212956E-1_krp,&
            2.152638534631578E-1_krp,2.152638534631578E-1_krp,2.051984637212956E-1_krp,&
            1.855383974779378E-1_krp,1.572031671581935E-1_krp,1.215185706879032E-1_krp,&
            8.015808715976021E-2_krp,3.511946033175186E-2_krp/)
    case(15)
      x=(/-9.879925180204854E-1_krp,-9.372733924007059E-1_krp,-8.482065834104272E-1_krp,&
          -7.244177313601700E-1_krp,-5.709721726085388E-1_krp,-3.941513470775634E-1_krp,&
          -2.011940939974345E-1_krp,                  0.0_krp, 2.011940939974345E-1_krp,&
           3.941513470775634E-1_krp, 5.709721726085388E-1_krp, 7.244177313601700E-1_krp,&
           8.482065834104272E-1_krp, 9.372733924007059E-1_krp, 9.879925180204854E-1_krp/)
      wtt=(/3.075324199611727E-2_krp,7.036604748810812E-2_krp,1.071592204671719E-1_krp,&
            1.395706779261543E-1_krp,1.662692058169939E-1_krp,1.861610000155622E-1_krp,&
            1.984314853271116E-1_krp,2.025782419255613E-1_krp,1.984314853271116E-1_krp,&
            1.861610000155622E-1_krp,1.662692058169939E-1_krp,1.395706779261543E-1_krp,&
            1.071592204671719E-1_krp,7.036604748810812E-2_krp,3.075324199611727E-2_krp/)
    case(16)
      x=(/-9.894009349916499E-1_krp,-9.445750230732326E-1_krp,-8.656312023878317E-1_krp,&
          -7.554044083550030E-1_krp,-6.178762444026437E-1_krp,-4.580167776572274E-1_krp,&
          -2.816035507792589E-1_krp,-9.501250983763744E-2_krp, 9.501250983763744E-2_krp,&
           2.816035507792589E-1_krp, 4.580167776572274E-1_krp, 6.178762444026437E-1_krp,&
           7.554044083550030E-1_krp, 8.656312023878317E-1_krp, 9.445750230732326E-1_krp,&
           9.894009349916499E-1_krp/)
      wtt=(/2.715245941175409E-2_krp,6.225352393864789E-2_krp,9.515851168249278E-2_krp,&
            1.246289712555339E-1_krp,1.495959888165767E-1_krp,1.691565193950025E-1_krp,&
            1.826034150449236E-1_krp,1.894506104550685E-1_krp,1.894506104550685E-1_krp,&
            1.826034150449236E-1_krp,1.691565193950025E-1_krp,1.495959888165767E-1_krp,&
            1.246289712555339E-1_krp,9.515851168249278E-2_krp,6.225352393864789E-2_krp,&
            2.715245941175409E-2_krp/)
    case(17)
      x=(/-9.905754753144173E-1_krp,-9.506755217687678E-1_krp,-8.802391537269859E-1_krp,&
          -7.815140038968014E-1_krp,-6.576711592166908E-1_krp,-5.126905370864770E-1_krp,&
          -3.512317634538763E-1_krp,-1.784841814958479E-1_krp,                  0.0_krp,&
           1.784841814958479E-1_krp, 3.512317634538763E-1_krp, 5.126905370864770E-1_krp,&
           6.576711592166908E-1_krp, 7.815140038968014E-1_krp, 8.802391537269859E-1_krp,&
           9.506755217687678E-1_krp, 9.905754753144173E-1_krp/)
      wtt=(/2.414830286854793E-2_krp,5.545952937398720E-2_krp,8.503614831717918E-2_krp,&
            1.118838471934040E-1_krp,1.351363684685255E-1_krp,1.540457610768103E-1_krp,&
            1.680041021564500E-1_krp,1.765627053669926E-1_krp,1.794464703562065E-1_krp,&
            1.765627053669926E-1_krp,1.680041021564500E-1_krp,1.540457610768103E-1_krp,&
            1.351363684685255E-1_krp,1.118838471934040E-1_krp,8.503614831717918E-2_krp,&
            5.545952937398720E-2_krp,2.414830286854793E-2_krp/)
    case(18)
      x=(/-9.915651684209309E-1_krp,-9.558239495713978E-1_krp,-8.926024664975557E-1_krp,&
          -8.037049589725231E-1_krp,-6.916870430603532E-1_krp,-5.597708310739475E-1_krp,&
          -4.117511614628426E-1_krp,-2.518862256915055E-1_krp,-8.477501304173530E-2_krp,&
           8.477501304173530E-2_krp, 2.518862256915055E-1_krp, 4.117511614628426E-1_krp,&
           5.597708310739475E-1_krp, 6.916870430603532E-1_krp, 8.037049589725231E-1_krp,&
           8.926024664975557E-1_krp, 9.558239495713978E-1_krp, 9.915651684209309E-1_krp/)
      wtt=(/2.161601352648331E-2_krp,4.971454889496980E-2_krp,7.642573025488906E-2_krp,&
            1.009420441062872E-1_krp,1.225552067114785E-1_krp,1.406429146706507E-1_krp,&
            1.546846751262652E-1_krp,1.642764837458327E-1_krp,1.691423829631436E-1_krp,&
            1.691423829631436E-1_krp,1.642764837458327E-1_krp,1.546846751262652E-1_krp,&
            1.406429146706507E-1_krp,1.225552067114785E-1_krp,1.009420441062872E-1_krp,&
            7.642573025488906E-2_krp,4.971454889496980E-2_krp,2.161601352648331E-2_krp/)
    case(19)
      x=(/-9.924068438435844E-1_krp,-9.602081521348300E-1_krp,-9.031559036148179E-1_krp,&
          -8.227146565371428E-1_krp,-7.209661773352294E-1_krp,-6.005453046616810E-1_krp,&
          -4.645707413759609E-1_krp,-3.165640999636298E-1_krp,-1.603586456402254E-1_krp,&
                            0.0_krp, 1.603586456402254E-1_krp, 3.165640999636298E-1_krp,&
           4.645707413759609E-1_krp, 6.005453046616810E-1_krp, 7.209661773352294E-1_krp,&
           8.227146565371428E-1_krp, 9.031559036148179E-1_krp, 9.602081521348300E-1_krp,&
           9.924068438435844E-1_krp/)
      wtt=(/1.946178822972648E-2_krp,4.481422676569960E-2_krp,6.904454273764123E-2_krp,&
            9.149002162245000E-2_krp,1.115666455473340E-1_krp,1.287539625393362E-1_krp,&
            1.426067021736066E-1_krp,1.527660420658597E-1_krp,1.589688433939543E-1_krp,&
            1.610544498487837E-1_krp,1.589688433939543E-1_krp,1.527660420658597E-1_krp,&
            1.426067021736066E-1_krp,1.287539625393362E-1_krp,1.115666455473340E-1_krp,&
            9.149002162245000E-2_krp,6.904454273764123E-2_krp,4.481422676569960E-2_krp,&
            1.946178822972648E-2_krp/)
    case(20)
      x=(/-9.931285991850949E-1_krp,-9.639719272779138E-1_krp,-9.122344282513259E-1_krp,&
          -8.391169718222188E-1_krp,-7.463319064601508E-1_krp,-6.360536807265150E-1_krp,&
          -5.108670019508271E-1_krp,-3.737060887154196E-1_krp,-2.277858511416451E-1_krp,&
          -7.652652113349733E-2_krp, 7.652652113349733E-2_krp, 2.277858511416451E-1_krp,&
           3.737060887154196E-1_krp, 5.108670019508271E-1_krp, 6.360536807265150E-1_krp,&
           7.463319064601508E-1_krp, 8.391169718222188E-1_krp, 9.122344282513259E-1_krp,&
           9.639719272779138E-1_krp, 9.931285991850949E-1_krp/)
      wtt=(/1.761400713915212E-2_krp,4.060142980038694E-2_krp,6.267204833410906E-2_krp,&
            8.327674157670475E-2_krp,1.019301198172404E-1_krp,1.181945319615184E-1_krp,&
            1.316886384491766E-1_krp,1.420961093183821E-1_krp,1.491729864726037E-1_krp,&
            1.527533871307259E-1_krp,1.527533871307259E-1_krp,1.491729864726037E-1_krp,&
            1.420961093183821E-1_krp,1.316886384491766E-1_krp,1.181945319615184E-1_krp,&
            1.019301198172404E-1_krp,8.327674157670475E-2_krp,6.267204833410906E-2_krp,&
            4.060142980038694E-2_krp,1.761400713915212E-2_krp/)
    case(21)
      x=(/-9.937521706203895E-1_krp,-9.672268385663063E-1_krp,-9.200993341504008E-1_krp,&
          -8.533633645833173E-1_krp,-7.684399634756779E-1_krp,-6.671388041974123E-1_krp,&
          -5.516188358872198E-1_krp,-4.243421202074388E-1_krp,-2.880213168024011E-1_krp,&
          -1.455618541608951E-1_krp,                  0.0_krp, 1.455618541608951E-1_krp,&
           2.880213168024011E-1_krp, 4.243421202074388E-1_krp, 5.516188358872198E-1_krp,&
           6.671388041974123E-1_krp, 7.684399634756779E-1_krp, 8.533633645833173E-1_krp,&
           9.200993341504008E-1_krp, 9.672268385663063E-1_krp, 9.937521706203895E-1_krp/)
      wtt=(/1.601722825777433E-2_krp,3.695378977085249E-2_krp,5.713442542685721E-2_krp,&
            7.610011362837930E-2_krp,9.344442345603386E-2_krp,1.087972991671484E-1_krp,&
            1.218314160537285E-1_krp,1.322689386333375E-1_krp,1.398873947910732E-1_krp,&
            1.445244039899701E-1_krp,1.460811336496904E-1_krp,1.445244039899701E-1_krp,&
            1.398873947910732E-1_krp,1.322689386333375E-1_krp,1.218314160537285E-1_krp,&
            1.087972991671484E-1_krp,9.344442345603386E-2_krp,7.610011362837930E-2_krp,&
            5.713442542685721E-2_krp,3.695378977085249E-2_krp,1.601722825777433E-2_krp/)
    case(22)
      x=(/-9.942945854823993E-1_krp,-9.700604978354287E-1_krp,-9.269567721871740E-1_krp,&
          -8.658125777203001E-1_krp,-7.878168059792082E-1_krp,-6.944872631866828E-1_krp,&
          -5.876404035069116E-1_krp,-4.693558379867570E-1_krp,-3.419358208920842E-1_krp,&
          -2.078604266882213E-1_krp,-6.973927331972222E-2_krp, 6.973927331972222E-2_krp,&
           2.078604266882213E-1_krp, 3.419358208920842E-1_krp, 4.693558379867570E-1_krp,&
           5.876404035069116E-1_krp, 6.944872631866828E-1_krp, 7.878168059792082E-1_krp,&
           8.658125777203001E-1_krp, 9.269567721871740E-1_krp, 9.700604978354287E-1_krp,&
           9.942945854823993E-1_krp/)
      wtt=(/1.462799529827220E-2_krp,3.377490158481415E-2_krp,5.229333515268329E-2_krp,&
            6.979646842452049E-2_krp,8.594160621706773E-2_krp,1.004141444428810E-1_krp,&
            1.129322960805392E-1_krp,1.232523768105124E-1_krp,1.311735047870624E-1_krp,&
            1.365414983460152E-1_krp,1.392518728556320E-1_krp,1.392518728556320E-1_krp,&
            1.365414983460152E-1_krp,1.311735047870624E-1_krp,1.232523768105124E-1_krp,&
            1.129322960805392E-1_krp,1.004141444428810E-1_krp,8.594160621706773E-2_krp,&
            6.979646842452049E-2_krp,5.229333515268329E-2_krp,3.377490158481415E-2_krp,&
            1.462799529827220E-2_krp/)
    case(23)
      x=(/-9.947693349975521E-1_krp,-9.725424712181152E-1_krp,-9.329710868260161E-1_krp,&
          -8.767523582704417E-1_krp,-8.048884016188399E-1_krp,-7.186613631319502E-1_krp,&
          -6.196098757636462E-1_krp,-5.095014778460075E-1_krp,-3.903010380302908E-1_krp,&
          -2.641356809703449E-1_krp,-1.332568242984661E-1_krp,                  0.0_krp,&
           1.332568242984661E-1_krp, 2.641356809703449E-1_krp, 3.903010380302908E-1_krp,&
           5.095014778460075E-1_krp, 6.196098757636462E-1_krp, 7.186613631319502E-1_krp,&
           8.048884016188399E-1_krp, 8.767523582704417E-1_krp, 9.329710868260161E-1_krp,&
           9.725424712181152E-1_krp, 9.947693349975521E-1_krp/)
      wtt=(/1.341185948714177E-2_krp,3.098800585697944E-2_krp,4.803767173108467E-2_krp,&
            6.423242140852585E-2_krp,7.928141177671895E-2_krp,9.291576606003515E-2_krp,&
            1.048920914645414E-1_krp,1.149966402224114E-1_krp,1.230490843067295E-1_krp,&
            1.289057221880821E-1_krp,1.324620394046966E-1_krp,1.336545721861062E-1_krp,&
            1.324620394046966E-1_krp,1.289057221880821E-1_krp,1.230490843067295E-1_krp,&
            1.149966402224114E-1_krp,1.048920914645414E-1_krp,9.291576606003515E-2_krp,&
            7.928141177671895E-2_krp,6.423242140852585E-2_krp,4.803767173108467E-2_krp,&
            3.098800585697944E-2_krp,1.341185948714177E-2_krp/)
    case(24)
      x=(/-9.951872199970214E-1_krp,-9.747285559713095E-1_krp,-9.382745520027328E-1_krp,&
          -8.864155270044010E-1_krp,-8.200019859739029E-1_krp,-7.401241915785544E-1_krp,&
          -6.480936519369756E-1_krp,-5.454214713888395E-1_krp,-4.337935076260451E-1_krp,&
          -3.150426796961634E-1_krp,-1.911188674736163E-1_krp,-6.405689286260563E-2_krp,&
           6.405689286260563E-2_krp, 1.911188674736163E-1_krp, 3.150426796961634E-1_krp,&
           4.337935076260451E-1_krp, 5.454214713888395E-1_krp, 6.480936519369756E-1_krp,&
           7.401241915785544E-1_krp, 8.200019859739029E-1_krp, 8.864155270044010E-1_krp,&
           9.382745520027328E-1_krp, 9.747285559713095E-1_krp, 9.951872199970214E-1_krp/)
      wtt=(/1.234122979998720E-2_krp,2.853138862893366E-2_krp,4.427743881741981E-2_krp,&
            5.929858491543678E-2_krp,7.334648141108031E-2_krp,8.619016153195328E-2_krp,&
            9.761865210411389E-2_krp,1.074442701159656E-1_krp,1.155056680537256E-1_krp,&
            1.216704729278034E-1_krp,1.258374563468283E-1_krp,1.279381953467522E-1_krp,&
            1.279381953467522E-1_krp,1.258374563468283E-1_krp,1.216704729278034E-1_krp,&
            1.155056680537256E-1_krp,1.074442701159656E-1_krp,9.761865210411389E-2_krp,&
            8.619016153195328E-2_krp,7.334648141108031E-2_krp,5.929858491543678E-2_krp,&
            4.427743881741981E-2_krp,2.853138862893366E-2_krp,1.234122979998720E-2_krp/)
    case(25)
      x=(/-9.955569697904981E-1_krp,-9.766639214595175E-1_krp,-9.429745712289743E-1_krp,&
          -8.949919978782754E-1_krp,-8.334426287608340E-1_krp,-7.592592630373576E-1_krp,&
          -6.735663684734684E-1_krp,-5.776629302412230E-1_krp,-4.730027314457150E-1_krp,&
          -3.611723058093878E-1_krp,-2.438668837209884E-1_krp,-1.228646926107104E-1_krp,&
                            0.0_krp, 1.228646926107104E-1_krp, 2.438668837209884E-1_krp,&
           3.611723058093878E-1_krp, 4.730027314457150E-1_krp, 5.776629302412230E-1_krp,&
           6.735663684734684E-1_krp, 7.592592630373576E-1_krp, 8.334426287608340E-1_krp,&
           8.949919978782754E-1_krp, 9.429745712289743E-1_krp, 9.766639214595175E-1_krp,&
           9.955569697904981E-1_krp/)
      wtt=(/1.139379850102629E-2_krp,2.635498661503214E-2_krp,4.093915670130631E-2_krp,&
            5.490469597583519E-2_krp,6.803833381235692E-2_krp,8.014070033500102E-2_krp,&
            9.102826198296365E-2_krp,1.005359490670506E-1_krp,1.085196244742637E-1_krp,&
            1.148582591457116E-1_krp,1.194557635357848E-1_krp,1.222424429903100E-1_krp,&
            1.231760537267155E-1_krp,1.222424429903100E-1_krp,1.194557635357848E-1_krp,&
            1.148582591457116E-1_krp,1.085196244742637E-1_krp,1.005359490670506E-1_krp,&
            9.102826198296365E-2_krp,8.014070033500102E-2_krp,6.803833381235692E-2_krp,&
            5.490469597583519E-2_krp,4.093915670130631E-2_krp,2.635498661503214E-2_krp,&
            1.139379850102629E-2_krp/)
    case(26)
      x=(/-9.958857011456169E-1_krp,-9.783854459564710E-1_krp,-9.471590666617143E-1_krp,&
          -9.026378619843071E-1_krp,-8.454459427884980E-1_krp,-7.763859488206789E-1_krp,&
          -6.964272604199573E-1_krp,-6.066922930176181E-1_krp,-5.084407148245057E-1_krp,&
          -4.030517551234863E-1_krp,-2.920048394859569E-1_krp,-1.768588203568902E-1_krp,&
          -5.923009342931321E-2_krp, 5.923009342931321E-2_krp, 1.768588203568902E-1_krp,&
           2.920048394859569E-1_krp, 4.030517551234863E-1_krp, 5.084407148245057E-1_krp,&
           6.066922930176181E-1_krp, 6.964272604199573E-1_krp, 7.763859488206789E-1_krp,&
           8.454459427884980E-1_krp, 9.026378619843071E-1_krp, 9.471590666617143E-1_krp,&
           9.783854459564710E-1_krp, 9.958857011456169E-1_krp/)
      wtt=(/1.055137261734301E-2_krp,2.441785109263191E-2_krp,3.796238329436276E-2_krp,&
            5.097582529714781E-2_krp,6.327404632957484E-2_krp,7.468414976565975E-2_krp,&
            8.504589431348524E-2_krp,9.421380035591415E-2_krp,1.020591610944254E-1_krp,&
            1.084718405285766E-1_krp,1.133618165463197E-1_krp,1.166604434852966E-1_krp,&
            1.183214152792623E-1_krp,1.183214152792623E-1_krp,1.166604434852966E-1_krp,&
            1.133618165463197E-1_krp,1.084718405285766E-1_krp,1.020591610944254E-1_krp,&
            9.421380035591415E-2_krp,8.504589431348524E-2_krp,7.468414976565975E-2_krp,&
            6.327404632957484E-2_krp,5.097582529714781E-2_krp,3.796238329436276E-2_krp,&
            2.441785109263191E-2_krp,1.055137261734301E-2_krp/)
    case(27)
      x=(/-9.961792628889886E-1_krp,-9.799234759615012E-1_krp,-9.509005578147050E-1_krp,&
          -9.094823206774911E-1_krp,-8.562079080182945E-1_krp,-7.917716390705082E-1_krp,&
          -7.170134737394237E-1_krp,-6.329079719464951E-1_krp,-5.405515645794569E-1_krp,&
          -4.411482517500269E-1_krp,-3.359939036385089E-1_krp,-2.264593654395369E-1_krp,&
          -1.139725856095300E-1_krp,                  0.0_krp, 1.139725856095300E-1_krp,&
           2.264593654395369E-1_krp, 3.359939036385089E-1_krp, 4.411482517500269E-1_krp,&
           5.405515645794569E-1_krp, 6.329079719464951E-1_krp, 7.170134737394237E-1_krp,&
           7.917716390705082E-1_krp, 8.562079080182945E-1_krp, 9.094823206774911E-1_krp,&
           9.509005578147050E-1_krp, 9.799234759615012E-1_krp, 9.961792628889886E-1_krp/)
      wtt=(/9.798996051294360E-3_krp,2.268623159618062E-2_krp,3.529705375741971E-2_krp,&
            4.744941252061506E-2_krp,5.898353685983360E-2_krp,6.974882376624559E-2_krp,&
            7.960486777305777E-2_krp,8.842315854375695E-2_krp,9.608872737002851E-2_krp,&
            1.025016378177458E-1_krp,1.075782857885332E-1_krp,1.112524883568452E-1_krp,&
            1.134763461089651E-1_krp,1.142208673789570E-1_krp,1.134763461089651E-1_krp,&
            1.112524883568452E-1_krp,1.075782857885332E-1_krp,1.025016378177458E-1_krp,&
            9.608872737002851E-2_krp,8.842315854375695E-2_krp,7.960486777305777E-2_krp,&
            6.974882376624559E-2_krp,5.898353685983360E-2_krp,4.744941252061506E-2_krp,&
            3.529705375741971E-2_krp,2.268623159618062E-2_krp,9.798996051294360E-3_krp/)
    case(28)
      x=(/-9.964424975739544E-1_krp,-9.813031653708728E-1_krp,-9.542592806289382E-1_krp,&
          -9.156330263921321E-1_krp,-8.658925225743950E-1_krp,-8.056413709171792E-1_krp,&
          -7.356108780136318E-1_krp,-6.566510940388650E-1_krp,-5.697204718114017E-1_krp,&
          -4.758742249551183E-1_krp,-3.762515160890787E-1_krp,-2.720616276351781E-1_krp,&
          -1.645692821333808E-1_krp,-5.507928988403427E-2_krp, 5.507928988403427E-2_krp,&
           1.645692821333808E-1_krp, 2.720616276351781E-1_krp, 3.762515160890787E-1_krp,&
           4.758742249551183E-1_krp, 5.697204718114017E-1_krp, 6.566510940388650E-1_krp,&
           7.356108780136318E-1_krp, 8.056413709171792E-1_krp, 8.658925225743950E-1_krp,&
           9.156330263921321E-1_krp, 9.542592806289382E-1_krp, 9.813031653708728E-1_krp,&
           9.964424975739544E-1_krp/)
      wtt=(/9.124282593094518E-3_krp,2.113211259277126E-2_krp,3.290142778230438E-2_krp,&
            4.427293475900423E-2_krp,5.510734567571675E-2_krp,6.527292396699960E-2_krp,&
            7.464621423456878E-2_krp,8.311341722890122E-2_krp,9.057174439303284E-2_krp,&
            9.693065799792992E-2_krp,1.021129675780608E-1_krp,1.060557659228464E-1_krp,&
            1.087111922582941E-1_krp,1.100470130164752E-1_krp,1.100470130164752E-1_krp,&
            1.087111922582941E-1_krp,1.060557659228464E-1_krp,1.021129675780608E-1_krp,&
            9.693065799792992E-2_krp,9.057174439303284E-2_krp,8.311341722890122E-2_krp,&
            7.464621423456878E-2_krp,6.527292396699960E-2_krp,5.510734567571675E-2_krp,&
            4.427293475900423E-2_krp,3.290142778230438E-2_krp,2.113211259277126E-2_krp,&
            9.124282593094518E-3_krp/)
    case(29)
      x=(/-9.966794422605966E-1_krp,-9.825455052614132E-1_krp,-9.572855957780877E-1_krp,&
          -9.211802329530588E-1_krp,-8.746378049201028E-1_krp,-8.181854876152524E-1_krp,&
          -7.524628517344771E-1_krp,-6.782145376026865E-1_krp,-5.962817971382278E-1_krp,&
          -5.075929551242276E-1_krp,-4.131528881740087E-1_krp,-3.140316378676399E-1_krp,&
          -2.113522861660011E-1_krp,-1.062782301326792E-1_krp,                  0.0_krp,&
           1.062782301326792E-1_krp, 2.113522861660011E-1_krp, 3.140316378676399E-1_krp,&
           4.131528881740087E-1_krp, 5.075929551242276E-1_krp, 5.962817971382278E-1_krp,&
           6.782145376026865E-1_krp, 7.524628517344771E-1_krp, 8.181854876152524E-1_krp,&
           8.746378049201028E-1_krp, 9.211802329530588E-1_krp, 9.572855957780877E-1_krp,&
           9.825455052614132E-1_krp, 9.966794422605966E-1_krp/)
      wtt=(/8.516903878746410E-3_krp,1.973208505612271E-2_krp,3.074049220209362E-2_krp,&
            4.140206251868284E-2_krp,5.159482690249792E-2_krp,6.120309065707914E-2_krp,&
            7.011793325505128E-2_krp,7.823832713576378E-2_krp,8.547225736617253E-2_krp,&
            9.173775713925876E-2_krp,9.696383409440861E-2_krp,1.010912737599150E-1_krp,&
            1.040733100777294E-1_krp,1.058761550973209E-1_krp,1.064793817183142E-1_krp,&
            1.058761550973209E-1_krp,1.040733100777294E-1_krp,1.010912737599150E-1_krp,&
            9.696383409440861E-2_krp,9.173775713925876E-2_krp,8.547225736617253E-2_krp,&
            7.823832713576378E-2_krp,7.011793325505128E-2_krp,6.120309065707914E-2_krp,&
            5.159482690249792E-2_krp,4.140206251868284E-2_krp,3.074049220209362E-2_krp,&
            1.973208505612271E-2_krp,8.516903878746410E-3_krp/)
    case(30)
      x=(/-9.968934840746495E-1_krp,-9.836681232797472E-1_krp,-9.600218649683075E-1_krp,&
          -9.262000474292743E-1_krp,-8.825605357920527E-1_krp,-8.295657623827684E-1_krp,&
          -7.677774321048262E-1_krp,-6.978504947933158E-1_krp,-6.205261829892429E-1_krp,&
          -5.366241481420199E-1_krp,-4.470337695380892E-1_krp,-3.527047255308781E-1_krp,&
          -2.546369261678898E-1_krp,-1.538699136085835E-1_krp,-5.147184255531770E-2_krp,&
           5.147184255531770E-2_krp, 1.538699136085835E-1_krp, 2.546369261678898E-1_krp,&
           3.527047255308781E-1_krp, 4.470337695380892E-1_krp, 5.366241481420199E-1_krp,&
           6.205261829892429E-1_krp, 6.978504947933158E-1_krp, 7.677774321048262E-1_krp,&
           8.295657623827684E-1_krp, 8.825605357920527E-1_krp, 9.262000474292743E-1_krp,&
           9.600218649683075E-1_krp, 9.836681232797472E-1_krp, 9.968934840746495E-1_krp/)
      wtt=(/7.968192496166606E-3_krp,1.846646831109096E-2_krp,2.878470788332337E-2_krp,&
            3.879919256962705E-2_krp,4.840267283059405E-2_krp,5.749315621761907E-2_krp,&
            6.597422988218050E-2_krp,7.375597473770521E-2_krp,8.075589522942022E-2_krp,&
            8.689978720108298E-2_krp,9.212252223778613E-2_krp,9.636873717464426E-2_krp,&
            9.959342058679527E-2_krp,1.017623897484055E-1_krp,1.028526528935588E-1_krp,&
            1.028526528935588E-1_krp,1.017623897484055E-1_krp,9.959342058679527E-2_krp,&
            9.636873717464426E-2_krp,9.212252223778613E-2_krp,8.689978720108298E-2_krp,&
            8.075589522942022E-2_krp,7.375597473770521E-2_krp,6.597422988218050E-2_krp,&
            5.749315621761907E-2_krp,4.840267283059405E-2_krp,3.879919256962705E-2_krp,&
            2.878470788332337E-2_krp,1.846646831109096E-2_krp,7.968192496166606E-3_krp/)
    case(31)
      x=(/-9.970874818194771E-1_krp,-9.846859096651525E-1_krp,-9.625039250929497E-1_krp,&
          -9.307569978966482E-1_krp,-8.897600299482710E-1_krp,-8.399203201462673E-1_krp,&
          -7.817331484166249E-1_krp,-7.157767845868533E-1_krp,-6.427067229242603E-1_krp,&
          -5.632491614071493E-1_krp,-4.781937820449025E-1_krp,-3.883859016082329E-1_krp,&
          -2.947180699817016E-1_krp,-1.981211993355706E-1_krp,-9.955531215234152E-2_krp,&
                            0.0_krp, 9.955531215234152E-2_krp, 1.981211993355706E-1_krp,&
           2.947180699817016E-1_krp, 3.883859016082329E-1_krp, 4.781937820449025E-1_krp,&
           5.632491614071493E-1_krp, 6.427067229242603E-1_krp, 7.157767845868533E-1_krp,&
           7.817331484166249E-1_krp, 8.399203201462673E-1_krp, 8.897600299482710E-1_krp,&
           9.307569978966482E-1_krp, 9.625039250929497E-1_krp, 9.846859096651525E-1_krp,&
           9.970874818194771E-1_krp/)
      wtt=(/7.470831579248776E-3_krp,1.731862079031058E-2_krp,2.700901918497942E-2_krp,&
            3.643227391238546E-2_krp,4.549370752720110E-2_krp,5.410308242491685E-2_krp,&
            6.217478656102843E-2_krp,6.962858323541037E-2_krp,7.639038659877662E-2_krp,&
            8.239299176158926E-2_krp,8.757674060847788E-2_krp,9.189011389364148E-2_krp,&
            9.529024291231951E-2_krp,9.774333538632873E-2_krp,9.922501122667231E-2_krp,&
            9.972054479342645E-2_krp,9.922501122667231E-2_krp,9.774333538632873E-2_krp,&
            9.529024291231951E-2_krp,9.189011389364148E-2_krp,8.757674060847788E-2_krp,&
            8.239299176158926E-2_krp,7.639038659877662E-2_krp,6.962858323541037E-2_krp,&
            6.217478656102843E-2_krp,5.410308242491685E-2_krp,4.549370752720110E-2_krp,&
            3.643227391238546E-2_krp,2.700901918497942E-2_krp,1.731862079031058E-2_krp,&
            7.470831579248776E-3_krp/)
    case(32)
      x=(/-9.972638618494816E-1_krp,-9.856115115452683E-1_krp,-9.647622555875064E-1_krp,&
          -9.349060759377397E-1_krp,-8.963211557660521E-1_krp,-8.493676137325700E-1_krp,&
          -7.944837959679424E-1_krp,-7.321821187402897E-1_krp,-6.630442669302152E-1_krp,&
          -5.877157572407623E-1_krp,-5.068999089322294E-1_krp,-4.213512761306353E-1_krp,&
          -3.318686022821276E-1_krp,-2.392873622521371E-1_krp,-1.444719615827965E-1_krp,&
          -4.830766568773832E-2_krp, 4.830766568773832E-2_krp, 1.444719615827965E-1_krp,&
           2.392873622521371E-1_krp, 3.318686022821276E-1_krp, 4.213512761306353E-1_krp,&
           5.068999089322294E-1_krp, 5.877157572407623E-1_krp, 6.630442669302152E-1_krp,&
           7.321821187402897E-1_krp, 7.944837959679424E-1_krp, 8.493676137325700E-1_krp,&
           8.963211557660521E-1_krp, 9.349060759377397E-1_krp, 9.647622555875064E-1_krp,&
           9.856115115452683E-1_krp, 9.972638618494816E-1_krp/)
      wtt=(/7.018610009470097E-3_krp,1.627439473090567E-2_krp,2.539206530926206E-2_krp,&
            3.427386291302143E-2_krp,4.283589802222668E-2_krp,5.099805926237618E-2_krp,&
            5.868409347853555E-2_krp,6.582222277636185E-2_krp,7.234579410884851E-2_krp,&
            7.819389578707031E-2_krp,8.331192422694676E-2_krp,8.765209300440381E-2_krp,&
            9.117387869576388E-2_krp,9.384439908080457E-2_krp,9.563872007927486E-2_krp,&
            9.654008851472780E-2_krp,9.654008851472780E-2_krp,9.563872007927486E-2_krp,&
            9.384439908080457E-2_krp,9.117387869576388E-2_krp,8.765209300440381E-2_krp,&
            8.331192422694676E-2_krp,7.819389578707031E-2_krp,7.234579410884851E-2_krp,&
            6.582222277636185E-2_krp,5.868409347853555E-2_krp,5.099805926237618E-2_krp,&
            4.283589802222668E-2_krp,3.427386291302143E-2_krp,2.539206530926206E-2_krp,&
            1.627439473090567E-2_krp,7.018610009470097E-3_krp/)
    case(33)
      x=(/-9.974246942464552E-1_krp,-9.864557262306425E-1_krp,-9.668229096899928E-1_krp,&
          -9.386943726111684E-1_krp,-9.023167677434336E-1_krp,-8.580096526765041E-1_krp,&
          -8.061623562741666E-1_krp,-7.472304964495622E-1_krp,-6.817319599697428E-1_krp,&
          -6.102423458363790E-1_krp,-5.333899047863476E-1_krp,-4.518500172724507E-1_krp,&
          -3.663392577480733E-1_krp,-2.776090971524970E-1_krp,-1.864392988279916E-1_krp,&
          -9.363106585473339E-2_krp,                  0.0_krp, 9.363106585473339E-2_krp,&
           1.864392988279916E-1_krp, 2.776090971524970E-1_krp, 3.663392577480733E-1_krp,&
           4.518500172724507E-1_krp, 5.333899047863476E-1_krp, 6.102423458363790E-1_krp,&
           6.817319599697428E-1_krp, 7.472304964495622E-1_krp, 8.061623562741666E-1_krp,&
           8.580096526765041E-1_krp, 9.023167677434336E-1_krp, 9.386943726111684E-1_krp,&
           9.668229096899928E-1_krp, 9.864557262306425E-1_krp, 9.974246942464552E-1_krp/)
      wtt=(/6.606227847587378E-3_krp,1.532170151293468E-2_krp,2.391554810174948E-2_krp,&
            3.230035863232895E-2_krp,4.040154133166959E-2_krp,4.814774281871170E-2_krp,&
            5.547084663166356E-2_krp,6.230648253031748E-2_krp,6.859457281865671E-2_krp,&
            7.427985484395415E-2_krp,7.931236479488674E-2_krp,8.364787606703871E-2_krp,&
            8.724828761884434E-2_krp,9.008195866063858E-2_krp,9.212398664331685E-2_krp,&
            9.335642606559612E-2_krp,9.376844616021000E-2_krp,9.335642606559612E-2_krp,&
            9.212398664331685E-2_krp,9.008195866063858E-2_krp,8.724828761884434E-2_krp,&
            8.364787606703871E-2_krp,7.931236479488674E-2_krp,7.427985484395415E-2_krp,&
            6.859457281865671E-2_krp,6.230648253031748E-2_krp,5.547084663166356E-2_krp,&
            4.814774281871170E-2_krp,4.040154133166959E-2_krp,3.230035863232895E-2_krp,&
            2.391554810174948E-2_krp,1.532170151293468E-2_krp,6.606227847587378E-3_krp/)
    case(34)
      x=(/-9.975717537908419E-1_krp,-9.872278164063095E-1_krp,-9.687082625333443E-1_krp,&
          -9.421623974051071E-1_krp,-9.078096777183245E-1_krp,-8.659346383345645E-1_krp,&
          -8.168842279009337E-1_krp,-7.610648766298730E-1_krp,-6.989391132162629E-1_krp,&
          -6.310217270805285E-1_krp,-5.578755006697466E-1_krp,-4.801065451903270E-1_krp,&
          -3.983592777586459E-1_krp,-3.133110813394632E-1_krp,-2.256666916164495E-1_krp,&
          -1.361523572591830E-1_krp,-4.550982195310254E-2_krp, 4.550982195310254E-2_krp,&
           1.361523572591830E-1_krp, 2.256666916164495E-1_krp, 3.133110813394632E-1_krp,&
           3.983592777586459E-1_krp, 4.801065451903270E-1_krp, 5.578755006697466E-1_krp,&
           6.310217270805285E-1_krp, 6.989391132162629E-1_krp, 7.610648766298730E-1_krp,&
           8.168842279009337E-1_krp, 8.659346383345645E-1_krp, 9.078096777183245E-1_krp,&
           9.421623974051071E-1_krp, 9.687082625333443E-1_krp, 9.872278164063095E-1_krp,&
           9.975717537908419E-1_krp/)
      wtt=(/6.229140555908685E-3_krp,1.445016274859504E-2_krp,2.256372198549497E-2_krp,&
            3.049138063844613E-2_krp,3.816659379638752E-2_krp,4.552561152335327E-2_krp,&
            5.250741457267811E-2_krp,5.905413582752449E-2_krp,6.511152155407641E-2_krp,&
            7.062937581425572E-2_krp,7.556197466003193E-2_krp,7.986844433977184E-2_krp,&
            8.351309969984566E-2_krp,8.646573974703575E-2_krp,8.870189783569387E-2_krp,&
            9.020304437064073E-2_krp,9.095674033025987E-2_krp,9.095674033025987E-2_krp,&
            9.020304437064073E-2_krp,8.870189783569387E-2_krp,8.646573974703575E-2_krp,&
            8.351309969984566E-2_krp,7.986844433977184E-2_krp,7.556197466003193E-2_krp,&
            7.062937581425572E-2_krp,6.511152155407641E-2_krp,5.905413582752449E-2_krp,&
            5.250741457267811E-2_krp,4.552561152335327E-2_krp,3.816659379638752E-2_krp,&
            3.049138063844613E-2_krp,2.256372198549497E-2_krp,1.445016274859504E-2_krp,&
            6.229140555908685E-3_krp/)
    case(35)
      x=(/-9.977065690996003E-1_krp,-9.879357644438515E-1_krp,-9.704376160392298E-1_krp,&
          -9.453451482078273E-1_krp,-9.128542613593176E-1_krp,-8.732191250252223E-1_krp,&
          -8.267498990922254E-1_krp,-7.738102522869126E-1_krp,-7.148145015566288E-1_krp,&
          -6.502243646658904E-1_krp,-5.805453447497645E-1_krp,-5.063227732414886E-1_krp,&
          -4.281375415178143E-1_krp,-3.466015544308139E-1_krp,-2.623529412092961E-1_krp,&
          -1.760510611659896E-1_krp,-8.837134327565926E-2_krp,                  0.0_krp,&
           8.837134327565926E-2_krp, 1.760510611659896E-1_krp, 2.623529412092961E-1_krp,&
           3.466015544308139E-1_krp, 4.281375415178143E-1_krp, 5.063227732414886E-1_krp,&
           5.805453447497645E-1_krp, 6.502243646658904E-1_krp, 7.148145015566288E-1_krp,&
           7.738102522869126E-1_krp, 8.267498990922254E-1_krp, 8.732191250252223E-1_krp,&
           9.128542613593176E-1_krp, 9.453451482078273E-1_krp, 9.704376160392298E-1_krp,&
           9.879357644438515E-1_krp, 9.977065690996003E-1_krp/)
      wtt=(/5.883433420443085E-3_krp,1.365082834836149E-2_krp,2.132297991148358E-2_krp,&
            2.882926010889425E-2_krp,3.611011586346338E-2_krp,4.310842232617022E-2_krp,&
            4.976937040135353E-2_krp,5.604081621237013E-2_krp,6.187367196608019E-2_krp,&
            6.722228526908690E-2_krp,7.204479477256006E-2_krp,7.630345715544205E-2_krp,&
            7.996494224232426E-2_krp,8.300059372885659E-2_krp,8.538665339209913E-2_krp,&
            8.710444699718353E-2_krp,8.814053043027546E-2_krp,8.848679490710429E-2_krp,&
            8.814053043027546E-2_krp,8.710444699718353E-2_krp,8.538665339209913E-2_krp,&
            8.300059372885659E-2_krp,7.996494224232426E-2_krp,7.630345715544205E-2_krp,&
            7.204479477256006E-2_krp,6.722228526908690E-2_krp,6.187367196608019E-2_krp,&
            5.604081621237013E-2_krp,4.976937040135353E-2_krp,4.310842232617022E-2_krp,&
            3.611011586346338E-2_krp,2.882926010889425E-2_krp,2.132297991148358E-2_krp,&
            1.365082834836149E-2_krp,5.883433420443085E-3_krp/)
    case(36)
      x=(/-9.978304624840858E-1_krp,-9.885864789022122E-1_krp,-9.720276910496979E-1_krp,&
          -9.482729843995075E-1_krp,-9.174977745156591E-1_krp,-8.799298008903971E-1_krp,&
          -8.358471669924753E-1_krp,-7.855762301322065E-1_krp,-7.294891715935566E-1_krp,&
          -6.680012365855211E-1_krp,-6.015676581359805E-1_krp,-5.306802859262452E-1_krp,&
          -4.558639444334203E-1_krp,-3.776725471196892E-1_krp,-2.966849953440283E-1_krp,&
          -2.135008923168656E-1_krp,-1.287361038093848E-1_krp,-4.301819847370861E-2_krp,&
           4.301819847370861E-2_krp, 1.287361038093848E-1_krp, 2.135008923168656E-1_krp,&
           2.966849953440283E-1_krp, 3.776725471196892E-1_krp, 4.558639444334203E-1_krp,&
           5.306802859262452E-1_krp, 6.015676581359805E-1_krp, 6.680012365855211E-1_krp,&
           7.294891715935566E-1_krp, 7.855762301322065E-1_krp, 8.358471669924753E-1_krp,&
           8.799298008903971E-1_krp, 9.174977745156591E-1_krp, 9.482729843995075E-1_krp,&
           9.720276910496979E-1_krp, 9.885864789022122E-1_krp, 9.978304624840858E-1_krp/)
      wtt=(/5.565719664245045E-3_krp,1.291594728406557E-2_krp,2.018151529773547E-2_krp,&
            2.729862149856878E-2_krp,3.421381077030723E-2_krp,4.087575092364490E-2_krp,&
            4.723508349026598E-2_krp,5.324471397775992E-2_krp,5.886014424532482E-2_krp,&
            6.403979735501549E-2_krp,6.874532383573644E-2_krp,7.294188500565306E-2_krp,&
            7.659841064587067E-2_krp,7.968782891207160E-2_krp,8.218726670433971E-2_krp,&
            8.407821897966193E-2_krp,8.534668573933863E-2_krp,8.598327567039475E-2_krp,&
            8.598327567039475E-2_krp,8.534668573933863E-2_krp,8.407821897966193E-2_krp,&
            8.218726670433971E-2_krp,7.968782891207160E-2_krp,7.659841064587067E-2_krp,&
            7.294188500565306E-2_krp,6.874532383573644E-2_krp,6.403979735501549E-2_krp,&
            5.886014424532482E-2_krp,5.324471397775992E-2_krp,4.723508349026598E-2_krp,&
            4.087575092364490E-2_krp,3.421381077030723E-2_krp,2.729862149856878E-2_krp,&
            2.018151529773547E-2_krp,1.291594728406557E-2_krp,5.565719664245045E-3_krp/)
    case(37)
      x=(/-9.979445824779136E-1_krp,-9.891859632143192E-1_krp,-9.734930300564857E-1_krp,&
          -9.509723432620948E-1_krp,-9.217814374124637E-1_krp,-8.861249621554861E-1_krp,&
          -8.442529873405560E-1_krp,-7.964592005099023E-1_krp,-7.430788339819653E-1_krp,&
          -6.844863091309594E-1_krp,-6.210926084089245E-1_krp,-5.533423918615818E-1_krp,&
          -4.817108778032056E-1_krp,-4.067005093183261E-1_krp,-3.288374298837070E-1_krp,&
          -2.486677927913658E-1_krp,-1.667539302398520E-1_krp,-8.367040895476990E-2_krp,&
                            0.0_krp, 8.367040895476990E-2_krp, 1.667539302398520E-1_krp,&
           2.486677927913658E-1_krp, 3.288374298837070E-1_krp, 4.067005093183261E-1_krp,&
           4.817108778032056E-1_krp, 5.533423918615818E-1_krp, 6.210926084089245E-1_krp,&
           6.844863091309594E-1_krp, 7.430788339819653E-1_krp, 7.964592005099023E-1_krp,&
           8.442529873405560E-1_krp, 8.861249621554861E-1_krp, 9.217814374124637E-1_krp,&
           9.509723432620948E-1_krp, 9.734930300564857E-1_krp, 9.891859632143192E-1_krp,&
           9.979445824779136E-1_krp/)
      wtt=(/5.273057279497939E-3_krp,1.223878010030756E-2_krp,1.912904448908397E-2_krp,&
            2.588603699055893E-2_krp,3.246163984752148E-2_krp,3.880960250193454E-2_krp,&
            4.488536466243717E-2_krp,5.064629765482460E-2_krp,5.605198799827492E-2_krp,&
            6.106451652322599E-2_krp,6.564872287275125E-2_krp,6.977245155570034E-2_krp,&
            7.340677724848817E-2_krp,7.652620757052924E-2_krp,7.910886183752938E-2_krp,&
            8.113662450846503E-2_krp,8.259527223643725E-2_krp,8.347457362586279E-2_krp,&
            8.376836099313890E-2_krp,8.347457362586279E-2_krp,8.259527223643725E-2_krp,&
            8.113662450846503E-2_krp,7.910886183752938E-2_krp,7.652620757052924E-2_krp,&
            7.340677724848817E-2_krp,6.977245155570034E-2_krp,6.564872287275125E-2_krp,&
            6.106451652322599E-2_krp,5.605198799827492E-2_krp,5.064629765482460E-2_krp,&
            4.488536466243717E-2_krp,3.880960250193454E-2_krp,3.246163984752148E-2_krp,&
            2.588603699055893E-2_krp,1.912904448908397E-2_krp,1.223878010030756E-2_krp,&
            5.273057279497939E-3_krp/)
    case(38)
      x=(/-9.980499305356876E-1_krp,-9.897394542663856E-1_krp,-9.748463285901535E-1_krp,&
          -9.534663309335296E-1_krp,-9.257413320485844E-1_krp,-8.918557390046322E-1_krp,&
          -8.520350219323622E-1_krp,-8.065441676053168E-1_krp,-7.556859037539707E-1_krp,&
          -6.997986803791844E-1_krp,-6.392544158296817E-1_krp,-5.744560210478071E-1_krp,&
          -5.058347179279311E-1_krp,-4.338471694323765E-1_krp,-3.589724404794350E-1_krp,&
          -2.817088097901653E-1_krp,-2.025704538921167E-1_krp,-1.220840253378674E-1_krp,&
          -4.078514790457824E-2_krp, 4.078514790457824E-2_krp, 1.220840253378674E-1_krp,&
           2.025704538921167E-1_krp, 2.817088097901653E-1_krp, 3.589724404794350E-1_krp,&
           4.338471694323765E-1_krp, 5.058347179279311E-1_krp, 5.744560210478071E-1_krp,&
           6.392544158296817E-1_krp, 6.997986803791844E-1_krp, 7.556859037539707E-1_krp,&
           8.065441676053168E-1_krp, 8.520350219323622E-1_krp, 8.918557390046322E-1_krp,&
           9.257413320485844E-1_krp, 9.534663309335296E-1_krp, 9.748463285901535E-1_krp,&
           9.897394542663856E-1_krp, 9.980499305356876E-1_krp/)
      wtt=(/5.002880749639346E-3_krp,1.161344471646867E-2_krp,1.815657770961324E-2_krp,&
            2.457973973823238E-2_krp,3.083950054517505E-2_krp,3.689408159402474E-2_krp,&
            4.270315850467443E-2_krp,4.822806186075868E-2_krp,5.343201991033232E-2_krp,&
            5.828039914699721E-2_krp,6.274093339213305E-2_krp,6.678393797914041E-2_krp,&
            7.038250706689895E-2_krp,7.351269258474346E-2_krp,7.615366354844640E-2_krp,&
            7.828784465821095E-2_krp,7.990103324352782E-2_krp,8.098249377059710E-2_krp,&
            8.152502928038579E-2_krp,8.152502928038579E-2_krp,8.098249377059710E-2_krp,&
            7.990103324352782E-2_krp,7.828784465821095E-2_krp,7.615366354844640E-2_krp,&
            7.351269258474346E-2_krp,7.038250706689895E-2_krp,6.678393797914041E-2_krp,&
            6.274093339213305E-2_krp,5.828039914699721E-2_krp,5.343201991033232E-2_krp,&
            4.822806186075868E-2_krp,4.270315850467443E-2_krp,3.689408159402474E-2_krp,&
            3.083950054517505E-2_krp,2.457973973823238E-2_krp,1.815657770961324E-2_krp,&
            1.161344471646867E-2_krp,5.002880749639346E-3_krp/)
    case(39)
      x=(/-9.981473830664329E-1_krp,-9.902515368546860E-1_krp,-9.760987093334711E-1_krp,&
          -9.557752123246523E-1_krp,-9.294091484867382E-1_krp,-8.971671192929929E-1_krp,&
          -8.592529379999062E-1_krp,-8.159062974301431E-1_krp,-7.674012429310635E-1_krp,&
          -7.140444358945347E-1_krp,-6.561732134320109E-1_krp,-5.941534549572780E-1_krp,&
          -5.283772686604375E-1_krp,-4.592605123091360E-1_krp,-3.872401639715615E-1_krp,&
          -3.127715592481859E-1_krp,-2.363255124618358E-1_krp,-1.583853399978378E-1_krp,&
          -7.944380460875548E-2_krp,                  0.0_krp, 7.944380460875548E-2_krp,&
           1.583853399978378E-1_krp, 2.363255124618358E-1_krp, 3.127715592481859E-1_krp,&
           3.872401639715615E-1_krp, 4.592605123091360E-1_krp, 5.283772686604375E-1_krp,&
           5.941534549572780E-1_krp, 6.561732134320109E-1_krp, 7.140444358945347E-1_krp,&
           7.674012429310635E-1_krp, 8.159062974301431E-1_krp, 8.592529379999062E-1_krp,&
           8.971671192929929E-1_krp, 9.294091484867382E-1_krp, 9.557752123246523E-1_krp,&
           9.760987093334711E-1_krp, 9.902515368546860E-1_krp, 9.981473830664329E-1_krp/)
      wtt=(/4.752944691635101E-3_krp,1.103478893916459E-2_krp,1.725622909372492E-2_krp,&
            2.336938483217816E-2_krp,2.933495598390338E-2_krp,3.511511149813133E-2_krp,&
            4.067327684793384E-2_krp,4.597430110891663E-2_krp,5.098466529212941E-2_krp,&
            5.567269034091630E-2_krp,6.000873608859615E-2_krp,6.396538813868239E-2_krp,&
            6.751763096623127E-2_krp,7.064300597060876E-2_krp,7.332175341426862E-2_krp,&
            7.553693732283606E-2_krp,7.727455254468202E-2_krp,7.852361328737118E-2_krp,&
            7.927622256836847E-2_krp,7.952762213944285E-2_krp,7.927622256836847E-2_krp,&
            7.852361328737118E-2_krp,7.727455254468202E-2_krp,7.553693732283606E-2_krp,&
            7.332175341426862E-2_krp,7.064300597060876E-2_krp,6.751763096623127E-2_krp,&
            6.396538813868239E-2_krp,6.000873608859615E-2_krp,5.567269034091630E-2_krp,&
            5.098466529212941E-2_krp,4.597430110891663E-2_krp,4.067327684793384E-2_krp,&
            3.511511149813133E-2_krp,2.933495598390338E-2_krp,2.336938483217816E-2_krp,&
            1.725622909372492E-2_krp,1.103478893916459E-2_krp,4.752944691635101E-3_krp/)
    case(40)
      x=(/-9.982377097105592E-1_krp,-9.907262386994570E-1_krp,-9.772599499837743E-1_krp,&
          -9.579168192137917E-1_krp,-9.328128082786765E-1_krp,-9.020988069688743E-1_krp,&
          -8.659595032122595E-1_krp,-8.246122308333117E-1_krp,-7.783056514265194E-1_krp,&
          -7.273182551899271E-1_krp,-6.719566846141795E-1_krp,-6.125538896679802E-1_krp,&
          -5.494671250951282E-1_krp,-4.830758016861787E-1_krp,-4.137792043716050E-1_krp,&
          -3.419940908257585E-1_krp,-2.681521850072537E-1_krp,-1.926975807013711E-1_krp,&
          -1.160840706752552E-1_krp,-3.877241750605082E-2_krp, 3.877241750605082E-2_krp,&
           1.160840706752552E-1_krp, 1.926975807013711E-1_krp, 2.681521850072537E-1_krp,&
           3.419940908257585E-1_krp, 4.137792043716050E-1_krp, 4.830758016861787E-1_krp,&
           5.494671250951282E-1_krp, 6.125538896679802E-1_krp, 6.719566846141795E-1_krp,&
           7.273182551899271E-1_krp, 7.783056514265194E-1_krp, 8.246122308333117E-1_krp,&
           8.659595032122595E-1_krp, 9.020988069688743E-1_krp, 9.328128082786765E-1_krp,&
           9.579168192137917E-1_krp, 9.772599499837743E-1_krp, 9.907262386994570E-1_krp,&
           9.982377097105592E-1_krp/)
      wtt=(/4.521277098533191E-3_krp,1.049828453115281E-2_krp,1.642105838190789E-2_krp,&
            2.224584919416696E-2_krp,2.793700698002340E-2_krp,3.346019528254785E-2_krp,&
            3.878216797447202E-2_krp,4.387090818567327E-2_krp,4.869580763507223E-2_krp,&
            5.322784698393682E-2_krp,5.743976909939155E-2_krp,6.130624249292894E-2_krp,&
            6.480401345660104E-2_krp,6.791204581523390E-2_krp,7.061164739128678E-2_krp,&
            7.288658239580406E-2_krp,7.472316905796826E-2_krp,7.611036190062624E-2_krp,&
            7.703981816424797E-2_krp,7.750594797842481E-2_krp,7.750594797842481E-2_krp,&
            7.703981816424797E-2_krp,7.611036190062624E-2_krp,7.472316905796826E-2_krp,&
            7.288658239580406E-2_krp,7.061164739128678E-2_krp,6.791204581523390E-2_krp,&
            6.480401345660104E-2_krp,6.130624249292894E-2_krp,5.743976909939155E-2_krp,&
            5.322784698393682E-2_krp,4.869580763507223E-2_krp,4.387090818567327E-2_krp,&
            3.878216797447202E-2_krp,3.346019528254785E-2_krp,2.793700698002340E-2_krp,&
            2.224584919416696E-2_krp,1.642105838190789E-2_krp,1.049828453115281E-2_krp,&
            4.521277098533191E-3_krp/)
    case default
      call raise_error(myName // "point number > 40 is not supported")
    end select

    x=(x*(b-a)+(a+b))*0.5_krp
    wtt=wtt*(b-a)*0.5_krp

  end subroutine gauss_legendre_integral_define

  !-----------------------------------------------------------------------------
  !> gauss hermite integration. do not use n > 23 for that the error is large
  !>
  !> @param n - in, # of gauss points
  !> @param x - inout, absciassaes
  !> @param wtt - inout, weights
  !-----------------------------------------------------------------------------
  subroutine gauss_hermite_integral_define(n,x,wtt)
    charSt,parameter :: myName = &
      'math => gauss_hermite_integral_define - '
    intknd, intent(in) :: n
    realkd, intent(inout) :: x(n)
    realkd, intent(inout) :: wtt(n)

    select case(n)
    case(1)
      x = (/0._krp/)
      wtt = sqrtpi
    case(2)
      x = (/-0.707106781186548_krp,0.707106781186548_krp/)
      wtt = 0.886226925452758_krp
    case(3)
      x = (/-1.224744871391589_krp,0._krp,1.224744871391589_krp/)
      wtt =(/0.295408975150919_krp,1.181635900603677_krp,0.295408975150919_krp/)
    case(4)
      x=(/-1.650680123885784_krp,-0.524647623275290_krp,0.524647623275290_krp, &
            1.650680123885784_krp/)
      wtt=(/0.081312835447245_krp,0.804914090005513_krp,0.804914090005513_krp, &
              0.081312835447245_krp/)
    case(5)
      x=(/-2.020182870456086_krp,-0.958572464613819_krp,0._krp,                &
           0.958572464613819_krp,2.020182870456086_krp/)
      wtt=(/0.019953242059046_krp,0.393619323152241_krp,0.945308720482942_krp, &
             0.393619323152241_krp,0.019953242059046_krp/)
    case(6)
      x=(/-2.350604973674492_krp,-1.335849074013697_krp,-0.436077411927617_krp,&
          0.436077411927617_krp,1.335849074013697_krp,2.350604973674492_krp/)
      wtt=(/0.004530009905509_krp,0.157067320322857_krp,0.724629595224392_krp, &
         0.724629595224392_krp,0.157067320322857_krp,0.004530009905509_krp/)
    case(7)
      x=(/-2.651961356835233_krp,-1.673551628767471_krp,-0.816287882858965_krp,&
          0._krp,0.816287882858965_krp,1.673551628767471_krp,                  &
          2.651961356835233_krp/)
      wtt=(/0.000971781245100_krp,0.054515582819127_krp,0.425607252610127_krp, &
            0.810264617556807_krp,0.425607252610127_krp,0.054515582819127_krp, &
            0.000971781245100_krp/)
    case(8)
      x=(/-2.930637420257244_krp,-1.981656756695843_krp,-1.157193712446780_krp,&
          -0.381186990207322_krp,0.381186990207322_krp,1.157193712446780_krp,  &
           1.981656756695843_krp,2.930637420257244_krp/)
      wtt=(/0.000199604072211_krp,0.017077983007413_krp,0.207802325814892_krp, &
            0.661147012558241_krp,0.661147012558241_krp,0.207802325814892_krp, &
            0.017077983007413_krp,0.000199604072211_krp/)
    case(9)
      x=(/-3.190993201781527_krp,-2.266580584531843_krp,-1.468553289216668_krp,&
          -0.723551018752838_krp,0._krp,0.723551018752838_krp,                 &
          1.468553289216668_krp,2.266580584531843_krp,3.190993201781527_krp/)
      wtt=(/0.000039606977263_krp,0.004943624275537_krp,0.088474527394377_krp, &
            0.432651559002554_krp,0.720235215606051_krp,0.432651559002554_krp, &
            0.088474527394377_krp,0.004943624275537_krp,0.000039606977263_krp/)
    case(10)
      x=(/-3.436159118837738_krp,-2.532731674232790_krp,-1.756683649299882_krp,&
          -1.036610829789514_krp,-0.342901327223705_krp,0.342901327223705_krp, &
          1.036610829789514_krp,1.756683649299882_krp,2.532731674232790_krp,   &
          3.436159118837738_krp/)
      wtt=(/0.000007640432855_krp,0.001343645746781_krp,0.033874394455481_krp, &
            0.240138611082313_krp,0.610862633735325_krp,0.610862633735325_krp, &
            0.240138611082313_krp,0.033874394455481_krp,0.001343645746781_krp, &
            0.000007640432855_krp/)
    case(11)
      x=(/-3.668470846559583_krp,-2.783290099781652_krp,-2.025948015825755_krp,&
          -1.326557084494933_krp,-0.656809566882100_krp,0._krp,                &
          0.656809566882100_krp,1.326557084494933_krp,2.025948015825755_krp,   &
          2.783290099781652_krp,3.668470846559583_krp/)
      wtt=(/0.000001439560394_krp,0.000346819466323_krp,0.011911395444912_krp, &
            0.117227875167708_krp,0.429359752356125_krp,0.654759286914591_krp, &
            0.429359752356125_krp,0.117227875167708_krp,0.011911395444912_krp, &
            0.000346819466323_krp,0.000001439560394_krp/)
    case(12)
      x=(/-3.889724897869782_krp,-3.020637025120890_krp,-2.279507080501060_krp,&
          -1.597682635152605_krp,-0.947788391240164_krp,-0.314240376254359_krp,&
          0.314240376254359_krp,0.947788391240164_krp,1.597682635152605_krp,   &
          2.279507080501060_krp,3.020637025120890_krp,3.889724897869782_krp/)
      wtt=(/0.000000265855168_krp,0.000085736870436_krp,0.003905390584629_krp, &
            0.051607985615884_krp,0.260492310264161_krp,0.570135236262480_krp, &
            0.570135236262480_krp,0.260492310264161_krp,0.051607985615884_krp, &
            0.003905390584629_krp,0.000085736870436_krp,0.000000265855168_krp/)
    case(13)
      x=(/-4.101337596178640_krp,-3.246608978372410_krp,-2.519735685678238_krp,&
          -1.853107651601512_krp,-1.220055036590749_krp,-0.605763879171060_krp,&
          0._krp, 0.605763879171060_krp,1.220055036590749_krp,                 &
          1.853107651601512_krp,2.519735685678238_krp,3.246608978372410_krp,   &
          4.101337596178640_krp/)
      wtt=(/0.000000048257319_krp,0.000020430360403_krp,0.001207459992719_krp, &
            0.020862775296170_krp,0.140323320687024_krp,0.421616296898542_krp, &
            0.604393187921162_krp,0.421616296898542_krp,0.140323320687024_krp, &
            0.020862775296170_krp,0.001207459992719_krp,0.000020430360403_krp, &
            0.000000048257319_krp/)
    case(14)
      x=(/-4.304448570473632_krp,-3.462656933602271_krp,-2.748470724985403_krp,&
          -2.095183258507717_krp,-1.476682731141141_krp,-0.878713787329399_krp,&
          -0.291745510672562_krp,0.291745510672562_krp,0.878713787329399_krp,  &
          1.476682731141141_krp,2.095183258507717_krp,2.748470724985403_krp,   &
          3.462656933602271_krp,4.304448570473632_krp/)
      wtt=(/0.000000008628591_krp,0.000004716484355_krp,0.000355092613552_krp, &
            0.007850054726458_krp,0.068505534223465_krp,0.273105609064219_krp, &
            0.536405909712090_krp,0.536405909712090_krp,0.273105609064219_krp, &
            0.068505534223465_krp,0.007850054726458_krp,0.000355092613552_krp, &
            0.000004716484355_krp,0.000000008628591_krp/)
    case(15)
      x=(/-4.499990707309391_krp,-3.669950373404453_krp,-2.967166927905603_krp,&
          -2.325732486173858_krp,-1.719992575186489_krp,-1.136115585210921_krp,&
          -0.565069583255576_krp,0.000000000000000_krp,0.565069583255576_krp,  &
          1.136115585210921_krp,1.719992575186489_krp,2.325732486173858_krp,   &
          2.967166927905603_krp,3.669950373404453_krp,4.499990707309391_krp/)
      wtt=(/0.000000001522476_krp,0.000001059115548_krp,0.000100004441232_krp, &
            0.002778068842913_krp,0.030780033872546_krp,0.158488915795936_krp, &
            0.412028687498899_krp,0.564100308726417_krp,0.412028687498899_krp, &
            0.158488915795936_krp,0.030780033872546_krp,0.002778068842913_krp, &
            0.000100004441232_krp,0.000001059115548_krp,0.000000001522476_krp/)
    case(16)
      x=(/-4.688738939305819_krp,-3.869447904860123_krp,-3.176999161979956_krp,&
          -2.546202157847481_krp,-1.951787990916254_krp,-1.380258539198881_krp,&
          -0.822951449144656_krp,-0.273481046138152_krp,0.273481046138152_krp, &
          0.822951449144656_krp,1.380258539198881_krp,1.951787990916254_krp,   &
          2.546202157847481_krp,3.176999161979956_krp,3.869447904860123_krp,   &
          4.688738939305819_krp/)
      wtt=(/0.000000000265481_krp,0.000000232098084_krp,0.000027118600925_krp, &
            0.000932284008624_krp,0.012880311535510_krp,0.083810041398986_krp, &
            0.280647458528533_krp,0.507929479016614_krp,0.507929479016614_krp, &
            0.280647458528533_krp,0.083810041398986_krp,0.012880311535510_krp, &
            0.000932284008624_krp,0.000027118600925_krp,0.000000232098084_krp, &
            0.000000000265481_krp/)
    case(17)
      x=(/-4.871345193674403_krp,-4.061946675875475_krp,-3.378932091141494_krp,&
          -2.757762915703889_krp,-2.173502826666621_krp,-1.612924314221231_krp,&
          -1.067648725743451_krp,-0.531633001342655_krp,0._krp,                &
          0.531633001342655_krp,1.067648725743451_krp,1.612924314221231_krp,   &
          2.173502826666621_krp,2.757762915703889_krp,3.378932091141494_krp,   &
          4.061946675875475_krp,4.871345193674403_krp/)
      wtt=(/0.000000000045806_krp,0.000000049770790_krp,0.000007112289140_krp, &
            0.000298643286698_krp,0.005067349957628_krp,0.040920034149756_krp, &
            0.172648297670090_krp,0.401826469470412_krp,0.530917937624863_krp, &
            0.401826469470412_krp,0.172648297670090_krp,0.040920034149756_krp, &
            0.005067349957628_krp,0.000298643286698_krp,0.000007112289140_krp, &
            0.000000049770790_krp,0.000000000045806_krp/)
    case(18)
      x=(/-5.048364008874467_krp,-4.248117873568126_krp,-3.573769068486266_krp,&
          -2.961377505531607_krp,-2.386299089166686_krp,-1.835531604261629_krp,&
          -1.300920858389618_krp,-0.776682919267412_krp,-0.258267750519097_krp,&
          0.258267750519097_krp,0.776682919267412_krp,1.300920858389618_krp,   &
          1.835531604261629_krp,2.386299089166686_krp,2.961377505531607_krp,   &
          3.573769068486266_krp,4.248117873568126_krp,5.048364008874467_krp/)
      wtt=(/0.000000000007828_krp,0.000000010467206_krp,0.000001810654481_krp, &
            0.000091811268679_krp,0.001888522630268_krp,0.018640042387545_krp, &
            0.097301747641315_krp,0.284807285669979_krp,0.483495694725455_krp, &
            0.483495694725455_krp,0.284807285669979_krp,0.097301747641315_krp, &
            0.018640042387545_krp,0.001888522630268_krp,0.000091811268679_krp, &
            0.000001810654481_krp,0.000000010467206_krp,0.000000000007828_krp/)
    case(19)
      x=(/-5.220271690537482_krp,-4.428532806603779_krp,-3.762187351964020_krp,&
          -3.157848818347602_krp,-2.591133789794542_krp,-2.049231709850619_krp,&
          -1.524170619393533_krp,-1.010368387134311_krp,-0.503520163423888_krp,&
          0.000000000000000_krp,0.503520163423888_krp,1.010368387134311_krp,   &
          1.524170619393533_krp,2.049231709850619_krp,2.591133789794542_krp,   &
          3.157848818347602_krp,3.762187351964020_krp,4.428532806603779_krp,   &
          5.220271690537482_krp/)
      wtt=(/0.000000000001326_krp,0.000000002163051_krp,0.000000448824315_krp, &
            0.000027209197763_krp,0.000670877521407_krp,0.007988866777723_krp, &
            0.050810386909052_krp,0.183632701306997_krp,0.391608988613030_krp, &
            0.502974888276187_krp,0.391608988613030_krp,0.183632701306997_krp, &
            0.050810386909052_krp,0.007988866777723_krp,0.000670877521407_krp, &
            0.000027209197763_krp,0.000000448824315_krp,0.000000002163051_krp, &
            0.000000000001326_krp/)
    case(20)
      x=(/-5.387480890011233_krp,-4.603682449550744_krp,-3.944764040115625_krp,&
          -3.347854567383216_krp,-2.788806058428131_krp,-2.254974002089276_krp,&
          -1.738537712116586_krp,-1.234076215395323_krp,-0.737473728545394_krp,&
          -0.245340708300901_krp,0.245340708300901_krp,0.737473728545394_krp,  &
          1.234076215395323_krp,1.738537712116586_krp,2.254974002089276_krp,   &
          2.788806058428131_krp,3.347854567383216_krp,3.944764040115625_krp,   &
          4.603682449550744_krp,5.387480890011233_krp/)
      wtt=(/0.000000000000223_krp,0.000000000439934_krp,0.000000108606937_krp, &
            0.000007802556479_krp,0.000228338636016_krp,0.003243773342238_krp, &
            0.024810520887464_krp,0.109017206020022_krp,0.286675505362834_krp, &
            0.462243669600610_krp,0.462243669600610_krp,0.286675505362834_krp, &
            0.109017206020022_krp,0.024810520887464_krp,0.003243773342238_krp, &
            0.000228338636016_krp,0.000007802556479_krp,0.000000108606937_krp, &
            0.000000000439934_krp,0.000000000000223_krp/)
    case(21)
      x=(/-5.550351873264678_krp,-4.773992343411219_krp,-4.121995547491840_krp,&
          -3.531972877137678_krp,-2.979991207704598_krp,-2.453552124512838_krp,&
          -1.944962949186254_krp,-1.448934250650732_krp,-0.961499634418369_krp,&
          -0.479450707079108_krp,0.000000000000000_krp,0.479450707079108_krp,  &
          0.961499634418369_krp,1.448934250650732_krp,1.944962949186254_krp,   &
          2.453552124512838_krp,2.979991207704598_krp,3.531972877137678_krp,   &
          4.121995547491840_krp,4.773992343411219_krp,5.550351873264678_krp/)
      wtt=(/0.000000000000037_krp,0.000000000088186_krp,0.000000025712302_krp, &
            0.000002171884898_krp,0.000074783988673_krp,0.001254982041726_krp, &
            0.011414065837434_krp,0.060179646658907_krp,0.192120324066998_krp, &
            0.381669073613502_krp,0.479023703120178_krp,0.381669073613502_krp, &
            0.192120324066998_krp,0.060179646658907_krp,0.011414065837434_krp, &
            0.001254982041726_krp,0.000074783988673_krp,0.000002171884898_krp, &
            0.000000025712302_krp,0.000000000088186_krp,0.000000000000037_krp/)
    case(22)
      x=(/-5.709201353205264_krp,-4.939834131060176_krp,-4.294312480593161_krp,&
          -3.710701532877805_krp,-3.165265909202137_krp,-2.645637441058172_krp,&
          -2.144233592798535_krp,-1.655874373286423_krp,-1.176713958481245_krp,&
          -0.703686097170007_krp,-0.234179139930991_krp,0.234179139930991_krp, &
          0.703686097170007_krp,1.176713958481245_krp,1.655874373286423_krp,   &
          2.144233592798535_krp,2.645637441058172_krp,3.165265909202137_krp,   &
          3.710701532877805_krp,4.294312480593161_krp,4.939834131060176_krp,   &
          5.709201353205264_krp/)
      wtt=(/0.000000000000006_krp,0.000000000017443_krp,0.000000005966991_krp, &
            0.000000588428756_krp,0.000023655128553_krp,0.000464885050884_krp, &
            0.004978399335052_krp,0.031140370884424_krp,0.119102360958782_krp, &
            0.286971433246907_krp,0.443545226434959_krp,0.443545226434959_krp, &
            0.286971433246907_krp,0.119102360958782_krp,0.031140370884424_krp, &
            0.004978399335052_krp,0.000464885050884_krp,0.000023655128553_krp, &
            0.000000588428756_krp,0.000000005966991_krp,0.000000000017443_krp, &
            0.000000000000006_krp/)
    case(23)
      x=(/-5.864309498984572_krp,-5.101534610476677_krp,-4.462091173740006_krp,&
          -3.884472708106102_krp,-3.345127159941224_krp,-2.831803787126157_krp,&
          -2.337016211474456_krp,-1.855677037671371_krp,-1.384039585682495_krp,&
          -0.919151465442564_krp,-0.458538350068105_krp,0.000000000000000_krp, &
          0.458538350068105_krp,0.919151465442564_krp,1.384039585682495_krp,   &
          1.855677037671371_krp,2.337016211474456_krp,2.831803787126157_krp,   &
          3.345127159941224_krp,3.884472708106102_krp,4.462091173740006_krp,   &
          5.101534610476677_krp,5.864309498984572_krp/)
      wtt=(/0.000000000000001_krp,0.000000000003408_krp,0.000000001359630_krp, &
            0.000000155533933_krp,0.000007249295918_krp,0.000165561699142_krp, &
            0.002069567874961_krp,0.015207084004484_krp,0.068890289429087_krp, &
            0.198644898578023_krp,0.372143824877565_krp,0.458196585593213_krp, &
            0.372143824877565_krp,0.198644898578023_krp,0.068890289429087_krp, &
            0.015207084004484_krp,0.002069567874961_krp,0.000165561699142_krp, &
            0.000007249295918_krp,0.000000155533933_krp,0.000000001359630_krp, &
            0.000000000003408_krp,0.000000000000001_krp/)
    case default
      call raise_error(myName // "point number > 23 is not supported")
    end select
  endsubroutine

  !-----------------------------------------------------------------------------
  !> gauss laguerre integration. do not use n > 8 for that the error is large
  !>
  !> @param n - in, # of gauss points
  !> @param x - inout, absciassaes
  !> @param wtt - inout, weights
  !-----------------------------------------------------------------------------
  subroutine gauss_laguerre_integral_define(n,x,wtt)
    charSt,parameter :: myName = &
      'math => gauss_laguerre_integral_define - '
    intknd, intent(in) :: n
    realkd, intent(inout) :: x(n)
    realkd, intent(inout) :: wtt(n)

    select case(n)
    case(8)
      x=(/0.170279632305101_krp,0.903701776799380_krp,2.251086629866130_krp,   &
          4.266700170287658_krp,7.045905402393464_krp,10.758516010180998_krp,  &
          15.740678641278004_krp,22.863131736889265_krp/)
      wtt=(/0.369188589341638_krp,0.418786780814343_krp,0.175794986637172_krp, &
            0.033343492261216_krp,0.002794536235226_krp,0.000090765087734_krp, &
            0.000000848574672_krp,0.000000001048001_krp/)
    case(12)
      x=(/0.115722117358020_krp,0.611757484515131_krp,1.512610269776417_krp,   &
          2.833751337743506_krp,4.599227639418351_krp,6.844525453115177_krp,   &
          9.621316842456873_krp,13.006054993306348_krp,17.116855187462257_krp, &
          22.151090379397015_krp,28.487967250983992_krp,37.099121044466919_krp/)
      wtt=(/0.264731371055443_krp,0.377759275873138_krp,0.244082011319878_krp, &
            0.090449222211681_krp,0.020102381154634_krp,0.002663973541865_krp, &
            0.000203231592663_krp,0.000008365055857_krp,0.000000166849388_krp, &
            0.000000001342391_krp,0.000000000003062_krp,0.000000000000001_krp/)
    case default
      call raise_error(myName // "point number > 8 is not supported")
    end select
  endsubroutine
  !-----------------------------------------------------------------------------
  !> calculate eigenvalues and eigenvectors of a matrix
  !-----------------------------------------------------------------------------
  subroutine eigen_matrix(n, A, r, i, v)
    charSt,parameter :: myName = &
      'math => eigen_matrix - '
    intknd, intent(in) :: n        ! rank
    realkd, intent(in) :: A(n, n)  ! matrix
    realkd, optional :: r(n)       ! real part of eigenvalues
    realkd, optional :: i(n)       ! imaginary part of eigenvalues
    realkd, optional :: v(n, n)    ! eigenvectors

    intknd :: matz
    intknd,allocatable :: iv1(:)
    realkd,allocatable :: fv1(:)
    realkd,allocatable :: A_bar(:, :)
    realkd,allocatable :: v_bar(:, :)
    realkd,allocatable :: r_bar(:)
    realkd,allocatable :: i_bar(:)
    intknd :: ierr
    
    allocate(iv1(n))
    allocate(fv1(n))
    allocate(A_bar(n,n))
    allocate(v_bar(n,n))
    allocate(r_bar(n))
    allocate(i_bar(n))
    
    A_bar(:,:) = A(:,:)
    if (present(v)) then
      matz = 1
    else
      matz = 0
    end if
    call rg(n, A_bar, r_bar, i_bar, matz, v_bar, iv1, fv1, ierr)
    if (ierr /= 0) then
      call raise_error(myname // 'error to calculate eigenvalues ' // &
        'and eigenvectors')
    end if
    if (present(r)) then
       r(:) = r_bar(:)
    end if
    if (present(i)) then
      i(:) = i_bar(:)
    end if
    if (present(v)) then
      v(:, :) = v_bar(:, :)
    end if

  end subroutine eigen_matrix

  !-----------------------------------------------------------------------------
  !> get root of polynomial. This algorithm is the same with that of Matlab
  !! function 'roots'.
  !-----------------------------------------------------------------------------
  subroutine get_roots(n, c, r, i)
    intknd, intent(in) :: n      ! order of polynomial
    realkd, intent(in) :: c(n+1) ! coefficients of polynomial
    realkd, optional :: r(n)     ! roots of polynomial (real)
    realkd, optional :: i(n)     ! roots of polynomial (imaginary)

    intknd :: j
    realkd :: A(n, n)

    A(:, :) = 0.0_krp
    do j = 1, n-1
      A(j+1, j) = 1.0_krp
    end do
    A(1, :) = -c(2:n+1) / c(1)
    call eigen_matrix(n, A, r, i)

  end subroutine get_roots

  subroutine output_dim2(matrix,name)
    implicit none
    realkd,intent(in) :: matrix(:,:)
    character(len=20),intent(in) :: name

    ! local
    intknd :: m,n
    intknd :: i
    character(len=20) :: for='(?????(1x,f7.4))'
    m = size(matrix,1)
    n = size(matrix,2)

    write(FOR(2:6),'(I5)')N
    !open(unit=100,file='output.txt')
    write(200,*)'---------------output '//trim(name)//' ----------------------'
    do i=1,m
      write(200,FMT=FOR)matrix(i,:)
    end do
    write(200,*)
  return

  end subroutine output_dim2

  !-----------------------------------------------------------------------------
  !> conjugate gradient method solver.
  !>
  !> @param n - in, matrix order
  !> @param a,b - in, matrix and vector
  !> @param, x - inout, unknown vector
  !> @param eps - in, precision
  !-----------------------------------------------------------------------------
  subroutine conjugate_grad(n,a,x,b,eps)
    intknd, intent(in) :: n
    realkd, intent(in) :: a(n,n),b(n)
    realkd, intent(inout) :: x(n)
    realkd, intent(in) :: eps
    realkd :: r_this(n),r_next(n),x_this(n),x_next(n),d_this(n),d_next(n)
    realkd :: ad(n),alpha,beta
    intknd :: k

    x_this = x
    r_this = b-matmul(a,x_this)
    d_this = r_this
    do k = 0,n+5
      ad = matmul(a,d_this)
      alpha = sum(r_this(:)*r_this(:))/sum(d_this(:)*ad(:))
      x_next = x_this+alpha*d_this
      r_next = b-matmul(a,x_next)
      if (maxval(r_next) <= eps .or. k == n+5)then
        x = x_next
        return
      else
        beta = sum(r_next(:)*r_next(:))/sum(r_this(:)*r_this(:))
        d_next = r_next+beta*d_this
        r_this = r_next
        d_this = d_next
        x_this = x_next
      endif
    enddo
  endsubroutine

!-------------------------------------------------------------------------------
!> endf general interpolation method
!-------------------------------------------------------------------------------
  function interp_n(x1,y1,x2,y2,x,method) result(y)
    realkd, intent(in) :: x1,y1,x2,y2,x
    intknd, intent(in) :: method
    realkd :: y

    select case(method)
    case(1)
      !> y is constant
      y = y1
    case(2)
      !> y is linear in x
      if (y1 == y2) then
        y = y1
      else
        y = y1+(x-x1)*(y2-y1)/(x2-x1)
      endif
    case(3)
      !> y is linear in ln(x)
      y = y1 +log(x/x1)*(y2-y1)/log(x2/x1)
    case(4)
      !> ln(y) is linear in x
      if (y1 == 0)then
        y = y1
      else
        y = y1*exp((x-x1)*log(y2/y1)/(x2-x1))
      endif
    case(5)
      !> ln(y) is linear in ln(x)
      if (y1 == 0)then
        y = y1
      else
        y = y1*exp(log(x/x1)*log(y2/y1)/log(x2/x1))
      endif
    case(6)
      !> Charged particle interpolation
    case(7)
      !> log10(y) is linear in log10(x)
      if (y1 == 0)then
        y = y1
      else
        y = y1*10*(log10(x/x1)*log10(y2/y1)/log10(x2/x1))
      endif
    end select
  endfunction

!-------------------------------------------------------------------------------
!> generate legendre polynomial at x by recursion
!-------------------------------------------------------------------------------
  function legndr(x,np) result(le)
    realkd, allocatable :: p(:)
    realkd :: g,h,le,x
    intknd :: i,np,m1

    if (np < 1)then
      le = 1.0_krp
      return
    endif
    if (np < 2)then
      le = x
      return
    endif
    allocate(p(np+1))
    p(1) = 1.0_krp
    p(2) = x
    m1 = np-1
    do i = 1,m1
      g = x*p(i+1)
      h = g-p(i)
      p(i+2) = h+g-h/(i+1)
    enddo
    le = p(np+1)
  endfunction

!-------------------------------------------------------------------------------
!> compute the exponential integral, using gauss numericial integral
!-------------------------------------------------------------------------------
  function f_e1(z) result(value)
    realkd, intent(in) :: z
    realkd :: value
    realkd :: a = 0.0_krp,d = 0.1_krp
    realkd :: b
    intknd :: n = 25
    intknd :: i,j
    realkd :: x(25),wtt(25)

    value = 0.0_krp
    do j = 1,10
      b = a+d
      call gauss_legendre_integral_define(n, x, wtt, a, b)
      do i = 1,25
        value = value+wtt(i)*exp(-z/x(i))/x(i)
      enddo
      a = b
    enddo
  endfunction

!-------------------------------------------------------------------------------
!> evaluate the incomplete gamma function, using the power series expansion
!-------------------------------------------------------------------------------
  function gami(s,x) result(value)
    realkd, intent(in) :: s,x
    realkd :: value

    intknd :: n = 32,loop_n
    realkd :: m_value = 1.0_krp,kum_seire = 1._krp
    value = 0.0_krp
    do loop_n = 1,n
      kum_seire = kum_seire*x/(s+loop_n)
      m_value = m_value+kum_seire
    enddo
    value = x**s*exp(-x)*m_value/s
  endfunction

!-------------------------------------------------------------------------------
!> Heaviside step function
!-------------------------------------------------------------------------------
  function heav(x) result(val)
    realkd, intent(in) :: x
    realkd :: val

    if( x > 0.0_krp )then
      val = 1.0_krp
    elseif (x == 0.0_krp)then
      val = 0.5_krp
    else
      val = 0.0_krp
    endif
  endfunction

!-------------------------------------------------------------------------------
!> Hn(a,b) function
!-------------------------------------------------------------------------------
  function hnab(n,a,b) result(value)
    intknd, intent(in) :: n
    realkd, intent(in) :: a,b
    realkd :: value
    realkd, allocatable :: h(:)

    intknd :: loop_n

    select case(n)
    case(0)
      value = (erfc(a)-erfc(b))*0.5_krp
    case(1)
      value = (exp(-a*a)-exp(-b*b))/sqrtpi*0.5_krp
    case default
      allocate(h(0:n))
      h(0) = (erfc(a)-erfc(b))*0.5_krp
      h(1) = (exp(-a*a)-exp(-b*b))/sqrtpi*0.5_krp
      do loop_n = 2,n
        h(loop_n) = (n-1.0_krp)*0.5_krp*h(loop_n-2)+a**(loop_n-1)*h(1)
      enddo
      value = h(n)
    end select
  endfunction

  ! gaussian elimination
  subroutine gaussian(N,A,b)
    implicit none
    intknd,intent(in) :: N
    realkd,allocatable,intent(in) :: A(:,:)
    realkd,allocatable,intent(inout) :: b(:)

    intknd :: i,k
    intknd :: id_max
    realkd,allocatable :: Aup(:,:)
    realkd,allocatable :: Ab(:,:)
    realkd :: vtemp1(N+1),vtemp2(N+1)
    realkd :: elmax,temp

    allocate(Aup(N,N))
    allocate(Ab(N,N+1))

    Ab(1:N,1:N) = A
    Ab(:,N+1) = b

    do k = 1,N-1
      elmax = abs(Ab(k,k))
      id_max = k

      do i = k+1,n
        if(abs(Ab(i,k)) > elmax) then
          elmax = Ab(i,k)
          id_max = i
        endif
      enddo

      vtemp1 = Ab(k,:)
      vtemp2 = Ab(id_max,:)

      Ab(k,:) = vtemp2
      Ab(id_max,:) = vtemp1

      do i = k+1,N
        temp = Ab(i,k)/Ab(k,k)
        Ab(i,:) = Ab(i,:)-temp*Ab(k,:)
      enddo
    enddo


  Aup(:,:) = Ab(1:N,1:N)
  b(:) = Ab(:,N+1)

    call uptri(n,Aup,b)

  endsubroutine gaussian

  subroutine uptri(n,A,b)
    implicit none
    intknd,intent(in) :: N
    realkd,allocatable,intent(in)  :: A(:,:)
    realkd,allocatable,intent(inout) :: b(:)

    intknd :: i,j

    b(N) = b(N)/A(N,N)

    do i = n-1,1,-1
      b(i) = b(i)
      do j = i+1,n
        b(i) = b(i)-A(i,j)*b(j)
      enddo
      b(i) = b(i)/A(i,i)
    enddo

  endsubroutine uptri

  !gauss iteration method
  subroutine gauss(N,A,b)
    implicit none
    realkd,allocatable,intent(in) :: A(:,:)
    realkd,allocatable,intent(inout) :: b(:)
    intknd,intent(in) :: N
    ! local
    intknd::i,j,k
    realkd :: s,dx2
    realkd,allocatable :: x1(:),x2(:)

    allocate(x1(N),x2(N))
    x1=0.
    x2=x1
    do k=1,2000
       do i=1,N
         s=0.
         do j=1,N
           if (j<i) then
            s=s+A(i,j)*x2(j)
           else if (j>i) then
           s=s+A(i,j)*x1(j)
           end if
         end do
         x2(i)=(b(i)-s)/A(i,i)
       end do

       dx2=0.
       do i=1,N
        dx2=dx2+(x1(i)-x2(i))**2
       end do
       dx2=sqrt(dx2)

       if (dx2 < 1.0E-6)  exit
       x1=x2
    end do
    b=x2
    deallocate(x1,x2)

  end subroutine gauss

  ! GMRES
  subroutine atx_cr ( n, nz_num, ia, ja, a, x, w )
    implicit none
    intknd,intent(in) :: n          !the order of the system
    intknd,intent(in) :: nz_num     !the number of nonzeros
    intknd,intent(in) :: ia(n+1)    !row indices of the matrix values
    intknd,intent(in) :: ja(nz_num) !column indices of the matrix values
    realkd,intent(in) :: a(nz_num)     !the matrix values
    realkd,intent(in) :: x(n)          !the vector to be multiplied by A'
    realkd,intent(out) :: w(n)         !the value of A'*X

    ! local
    intknd i,k,k1,k2
    w(1:n) = 0.0D+00
    do i = 1, n
      k1 = ia(i)
      k2 = ia(i+1) - 1
      w(ja(k1:k2)) = w(ja(k1:k2)) + a(k1:k2) * x(i)
    end do
    return
  endsubroutine atx_cr

  subroutine atx_st ( n,nz_num,ia,ja,a,x,w)
    implicit none
    intknd,intent(in) :: n           !the order of the system
    intknd,intent(in) :: nz_num      !the number of nonzeros
    intknd,intent(in) :: ia(nz_num)  !row indices of the matrix values
    intknd,intent(in) :: ja(nz_num)  !column indices of the matrix values
    realkd,intent(in) :: a(nz_num)      !the matrix values
    realkd,intent(in) :: x(n)           !the vector to be multiplied by A'
    realkd,intent(out) :: w(n)          !the value of A'*X

    ! local
    intknd i
    intknd j
    intknd k

    w(1:n) = 0.0D+00
    do k = 1, nz_num
      i = ia(k)
      j = ja(k)
      w(j) = w(j) + a(k) * x(i)
    end do
    return
  endsubroutine atx_st

  subroutine ax_cr (n,nz_num,ia,ja,a,x,w)
    implicit none
    intknd,intent(in) :: n          !the order of the system
    intknd,intent(in) :: nz_num     !the number of nonzeros
    intknd,intent(in) :: ia(n+1)    !row indices of the matrix values
    intknd,intent(in) :: ja(nz_num) !column indices of the matrix values
    realkd,intent(in)    :: a(nz_num)  !the matrix values
    realkd,intent(in)    :: x(n)       !the vector to be multiplied by A
    realkd,intent(out)   :: w(n)       !the value of A*X

    ! local
    intknd i,k,k1,k2

    w(1:n) = 0.0D+00
    do i = 1, n
      k1 = ia(i)
      k2 = ia(i+1) - 1
      w(i) = w(i) + dot_product ( a(k1:k2), x(ja(k1:k2)) )
    end do

    return
  endsubroutine ax_cr

  subroutine ax_st ( n, nz_num, ia, ja, a, x, w )
    implicit none
    intknd,intent(in) :: n            !the order of the system
    intknd,intent(in) :: nz_num       !the number of nonzeros
    intknd,intent(in) :: ia(nz_num)   !row indices of the matrix values
    intknd,intent(in) :: ja(nz_num)   !column indices of the matrix values
    realkd,intent(in) :: a(nz_num)       !the matrix values
    realkd,intent(in) :: x(n)            !the vector to be multiplied by A
    realkd,intent(out) :: w(n)           !the value of A*X

    ! local
    intknd :: i,j,k

    w(1:n) = 0.0D+00
    do k = 1, nz_num
      i = ia(k)
      j = ja(k)
      w(i) = w(i) + a(k) * x(j)
    end do
    return
  endsubroutine ax_st

  subroutine diagonal_pointer_cr ( n, nz_num, ia, ja, ua )
    implicit none
    intknd,intent(in) :: n
    intknd,intent(in) :: nz_num
    intknd,intent(in) :: ia(n+1)
    intknd,intent(in) :: ja(nz_num)
    intknd,intent(inout) :: ua(n)

    ! local
    intknd i,k
    ua(1:n) = -1

    do i = 1, n
      do k = ia(i), ia(i+1) - 1
        if ( ja(k) == i ) then
          ua(i) = k
        end if
      end do
    end do

    return
  endsubroutine diagonal_pointer_cr

  subroutine ilu_cr ( n, nz_num, ia, ja, a, ua, l )
    implicit none

    intknd,intent(in) :: n                     !the order of the system
    intknd,intent(in) :: nz_num                !the number of nonzeros
    intknd,intent(in) :: ia(n+1)               !row indices of the matrix values
    intknd,intent(in) :: ja(nz_num)            !column indices of the matrix values
    realkd,intent(in) :: a(nz_num)         !the matrix values
    intknd,intent(inout) :: ua(n)  !the index of the diagonal element of each row
    realkd,intent(out) :: l(nz_num)               !the ILU factorization of A

    ! local
    intknd i,j,k,jj,jw,jrow
    intknd iw(n)

    realkd tl

  !
  !  Copy A.
  !
    l(1:nz_num) = a(1:nz_num)

    do i = 1, n
  !
  !  IW points to the nonzero entries in row I.
  !
      iw(1:n) = -1

      do k = ia(i), ia(i+1) - 1
        iw(ja(k)) = k
      end do

      do j = ia(i), ia(i+1) - 1
        jrow = ja(j)
        if ( i <= jrow ) then
          exit
        end if
        tl = l(j) * l(ua(jrow))
        l(j) = tl
        do jj = ua(jrow) + 1, ia(jrow+1) - 1
          jw = iw(ja(jj))
          if ( jw /= -1 ) then
            l(jw) = l(jw) - tl * l(jj)
          end if
        end do
      end do

      ua(i) = j

      if ( jrow /= i ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ILU_CR - Fatal error!'
        write ( *, '(a)' ) '  JROW ~= I'
        write ( *, '(a,i8)' ) '  JROW = ', jrow
        write ( *, '(a,i8)' ) '  I    = ', i
        stop
      end if

      if ( l(j) == 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ILU_CR - Fatal error!'
        write ( *, '(a,i8)' ) '  Zero pivot on step I = ', i
        write ( *, '(a,i8,a)' ) '  L(', j, ') = 0.0'
        stop
      end if

      l(j) = 1.0D+00 / l(j)

    end do

    do i = 1,n
      l(ua(i)) = 1.0_krp / l(ua(i))
    enddo

    !l(ua(1:n)) = 1.0D+00 / l(ua(1:n))

    return
  endsubroutine ilu_cr

  subroutine lus_cr ( n, nz_num, ia, ja, l, ua, r, z )
    implicit none
    intknd,intent(in) :: n                 !the order of the system
    intknd,intent(in) :: nz_num            !the number of nonzeros
    intknd,intent(in) :: ia(n+1)           !row indices of the matrix values
    intknd,intent(in) :: ja(nz_num)        !column indices of the matrix values
    realkd,intent(in) :: l(nz_num)            !the matrix values
    intknd,intent(in) :: ua(n) !the index of the diagonal element of each row
    !realkd,allocatable,intent(in)  :: r(:)                !the right hand side
    !realkd,allocatable,intent(out) :: z(:)                !the solution of the system M * Z = R
    realkd,intent(in)  :: r(n)
    realkd,intent(out) :: z(n)

    intknd i,j
    realkd,allocatable :: w(:)

    !allocate(r(n))
    !allocate(z(n))
    allocate(w(n))

  !
  !  Copy R in.
  !
    !write(*,*)'n = ',n
    !write(*,*)'nz_num = ',nz_num

    w(1:n) = r(1:n)
  !
  !  Solve L * w = w where L is unit lower triangular.
  !
    do i = 2, n
      do j = ia(i), ua(i) - 1
        w(i) = w(i) - l(j) * w(ja(j))
      end do
    end do
  !
  !  Solve U * w = w, where U is upper triangular.
  !
    do i = n, 1, -1
      do j = ua(i) + 1, ia(i+1) - 1
        w(i) = w(i) - l(j) * w(ja(j))
      end do
      w(i) = w(i) / l(ua(i))
    end do
  !
  !  Copy Z out.
  !
    z(1:n) = w(1:n)

    return
  endsubroutine lus_cr

  subroutine mgmres_st ( n, nz_num, ia, ja, a, x, rhs, itr_max, mr, tol_abs, &
    tol_rel )
    implicit none
    intknd,intent(in) :: n              !order of the linear system
    intknd,intent(in) :: nz_num         !number of nonzero matrix values
    intknd,intent(in) :: ia(nz_num)     !row indices of the matrix values
    intknd,intent(in) :: ja(nz_num)     !column indices of the matrix values
    realkd,intent(in) :: a(nz_num)         !matrix values
    realkd,intent(inout) :: x(1:n)         !in-an approximation to the solution out-an improved approximation
    realkd,intent(in) :: rhs(1:n)          !right hand side of the linear system
    intknd,intent(in) :: itr_max        !maximum number of (outer) iterations to take
    intknd,intent(in) :: mr             !maximum number of (inner) iterations to take 0 < MR <= N
    realkd,intent(in) :: tol_abs           !an absolute tolerance applied to the current residual
    realkd,intent(in) :: tol_rel           !a relative tolerance comparing the current residual to the initial residual

    ! local
    realkd av
    realkd c(1:mr)
    realkd, parameter :: delta = 1.0D-03
    realkd g(1:mr+1)
    realkd h(1:mr+1,1:mr)
    realkd htmp
    intknd i,j,k,itr,itr_used,k_copy
    realkd mu
    realkd r(1:n)
    realkd rho
    realkd rho_tol
    realkd s(1:mr)
    realkd v(1:n,1:mr+1)
    logical, parameter :: verbose = .true.
    realkd :: y(1:mr+1)

    itr_used = 0
    if ( n < mr ) then
      write ( 10, '(a)' ) ' '
      write ( 10, '(a)' ) 'MGMRES_ST - Fatal error!'
      write ( 10, '(a)' ) '  N < MR.'
      write ( 10, '(a,i8)' ) '  N = ', n
      write ( 10, '(a,i8)' ) '  MR = ', mr
      stop
    end if
    do itr = 1, itr_max
      call ax_st ( n, nz_num, ia, ja, a, x, r )
      r(1:n) = rhs(1:n) - r(1:n)
      rho = sqrt ( dot_product ( r(1:n), r(1:n) ) )
      if ( verbose ) then
        write ( 10, '(a,i8,a,g14.6)' ) '  ITR = ', itr, '  Residual = ', rho
      end if
      if ( itr == 1 ) then
        rho_tol = rho * tol_rel
      end if
      v(1:n,1) = r(1:n) / rho
      g(1) = rho
      g(2:mr+1) = 0.0D+00
      h(1:mr+1,1:mr) = 0.0D+00
      do k = 1, mr
        k_copy = k
        call ax_st ( n, nz_num, ia, ja, a, v(1:n,k), v(1:n,k+1) )
        av = sqrt ( dot_product ( v(1:n,k+1), v(1:n,k+1) ) )
        do j = 1, k
          h(j,k) = dot_product ( v(1:n,k+1), v(1:n,j) )
          v(1:n,k+1) = v(1:n,k+1) - h(j,k) * v(1:n,j)
        end do
        h(k+1,k) = sqrt ( dot_product ( v(1:n,k+1), v(1:n,k+1) ) )
        if ( av + delta * h(k+1,k) == av ) then
          do j = 1, k
            htmp = dot_product ( v(1:n,k+1), v(1:n,j) )
            h(j,k) = h(j,k) + htmp
            v(1:n,k+1) = v(1:n,k+1) - htmp * v(1:n,j)
          end do
          h(k+1,k) = sqrt ( dot_product ( v(1:n,k+1), v(1:n,k+1) ) )
        end if
        if ( h(k+1,k) /= 0.0D+00 ) then
          v(1:n,k+1) = v(1:n,k+1) / h(k+1,k)
        end if
        if ( 1 < k ) then
          y(1:k+1) = h(1:k+1,k)
          do j = 1, k - 1
            call mult_givens ( c(j), s(j), j, y(1:k+1) )
          end do
          h(1:k+1,k) = y(1:k+1)
        end if

        mu = sqrt ( h(k,k)**2 + h(k+1,k)**2 )
        c(k) = h(k,k) / mu
        s(k) = -h(k+1,k) / mu
        h(k,k) = c(k) * h(k,k) - s(k) * h(k+1,k)
        h(k+1,k) = 0.0D+00
        call mult_givens ( c(k), s(k), k, g(1:k+1) )
        rho = abs ( g(k+1) )

        itr_used = itr_used + 1
        if ( verbose ) then
          write ( 10, '(a,i8,a,g14.6)' ) '  K =   ', k, '  Residual = ', rho
        end if
        if ( rho <= rho_tol .and. rho <= tol_abs ) then
          exit
        end if

      end do
      k = k_copy - 1
      y(k+1) = g(k+1) / h(k+1,k+1)
      do i = k, 1, -1
        y(i) = ( g(i) - dot_product ( h(i,i+1:k+1), y(i+1:k+1) ) ) / h(i,i)
      end do

      do i = 1, n
        x(i) = x(i) + dot_product ( v(i,1:k+1), y(1:k+1) )
      end do

      if ( rho <= rho_tol .and. rho <= tol_abs ) then
        exit
      end if

    end do

    if ( verbose ) then
      write ( 10, '(a)'       ) ' '
      write ( 10, '(a)'       ) 'MGMRES_ST:'
      write ( 10, '(a,i8)'    ) '  Iterations = ', itr_used
      write ( 10, '(a,g14.6)' ) '  Final residual = ', rho
    end if

    return
  endsubroutine mgmres_st

  subroutine mult_givens ( c, s, k, g )
    implicit none

    realkd,intent(in) :: c           !sine of a Givens rotation
    realkd,intent(in) :: s           !sine of a Givens rotation
    intknd,intent(in) :: k        !the location of the first vector entry
    realkd,intent(inout) :: g(1:k+1) !in-the vector to be modified
                                           !out-the Givens rotation has been applied to entries G(K) and G(K+1)

    ! local
    realkd g1
    realkd g2

    g1 = c * g(k) - s * g(k+1)
    g2 = s * g(k) + c * g(k+1)

    g(k)   = g1
    g(k+1) = g2

    return
  endsubroutine mult_givens

  subroutine pmgmres_ilu_cr(n,nz_num,ia,ja,a,x,rhs,itr_max,mr, &
    tol_abs,tol_rel)
    implicit none

    intknd,intent(in) :: n                   !order of the linear system
    intknd,intent(in) :: nz_num              !number of nonzero matrix values
    intknd,intent(in) :: ia(n+1)   !row indices of the matrix values
    !intknd,intent(inout) :: ja(nz_num)      !column indices of the matrix values
    intknd,intent(inout) :: ja(nz_num)!column indices of the matrix values
    realkd,intent(inout) :: a(nz_num)    !matrix values
    realkd,intent(inout) ::  x(n)   !in-an approximation to the solution out-an improved approximation
    realkd,intent(inout) :: rhs(n)  !right hand side of the linear system
    intknd,intent(in) :: itr_max             !the maximum number of (outer) iterations to take
    intknd,intent(in) :: mr                  !the maximum number of (inner) iterations to take.  MR must be less than N
    realkd,intent(in) :: tol_abs                !an absolute tolerance applied to the current residual
    realkd,intent(in) :: tol_rel                !a relative tolerance comparing the current residual to the initial residual

    ! local
    realkd :: av = 0.0_krp
    realkd :: c(mr+1)
    realkd, parameter :: delta = 1.0D-03
    realkd :: g(mr+1)
    realkd :: h(mr+1,mr)
    realkd :: htmp = 0.0_krp
    intknd :: i = 0
    intknd :: itr = 0
    intknd :: itr_used = 0
    intknd :: j = 0
    intknd :: k = 0
    intknd :: k_copy = 0

    logical, parameter :: verbose = .false.
    realkd,allocatable :: l(:)
    realkd,allocatable :: r(:)
    realkd,allocatable :: s(:)
    intknd,allocatable :: ua(:)
    realkd,allocatable :: v(:,:)
    realkd,allocatable :: v_tmp(:)
    realkd,allocatable :: y(:)

    realkd :: mu = 0.0_krp
    realkd :: rho = 0.0_krp
    realkd :: rho_tol = 0.0_krp

    allocate(l(nz_num+1))
    allocate(r(n))
    allocate(s(mr+1))
    allocate(ua(n))
    allocate(v(n,mr+1))
    allocate(v_tmp(n))
    allocate(y(mr+1))
    l = 0.0_krp
    r = 0.0_krp
    s = 0.0_krp
    ua = 0
    v = 0.0_krp
    v_tmp = 0.0_krp
    y = 0.0_krp

    call rearrange_cr ( n, nz_num, ia, ja, a )
    call diagonal_pointer_cr ( n, nz_num, ia, ja, ua )
    call ilu_cr ( n, nz_num, ia, ja, a, ua, l ) !bug
    if ( verbose ) then
      write ( 10, '(a)' ) ' '
      write ( 10, '(a)' ) 'PMGMRES_ILU_CR'
      write ( 10, '(a,i4)' ) '  Number of unknowns = ', n
    end if

    do itr = 1, itr_max
      call ax_cr ( n, nz_num, ia, ja, a, x, r )
      r(1:n) = rhs(1:n) - r(1:n)
      call lus_cr ( n, nz_num, ia, ja, l, ua, r, r )
      rho = sqrt ( dot_product ( r, r ) )
      if ( verbose ) then
        write ( 10, '(a,i4,a,g14.6)' ) '  ITR = ', itr, '  Residual = ', rho
      end if
      if ( itr == 1 ) then
        rho_tol = rho * tol_rel
      end if
      v(1:n,1) = r(1:n) / rho
      g(1) = rho
      g(2:mr+1) = 0.0D+00

      h(1:mr+1,1:mr) = 0.0D+00
      do k = 1, mr
        k_copy = k
        call ax_cr ( n, nz_num, ia, ja, a, v(1:n,k), v(1:n,k+1) )
        !v_tmp(1:n) =  v(1:n,k+1)
        call lus_cr ( n, nz_num, ia, ja, l, ua, v(1:n,k+1), v(1:n,k+1) )
        !call lus_cr ( n, nz_num, ia, ja, l, ua, v_tmp, v_tmp )
        !v(1:n,k+1) = v_tmp(1:n)
        av = sqrt ( dot_product ( v(1:n,k+1), v(1:n,k+1) ) )
        do j = 1, k
          h(j,k) = dot_product ( v(1:n,k+1), v(1:n,j) )
          v(1:n,k+1) = v(1:n,k+1) - v(1:n,j) * h(j,k)
        end do
        h(k+1,k) = sqrt ( dot_product ( v(1:n,k+1), v(1:n,k+1) ) )
        if ( ( av + delta * h(k+1,k)) == av ) then
          do j = 1, k
            htmp = dot_product ( v(1:n,k+1), v(1:n,j) )
            h(j,k) = h(j,k) + htmp
            v(1:n,k+1) = v(1:n,k+1) - htmp * v(1:n,j)
          end do
          h(k+1,k) = sqrt ( dot_product ( v(1:n,k+1), v(1:n,k+1) ) )
        end if
        if ( h(k+1,k) /= 0.0D+00 ) then
          v(1:n,k+1) = v(1:n,k+1) / h(k+1,k)
        end if
        if ( 1 < k ) then
          y(1:k+1) = h(1:k+1,k)
          do j = 1, k - 1
            call mult_givens ( c(j), s(j), j, y )
          end do
          h(1:k+1,k) = y(1:k+1)
        end if
        mu = sqrt ( h(k,k)**2 + h(k+1,k)**2 )
        c(k) = h(k,k) / mu
        s(k) = -h(k+1,k) / mu
        h(k,k) = c(k) * h(k,k) - s(k) * h(k+1,k)
        h(k+1,k) = 0.0D+00
        call mult_givens ( c(k), s(k), k, g )
        rho = abs ( g(k+1) )
        itr_used = itr_used + 1
        if ( verbose ) then
          write ( 10, '(a,i4,a,g14.6)' ) '  K = ', k, '  Residual = ', rho
        end if
        if ( rho <= rho_tol .and. rho <= tol_abs ) then
          exit
        end if

      enddo !k

      k = k_copy - 1
      y(k+1) = g(k+1) / h(k+1,k+1)

      do i = k, 1, -1
        y(i) = ( g(i) - dot_product ( h(i,i+1:k+1), y(i+1:k+1) ) ) / h(i,i)
      end do

      do i = 1, n
        x(i) = x(i) + dot_product ( v(i,1:k+1), y(1:k+1) )
      end do

      if ( rho <= rho_tol .and. rho <= tol_abs ) then
        exit
      end if

    end do

    if ( verbose ) then
      write ( 10, '(a)' ) ' '
      write ( 10, '(a)' ) 'PMGMRES_ILU_CR:'
      write ( 10, '(a,i6)' ) '  Iterations = ', itr_used
      write ( 10, '(a,g14.6)' ) '  Final residual = ', rho
    end if

    deallocate(l)
    deallocate(ua)
    deallocate(r)
    deallocate(v)
    deallocate(v_tmp)
    deallocate(s)
    deallocate(y)
    return
  endsubroutine pmgmres_ilu_cr

  subroutine r8vec_uniform_01 ( n, seed, r )
    implicit none

    intknd,intent(in) :: n        !the number of entries in the vector
    intknd,intent(inout) :: seed  !in-the "seed" value, which should NOT be 0 out-updated
    realkd,intent(out) :: r(n)       !the vector of pseudorandom values

    ! local
    intknd i
    intknd k

    if ( seed == 0 ) then
      write ( 10, '(a)' ) ' '
      write ( 10, '(a)' ) 'R8VEC_UNIFORM_01 - Fatal error!'
      write ( 10, '(a)' ) '  Input value of SEED = 0.'
      stop
    end if

    do i = 1, n
      k = seed / 127773
      seed = 16807 * ( seed - k * 127773 ) - k * 2836
      if ( seed < 0 ) then
        seed = seed + 2147483647
      end if
      r(i) = real ( seed, kind = 8 ) * 4.656612875D-10
    end do

    return
  endsubroutine r8vec_uniform_01

  subroutine rearrange_cr ( n, nz_num, ia, ja, a )
    implicit none

    intknd,intent(in) :: n                !the order of the system
    intknd,intent(in) :: nz_num           !the number of nonzeros
    intknd,intent(in) :: ia(n+1)          !the compressed row indices
    intknd,intent(inout) :: ja(nz_num)    !the column indices out-rearranged by the sorting
    realkd,intent(inout) :: a(nz_num) !the matrix values out-moved somewhat because of the sorting

    ! local
    intknd i
    intknd i4temp
    intknd k
    intknd l
    realkd r8temp

    do i = 1, n
      do k = ia(i), ia(i+1) - 2
        do l = k + 1, ia(i+1) - 1
          if ( ja(l) < ja(k) ) then
            i4temp = ja(l)
            ja(l)  = ja(k)
            ja(k)  = i4temp

            r8temp = a(l)
            a(l)   = a(k)
            a(k)   = r8temp
          end if
        end do
      end do
    end do
    return
  endsubroutine rearrange_cr

  !------------------------------------------------------------------------------
  !> Lagrange interpolation method
  !------------------------------------------------------------------------------
  subroutine lagrange_interp(n, m, x, x0, c, i)
    charSt,parameter :: myName = 'math => lagrange_interp - '
    intknd, intent(in) :: n
    intknd, intent(in) :: m
    realkd, intent(in) :: x(n)
    realkd, intent(in) :: x0
    realkd, intent(inout) :: c(m)
    intknd, intent(inout) :: i(m)

    intknd :: nerror
    intknd :: i0
    intknd :: j
    intknd :: k
    intknd :: ml
    intknd :: mr

    nerror = get_nError()
    if (m > n) then
      call raise_error(myname // 'm should be <= n')
    end if

    if (nerror == get_nError()) then
      nerror = get_nError()

      i0 = binary_search_real(x, x0)
      if (mod(m, 2) == 0) then
        ml = m / 2 - 1
        mr = ml + 1
      else
        ml = (m - 1) / 2
        mr = ml
      end if
      i0 = min(i0, n - mr)
      i0 = max(i0, ml + 1)
      do j = 1, m
        i(j) = i0 - ml + j - 1
      end do

      c = 1.0_krp
      do k = 0, m - 1
        do j = 0, k - 1
          c(k+1) = c(k+1) * (x0 - x(i(j+1))) / (x(i(k+1)) - x(i(j+1)))
        end do
        do j = k + 1, m - 1
          c(k+1) = c(k+1) * (x0 - x(i(j+1))) / (x(i(k+1)) - x(i(j+1)))
        end do
      end do

    end if

  end subroutine lagrange_interp

  ! ***********************************
  ! * Sort Array x(:) in ascendent order
  ! * if present ipt, a pointer with the
  ! * changes is returned in ipt.
  ! ***********************************
  subroutine qsort(x, ipt)

    ! For a list with isw number of elements or
    ! less use Insrt
    realkd, intent (inout) :: x(:)
    intknd, intent (out), optional :: ipt(:)

    intknd :: i, ipvn, ileft, iright, ispos, ismax
    intknd, allocatable :: iipt(:)
    type (limits), allocatable :: stack(:)

    allocate(stack(size(x)))

    stack(:)%ileft = 0
    if (present(ipt)) then
      forall (i=1:size(ipt)) ipt(i) = i

      ! Iniitialize the stack
      ispos = 1
      ismax = 1
      stack(ispos)%ileft  = 1
      stack(ispos)%iright = size(x)

      do while (stack(ispos)%ileft /= 0)

        ileft = stack(ispos)%ileft
        iright = stack(ispos)%iright
        if (iright-ileft <= isw) then
          call insrtlc(x, ipt, ileft,iright)
          ispos = ispos + 1
        else
          ipvn = choosepiv(x, ileft, iright)
          ipvn = partition(x, ileft, iright, ipvn, ipt)

          stack(ismax+1)%ileft = ileft
          stack(ismax+1) %iright = ipvn-1
          stack(ismax+2)%ileft = ipvn + 1
          stack(ismax+2)%iright = iright
          ispos = ispos + 1
          ismax = ismax + 2
        end if
      end do

    else

      ! Iniitialize the stack
      ispos = 1
      ismax = 1
      stack(ispos)%ileft  = 1
      stack(ispos)%iright = size(x)

      allocate(iipt(10))
      do while (stack(ispos)%ileft /= 0)

        ileft = stack(ispos)%ileft
        iright = stack(ispos)%iright
        if (iright-ileft <= isw) then
          call insrtlc(x, iipt, ileft, iright)
          ispos = ispos + 1
        else
          ipvn = choosepiv(x, ileft, iright)
          ipvn = partition(x, ileft, iright, ipvn)

          stack(ismax+1)%ileft = ileft
          stack(ismax+1) %iright = ipvn-1
          stack(ismax+2)%ileft = ipvn + 1
          stack(ismax+2)%iright = iright
          ispos = ispos + 1
          ismax = ismax + 2
        end if
      end do
      deallocate(iipt)

    end if

    deallocate(stack)

  end subroutine qsort

  ! ***********************************
  ! * Choose a Pivot element from xx(ileft:iright)
  ! * for qsort. This routine chooses the median
  ! * of the first, last and mid element of the
  ! * list.
  ! ***********************************
  intknd function choosepiv(xx, iileft, iiright) Result (iipv)

    realkd, intent (in) :: xx(:)
    intknd, intent (in) :: iileft, iiright

    realkd :: XXcp(3)
    intknd :: iipt(3), iimd

    iimd = int((iileft+iiright)/2)
    XXcp(1) = xx(iileft)
    XXcp(2) = xx(iimd)
    XXcp(3) = xx(iiright)
    iipt = (/1,2,3/)

    call insrtlc(XXcp, iipt, 1, 3)

    select case (iipt(2))
    case (1)
      iipv = iileft
    case (2)
      iipv = iimd
    case (3)
      iipv = iiright
    end select

  end function choosepiv

  ! ***********************************
  ! * Perform an insertion sort of the list
  ! * xx(:) between index values iil and iir.
  ! * iipt(:) returns the permutations
  ! * made to sort.
  ! ***********************************
  subroutine insrtlc(xx, iipt, iil, iir)

    realkd, intent (inout) :: xx(:)
    intknd, intent (inout) :: iipt(:)
    intknd, intent (in) :: iil, iir

    realkd :: rrtmp
    intknd :: ii, jj

    do ii = iil+1, iir
      rrtmp = xx(ii)
      do jj = ii-1, 1, -1
        if (rrtmp < xx(jj)) then
          xx(jj+1) = xx(jj)
          call swap_in(iipt, jj, jj+1)
        else
          exit
        end if
      end do
      xx(jj+1) = rrtmp
    end do

  end subroutine insrtlc

  ! ***********************************
  ! * This routine arranges the array x
  ! * between the index values ileft and iright
  ! * positioning elements smallers than
  ! * x(ipv) at the left and the others
  ! * at the right.
  ! * Internal routine used by qsort.
  ! ***********************************
  intknd function partition(x, ileft, iright, ipv, ipt) Result (ipvfn)

    realkd, intent (inout) :: x(:)
    intknd, intent (in) :: ileft, iright, ipv
    intknd, intent (inout), optional :: ipt(:)

    realkd :: rpv
    intknd :: i

    rpv = x(ipv)
    call swap(x, ipv, iright)
    if (present(ipt)) call swap_in(ipt, ipv, iright)
    ipvfn = ileft

    if (present(ipt))  then
      do i = ileft, iright-1
        if (x(i) <= rpv) then
          call swap(x, i, ipvfn)
          call swap_in(ipt, i, ipvfn)
          ipvfn = ipvfn + 1
        end if
      end do
    else
      do i = ileft, iright-1
        if (x(i) <= rpv) then
          call swap(x, i, ipvfn)
          ipvfn = ipvfn + 1
        end if
      end do
    end if

    call swap(x, ipvfn, iright)
    if (present(ipt)) call swap_in(ipt, ipvfn, iright)

  end function partition

  ! ***********************************
  ! * Swaps elements i and j of array x(:).
  ! ***********************************
  subroutine swap(x, i, j)

    realkd, intent (inout) :: x(:)
    intknd, intent (in) :: i, j

    realkd :: itmp

    itmp = x(i)
    x(i) = x(j)
    x(j) = itmp

  end subroutine swap

  ! ***********************************
  ! * Swaps elements i and j of array x(:).
  ! ***********************************
  subroutine swap_in(x, i, j)

    intknd, intent (inout) :: x(:)
    intknd, intent (in) :: i, j

    intknd :: itmp

    itmp = x(i)
    x(i) = x(j)
    x(j) = itmp

  end subroutine swap_in
   
  function a_table(n,l) result(a)
    intknd,intent(in) :: n
    intknd,intent(in) :: l
    
    realkd :: a

    selectcase(n)
    case(0)
      selectcase(l)
      case(0)
        a = 1.0_krp
      endselect
      
    case(1)
      selectcase(l)
      case(-1)
        a = 1.0_krp
      case(0)
        a = 2.0_krp
      case(1)
        a = 0.5_krp
      endselect
      
    case(2)
      selectcase(l)
      case(-2)
        a = 8.333333333333333E-002_krp
      case(-1)
        a = 0.333333333333333_krp
      case(0)
        a = 2.0_krp
      case(1)
        a = 0.333333333333333_krp
      case(2)
        a = 4.166666666666666E-002_krp
      endselect
      
    case(3)
      selectcase(l)
      case(-3)
        a = 2.777777777777778E-003_krp
      case(-2)
        a = 1.666666666666667E-002_krp
      case(-1)
        a = 0.166666666666667_krp
      case(0)
        a = 2.0_krp
      case(1)
        a = 0.166666666666667_krp
      case(2)
        a = 1.666666666666667E-002_krp
      case(3)
        a = 1.388888888888889E-003_krp
      endselect
      
    case(4)
      selectcase(l)
      case(-4)
        a = 4.960317460317460E-005_krp
      case(-3)
        a = 3.968253968253968E-004_krp
      case(-2)
        a = 5.555555555555556E-003_krp
      case(-1)
        a = 0.1_krp
      case(0)
        a = 2.0_krp
      case(1)
        a = 0.1_krp
      case(2)
        a = 5.555555555555556E-003_krp
      case(3)
        a = 3.968253968253968E-004_krp
      case(4)
        a = 2.480158730158730E-005_krp
      endselect
      
    case(5)
      selectcase(l)
      case(-5)
        a = 5.511463844797178E-007_krp
      case(-4)
        a = 5.511463844797179E-006_krp
      case(-3)
        a = 9.920634920634921E-005_krp
      case(-2)
        a = 2.380952380952381E-003_krp
      case(-1)
        a = 6.666666666666667E-002_krp
      case(0)
        a = 2.0_krp
      case(1)
        a = 6.666666666666667E-002_krp
      case(2)
        a = 2.380952380952381E-003_krp
      case(3)
        a = 9.920634920634921E-005_krp
      case(4)
        a = 5.511463844797179E-006_krp
      case(5)
        a = 2.755731922398589E-007_krp
      endselect
      
    case(6)
      selectcase(l)
      case(-6)
        a = 4.175351397573620E-009_krp
      case(-5)
        a = 5.010421677088344E-008_krp
      case(-4)
        a = 1.102292768959436E-006_krp
      case(-3)
        a = 3.306878306878307E-005_krp
      case(-2)
        a = 1.190476190476191E-003_krp
      case(-1)
        a = 4.761904761904762E-002_krp
      case(0)
        a = 2.0_krp
      case(1)
        a = 4.761904761904762E-002_krp
      case(2)
        a = 1.190476190476191E-003_krp
      case(3)
        a = 3.306878306878307E-005_krp
      case(4)
        a = 1.102292768959436E-006_krp
      case(5)
        a = 5.010421677088344E-008_krp
      case(6)
        a = 2.087675698786810E-009_krp
      endselect
      
    case(7)
      selectcase(l)
      case(-7)
        a = 2.294149119545945E-011_krp
      case(-6)
        a = 3.211808767364323E-010_krp
      case(-5)
        a = 8.350702795147240E-009_krp
      case(-4)
        a = 3.006253006253006E-007_krp
      case(-3)
        a = 1.322751322751323E-005_krp
      case(-2)
        a = 6.613756613756613E-004_krp
      case(-1)
        a = 3.571428571428571E-002_krp
      case(0)
        a = 2.0_krp
      case(1)
        a = 3.571428571428571E-002_krp
      case(2)
        a = 6.613756613756613E-004_krp
      case(3)
        a = 1.322751322751323E-005_krp
      case(4)
        a = 3.006253006253006E-007_krp
      case(5)
        a = 8.350702795147240E-009_krp
      case(6)
        a = 3.211808767364323E-010_krp
      case(7)
        a = 1.147074559772972E-011_krp
      endselect
      
    case(8)
      selectcase(l)
      case(-8)
        a = 9.558954664774771E-014_krp
      case(-7)
        a = 1.529432746363963E-012_krp
      case(-6)
        a = 4.588298239091890E-011_krp
      case(-5)
        a = 1.927085260418594E-009_krp
      case(-4)
        a = 1.002084335417669E-007_krp
      case(-3)
        a = 6.012506012506012E-006_krp
      case(-2)
        a = 3.968253968253968E-004_krp
      case(-1)
        a = 2.777777777777778E-002_krp
      case(0)
        a = 2.0_krp
      case(1)
        a = 2.777777777777778E-002_krp
      case(2)
        a = 3.968253968253968E-004_krp
      case(3)
        a = 6.012506012506012E-006_krp
      case(4)
        a = 1.002084335417669E-007_krp
      case(5)
        a = 1.927085260418594E-009_krp
      case(6)
        a = 4.588298239091890E-011_krp
      case(7)
        a = 1.529432746363963E-012_krp
      case(8)
        a = 4.779477332387385E-014_krp
      endselect
      
    case(9)
      selectcase(l)
      case(-9)
        a = 3.123841393717245E-016_krp
      case(-8)
        a = 5.622914508691041E-015_krp
      case(-7)
        a = 1.911790932954954E-013_krp
      case(-6)
        a = 9.176596478183779E-012_krp
      case(-5)
        a = 5.505957886910268E-010_krp
      case(-4)
        a = 3.854170520837188E-008_krp
      case(-3)
        a = 3.006253006253006E-006_krp
      case(-2)
        a = 2.525252525252525E-004_krp
      case(-1)
        a = 2.222222222222222E-002_krp
      case(0)
        a = 2.0_krp
      case(1)
        a = 2.222222222222222E-002_krp
      case(2)
        a = 2.525252525252525E-004_krp
      case(3)
        a = 3.006253006253006E-006_krp
      case(4)
        a = 3.854170520837188E-008_krp
      case(5)
        a = 5.505957886910268E-010_krp
      case(6)
        a = 9.176596478183779E-012_krp
      case(7)
        a = 1.911790932954954E-013_krp
      case(8)
        a = 5.622914508691041E-015_krp
      case(9)
        a = 1.561920696858623E-016_krp
      endselect

    endselect
  
  endfunction a_table

  function factorial(x) result(factor)

    intknd,intent(in) :: x
    realkd :: factor
    intknd :: i
    
    factor = 1
     select case (x)
     case (1)
      factor=1
     case (2)
      factor=2
     case (3)
      factor=6
     case (4)
      factor=24
     case (5)
      factor=120
     case (6)
      factor=720
     case (7)
      factor=5040
     case (8)
      factor=40320
     case (9:)
      do i=2, x
        factor=factor*i
      end do
     end select
  end function factorial

endmodule math
