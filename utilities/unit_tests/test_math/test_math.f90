program test_math
  use utilities
  implicit none

# include <kind_parameter.h>

  charwd :: str_arry(5) = (/'b','c','e','g','j'/)
  intknd :: int_arry(5) = (/2,3,5,7,10/)
  intknd :: ipt(5) = (/1,1,1,1,1/)
  realkd :: real_array(5) = (/5.0_krp, 4.0_krp, 3.0_krp, 2.0_krp, 1.0_krp/)
  realkd :: matrix(3,3) = reshape((/2._krp, 3._krp, 1._krp, &
                                   -1._krp, 2._krp, 3._krp, &
                                    1._krp,-5._krp,-2._krp/),(/3,3/)), &

            vector(3)   = (/0._krp,17._krp,13._krp/), &
            ref(3)      = (/2._krp,3._krp,-1._krp/),  &
            x(3)
  logknd :: bool
  intknd :: i
  intknd :: j
  intknd :: n
  intknd :: is(4)
  realkd :: val
  realkd :: integral
  realkd :: cs(4)
  realkd :: ys(4)
  intknd :: order,k
  realkd :: theta,azimuthal
  realkd, allocatable :: sph_har(:)
  realkd, allocatable :: r(:)
  realkd, allocatable :: c(:)
  realkd, allocatable :: xx(:)
  realkd, allocatable :: w(:)
  realkd, allocatable :: A(:, :)


  call set_stop_on_error(.false.)
  call set_quiet(.false.)
  assert_start()

  ! Test spherical_harmonic
  assert_component_begin('%spherical_harmonic(...)')
  theta = 0.798184_krp
  azimuthal = 0.78539816339744828_krp
  order = 4

  allocate(sph_har((order+1)**2))
  sph_har = 0.0_krp
  do k = 0,order
    sph_har(k*k+1:(k+1)*(k+1)) =  spherical_harmonic(k,theta,azimuthal)
  enddo
  write(*,*)'sph_har = '
  write(*,*)sph_har
  write(200,*)'sph_har = '
  write(200,*)sph_har

  assert_component_summary('%spherical_harmonic(...)')

! Test binary_search_str(...)
  assert_component_begin('binary_search_str(...)')


  assert((binary_search_str(str_arry,'a') == -1), 'Error to search a')
  assert((binary_search_str(str_arry,'b') ==  1), 'Error to search b')
  assert((binary_search_str(str_arry,'c') ==  2), 'Error to search c')
  assert((binary_search_str(str_arry,'d') ==  2), 'Error to search d')
  assert((binary_search_str(str_arry,'e') ==  3), 'Error to search e')
  assert((binary_search_str(str_arry,'f') ==  3), 'Error to search f')
  assert((binary_search_str(str_arry,'g') ==  4), 'Error to search g')
  assert((binary_search_str(str_arry,'h') ==  4), 'Error to search h')
  assert((binary_search_str(str_arry,'i') ==  4), 'Error to search i')
  assert((binary_search_str(str_arry,'j') ==  5), 'Error to search j')
  assert((binary_search_str(str_arry,'k') ==  6), 'Error to search k')

  assert_component_summary('%binary_search_str(...)')

! Test binary_search_integer(...)
  assert_component_begin('%binary_search_integer(...)')

  assert((binary_search_intknd(int_arry,1 ) == -1), 'Error to search 1 ')
  assert((binary_search_intknd(int_arry,2 ) ==  1), 'Error to search 2 ')
  assert((binary_search_intknd(int_arry,3 ) ==  2), 'Error to search 3 ')
  assert((binary_search_intknd(int_arry,4 ) ==  2), 'Error to search 4 ')
  assert((binary_search_intknd(int_arry,5 ) ==  3), 'Error to search 5 ')
  assert((binary_search_intknd(int_arry,6 ) ==  3), 'Error to search 6 ')
  assert((binary_search_intknd(int_arry,7 ) ==  4), 'Error to search 7 ')
  assert((binary_search_intknd(int_arry,8 ) ==  4), 'Error to search 8 ')
  assert((binary_search_intknd(int_arry,9 ) ==  4), 'Error to search 9 ')
  assert((binary_search_intknd(int_arry,10) ==  5), 'Error to search 10')
  assert((binary_search_intknd(int_arry,11) ==  6), 'Error to search 11')

  assert_component_summary('%binary_search_integer(...)')

  ! test binary_search_real1
  assert_component_begin('%binary_search_real1(...)')
  assert((binary_search_real1(real_array, 6.0_krp) == -1), 'Error of search')
  assert((binary_search_real1(real_array, 5.0_krp) == 1), 'Error of search')
  assert((binary_search_real1(real_array, 4.5_krp) == 1), 'Error of search')
  assert((binary_search_real1(real_array, 3.5_krp) == 2), 'Error of search')
  assert((binary_search_real1(real_array, 3.0_krp) == 3), 'Error of search')
  assert((binary_search_real1(real_array, 2.5_krp) == 3), 'Error of search')
  assert((binary_search_real1(real_array, 1.5_krp) == 4), 'Error of search')
  assert((binary_search_real1(real_array, 1.0_krp) == 5), 'Error of search')
  assert((binary_search_real1(real_array, -2.5_krp) == 6), 'Error of search')
  assert_component_summary('%binary_search_real1(...)')

  call lu_linsol(3,matrix,vector,x)
  bool = .false.
  if(all(x .abseq. ref)) bool = .true.
  assert(bool, 'Error when solving linear equation.')

  ! test interpolation in sqrt-linear relationship
  assert_component_begin('%interp_sqli_1d(...)')
  val = interp_sqli_1d(4.0_krp, 2, (/1.0_krp, 9.0_krp/), &
    (/1.0_krp, 2.0_krp/))
  assert((val .abseq. 1.5_krp), "Error of interp_sqli_1d")
  assert_component_summary('%interp_sqli_1d(...)')
  assert_component_begin('%interp_sqli_2d(...)')
  val = interp_sqli_2d(4.0_krp, 400.0_krp, 2, 2, (/1.0_krp, 9.0_krp/), &
    (/100.0_krp, 900.0_krp/), &
    reshape((/1.0_krp, 3.0_krp, -1.0_krp, 1.0_krp/), (/2, 2/)))
  assert((val .abseq. 1.0_krp), "Error of interp_sqli_2d")
  assert_component_summary('%interp_sqli_2d(...)')

  ! test gauss integration
  assert_component_begin('%gauss_legendre_integral_define(...)')
  do j = 3, 30
    if (allocated(xx)) deallocate(xx)
    if (allocated(w)) deallocate(w)
    allocate(xx(j))
    allocate(w(j))
    call gauss_legendre_integral_define(j, xx, w, -2.0_krp, 2.0_krp)
    integral = 0.0_krp
    do i = 1, j
      integral = integral + xx(i) ** 3 * w(i)
    end do
    assert((abs(integral) < 1e-12), 'Error of gauss integration')
  end do
  assert_component_summary('%gauss_legendre_integral_define(...)')

  ! test eigenvalues of matrix
  assert_component_begin('%eigen_matrix(...)')
  n = 4
  allocate(A(n, n))
  allocate(r(n))
  A = reshape((/-1.5_krp, 1.0_krp, 0.0_krp, 0.0_krp,&
      -0.5_krp, 0.0_krp, 1.0_krp, 0.0_krp,&
      -2.5_krp, 0.0_krp, 0.0_krp, 1.0_krp,&
      -24.5_krp, 0.0_krp, 0.0_krp, 0.0_krp/),(/4,4/))
  !A = reshape((/15,1, 3, 3,&
  !              5, 5, 1, 2,&
  !              25, 2, 4, 1,&
  !              24, 12, 3, 4/),(/4,4/))
  !
  call eigen_matrix(n, A, r)
  assert(abs(r(1) + 1.9630263980449807_krp) < 1e-6, 'Error of eigenvalue')
  assert(abs(r(2) + 1.9630263980449807_krp) < 1e-6, 'Error of eigenvalue')
  assert(abs(r(3) - 1.2130263980449774_krp) < 1e-6, 'Error of eigenvalue')
  assert(abs(r(4) - 1.2130263980449774_krp) < 1e-6, 'Error of eigenvalue')
  assert_component_summary('%eigen_matrix(...)')

  ! test get roots of polynomial
  assert_component_begin('%get_roots(...)')
  if (allocated(r)) deallocate(r)
  allocate(r(n))
  allocate(c(n+1))
  c = (/5.0_krp, 17.0_krp, 5.0_krp, 67.0_krp, 90.0_krp/)
  call get_roots(n, c, r)
  assert(abs(r(1) + 3.7456097635658780_krp) < 1e-6, 'Error of roots')
  assert(abs(r(2) - 0.76100615282056516_krp) < 1e-6, 'Error of roots')
  assert(abs(r(3) - 0.76100615282056516_krp) < 1e-6, 'Error of roots')
  assert(abs(r(4) + 1.1764025420752520_krp) < 1e-6, 'Error of roots')
  assert_component_summary('%get_roots(...)')

  ! test Lagrange interpolation method
  assert_component_begin('%lagrange_interp(...)')
  ys = (/-7.0_krp, 7.0_krp, -4.0_krp, 35.0_krp/)
  call lagrange_interp(4, 4, (/-1.0_krp, 1.0_krp, 2.0_krp, 5.0_krp/), 3.0_krp, cs&
    &, is)
  assert((is(1) == 1), 'Error of Lagrange interpolation')
  assert((is(2) == 2), 'Error of Lagrange interpolation')
  assert((is(3) == 3), 'Error of Lagrange interpolation')
  assert((is(4) == 4), 'Error of Lagrange interpolation')
  val = rzero
  do i = 1, 4
    val = val + cs(i) * ys(is(i))
  end do
  assert((val .abseq. -11.0_krp), 'Error of Lagrange interpolation')
  assert_component_summary('%lagrange_interp(...)')

  ! test quick sort
  assert_component_begin('%qsort(...)')
  call qsort(real_array, ipt)
  do i = 2, size(real_array)
    assert((real_array(i) > real_array(i-1)), 'Error of qsort')
  end do
  assert_component_summary('%qsort(...)')

endprogram test_math
