module quadrature_set
  use error_warning
  use math
  use string
  use file_text
  implicit none
  private

# include <kind_parameter.h>

  public :: quadrature_set_type
  public :: chebyshev_yamamoto
  public :: chebyshev_McDaniel
  public :: chebyshev_gauss

  !-----------------------------------------------------------------------------
  !> Angular quadrature set type, store azimuthal and polar angles and
  !> correspording weights
  type :: quadrature_set_type
    logknd :: is_init = .false.           !< whether the quad set is initialized
    intknd :: i_type  = 0                 !< Type of quadrature,
                                          !      1: octance,
                                          !      2: quarter sphere
                                          !      3: half sphere
    intknd :: n_alpha = 0                 !< Number of azimuthal angles
    intknd :: n_theta = 0                 !< Number of polar angles
    realkd,allocatable :: alpha(:)        !< values of azimuthal angles
    realkd,allocatable :: weight_alpha(:) !< Weights of azimuthal angles

    realkd,allocatable :: theta(:)        !< Values of polar angles
    realkd,allocatable :: sin_theta(:)    !< Sine values of polar angles
    realkd,allocatable :: r_sin_theta(:)  !< Reciprocal of polar angle sine values
    realkd,allocatable :: weight_theta(:) !< Weights of polar angles

    intknd :: n_theta_2d = 0
    realkd,allocatable :: theta_2d(:)
    realkd,allocatable :: sin_theta_2d(:)
    realkd,allocatable :: r_sin_theta_2d(:)
    realkd,allocatable :: weight_theta_2d(:)

    !realkd,allocatable :: weight(:,:,:)   !< Weights of angle for CMFD

  contains
    procedure,pass :: init
    procedure,pass :: updates_weight_expand_to_half
    procedure,pass :: updates_weight_expand_to_all
    procedure,pass :: clear
  endtype

  intknd,parameter :: chebyshev_yamamoto = 1
  intknd,parameter :: chebyshev_McDaniel = 2
  intknd,parameter :: chebyshev_gauss    = 3
!===============================================================================

contains

!-------------------------------------------------------------------------------
!> Clear method for angular quadrature set, set the integers to 0, reals to 0.0,
!> and deallocate all allocatable variables.
!>
!> It only works when quadrature set is initialized, but it can be called
!> anywhere anytimes
!-------------------------------------------------------------------------------
  subroutine clear(this)
    class(quadrature_set_type),intent(inout) :: this
    if(this % is_init) then
      this % n_alpha = 0
      this % n_theta = 0
      this % i_type  = 0
      deallocate(this%alpha)
      deallocate(this%weight_alpha)
      deallocate(this%theta)
      deallocate(this%sin_theta)
      deallocate(this%r_sin_theta)
      deallocate(this%weight_theta)
      deallocate(this%theta_2d)
      deallocate(this%sin_theta_2d)
      deallocate(this%r_sin_theta_2d)
      deallocate(this%weight_theta_2d)
      this % is_init = .false.
    endif

  endsubroutine clear

!-------------------------------------------------------------------------------
!> Initialize method for angular quadrature set with an octan type
!>
!> i_type_in     - in, the type of quadrature set;
!> n_alpha_in - in, the number of azimuthal angles in the octan (0,pi/2);
!> n_theta_in - in, the number of polar angles in the octans (0,pi/2);
!>
!> %i_type will be 1,2,3, or it will be set to 1 by default
!-------------------------------------------------------------------------------
  subroutine init(this,i_type_in, n_alpha_in,n_theta_in)

    charSt,parameter :: myName = 'Quadrature_set => init - '
    class(quadrature_set_type),intent(inout) :: this
    intknd,intent(in) :: i_type_in  ! No. of azimuthal angle
    intknd,intent(in) :: n_alpha_in ! No. of azimuthal angle
    intknd,intent(in) :: n_theta_in ! No. of polar angle

    intknd :: n_alpha
    intknd :: n_theta
    intknd :: i_type

    n_alpha = n_alpha_in
    n_theta = n_theta_in
    i_type  = i_type_in

    if(.not.this%is_init) then
      !The following check should be moved to the option_input.f90
      if(i_type > 3 .and. i_type < 1) then
        i_type = 1
        call raise_warning(myName//'type of angular quadrature set is wrong,'//&
          ' chebyshev_yamamoto will be used instead')
      endif

      if(n_alpha > 1) then
        if(mod(n_alpha,2) == 1) then
          call raise_warning(myName//'# of azimuthal should be even if it is'//&
            ' > 1, '//trim(to_str(n_alpha+1))//                                &
            ' azimuthal angles in [1,pi/2] will be used instead')
          n_alpha = n_alpha + 1
        endif
      endif

      if(n_alpha < 1) then
        n_alpha = 2
        call raise_warning(myName//'# of azimuthal angle should be positive, 2'//  &
          ' azimuthal angle in [1,pi/2] will be used instead')
      endif

      if(n_theta < 1) then
        n_theta = 1
        call raise_warning(myName//'# of polar angle should be positive, 1'//  &
          ' polar angle in [1,pi/2] will be used instead')
      endif

      !set alpha, azimuthal angle
      this%n_alpha = n_alpha
      allocate(this%alpha(this%n_alpha))
      allocate(this%weight_alpha(this%n_alpha))

      call set_alpha(this%n_alpha,this%alpha,this%weight_alpha)

      !Set theta, polar angle
      this%n_theta = n_theta
      allocate(this%theta(this%n_theta))
      allocate(this%sin_theta(this%n_theta))
      allocate(this%r_sin_theta(this%n_theta))
      allocate(this%weight_theta(this%n_theta))

      this%n_theta_2d = n_theta
      allocate(this%theta_2d(this%n_theta_2d))
      allocate(this%sin_theta_2d(this%n_theta_2d))
      allocate(this%r_sin_theta_2d(this%n_theta_2d))
      allocate(this%weight_theta_2d(this%n_theta_2d))

      call set_theta(i_type,this%n_theta,this%theta,this%weight_theta)
      this%sin_theta=sin(this%theta)
      this%r_sin_theta=1.0_krp/this%sin_theta

      this%theta_2d = this%theta
      this%sin_theta_2d = this%sin_theta
      this%r_sin_theta_2d = this%r_sin_theta
      this%weight_theta_2d = this%weight_theta

      this%i_type = 1
      this%is_init = .true.
    endif
  endsubroutine init

!-------------------------------------------------------------------------------
!> update weights according to the new angles
!
!> Note:
!>     1) It works only when the quadrature set is of the octant type.
!>     2) Error message will be given when %i_type is 2 or 3.
!-------------------------------------------------------------------------------
  subroutine update_weights(this)
    class(quadrature_set_type),intent(inout) :: this
    charSt,parameter :: myName = 'Quadrature_set => updates_weight - '

    realkd :: matrix(this%n_alpha,this%n_alpha)
    realkd :: vector(this%n_alpha)

    ! Locals
    intknd :: i,j
    realkd,allocatable :: alpha_temp(:)
    realkd,allocatable :: weight_temp(:)

    if(this%is_init) then
      if(this%i_type == 1) then
        do I = 1,this%n_alpha
            do J = 1,this%n_alpha
                matrix(I,J) = cos(2.0_krp*real(i-1,krp)*this%alpha(J))
            enddo
        enddo
        vector(1) = 0.25_krp
        Vector(2:this%n_alpha) = rzero

        call lu_linsol(this%n_alpha,matrix,vector,         &
          this%weight_alpha)
      else
        call raise_Error(myName//'quadrature set weight could not be updated'//&
          ' when the quadrature type is of one forth or half sphere, '//       &
          '%i_type should be 1')
      endif
    else
      call raise_warning(myName//'quadrature set is not initialized')
    endif

  endsubroutine update_weights

!-------------------------------------------------------------------------------
!> Expand octant quadrature set to  one forth quadrature set or shrink half
!> quadrature set to the one forth quadrature.
!>
!> This subroutine works when %i_type is 1 or 3. %i_type will be set to 2.
!-------------------------------------------------------------------------------
  subroutine to_quarter_sphere(this)

    charSt,parameter :: myName = 'Quadrature_set => to_quarter_sphere - '
    class(quadrature_set_type),intent(inout) :: this

    realkd :: matrix(this%n_alpha,this%n_alpha)
    realkd :: vector(this%n_alpha)

    ! Locals
    intknd :: i,j
    realkd,allocatable :: alpha_temp(:)
    realkd,allocatable :: weight_temp(:)
    realkd,allocatable :: theta_temp(:)
    realkd,allocatable :: sin_theta_temp(:)
    realkd,allocatable :: r_sin_theta_temp(:)
    realkd,allocatable :: weight_theta_temp(:)

    ! Expand octant quadrature set to one forth quadrature set
    if(this%is_init .and. this%i_type == 1) then

      allocate(alpha_temp(this%n_alpha))
      allocate(weight_temp(this%n_alpha))

      alpha_temp = this%alpha
      weight_temp = this%weight_alpha

      this%n_alpha = this%n_alpha*2

      deallocate(this%alpha)
      deallocate(this%weight_alpha)
      allocate(this%alpha(this%n_alpha))
      allocate(this%weight_alpha(this%n_alpha))

      do i = 1, this%n_alpha/2
        j = i*2
        this%alpha(j-1) = alpha_temp(i)
        this%alpha(j) = pi - this%alpha(j-1)

        this%weight_alpha(j-1) = weight_temp(i)
        this%weight_alpha(j) = this%weight_alpha(j-1)
      enddo

      this%i_type = 2

      deallocate(alpha_temp)
      deallocate(weight_temp)

    !Shrink half quadrature set to quarter quadrature set
    elseif(this%is_init .and. this%i_type == 3) then
      allocate(theta_temp(this%n_theta))
      allocate(sin_theta_temp(this%n_theta))
      allocate(r_sin_theta_temp(this%n_theta))
      allocate(weight_theta_temp(this%n_theta))
      theta_temp = this%theta
      sin_theta_temp = this%sin_theta
      r_sin_theta_temp = this%r_sin_theta
      weight_theta_temp = this%weight_theta

      this%n_theta = this%n_theta/2

      !theta
      deallocate(this%theta)
      deallocate(this%sin_theta)
      deallocate(this%r_sin_theta)
      deallocate(this%weight_theta)
      allocate(this%theta(this%n_theta))
      allocate(this%sin_theta(this%n_theta))
      allocate(this%r_sin_theta(this%n_theta))
      allocate(this%weight_theta(this%n_theta))

      do i = 1, this%n_theta
        this%theta(i) = theta_temp(i)
        this%sin_theta(i) = sin_theta_temp(i)
        this%r_sin_theta(i) = r_sin_theta_temp(i)
        this%weight_theta(i) = weight_theta_temp(i)/2
      enddo

      this%i_type = 2

      deallocate(theta_temp)
      deallocate(sin_theta_temp)
      deallocate(r_sin_theta_temp)
      deallocate(weight_theta_temp)
    endif

  endsubroutine to_quarter_sphere

!-------------------------------------------------------------------------------
!> Expand octant quadrature set or quarter quadrature set to  half sphere
!> quadrature set.
!>
!> This subroutine works when %i_type is 1 or 2. %i_type will be set to 3.
!-------------------------------------------------------------------------------
  subroutine to_half_sphere(this)

    charSt,parameter :: myName = 'Quadrature_set => to_half_sphere - '
    class(quadrature_set_type),intent(inout) :: this

    realkd :: matrix(this%n_alpha,this%n_alpha)
    realkd :: vector(this%n_alpha)

    ! Locals
    intknd :: i,j
    realkd,allocatable :: alpha_temp(:)
    realkd,allocatable :: weight_temp(:)
    realkd,allocatable :: theta_temp(:)
    realkd,allocatable :: sin_theta_temp(:)
    realkd,allocatable :: r_sin_theta_temp(:)
    realkd,allocatable :: weight_theta_temp(:)

    ! Set azimuthal angles
    if(this%is_init .and. this%i_type == 1) then

      allocate(alpha_temp(this%n_alpha))
      allocate(weight_temp(this%n_alpha))

      alpha_temp = this%alpha
      weight_temp = this%weight_alpha

      this%n_alpha = this%n_alpha*2

      deallocate(this%alpha)
      deallocate(this%weight_alpha)
      allocate(this%alpha(this%n_alpha))
      allocate(this%weight_alpha(this%n_alpha))

      do i = 1, this%n_alpha/2
        j = i*2
        this%alpha(j-1) = alpha_temp(i)
        this%alpha(j) = pi - this%alpha(j-1)

        this%weight_alpha(j-1) = weight_temp(i)
        this%weight_alpha(j) = this%weight_alpha(j-1)
      enddo

      this%i_type = 3

      deallocate(alpha_temp)
      deallocate(weight_temp)
    endif

    !Set polar angles
    if(this%is_init .and. this%i_type /= 3) then
      allocate(theta_temp(this%n_theta))
      allocate(sin_theta_temp(this%n_theta))
      allocate(r_sin_theta_temp(this%n_theta))
      allocate(weight_theta_temp(this%n_theta))
      theta_temp = this%theta
      sin_theta_temp = this%sin_theta
      r_sin_theta_temp = this%r_sin_theta
      weight_theta_temp = this%weight_theta

      this%n_theta = this%n_theta*2

      !theta
      deallocate(this%theta)
      deallocate(this%sin_theta)
      deallocate(this%r_sin_theta)
      deallocate(this%weight_theta)
      allocate(this%theta(this%n_theta))
      allocate(this%sin_theta(this%n_theta))
      allocate(this%r_sin_theta(this%n_theta))
      allocate(this%weight_theta(this%n_theta))

      do i = 1, this%n_theta/2
        this%theta(i) = theta_temp(i)
        this%sin_theta(i) = sin_theta_temp(i)
        this%r_sin_theta(i) = r_sin_theta_temp(i)
        this%weight_theta(i) = weight_theta_temp(i)/2
      enddo
      do i = 1, this%n_theta/2
        this%theta(this%n_theta+1-i) = pi - this%theta(i)
        this%sin_theta(this%n_theta+1-i) = this%sin_theta(i)
        this%r_sin_theta(this%n_theta+1-i) = this%r_sin_theta(i)
        this%weight_theta(this%n_theta+1-i) = this%weight_theta(i)
      enddo

      this%i_type = 3

      deallocate(theta_temp)
      deallocate(sin_theta_temp)
      deallocate(r_sin_theta_temp)
      deallocate(weight_theta_temp)
    endif

  endsubroutine to_half_sphere

!-------------------------------------------------------------------------------
! updates weights according to the new angles
!-------------------------------------------------------------------------------
  subroutine updates_weight_expand_to_half(this)
    class(quadrature_set_type),intent(inout) :: this

    realkd :: matrix(this%n_alpha,this%n_alpha)
    realkd :: vector(this%n_alpha)

    ! Locals
    intknd :: i,j
    realkd,allocatable :: alpha_temp(:)
    realkd,allocatable :: weight_temp(:)

    if(this%is_init .and. this%i_type == 1) then
      do I = 1,this%n_alpha
          do J = 1,this%n_alpha
              matrix(I,J) = cos(2.0_krp*real(i-1,krp)*this%alpha(J))
          enddo
      enddo
      vector(1) = 0.25_krp
      Vector(2:this%n_alpha) = rzero

      call lu_linsol(this%n_alpha,matrix,vector,           &
        this%weight_alpha)

      allocate(alpha_temp(this%n_alpha))
      allocate(weight_temp(this%n_alpha))

      alpha_temp = this%alpha
      weight_temp = this%weight_alpha

      this%n_alpha = this%n_alpha*2

      deallocate(this%alpha)
      deallocate(this%weight_alpha)
      allocate(this%alpha(this%n_alpha))
      allocate(this%weight_alpha(this%n_alpha))

      do i = 1, this%n_alpha/2
        j = i*2
        this%alpha(j-1) = alpha_temp(i)
        this%alpha(j) = pi - this%alpha(j-1)

        this%weight_alpha(j-1) = weight_temp(i)
        this%weight_alpha(j) = this%weight_alpha(j-1)
      enddo

      deallocate(alpha_temp)
      deallocate(weight_temp)
    endif

  endsubroutine updates_weight_expand_to_half

!-------------------------------------------------------------------------------
! updates weights according to the all angles
!-------------------------------------------------------------------------------
  subroutine updates_weight_expand_to_all(this)
    class(quadrature_set_type),intent(inout) :: this

    realkd :: matrix(this%n_alpha,this%n_alpha)
    realkd :: vector(this%n_alpha)

    ! Locals
    intknd :: i,j
    realkd,allocatable :: alpha_temp(:)
    realkd,allocatable :: weight_temp(:)
    realkd,allocatable :: theta_temp(:)
    realkd,allocatable :: sin_theta_temp(:)
    realkd,allocatable :: r_sin_theta_temp(:)
    realkd,allocatable :: weight_theta_temp(:)

    if(this%is_init) then
      do I = 1,this%n_alpha
          do J = 1,this%n_alpha
              matrix(I,J) = cos(2.0_krp*real(i-1,krp)*this%alpha(J))
          enddo
      enddo
      vector(1) = 0.25_krp
      Vector(2:this%n_alpha) = rzero

      call lu_linsol(this%n_alpha,matrix,vector,           &
        this%weight_alpha)

      allocate(alpha_temp(this%n_alpha))
      allocate(weight_temp(this%n_alpha))
      allocate(theta_temp(this%n_theta))
      allocate(sin_theta_temp(this%n_theta))
      allocate(r_sin_theta_temp(this%n_theta))
      allocate(weight_theta_temp(this%n_theta))

      alpha_temp = this%alpha
      weight_temp = this%weight_alpha

      theta_temp = this%theta
      sin_theta_temp = this%sin_theta
      r_sin_theta_temp = this%r_sin_theta
      weight_theta_temp = this%weight_theta

      this%n_alpha = this%n_alpha*2
      this%n_theta = this%n_theta*2

      !theta
      deallocate(this%theta)
      deallocate(this%sin_theta)
      deallocate(this%r_sin_theta)
      deallocate(this%weight_theta)
      allocate(this%theta(this%n_theta))
      allocate(this%sin_theta(this%n_theta))
      allocate(this%r_sin_theta(this%n_theta))
      allocate(this%weight_theta(this%n_theta))

      do i = 1, this%n_theta/2
        this%theta(i) = theta_temp(i)
        this%sin_theta(i) = sin_theta_temp(i)
        this%r_sin_theta(i) = r_sin_theta_temp(i)
        this%weight_theta(i) = weight_theta_temp(i)/2
      enddo
      do i = 1, this%n_theta/2
        this%theta(this%n_theta+1-i) = pi - this%theta(i)
        this%sin_theta(this%n_theta+1-i) = this%sin_theta(i)
        this%r_sin_theta(this%n_theta+1-i) = this%r_sin_theta(i)
        this%weight_theta(this%n_theta+1-i) = this%weight_theta(i)
      enddo

      !alpha
      deallocate(this%alpha)
      deallocate(this%weight_alpha)
      allocate(this%alpha(this%n_alpha))
      allocate(this%weight_alpha(this%n_alpha))

      do i = 1, this%n_alpha/2
        j = i*2
        this%alpha(j-1) = alpha_temp(i)
        this%alpha(j) = pi - this%alpha(j-1)

        this%weight_alpha(j-1) = weight_temp(i)
        this%weight_alpha(j) = this%weight_alpha(j-1)
      enddo

      deallocate(alpha_temp)
      deallocate(weight_temp)
      deallocate(theta_temp)
      deallocate(sin_theta_temp)
      deallocate(r_sin_theta_temp)
      deallocate(weight_theta_temp)
    endif

  endsubroutine updates_weight_expand_to_all

!-------------------------------------------------------------------------------
!> Set polar angles and weights of the octant for the quadrature set object.
!>
!> i_type       - in , type of polar angles in [0, pi/2]
!> n_theta      - in , number of polar angles in [0, pi/2]
!> theta        - out, values of polar angles in [0, pi/2]
!> weight_theta - out, weights of polar angles
!>
!> Note:
!>    1) n_theta should only be: 1, 2, 3, 4, 6, 8, 12, 16, 24, 32, 48, 64.
!>    2) Optimal angle will be used when chosing 2 polar angles.
!>    4) Sum of weights is 1
!>    3) This is a private subroutine, it will not be used directly outside.
!-------------------------------------------------------------------------------
  subroutine set_theta(i_type,n_theta,theta,weight_theta)
    intknd,intent(in) :: i_type
    intknd,intent(in) :: n_theta
    realkd,intent(out) :: theta(:)
    realkd,intent(out) :: weight_theta(:)

    if(i_type == chebyshev_yamamoto) then
      call get_yamamoto_quadrature(n_theta,theta,weight_theta)
    else if(i_type == chebyshev_McDaniel) then
      call get_McDaniel_quadrature(n_theta,theta,weight_theta)
    else
      call get_gauss_quadrature(n_theta,theta,weight_theta)
    endif
  endsubroutine set_theta

!-------------------------------------------------------------------------------
!> Set azimuthal angles and weights of the octant for the quadrature set object.
!>
!> n_alpha      - in , number of azimuthal angles in [0, pi/2]
!> alpha        - out, values of azimuthal angles in [0, pi/2]
!> weight_alpha - out, weights of azimuthal angles, sum of weights is 0.25
!>
!> Note:
!>    1) n_alpha should be larger than 0
!>    3) This is a private subroutine, it will not be used directly outside.
!-------------------------------------------------------------------------------
  subroutine set_alpha(n_alpha,alpha,weight_alpha)
    intknd,intent(in) :: n_alpha
    realkd,intent(inout) :: alpha(:)
    realkd,intent(inout) :: weight_alpha(:)

    intknd :: iang
    realkd :: delt

    delt = halfpi/(2.0_krp*n_alpha)
    do iang = 1, n_alpha
      alpha(iang) = delt*(2.0_krp*iang - 1._krp)
    enddo
    weight_alpha = 0.25_krp/n_alpha
  endsubroutine set_alpha

!-------------------------------------------------------------------------------
!> Edit the angular quadrature set
!>     file - in, output file object
!>
!> Only when angular and file object is initialized, angular quadrature
!> information will be write to the file, else warnning will be show up.
!-------------------------------------------------------------------------------
  subroutine edit(this,file)

    charSt,parameter :: myName = 'Quadrature_set => edit - '
    class(quadrature_set_type),intent(inout) :: this
    type(file_text_type),intent(inout) :: file

    intknd :: unit

    if (.not. file%is_init) then
      call raise_warning(myName//'file input not initialized')
    elseif(.not.this%is_init) then
      call raise_warning(myName//'quadrature set not initialized')
    else
      unit = file%unit
      write(unit,*) "Quadrature set information"
      write(unit,*) "    # of azimuthal angle = ",this%n_alpha
      write(unit,*) "    # of polar angle     = ",this%n_theta
      write(unit,*)
      write(unit,*) "    Values of azimuthal angles:"
      write(unit,*) '    ',this % alpha
      write(unit,*) "    Weights of azimuthal angles:"
      write(unit,*) '    ',this % weight_alpha
      write(unit,*)
      write(unit,*) "    Values of polar angles:"
      write(unit,*) '    ',this % theta
      write(unit,*) "    Weights of polar angles:"
      write(unit,*) '    ',this % weight_theta
    endif
  endsubroutine edit

!-------------------------------------------------------------------------------
!> Set the optmized angles and coresspording weights based on the bickley
!> function proposed by Yamamoto.
!>
!> n_angle      - in , number of angles in [0, pi/2]
!> angles_in    - out, values of angles in [0, pi/2]
!> weights_in   - out, weights of angles, sum of weights is 1.0
!>
!> Note:
!>    1) n_angle should be equal to the size of angles_in and weights_in
!>    2) n_angle should be 1,2,3, else fatal error happens
!>    3) This is a private subroutine, it will not be used directly outside.
!-------------------------------------------------------------------------------
  subroutine get_yamamoto_quadrature(n_angle,angles_in,weights_in)
    intknd,intent(in) :: n_angle
    realkd,intent(inout) :: angles_in(:)
    realkd,intent(inout) :: weights_in(:)

    ! Locals
    realkd :: angles(n_angle)
    realkd :: weights(n_angle)

    intknd :: iang

    selectcase(n_angle)
    case(1)
      angles(1) =0.924274629374_krp
      weights(1)=1.000000000000_krp
    case(2)
      angles(1) =0.372451560620_krp
      angles(2) =1.119540153572_krp

      weights(1)=0.212854000000_krp
      weights(2)=0.787146000000_krp
    case(3)
      angles(1) =0.167429147795_krp
      angles(2) =0.567715121084_krp
      angles(3) =1.202533146789_krp

      weights(1)=0.046233000000_krp
      weights(2)=0.283619000000_krp
      weights(3)=0.670148000000_krp
    case default
      call raise_warning('The number of polar angles is incorrect, '// &
        'chebyshev_gauss quadrature set is used')
      call get_gauss_quadrature(n_angle,angles,weights)
    endselect

    do iang = 1, n_angle
        angles_in(iang) = angles(iang)
        weights_in(iang) = weights(iang)
    enddo
  endsubroutine get_yamamoto_quadrature

!-------------------------------------------------------------------------------
!> Set the optmized angles and coresspording weights based on the bickley
!> function proposed by Leonard & McDaniel.
!>
!> n_angle      - in , number of angles in [0, pi/2]
!> angles_in    - out, values of angles in [0, pi/2]
!> weights_in   - out, weights of angles, sum of weights is 1.0
!>
!> Note:
!>    1) n_angle should be equal to the size of angles_in and weights_in
!>    2) n_angle should be 1,2,3, else fatal error happens
!>    3) This is a private subroutine, it will not be used directly outside.
!-------------------------------------------------------------------------------
  subroutine get_McDaniel_quadrature(n_angle,angles_in,weights_in)
    intknd,intent(in) :: n_angle
    realkd,intent(inout) :: angles_in(:)
    realkd,intent(inout) :: weights_in(:)

    ! Locals
    realkd :: angles(n_angle)
    realkd :: weights(n_angle)

    intknd :: iang

    selectcase(n_angle)
    case(1)
      angles(1) =0.851461245321_krp
      weights(1)=1.000000000000_krp
    case(2)
      angles(1) =0.277194163666_krp
      angles(2) =1.046575079148_krp

      weights(1)=0.139473000000_krp
      weights(2)=0.860527000000_krp
    case(3)
      angles(1) =0.104027524731_krp
      angles(2) =0.445293746002_krp
      angles(3) =1.132403375304_krp

      weights(1)=0.020530000000_krp
      weights(2)=0.219161000000_krp
      weights(3)=0.760309000000_krp
    case default
      call raise_warning('The number of polar angles is incorrect, '// &
        'chebyshev_gauss quadrature set is used')
      call get_gauss_quadrature(n_angle,angles,weights)
    endselect

    do iang = 1, n_angle
        angles_in(iang) = angles(iang)
        weights_in(iang) = weights(iang)
    enddo
  endsubroutine get_McDaniel_quadrature



!-------------------------------------------------------------------------------
!> Set the gauss angles and coresspording weights.
!>
!> n_angle      - in , number of angles in [0, pi/2]
!> angles_in    - out, values of angles in [0, pi/2]
!> weights_in   - out, weights of angles, sum of weights is 1.0
!>
!> Note:
!>    1) n_angle should be equal to the size of angles_in and weights_in
!>    2) n_angle should be 1,2,3,4,6,8,12,16,24,32,48,64, else fatal error happens
!>    3) This is a private subroutine, it will not be used directly outside.
!-------------------------------------------------------------------------------
  subroutine get_gauss_quadrature(n_angle,angles_in,weights_in)
    intknd,intent(in) :: n_angle
    realkd,intent(inout) :: angles_in(:)
    realkd,intent(inout) :: weights_in(:)

    ! Locals
    realkd :: angles(n_angle)
    realkd :: weights(n_angle)

    intknd :: iang

    selectcase(n_angle)
    case(1)
      angles(1) =0.955316618124509_krp
      weights(1)=1.000000000000000_krp
    case(2)
      angles(1) =0.533295680249127_krp
      angles(2) =1.223899586470373_krp

      weights(1)=0.347854845137454_krp
      weights(2)=0.652145154862546_krp
    case(3)
      angles(1) =0.369606651944829_krp
      angles(2) =0.848366626487488_krp
      angles(3) =1.329852612388110_krp

      weights(1)=0.171324492379170_krp
      weights(2)=0.360761573048139_krp
      weights(3)=0.467913934572691_krp
    case(4)
      angles(1) =0.282757063593796_krp
      angles(2) =0.649036580460780_krp
      angles(3) =1.017455539490153_krp
      angles(4) =1.386317078892131_krp

      weights(1)=0.101228536290377_krp
      weights(2)=0.222381034453374_krp
      weights(3)=0.313706645877887_krp
      weights(4)=0.362683783378362_krp
    case(6)
      angles(1) =0.192334679304668_krp
      angles(2) =0.441487081489332_krp
      angles(3) =0.692107698881841_krp
      angles(4) =0.943055287060574_krp
      angles(5) =1.194120375947707_krp
      angles(6) =1.445233238471440_krp

      weights(1)=0.047175336386512_krp
      weights(2)=0.106939325995318_krp
      weights(3)=0.160078328543346_krp
      weights(4)=0.203167426723066_krp
      weights(5)=0.233492536538355_krp
      weights(6)=0.249147045813403_krp
    case(8)
      angles(1) =0.145724682003674_krp
      angles(2) =0.334498638687629_krp
      angles(3) =0.524386640903594_krp
      angles(4) =0.714525253234025_krp
      angles(5) =0.904757532389517_krp
      angles(6) =1.095033401803444_krp
      angles(7) =1.285331444322965_krp
      angles(8) =1.475640280808194_krp

      weights(1)=0.027152459411754_krp
      weights(2)=0.062253523938648_krp
      weights(3)=0.095158511682493_krp
      weights(4)=0.124628971255534_krp
      weights(5)=0.149595988816577_krp
      weights(6)=0.169156519395003_krp
      weights(7)=0.182603415044924_krp
      weights(8)=0.189450610455068_krp
    case(12)
      angles(1) =0.098149329497937_krp
      angles(2) =0.225293622635308_krp
      angles(3) =0.353188667569078_krp
      angles(4) =0.481253195131369_krp
      angles(5) =0.609381838244957_krp
      angles(6) =0.737541307543754_krp
      angles(7) =0.865717777040108_krp
      angles(8) =0.993904442298945_krp
      angles(9) =1.122097523267251_krp
      angles(10)=1.250294703417273_krp
      angles(11)=1.378494427506219_krp
      angles(12)=1.506695545558101_krp

      weights(1)=0.012341229799987_krp
      weights(2)=0.028531388628933_krp
      weights(3)=0.044277438817420_krp
      weights(4)=0.059298584915437_krp
      weights(5)=0.073346481411080_krp
      weights(6)=0.086190161531953_krp
      weights(7)=0.097618652104114_krp
      weights(8)=0.107444270115966_krp
      weights(9)=0.115505668053726_krp
      weights(10)=0.121670472927803_krp
      weights(11)=0.125837456346828_krp
      weights(12)=0.127938195346752_krp
    case(16)
      angles(1) =0.073991713099710_krp
      angles(2) =0.169841845428215_krp
      angles(3) =0.266257999472386_krp
      angles(4) =0.362802007535003_krp
      angles(5) =0.459394473076210_krp
      angles(6) =0.556010341800530_krp
      angles(7) =0.652639239459456_krp
      angles(8) =0.749276095118141_krp
      angles(9) =0.845918131583799_krp
      angles(10)=0.942563694004678_krp
      angles(11)=1.039211728068952_krp
      angles(12)=1.135861522840294_krp
      angles(13)=1.232512573416363_krp
      angles(14)=1.329164502391081_krp
      angles(15)=1.425817011963825_krp
      angles(16)=1.522469852641529_krp

      weights(1)=0.007018610009470_krp
      weights(2)=0.016274394730906_krp
      weights(3)=0.025392065309262_krp
      weights(4)=0.034273862913021_krp
      weights(5)=0.042835898022227_krp
      weights(6)=0.050998059262376_krp
      weights(7)=0.058684093478536_krp
      weights(8)=0.065822222776362_krp
      weights(9)=0.072345794108849_krp
      weights(10)=0.078193895787070_krp
      weights(11)=0.083311924226947_krp
      weights(12)=0.087652093004404_krp
      weights(13)=0.091173878695764_krp
      weights(14)=0.093844399080804_krp
      weights(15)=0.095638720079275_krp
      weights(16)=0.096540088514728_krp
    case(24)
      angles(1) =0.049583153738025_krp
      angles(2) =0.113814025851483_krp
      angles(3) =0.178424212604353_krp
      angles(4) =0.243120098126499_krp
      angles(5) =0.307848485884162_krp
      angles(6) =0.372592595683389_krp
      angles(7) =0.437345485552230_krp
      angles(8) =0.502103768487069_krp
      angles(9) =0.566865596001083_krp
      angles(10)=0.631629873537115_krp
      angles(11)=0.696395911288766_krp
      angles(12)=0.761163252494659_krp
      angles(13)=0.825931582213486_krp
      angles(14)=0.890700675760831_krp
      angles(15)=0.955470368042241_krp
      angles(16)=1.020240534516704_krp
      angles(17)=1.085011078936666_krp
      angles(18)=1.149781925191719_krp
      angles(19)=1.214553011719529_krp
      angles(20)=1.279324287566780_krp
      angles(21)=1.344095709533509_krp
      angles(22)=1.408867240039223_krp
      angles(23)=1.473638845472166_krp
      angles(24)=1.538410494858190_krp

      weights(1)=0.003153346052305_krp
      weights(2)=0.007327553901276_krp
      weights(3)=0.011477234579235_krp
      weights(4)=0.015579315722944_krp
      weights(5)=0.019616160457355_krp
      weights(6)=0.023570760839324_krp
      weights(7)=0.027426509708357_krp
      weights(8)=0.031167227832798_krp
      weights(9)=0.034777222564770_krp
      weights(10)=0.038241351065831_krp
      weights(11)=0.041545082943465_krp
      weights(12)=0.044674560856694_krp
      weights(13)=0.047616658492491_krp
      weights(14)=0.050359035553854_krp
      weights(15)=0.052890189485194_krp
      weights(16)=0.055199503699984_krp
      weights(17)=0.057277292100403_krp
      weights(18)=0.059114839698396_krp
      weights(19)=0.060704439165894_krp
      weights(20)=0.062039423159893_krp
      weights(21)=0.063114192286254_krp
      weights(22)=0.063924238584648_krp
      weights(23)=0.064466164435950_krp
      weights(24)=0.064737696812684_krp
    case(32)
      angles(1) =0.037283743740312_krp
      angles(2) =0.085581748836546_krp
      angles(3) =0.134164978946810_krp
      angles(4) =0.182812652456346_krp
      angles(5) =0.231484769599885_krp
      angles(6) =0.280168713689376_krp
      angles(7) =0.328859265875079_krp
      angles(8) =0.377553880504367_krp
      angles(9) =0.426251168877035_krp
      angles(10)=0.474950309295007_krp
      angles(11)=0.523650784516478_krp
      angles(12)=0.572352252662328_krp
      angles(13)=0.621054478642514_krp
      angles(14)=0.669757295409512_krp
      angles(15)=0.718460580929007_krp
      angles(16)=0.767164243901456_krp
      angles(17)=0.815868214585956_krp
      angles(18)=0.864572438718184_krp
      angles(19)=0.913276873369126_krp
      angles(20)=0.961981484057505_krp
      angles(21)=1.010686242693214_krp
      angles(22)=1.059391126084217_krp
      angles(23)=1.108096114833250_krp
      angles(24)=1.156801192508981_krp
      angles(25)=1.205506345013417_krp
      angles(26)=1.254211560091484_krp
      angles(27)=1.302916826944702_krp
      angles(28)=1.351622135921669_krp
      angles(29)=1.400327478265391_krp
      angles(30)=1.449032845902632_krp
      angles(31)=1.497738231263909_krp
      angles(32)=1.546443627125266_krp

      weights(1)=0.001783280721696_krp
      weights(2)=0.004147033260562_krp
      weights(3)=0.006504457968978_krp
      weights(4)=0.008846759826364_krp
      weights(5)=0.011168139460131_krp
      weights(6)=0.013463047896718_krp
      weights(7)=0.015726030476025_krp
      weights(8)=0.017951715775697_krp
      weights(9)=0.020134823153530_krp
      weights(10)=0.022270173808383_krp
      weights(11)=0.024352702568711_krp
      weights(12)=0.026377469715055_krp
      weights(13)=0.028339672614259_krp
      weights(14)=0.030234657072402_krp
      weights(15)=0.032057928354852_krp
      weights(16)=0.033805161837142_krp
      weights(17)=0.035472213256882_krp
      weights(18)=0.037055128540240_krp
      weights(19)=0.038550153178616_krp
      weights(20)=0.039953741132720_krp
      weights(21)=0.041262563242624_krp
      weights(22)=0.042473515123654_krp
      weights(23)=0.043583724529323_krp
      weights(24)=0.044590558163757_krp
      weights(25)=0.045491627927418_krp
      weights(26)=0.046284796581314_krp
      weights(27)=0.046968182816210_krp
      weights(28)=0.047540165714830_krp
      weights(29)=0.047999388596458_krp
      weights(30)=0.048344762234803_krp
      weights(31)=0.048575467441503_krp
      weights(32)=0.048690957009140_krp
    case(48)
      angles(1) =0.024920360594216_krp
      angles(2) =0.057202625973236_krp
      angles(3) =0.089675535469142_krp
      angles(4) =0.122191519456750_krp
      angles(5) =0.154723842448090_krp
      angles(6) =0.187264071740058_krp
      angles(7) =0.219808719332383_krp
      angles(8) =0.252356083990787_krp
      angles(9) =0.284905237794411_krp
      angles(10)=0.317455631816170_krp
      angles(11)=0.350006920639551_krp
      angles(12)=0.382558876074702_krp
      angles(13)=0.415111341326121_krp
      angles(14)=0.447664205096842_krp
      angles(15)=0.480217386198250_krp
      angles(16)=0.512770824009215_krp
      angles(17)=0.545324472345925_krp
      angles(18)=0.577878295400151_krp
      angles(19)=0.610432264975162_krp
      angles(20)=0.642986358560120_krp
      angles(21)=0.675540557960490_krp
      angles(22)=0.708094848305772_krp
      angles(23)=0.740649217318562_krp
      angles(24)=0.773203654768026_krp
      angles(25)=0.805758152055642_krp
      angles(26)=0.838312701897311_krp
      angles(27)=0.870867298076600_krp
      angles(28)=0.903421935251205_krp
      angles(29)=0.935976608799659_krp
      angles(30)=0.968531314698813_krp
      angles(31)=1.001086049425085_krp
      angles(32)=1.033640809874213_krp
      angles(33)=1.066195593295557_krp
      angles(34)=1.098750397237915_krp
      angles(35)=1.131305219504507_krp
      angles(36)=1.163860058115329_krp
      angles(37)=1.196414911275444_krp
      angles(38)=1.228969777348084_krp
      angles(39)=1.261524654831669_krp
      angles(40)=1.294079542340035_krp
      angles(41)=1.326634438585269_krp
      angles(42)=1.359189342362693_krp
      angles(43)=1.391744252537595_krp
      angles(44)=1.424299168033389_krp
      angles(45)=1.456854087820918_krp
      angles(46)=1.489409010908686_krp
      angles(47)=1.521963936333783_krp
      angles(48)=1.554518863153355_krp

      weights(1)=0.000796792065552_krp
      weights(2)=0.001853960788947_krp
      weights(3)=0.002910731817935_krp
      weights(4)=0.003964554338445_krp
      weights(5)=0.005014202742928_krp
      weights(6)=0.006058545504236_krp
      weights(7)=0.007096470791154_krp
      weights(8)=0.008126876925699_krp
      weights(9)=0.009148671230783_krp
      weights(10)=0.010160770535008_krp
      weights(11)=0.011162102099839_krp
      weights(12)=0.012151604671088_krp
      weights(13)=0.013128229566962_krp
      weights(14)=0.014090941772315_krp
      weights(15)=0.015038721026995_krp
      weights(16)=0.015970562902562_krp
      weights(17)=0.016885479864245_krp
      weights(18)=0.017782502316045_krp
      weights(19)=0.018660679627411_krp
      weights(20)=0.019519081140145_krp
      weights(21)=0.020356797154333_krp
      weights(22)=0.021172939892191_krp
      weights(23)=0.021966644438744_krp
      weights(24)=0.022737069658329_krp
      weights(25)=0.023483399085926_krp
      weights(26)=0.024204841792365_krp
      weights(27)=0.024900633222484_krp
      weights(28)=0.025570036005349_krp
      weights(29)=0.026212340735672_krp
      weights(30)=0.026826866725592_krp
      weights(31)=0.027412962726029_krp
      weights(32)=0.027970007616848_krp
      weights(33)=0.028497411065085_krp
      weights(34)=0.028994614150555_krp
      weights(35)=0.029461089958168_krp
      weights(36)=0.029896344136328_krp
      weights(37)=0.030299915420828_krp
      weights(38)=0.030671376123669_krp
      weights(39)=0.031010332586314_krp
      weights(40)=0.031316425596861_krp
      weights(41)=0.031589330770727_krp
      weights(42)=0.031828758894411_krp
      weights(43)=0.032034456231993_krp
      weights(44)=0.032206204794030_krp
      weights(45)=0.032343822568576_krp
      weights(46)=0.032447163714064_krp
      weights(47)=0.032516118713869_krp
      weights(48)=0.032550614492363_krp
    case(64)
      angles(1) =0.018714548555174_krp
      angles(2) =0.042957697894846_krp
      angles(3) =0.067344016108394_krp
      angles(4) =0.091762682242332_krp
      angles(5) =0.116193618666266_krp
      angles(6) =0.140630492669831_krp
      angles(7) =0.165070684887416_krp
      angles(8) =0.189512917769083_krp
      angles(9) =0.213956494503877_krp
      angles(10)=0.238401002896027_krp
      angles(11)=0.262846183581670_krp
      angles(12)=0.287291865231959_krp
      angles(13)=0.311737930134266_krp
      angles(14)=0.336184294745206_krp
      angles(15)=0.360630898133103_krp
      angles(16)=0.385077694813171_krp
      angles(17)=0.409524650144027_krp
      angles(18)=0.433971737277567_krp
      angles(19)=0.458418935083341_krp
      angles(20)=0.482866226702624_krp
      angles(21)=0.507313598520040_krp
      angles(22)=0.531761039418562_krp
      angles(23)=0.556208540230779_krp
      angles(24)=0.580656093328654_krp
      angles(25)=0.605103692312643_krp
      angles(26)=0.629551331773188_krp
      angles(27)=0.653999007105654_krp
      angles(28)=0.678446714365254_krp
      angles(29)=0.702894450152211_krp
      angles(30)=0.727342211520074_krp
      angles(31)=0.751789995901884_krp
      angles(32)=0.776237801050298_krp
      angles(33)=0.800685624988649_krp
      angles(34)=0.825133465970707_krp
      angles(35)=0.849581322447361_krp
      angles(36)=0.874029193038890_krp
      angles(37)=0.898477076511743_krp
      angles(38)=0.922924971758996_krp
      angles(39)=0.947372877783820_krp
      angles(40)=0.971820793685435_krp
      angles(41)=0.996268718647113_krp
      angles(42)=1.020716651925891_krp
      angles(43)=1.045164592843710_krp
      angles(44)=1.069612540779759_krp
      angles(45)=1.094060495163815_krp
      angles(46)=1.118508455470453_krp
      angles(47)=1.142956421213967_krp
      angles(48)=1.167404391943913_krp
      angles(49)=1.191852367241178_krp
      angles(50)=1.216300346714501_krp
      angles(51)=1.240748329997374_krp
      angles(52)=1.265196316745280_krp
      angles(53)=1.289644306633217_krp
      angles(54)=1.314092299353460_krp
      angles(55)=1.338540294613543_krp
      angles(56)=1.362988292134418_krp
      angles(57)=1.387436291648768_krp
      angles(58)=1.411884292899458_krp
      angles(59)=1.436332295638092_krp
      angles(60)=1.460780299623675_krp
      angles(61)=1.485228304621342_krp
      angles(62)=1.509676310401164_krp
      angles(63)=1.534124316736996_krp
      angles(64)=1.558572323405373_krp

      weights(1)=0.000449380960292_krp
      weights(2)=0.001045812679340_krp
      weights(3)=0.001642503018669_krp
      weights(4)=0.002238288430963_krp
      weights(5)=0.002832751471458_krp
      weights(6)=0.003425526040910_krp
      weights(7)=0.004016254983739_krp
      weights(8)=0.004604584256703_krp
      weights(9)=0.005190161832676_krp
      weights(10)=0.005772637542866_krp
      weights(11)=0.006351663161707_krp
      weights(12)=0.006926892566899_krp
      weights(13)=0.007497981925635_krp
      weights(14)=0.008064589890486_krp
      weights(15)=0.008626377798617_krp
      weights(16)=0.009183009871661_krp
      weights(17)=0.009734153415007_krp
      weights(18)=0.010279479015832_krp
      weights(19)=0.010818660739503_krp
      weights(20)=0.011351376324080_krp
      weights(21)=0.011877307372740_krp
      weights(22)=0.012396139543951_krp
      weights(23)=0.012907562739267_krp
      weights(24)=0.013411271288616_krp
      weights(25)=0.013906964132952_krp
      weights(26)=0.014394345004167_krp
      weights(27)=0.014873122602147_krp
      weights(28)=0.015343010768865_krp
      weights(29)=0.015803728659399_krp
      weights(30)=0.016255000909785_krp
      weights(31)=0.016696557801589_krp
      weights(32)=0.017128135423111_krp
      weights(33)=0.017549475827118_krp
      weights(34)=0.017960327185009_krp
      weights(35)=0.018360443937331_krp
      weights(36)=0.018749586940545_krp
      weights(37)=0.019127523609951_krp
      weights(38)=0.019494028058707_krp
      weights(39)=0.019848881232831_krp
      weights(40)=0.020191871042130_krp
      weights(41)=0.020522792486960_krp
      weights(42)=0.020841447780751_krp
      weights(43)=0.021147646468221_krp
      weights(44)=0.021441205539208_krp
      weights(45)=0.021721949538052_krp
      weights(46)=0.021989710668460_krp
      weights(47)=0.022244328893800_krp
      weights(48)=0.022485652032745_krp
      weights(49)=0.022713535850236_krp
      weights(50)=0.022927844143687_krp
      weights(51)=0.023128448824387_krp
      weights(52)=0.023315229994063_krp
      weights(53)=0.023488076016536_krp
      weights(54)=0.023646883584448_krp
      weights(55)=0.023791557781003_krp
      weights(56)=0.023922012136703_krp
      weights(57)=0.024038168681024_krp
      weights(58)=0.024139957989019_krp
      weights(59)=0.024227319222815_krp
      weights(60)=0.024300200167972_krp
      weights(61)=0.024358557264691_krp
      weights(62)=0.024402355633850_krp
      weights(63)=0.024431569097850_krp
      weights(64)=0.024446180196263_krp
    case default
      call raise_fatal('The number of polar angles is incorrect.')
    endselect

    do iang = 1, n_angle
        angles_in(iang) = angles(iang)
        weights_in(iang) = weights(iang)
    enddo

  endsubroutine get_gauss_quadrature
!-------------------------------------------------------------------------------
endmodule quadrature_set
