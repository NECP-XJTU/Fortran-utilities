!###############################################################################
!> Module for defining "points"  for use in geometry
!>
!> @author Zhouyu Liu
!>   @date 03/18/2015
!>
!> @todo
!>   - Test: Added some comments.
!###############################################################################
module geom_point
  use math,            only : operator(.abseq.)
  implicit none
  private

# include <kind_parameter.h>

  public :: geom_point_type
  public :: geom_point_list_type
  public :: geom_point_linkedList_type
  public :: procedure_middle_point
  public :: procedure_distance_2points
  public :: operator(-),operator(+)

  !-----------------------------------------------------------------------------
  !> Derived type for a point
  type :: geom_point_type

    intknd             :: ndim = 0 !< number of dimensions
    realkd,allocatable :: dims(:)  !< coordinate values for each dimension

  !> List of type bound procedures
  contains

    procedure,pass :: init => point_init
    procedure,pass :: clear => point_clear

  endtype geom_point_type


  !-----------------------------------------------------------------------------
  !> Derived type for a point
  type :: geom_point_list_type
    intknd :: n_points = 0
    type(geom_point_type) :: p
    type(geom_point_list_type),pointer :: next => null()
    type(geom_point_list_type),pointer :: stp  => null()
  !> List of type bound procedures
  contains

    procedure,pass :: append => append_geom_point_list
    procedure,pass :: clear => clear_geom_point_list

  endtype geom_point_list_type

  !-----------------------------------------------------------------------------
  !> Used by the ray trace routines that determine segment infomation
  type :: geom_point_linkedList_type

    real(krp) :: d_to_first = rzero !> Distance to a reference point
    type(geom_point_type) :: p
    type(geom_point_linkedList_type),pointer :: next => null() !> Next record in the list

  contains

    procedure,pass :: insert => geom_point_linkedList_insert
    procedure,pass :: clear => geom_point_linkedList_clear

  endtype geom_point_linkedList_type

  !-----------------------------------------------------------------------------
  !
  interface operator(-)

    module procedure subtract_point

  endinterface operator(-)

  !-----------------------------------------------------------------------------
  !
  interface operator(+)

    module procedure add_point

  endinterface operator(+)

  !-----------------------------------------------------------------------------
  !> Compute distance
  interface procedure_distance_2points

   module procedure distance_2points

  endinterface procedure_distance_2points

  !-----------------------------------------------------------------------------
  !> Calculate the middle point of points
  interface procedure_middle_point

   module procedure middle_2points

  endinterface procedure_middle_point

!===============================================================================

contains
!-------------------------------------------------------------------------------
!> Initial point
!-------------------------------------------------------------------------------
  subroutine point_init(this,ndim,X,Y,Z,coord)

    class(geom_Point_Type),intent(inout) :: this
    intknd,intent(in),optional :: ndim
    realkd,intent(in),optional :: X,Y,Z
    realkd,intent(in),optional :: coord(:)

    if(.not.allocated(this%dims)) then
      if(present(coord)) then
        this%ndim=size(coord)
        allocate(this%dims(1:this%ndim))
        this%dims=coord
      elseif(present(ndim)) then
          selectcase(ndim)
             case(1)
               if(present(X)) then
                 this%ndim=1
                 allocate(this%dims(1))
                 this%dims(1)=X
               endif
             case(2)
               if(present(X) .and. present(Y)) then
                 this%ndim=2
                 allocate(this%dims(2))
                 this%dims(1)=X
                 this%dims(2)=Y
               endif
             case(3)
               if(present(X) .and. present(Y) .and. present(Z)) then
                 this%ndim=3
                 allocate(this%dims(3))
                 this%dims(1)=X
                 this%dims(2)=Y
                 this%dims(3)=Z
               endif
          endselect
      else
          if(present(X) .and. present(Y) .and. present(Z)) then
            this%ndim=3
            allocate(this%dims(3))
            this%dims(1)=X
            this%dims(2)=Y
            this%dims(3)=Z
          elseif(present(X) .and. present(Y)) then
            this%ndim=2
            allocate(this%dims(2))
            this%dims(1)=X
            this%dims(2)=Y
          elseif(present(X)) then
            this%ndim=1
            allocate(this%dims(1))
            this%dims(1)=X
          endif
      endif
    endif
  endsubroutine point_init

!-------------------------------------------------------------------------------
!> Clear method of point
!-------------------------------------------------------------------------------
  elemental subroutine point_clear(this)

    class(geom_point_type),intent(inout) :: this

    this%ndim=0
    if(allocated(this%dims)) deallocate(this%dims)
  endsubroutine point_clear

!-------------------------------------------------------------------------------
!> Add operator between two points
!-------------------------------------------------------------------------------
  elemental function add_point(p1,p2) result(p)
    type(geom_point_type),intent(in) :: p1,p2
    type(geom_point_type) :: p
    call p%clear()
    if(p1%ndim == p2%ndim .and. p1%ndim > 0) then
      p%ndim = p1%ndim
      allocate(p%dims(p%ndim))
      selectcase(p%ndim)
      case(1)
        p%dims(1) = p1%dims(1) + p2%dims(1)
      case(2)
        p%dims(1) = p1%dims(1) + p2%dims(1)
        p%dims(2) = p1%dims(2) + p2%dims(2)
      case(3)  !added  by  lucao for 3D CSG
        p%dims(1) = p1%dims(1) + p2%dims(1)
        p%dims(2) = p1%dims(2) + p2%dims(2)
        p%dims(3) = p1%dims(3) + p2%dims(3)
      endselect
    endif
  endfunction add_point

!-------------------------------------------------------------------------------
!> Subtraction operator between two points
!-------------------------------------------------------------------------------
  elemental function subtract_point(p1,p2) result(p)
    type(geom_point_type),intent(in) :: p1,p2
    type(geom_point_type) :: p
    call p%clear()
    if(p1%ndim == p2%ndim .and. p1%ndim > 0) then
      p%ndim = p1%ndim
      allocate(p%dims(p%ndim))
      selectcase(p%ndim)
      case(1)
        p%dims(1) = p1%dims(1) - p2%dims(1)
      case(2)
        p%dims(1) = p1%dims(1) - p2%dims(1)
        p%dims(2) = p1%dims(2) - p2%dims(2)
      case(3) !added  by  lucao for 3D CSG
        p%dims(1) = p1%dims(1) - p2%dims(1)
        p%dims(2) = p1%dims(2) - p2%dims(2)
        p%dims(3) = p1%dims(3) - p2%dims(3)
      endselect
    endif
  endfunction subtract_point

!-------------------------------------------------------------------------------
!> Computes the distance between two points
!> Returns distance the distance between p1 and p2
!> Note p1 and p2 must be defined in the same dimension or the result is 0.0
!-------------------------------------------------------------------------------
  function distance_2points(p1,p2) result(distance)

    type(geom_point_type),intent(in) :: p1  !< the first point
    type(geom_point_type),intent(in) :: p2  !< the second point
    intknd :: i
    realkd :: distance

    distance=0.0_KRP
    if(p1%ndim == p2%ndim) then
      selectcase(p1%ndim)
      case(1)
        distance=abs(p2%dims(1)-p1%dims(1))
      case(2)
        distance=sqrt((p2%dims(1)-p1%dims(1))*(p2%dims(1)-p1%dims(1))+ &
                  (p2%dims(2)-p1%dims(2))*(p2%dims(2)-p1%dims(2)))
      case(3)
        distance=sqrt((p2%dims(1)-p1%dims(1))*(p2%dims(1)-p1%dims(1))+ &
                  (p2%dims(2)-p1%dims(2))*(p2%dims(2)-p1%dims(2))+ &
                  (p2%dims(3)-p1%dims(3))*(p2%dims(3)-p1%dims(3)))
      case default
        do i=1,p1%ndim
          distance=distance+(p2%dims(i)-p1%dims(i))*(p2%dims(i)-p1%dims(i))
        enddo
        distance=sqrt(distance)
      endselect
    endif
  endfunction distance_2points

!-------------------------------------------------------------------------------
!> Computes the midpoint between two points in any dimension
!> Returns middle_point the midpoint between p1 and p2
!> function is elemental so it can be used on an array of points
!-------------------------------------------------------------------------------
  function middle_2points(p1,p2) result(middle_point)

    type(geom_point_type),intent(in)  :: p1
    type(geom_point_type),intent(in)  :: p2
    type(geom_point_type) :: middle_point

    if(p1%ndim == p2%ndim .and. p1%ndim >0) then
      middle_point%ndim=p1%ndim
      allocate(middle_point%dims(middle_point%ndim))
      selectcase(p1%ndim)
      case(1)
        middle_point%dims(1)=rhalf*(p1%dims(1)+p2%dims(1))
      case(2)
        middle_point%dims(1)=rhalf*(p1%dims(1)+p2%dims(1))
        middle_point%dims(2)=rhalf*(p1%dims(2)+p2%dims(2))
      case(3)
        middle_point%dims(1)=rhalf*(p1%dims(1)+p2%dims(1))
        middle_point%dims(2)=rhalf*(p1%dims(2)+p2%dims(2))
        middle_point%dims(3)=rhalf*(p1%dims(3)+p2%dims(3))
      case default
        middle_point%dims=rhalf*(p1%dims+p2%dims)
      endselect
    endif
  endfunction middle_2points

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  subroutine append_geom_point_list(this,geom_point)
    class(geom_point_list_type),intent(inout) :: this
    type(geom_point_type),intent(in) :: geom_point

    if(this%n_points == 0) then
      this%p = geom_point
      this%stp => this%next
      this%n_points = 1
    elseif(this%n_points == 1) then
      allocate(this%stp)
      this%stp%p = geom_point
      this%stp => this%stp%next
      this%n_points = this%n_points + 1
    endif
  endsubroutine append_geom_point_list

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  subroutine clear_geom_point_list(this)
    class(geom_point_list_type),intent(inout) :: this

    class(geom_point_list_type),pointer :: current => null()
    class(geom_point_list_type),pointer :: next    => null()

    if (associated(this%next)) then
      current => this%next
      do while (associated(current%next))
        next => current%next
        call current%p%clear()
        nullify(current%next)
        deallocate(current)
        current => next
      enddo
    endif
    call this%p%clear()
    this%n_points = 0
  endsubroutine clear_geom_point_list

!-------------------------------------------------------------------------------
!> Inserts a point into a linked list of points.
!> this routine assumes that the point to be inserted will come after
!> stt_point and before the last point in the list. If point
!> returns to the calling routine associated then it was not inserted
!> into the list successfully.
!-------------------------------------------------------------------------------
  subroutine geom_point_linkedList_insert(pointlist,point_in)

    class(geom_point_linkedList_Type),target,intent(inout) :: pointlist
    type(geom_point_linkedList_Type),intent(in) :: point_in

    ! Local
    type(geom_point_linkedList_Type),pointer :: point => null()
    type(geom_point_linkedList_Type),pointer :: point1,point2
    logknd :: is_insert
    realkd :: distance

    allocate(point)
    point = point_in
    selecttype(pointlist); type is(geom_point_linkedList_Type)
      point1 => pointlist
      distance = point%d_to_first
      if(associated(pointlist%next)) then
        is_insert=.true.
        do while(is_insert)
          point2 => point1%next
          if(abs(distance-point1%d_to_first) < epsr) then
            is_insert=.false.
            deallocate(point)
          elseif(abs(distance-point2%d_to_first) < epsr) then
            is_insert=.false.
            deallocate(point)
          elseif(point1%d_to_first < distance .and. distance < &
                 point2%d_to_first) then
            point%next => point2
            point1%next => point
            nullify(point)
            is_insert=.false.
          endif
          point1 => point2
          if(.not.associated(point2%next)) is_insert=.false.
        enddo
      endif
      nullify(point1)
      nullify(point2)
    endselect
  endsubroutine geom_point_linkedList_insert

!-------------------------------------------------------------------------------
! by zhouyuliu
  subroutine geom_point_linkedList_clear(this)
    class(geom_point_linkedList_Type),intent(inout) :: this

    type(geom_point_linkedList_Type),pointer :: current
    type(geom_point_linkedList_Type),pointer :: next


!    real(krp) :: d_to_first = 0.0_KRP !> Distance to a reference point
!    type(geom_point_type) :: p
     !> Next record in the list
!    type(geom_point_linkedList_type),pointer :: next => null()


!    current => this%next
!    do while(associated(current))
!
!
!      next => current%next
!      deallocate(current)
!      current => next
!    enddo

    current => this%next
    do while(associated(current))
      if(associated(current%next)) then
        next => current%next
        nullify(current%next)
        deallocate(current)
        current => next
      else
        nullify(current)
      endif
    enddo

  !write(*,*) 'hahahahahaahhah'
  endsubroutine geom_point_linkedList_clear

!-------------------------------------------------------------------------------
endmodule geom_point




