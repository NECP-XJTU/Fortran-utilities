!###############################################################################
!> Module for defining "lines"  for use in geometry
!>
!> @author Jun Chen
!>   @date 03/18/2015
!>
!> @todo
!>   - Test: Added some comments.
!###############################################################################
module geom_line
  use geom_point,      only : geom_point_type

  implicit none
  private

# include <kind_parameter.h>

  public :: geom_line_type

  type :: geom_line_type

    type(geom_point_type) :: p1
    type(geom_point_type) :: p2

  !List of type bound procedures
  contains

    procedure,pass :: init => line_init
    procedure,pass :: clear => line_clear
    procedure,pass :: intersect_line => intersect_line2line

  endtype geom_line_type

!===============================================================================

contains
!-------------------------------------------------------------------------------
!> Constructor for LineType
!-------------------------------------------------------------------------------
  !elemental subroutine line_init(this,p1,p2)
  subroutine line_init(this,p1,p2)
  
    class(geom_line_type),intent(inout) :: this
    type(geom_point_type),intent(in) :: p1,p2
    
    call this%clear()
    if(p1%ndim == p2%ndim .AND. p1%ndim > 0 ) then
      if((p1%dims(1) == p2%dims(1)) .and. (p1%dims(2) == p2%dims(2))) then
        write(*,*)'Warning: two end points of line are the same'
      endif
      
      this%p1=p1
      this%p2=p2
    endif
    
  endsubroutine line_init

!-------------------------------------------------------------------------------
!> Clear line
!-------------------------------------------------------------------------------
  elemental subroutine line_clear(this)

    class(geom_line_type),intent(inout) :: this

    call this%p1%clear
    call this%p2%clear
  endsubroutine line_clear

!-------------------------------------------------------------------------------
!> Finds the intersection between 2 line segments (if it exists)
!>   l1 in the line to test for intersection
!>   l2 in the other line to test for intersection
!> Returns p the point of intersection
!> Note a return code is assigned to p%ndim indicating the type of
!>  intersection.
!>    -3: there is no intersection (disjoint)
!>    -2: the line segments overlap
!>    -1: problem with dummy arguments passed to routine
!>    >0: success; an intersection point was found
!-------------------------------------------------------------------------------
   function intersect_line2line(l1,l2) result(p)
  
    class(geom_line_type),intent(in) :: l1
    class(geom_line_type),intent(in) :: l2
    type(geom_point_type) :: p

    p%ndim=-1
    if(l1%p1%ndim == l1%p2%ndim .AND. l2%p1%ndim == l2%p2%ndim .AND. &
      l1%p1%ndim == l2%p1%ndim .AND. l1%p1%ndim > 0) then
      SELECTCASE(l1%p1%ndim)
      CASE(1)
        p%ndim=-2 !Always collinear/overlap
      CASE(2)
        p=intersect_lines2D(l1%p1,l1%p2,l2%p1,l2%p2)
      CASE(3)
        p=intersect_lines2D(l1%p1,l1%p2,l2%p1,l2%p2)
      endselect
    endif
  endfunction intersect_line2line

!-------------------------------------------------------------------------------
!> Find the intersection between two 2-D line segments (if it exists)
!>   s1p0 in the line to test for intersection
!>   s1p1 in the line to test for intersection
!>   s2p0 in the line to test for intersection
!>   s2p1 in the line to test for intersection
!> Returns p the point of intersection (if it exists)
!> Note a return code is assigned to p%ndim indicating the type of
!>  intersection.
!>    -3: there is no intersection (disjoint)
!>    -2: the line segments overlap
!>   > 0: success; an intersection point was found
!-------------------------------------------------------------------------------
  elemental function intersect_lines2D(s1p0,s1p1,s2p0,s2p1) result(p)

    type(geom_point_type),intent(in) :: s1p0,s1p1,s2p0,s2p1
    
    ! Local
    type(geom_point_type) :: p
    real(KRP) :: u(2),v(2),w(2),d,s,t

    u(1)=s1p1%dims(1)-s1p0%dims(1)
    u(2)=s1p1%dims(2)-s1p0%dims(2)
    v(1)=s2p1%dims(1)-s2p0%dims(1)
    v(2)=s2p1%dims(2)-s2p0%dims(2)
    w(1)=s1p0%dims(1)-s2p0%dims(1)
    w(2)=s1p0%dims(2)-s2p0%dims(2)
    ! Slope
    d=u(1)*v(2)-u(2)*v(1)
    s=v(1)*w(2)-v(2)*w(1)
    t=u(1)*w(2)-u(2)*w(1)
    ! Same slope
    if(abs(d) < epsr) then
      !Segments are collinear
      if(s /= 0._KRP .OR. t /= 0._KRP) then
        p%ndim=-3 !parallel lines
      else
        p%ndim=-2 !overlap
      endif
    else
      s=s/d
      t=t/d
      if((0._KRP <= s .AND. s <= 1.0_KRP) .AND. &
         (0._KRP <= t .AND. t <= 1.0_KRP)) then
        !Success, intersection point was found.
        p=s1p0
        p%dims(1)=p%dims(1)+s*u(1)
        p%dims(2)=p%dims(2)+s*u(2)
      else
        !would intersect if segments were infinite. Still store how close it
        !was though. this is useful for calling routines that might need to
        !know that the point was close (i.e. Coarse ray trace)
        p=s1p0
        p%ndim=-3
        p%dims(1)=p%dims(1)+s*u(1)
        p%dims(2)=p%dims(2)+s*u(2)
      endif
    endif
  endfunction intersect_lines2D

!-------------------------------------------------------------------------------
!> Find the intersection between two 3-D line segments (if it exists)
!>   l1p1 in the line to test for intersection
!>   l1p2 in the line to test for intersection
!>   l2p1 in the line to test for intersection
!>   l2p2 in the line to test for intersection
!> Returns p the point of intersection (if it exists)
!> Note a return code is assigned to p%ndim indicating the type of
!>  intersection.
!>    -3: there is no intersection (disjoint),the two lines in different face
!>    -2: the line segments overlap
!>   > 0: success; an intersection point was found
!>    added by lucao for 3D CSG
!-------------------------------------------------------------------------------
  elemental function intersect_lines3D(l1p1,l1p2,l2p1,l2p2) result(p)

    type(geom_point_type),intent(in) :: l1p1,l1p2,l2p1,l2p2
    
    ! Local
    type(geom_point_type) :: p
    real(KRP) :: a(3),b(3),c(3)

    a(1)=l1p2%dims(1)-l1p1%dims(1)
    a(2)=l2p1%dims(1)-l1p1%dims(1)
    a(3)=l2p2%dims(1)-l1p1%dims(1)
    b(1)=l1p2%dims(2)-l1p1%dims(2)
    b(2)=l2p1%dims(2)-l1p1%dims(2)
    b(3)=l2p2%dims(2)-l1p1%dims(2)
    c(1)=l1p2%dims(3)-l1p1%dims(3)
    c(2)=l2p1%dims(3)-l1p1%dims(3)
    c(3)=l2p2%dims(3)-l1p1%dims(3)
    if((a(1)*b(2)==b(1)*a(2) .and. a(1)*b(3)==b(1)*a(3)).and.                  &
       (a(1)*b(3)==b(1)*a(3) .and. a(1)*c(3)==c(1)*a(3))) then
      p%ndim=-2 !overlap
    else
      if((a(1)*b(2)*c(3)+a(2)*b(3)*c(1)+a(3)*b(1)*c(2)-a(3)*b(2)*c(1)-a(1)*b(3)&
          *c(2)-a(2)*b(1)*c(3)) /= 0.0_krp) then
        p%ndim=-3 !there is no intersection,the two lines in different face
      else
        ! determine if the two lines are parallel lines
        if(((a(3)-a(2))*b(1) == (b(3)-b(2))*a(1)) .and. ((a(3)-a(2))*c(1) ==   &
            (c(3)-c(2))*a(1))) then
          p%ndim = -3 !parallel lines
        else
          p=l1p1
          p%dims(1)=((a(1)*a(3)*b(2)-a(1)*a(2)*b(3))/(b(1)*a(3)+b(2)*a(1)-b(3) &
            *a(1)-a(2)*b(1)))+l1p1%dims(1)
          p%dims(2)=((b(1)*a(3)*b(2)-b(1)*a(2)*b(3))/(b(1)*a(3)+b(2)*a(1)-b(3) &
            *a(1)-a(2)*b(1)))+l1p1%dims(2)
          p%dims(3)=((c(1)*a(3)*b(2)-c(1)*a(2)*b(3))/(b(1)*a(3)+b(2)*a(1)-b(3) &
            *a(1)-a(2)*b(1)))+l1p1%dims(3)
        endif
      endif
    endif
  endfunction intersect_lines3D
!-------------------------------------------------------------------------------
endmodule geom_line