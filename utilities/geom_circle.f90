!###############################################################################
!> Module for defining "circles"  for use in geometry
!>
!> @author Jun Chen
!>   @date 05/02/2015
!>
!> @todo
!>   - Test: Added some comments.
!###############################################################################
module geom_circle

  use math
  use geom_point
  use geom_line
  implicit none
  private

# include <kind_parameter.h>

  public :: geom_circle_type

  type :: geom_circle_type

    type(geom_point_type) :: center
    realkd :: r = 0.0_krp

  contains

    procedure :: init => circle_init
    procedure :: clear => circle_clear
    procedure :: intersect_line => intersect_circle2line

  endtype geom_circle_type

!===============================================================================

contains
!-------------------------------------------------------------------------------
!> Constructor for LineType
!-------------------------------------------------------------------------------
  elemental subroutine circle_init(this,p,r)

    class(geom_circle_type),intent(inout) :: this
    type(geom_point_type),intent(in) :: p
    realkd,intent(in) :: r

    call this%clear()
    if (p%ndim == 2 .and. r > 0.0_krp) then
      this%center = p
      this%r = r
    endif

  endsubroutine

!-------------------------------------------------------------------------------
!> Clear circle
!-------------------------------------------------------------------------------
  elemental subroutine circle_clear(this)

    class(geom_circle_type),intent(inout) :: this

    call this%center%clear()
    this%r = 0.0_krp

  endsubroutine circle_clear

!-------------------------------------------------------------------------------
!> Determines point(s) of intersection between a line and circle (if any)
!>   circle the circle type to test for intersection
!>   line the line type thats being tested against the circle
!>   p1 the first point of intersection (if it exists)
!>   p2 the second point of intersection (if it exists)
!> note a return code is assigned to p1%ndim and p2%ndim indicating the type of
!> intersection.
!>  > 0: success; an intersection point was found
!> == 0: no intersection was found (intersection outside segment)
!>   -1: problem with dummy arguments passed to routine
!>   -2: line is not directed toward circle (disjoint)
!>   -3: line segment is tangent
!-------------------------------------------------------------------------------
  elemental subroutine intersect_circle2line(circle,line,p1,p2)
    class(geom_circle_type),intent(in) :: circle
    type(geom_line_type),intent(in) :: line
    type(geom_point_type),intent(inout) :: p1
    type(geom_point_type),intent(inout) :: p2

    ! Local
    realkd :: a,b,c,t1,t2,u(2),w(2),ra,discr

    call p1%clear()
    call p2%clear()
    p1%ndim = -1
    p2%ndim = -1
    if (circle%center%ndim == 2 .and. circle%r > 0.0_krp) then
    !(circle%center%ndim == 2 .and. line%p1%ndim == 2 .and. &
       !line%p2%ndim == 2 .and. circle%r > 0.0_krp) then  !CSG
       u(1) = line%p2%dims(1) - line%p1%dims(1)
       u(2) = line%p2%dims(2) - line%p1%dims(2)
       w(1) = line%p1%dims(1) - circle%center%dims(1)
       w(2) = line%p1%dims(2) - circle%center%dims(2)
       b = w(1)*u(1)+w(2)*u(2)
       c = w(1)*w(1)+w(2)*w(2)-circle%r*circle%r
       if (c > 0.0_krp .and. b > 0.0_krp) then
         p1%ndim = -2
         p2%ndim = -2
       else
         a = u(1)*u(1)+u(2)*u(2)
         discr = b*b-a*c
         if (discr < -epsr) then
           ! Disjoint
           p1%ndim = -2
           p2%ndim = -2
         elseif (discr .abseq. 0.0_krp) then
           ! Tangent
           p1%ndim = -3
           p2%ndim = -3
         else
           p1%ndim = 0
           p2%ndim = 0
           ra = 1.0_krp/a
           discr = sqrt(discr)
           t1 = (-b-discr)*ra
           t2 = (-b+discr)*ra
           if(0.0_krp < t1 .and. t1 < 1.0_krp) then
             p1 = line%p1
             p1%dims(1) = p1%dims(1)+u(1)*t1
             p1%dims(2) = p1%dims(2)+u(2)*t1
           endif
           if(0.0_krp < t2 .and. t2 < 1.0_krp) then
             p2 = line%p1
             p2%dims(1) = p2%dims(1)+u(1)*t2
             p2%dims(2) = p2%dims(2)+u(2)*t2
           endif
         endif
       endif
     endif
  endsubroutine intersect_circle2line

endmodule