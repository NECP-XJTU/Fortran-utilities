!###############################################################################
!> Module for math base types
!>
!> @author Chen Zhao
!>   @date 03/18/2015
!>
!> @todo
!>   - Test: Added some comments.
!###############################################################################
module linear_solver
  use linear_solver_base
  use parallel_env

  implicit none
  private

# include <kind_parameter.h>

  public :: linear_solver_direct_type

  !-----------------------------------------------------------------------------
  ! the sparse matrix type
  type,extends(matrix_base_type) :: sparse_matrix_type
    intknd :: nnz = 0
    intknd,allocatable :: ia(:)
    intknd,allocatable :: ja(:)
    realkd,allocatable :: a(:)
  contains
    procedure,pass :: init => sparse_matrix_init
    procedure,pass :: set_value => sparse_matrix_set_value
    procedure,pass :: clear => sparse_matrix_clear
    procedure,pass :: assemble => sparse_matrix_assemble
    procedure,pass :: printout => sparse_matrix_printout
  endtype sparse_matrix_type


  !-----------------------------------------------------------------------------
  ! the direct linear solver type
  type,extends(linear_solver_base_type) :: linear_solver_direct_type

  contains
    procedure,pass :: init => linear_solver_direct_init
    procedure,pass :: solve => linear_solver_direct_solve
    procedure,pass :: clear => linear_solver_direct_clear
  endtype linear_solver_direct_type

  !-----------------------------------------------------------------------------
  ! the iterative linear solver type
  !@ to be done
!   type,extends(linear_solver_base_type) :: linear_solver_iterative_type
!     intknd :: norm = 2
!     realkd :: n_iter = 0
!   contains
!     procedure,pass :: init => linear_solver_iterative_init
!     procedure,pass :: solve => linear_solver_iterative_solve
!     procedure,pass :: clear => linear_solver_iterative_clear
!   endtype linear_solver_iterative_type
!===============================================================================

contains
!-------------------------------------------------------------------------------
!> initialize sparse matrix type
!-------------------------------------------------------------------------------
  subroutine sparse_matrix_init(this,nrow,ncol)
    class(sparse_matrix_type),intent(inout) :: this
    intknd,                   intent(in   ) :: nrow
    intknd,                   intent(in   ) :: ncol

  endsubroutine sparse_matrix_init

!-------------------------------------------------------------------------------
!> set the values in the sparse matrix
!-------------------------------------------------------------------------------
  subroutine sparse_matrix_set_value(this,irow,icol,value)
    class(sparse_matrix_type),intent(inout) :: this
    intknd,                   intent(in   ) :: irow
    intknd,                   intent(in   ) :: icol
    realkd,                   intent(in   ) :: value
  endsubroutine sparse_matrix_set_value

!-------------------------------------------------------------------------------
!> assemble vector
!-------------------------------------------------------------------------------
  subroutine sparse_matrix_assemble(this)
   class(sparse_matrix_type),intent(inout) :: this
  endsubroutine sparse_matrix_assemble

!-------------------------------------------------------------------------------
!> print petsc matrix on the screen
!-------------------------------------------------------------------------------
  subroutine sparse_matrix_printout(this,message)
    class(sparse_matrix_type),intent(inout) :: this
    charSt,                   intent(in   ) :: message !< message for printing
  endsubroutine sparse_matrix_printout

!-------------------------------------------------------------------------------
!> clear the sparse matrix
!-------------------------------------------------------------------------------
  subroutine sparse_matrix_clear(this)
    class(sparse_matrix_type),intent(inout) :: this

  endsubroutine sparse_matrix_clear


!-------------------------------------------------------------------------------
!> initialize direct linear solver type
!-------------------------------------------------------------------------------
  subroutine linear_solver_direct_init(this,para_env,nrow,ncol)
    class(linear_solver_direct_type),intent(inout) :: this
    type(parallel_env_type),pointer, intent(in   ) :: para_env
    intknd,                 intent(in   ) :: nrow
    intknd,                 intent(in   ) :: ncol

  endsubroutine linear_solver_direct_init

!-------------------------------------------------------------------------------
!> solve direct linear solver type
!-------------------------------------------------------------------------------
  subroutine linear_solver_direct_solve(this)
    class(linear_solver_direct_type),intent(inout) :: this

  endsubroutine linear_solver_direct_solve

!-------------------------------------------------------------------------------
!> clear direct linear solver type
!-------------------------------------------------------------------------------
  subroutine linear_solver_direct_clear(this)
    class(linear_solver_direct_type),intent(inout) :: this

  endsubroutine linear_solver_direct_clear

! !-------------------------------------------------------------------------------
! !> initialize iterative linear solver type
! !-------------------------------------------------------------------------------
!   subroutine linear_solver_iterative_init(this)
!     class(linear_solver_iterative_type),intent(inout) :: this

!   endsubroutine linear_solver_iterative_init

! !-------------------------------------------------------------------------------
! !> solve the linear systems iteratively
! !-------------------------------------------------------------------------------
!   subroutine linear_solver_iterative_solve(this)
!     class(linear_solver_iterative_type),intent(inout) :: this
!   endsubroutine linear_solver_iterative_solve

! !-------------------------------------------------------------------------------
! !> clear iterative linear solver type
! !-------------------------------------------------------------------------------
!   subroutine linear_solver_iterative_clear(this)
!     class(linear_solver_iterative_type),intent(inout) :: this

!   endsubroutine linear_solver_iterative_clear
!-------------------------------------------------------------------------------
endmodule linear_solver
