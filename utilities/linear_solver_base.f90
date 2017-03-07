!###############################################################################
!> Module for math base types
!>
!> @author Zhouyu Liu
!>   @date 03/18/2015
!>
!> @todo
!>   - Test: Added some comments.
!###############################################################################
module linear_solver_base
  use parallel_env
  
  implicit none
  private

# include <kind_parameter.h>

  public :: matrix_base_type
  public :: vector_base_type
  public :: linear_solver_base_type
  !-----------------------------------------------------------------------------
  ! the base matrix type
  type,abstract :: matrix_base_type
    logknd :: is_init = .false.
    intknd :: nrow = 0  !< number of global rows
    intknd :: ncol = 0  !< number of global columns
    logknd :: is_assembled = .false.
    logknd :: is_created = .true.
  contains
    procedure(matrix_init_absintf),deferred,pass :: init
    procedure(matrix_set_value_absintf),deferred,pass :: set_value
    procedure(matrix_assemble_absintf),deferred,pass :: assemble
    procedure(matrix_printout_absintf),deferred,pass :: printout
    procedure(matrix_clear_absintf),deferred,pass :: clear
  endtype matrix_base_type

  !-----------------------------------------------------------------------------
  ! the vector base type
  type,abstract :: vector_base_type
    logknd :: is_init = .false.
    intknd :: size = 0     !< size of the vector
    logknd :: is_created = .false.
    logknd :: is_assembled = .false.
  contains
    procedure(vector_init_absintf),deferred,pass :: init
    procedure(vector_set_value_absintf),deferred,pass :: set_value
    procedure(vector_assemble_absintf),deferred,pass :: assemble
    procedure(vector_printout_absintf),deferred,pass :: printout
    procedure(vector_clear_absintf),deferred,pass :: clear
  endtype vector_base_type

  !-----------------------------------------------------------------------------
  ! linear solver base type
  type,abstract :: linear_solver_base_type
    logknd :: is_init = .false.
    intknd :: method = 1                !< 1--gmres, 2--gauss
    intknd :: flag = -1                 !< Return of solver, 0-normal,1-abnormal

    class(vector_base_type),allocatable :: b !< right-hand side vector
    class(vector_base_type),allocatable :: x !< solution vector
    class(matrix_base_type),allocatable :: a !< coefficient matrix
    class(matrix_base_type),allocatable :: m !< decomposed coefficient matrix
    intknd :: solver_type = 0

    type(parallel_env_type), pointer :: para_env => null()
  contains
    procedure(linear_solver_init_absintf),deferred,pass :: init
    procedure(linear_solver_solve_absintf),deferred,pass :: solve
    procedure(linear_solver_clear_asbintf),deferred,pass :: clear
  endtype

  !> matrix interfaces
  abstract interface
    subroutine matrix_init_absintf(this,nrow,ncol)
      import :: matrix_base_type
# include <kind_parameter.h>
      class(matrix_base_type),intent(inout) :: this
      intknd,                 intent(in   ) :: nrow
      intknd,                 intent(in   ) :: ncol
    endsubroutine matrix_init_absintf
  endinterface

  abstract interface
    subroutine matrix_set_value_absintf(this,irow,icol,value)
      import :: matrix_base_type
# include <kind_parameter.h>
      class(matrix_base_type),intent(inout) :: this
      intknd,                 intent(in   ) :: irow
      intknd,                 intent(in   ) :: icol
      realkd,                 intent(in   ) :: value
    endsubroutine matrix_set_value_absintf
  endinterface

  abstract interface
    subroutine matrix_assemble_absintf(this)
      import :: matrix_base_type
      class(matrix_base_type),intent(inout) :: this
    endsubroutine matrix_assemble_absintf
  endinterface

  abstract interface
    subroutine matrix_printout_absintf(this,message)
      import :: matrix_base_type
# include <kind_parameter.h>
      class(matrix_base_type),intent(inout) :: this
      charSt,                 intent(in   ) :: message
    endsubroutine matrix_printout_absintf
  endinterface

  abstract interface
    subroutine matrix_clear_absintf(this)
      import :: matrix_base_type
      class(matrix_base_type),intent(inout) :: this
    endsubroutine matrix_clear_absintf
  endinterface

  !> vector interfaces
  abstract interface
    subroutine vector_init_absintf(this,size)
      import :: vector_base_type
# include <kind_parameter.h>
      class(vector_base_type),intent(inout) :: this
      intknd,                 intent(in   ) :: size
    endsubroutine vector_init_absintf
  endinterface

  abstract interface
    subroutine vector_set_value_absintf(this,isize,value)
      import :: vector_base_type
# include <kind_parameter.h>
      class(vector_base_type),intent(inout) :: this
      intknd,                 intent(in   ) :: isize !< position
      realkd,                 intent(in   ) :: value
    endsubroutine vector_set_value_absintf
  endinterface

  abstract interface
    subroutine vector_assemble_absintf(this)
      import :: vector_base_type
      class(vector_base_type),intent(inout) :: this
    endsubroutine vector_assemble_absintf
  endinterface

  abstract interface
    subroutine vector_printout_absintf(this,message)
      import :: vector_base_type
# include <kind_parameter.h>
      class(vector_base_type),intent(inout) :: this
      charSt,                 intent(in   ) :: message
    endsubroutine vector_printout_absintf
  endinterface

  abstract interface
    subroutine vector_clear_absintf(this)
      import :: vector_base_type
      class(vector_base_type),intent(inout) :: this
    endsubroutine vector_clear_absintf
  endinterface

  !> linear solver interface
  abstract interface
    subroutine linear_solver_init_absintf(this,para_env,nrow,ncol)
      import :: linear_solver_base_type
      import :: parallel_env_type
# include <kind_parameter.h>
      class(linear_solver_base_type) ,intent(inout) :: this
      type(parallel_env_type),pointer,intent(in   ) :: para_env
      intknd,                 intent(in   ) :: nrow
      intknd,                 intent(in   ) :: ncol
    endsubroutine linear_solver_init_absintf
  end interface

  abstract interface
    subroutine linear_solver_solve_absintf(this)
      import :: linear_solver_base_type
      class(linear_solver_base_type),intent(inout) :: this
    endsubroutine
  endinterface

  abstract interface
    subroutine linear_solver_clear_asbintf(this)
      import :: linear_solver_base_type
      class(linear_solver_base_type),intent(inout) :: this
    endsubroutine linear_solver_clear_asbintf
  endinterface
!-------------------------------------------------------------------------------
endmodule linear_solver_base
