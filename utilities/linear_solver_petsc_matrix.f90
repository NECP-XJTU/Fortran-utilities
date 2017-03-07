!###############################################################################
!> Module for math base types
!>
!> @author Chen Zhao
!>   @date 03/18/2015
!>
!> @to do
!>   - Test: Added some comments.
!###############################################################################
module linear_solver_petsc_matrix
  use error_warning
  use linear_solver_base
  use parallel_env, only : parallel_env_type

  implicit none
  private

# include <kind_parameter.h>

#ifdef WITH_PETSC
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscvec.h90>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscksp.h>
#include <petsc/finclude/petscpc.h>
#endif

  !> iteration method
  !intknd,parameter,public :: gmres = 1

  public :: linear_solver_petsc_matrix_type
  !-----------------------------------------------------------------------------
  ! the petsc matrix type
  type,extends(matrix_base_type) :: petsc_matrix_type
    intknd :: comm = -1
#ifdef WITH_PETSC
    Mat :: matrix
#endif
  contains
    procedure,pass :: init => petsc_matrix_init
    procedure,pass :: set_value => petsc_matrix_set_value
    procedure,pass :: assemble => petsc_matrix_assemble
    procedure,pass :: printout => petsc_matrix_printout
    procedure,pass :: clear => petsc_matrix_clear
  endtype petsc_matrix_type


  !-----------------------------------------------------------------------------
  ! the petsc vector type
  type,extends(vector_base_type) :: petsc_vector_type
#ifdef WITH_PETSC
    Vec :: vector
#endif
    intknd :: comm = -1
  contains
    procedure,pass :: init => petsc_vector_init
    procedure,pass :: set_value => petsc_vector_set_value
    procedure,pass :: assemble => petsc_vector_assemble
    procedure,pass :: printout => petsc_vector_printout
    procedure,pass :: clear => petsc_vector_clear
  endtype


  !-----------------------------------------------------------------------------
  ! the direct linear solver type with PETSC
  type,extends(linear_solver_base_type) :: linear_solver_petsc_matrix_type
    intknd :: norm = 2
    realkd,pointer :: results(:)  !> save results as an array

#ifdef WITH_PETSC
    KSP :: ksp
    PC  :: pc
    intknd :: n_proc !< total CPU number for solver
    intknd :: rank   !< rank number for the CPU
    realkd :: tolerance !< convergence criterion
#endif
  contains
    procedure,pass :: init => linear_solver_petsc_matrix_init
    procedure,pass :: solve => linear_solver_petsc_matrix_solve
    procedure,pass :: clear => linear_solver_petsc_matrix_clear
    procedure,pass :: get_result => linear_solver_petsc_matrix_get_result
  endtype linear_solver_petsc_matrix_type



!===============================================================================

contains
!-------------------------------------------------------------------------------
!> initialize petsc matrix type
!-------------------------------------------------------------------------------
  subroutine petsc_matrix_init(this,nrow,ncol)
    class(petsc_matrix_type),intent(inout) :: this
    intknd,                  intent(in   ) :: nrow
    intknd,                  intent(in   ) :: ncol

    intknd :: size_matrix  !< maximum of matrix size

#ifdef WITH_PETSC
    PetscErrorCode :: ierr
    size_matrix = max(nrow,ncol)
    call MatCreate(PETSC_COMM_WORLD,this%matrix,ierr)
    call MatSetSizes(this%matrix,PETSC_DECIDE,PETSC_DECIDE,nrow,ncol,ierr)

    call MatSetType(this%matrix, MATAIJ,ierr)
    call MatSetFromOptions(this%matrix,ierr)

    call MatMPIAIJSetPreallocation(this%matrix,size_matrix,PETSC_NULL_INTEGER, &
      size_matrix,PETSC_NULL_INTEGER,ierr)

    call MatSeqAIJSetPreallocation(this%matrix,size_matrix,PETSC_NULL_INTEGER, &
      ierr)

    !call MatGetOwnershipRange(this%matrix,self%istart,self%iend,ierr)

    this%is_created = .true.
#endif

  endsubroutine petsc_matrix_init

!-------------------------------------------------------------------------------
!> set value in the petsc matrix
!-------------------------------------------------------------------------------
  subroutine petsc_matrix_set_value(this,irow,icol,value)
    class(petsc_matrix_type),intent(inout) :: this
    intknd,                  intent(in   ) :: irow
    intknd,                  intent(in   ) :: icol
    realkd,                  intent(in   ) :: value

#ifdef WITH_PETSC
    PetscErrorCode :: ierr

    call MatSetValues(this%matrix,1,irow,1,icol,value,ADD_VALUES,ierr)

#endif
  endsubroutine petsc_matrix_set_value

!-------------------------------------------------------------------------------
!> assemble matrix
!-------------------------------------------------------------------------------
  subroutine petsc_matrix_assemble(this)
    class(petsc_matrix_type),intent(inout) :: this
#ifdef WITH_PETSC
    PetscErrorCode :: ierr

    call MatAssemblyBegin(this%matrix,MAT_FINAL_ASSEMBLY,ierr)
    call MatAssemblyEnd(this%matrix,MAT_FINAL_ASSEMBLY,ierr)

    this%is_assembled = .true.
#endif
  endsubroutine petsc_matrix_assemble

!-------------------------------------------------------------------------------
!> print petsc matrix on the screen
!-------------------------------------------------------------------------------
  subroutine petsc_matrix_printout(this,message)
    class(petsc_matrix_type),intent(inout) :: this
    charSt,                  intent(in   ) :: message !< message for printing

#ifdef WITH_PETSC
    PetscErrorCode :: ierr

    call PetscObjectSetName(this%matrix,message,ierr)
    call MatView(this%matrix,Petsc_VIEWER_STDOUT_WORLD,ierr)
#endif
  endsubroutine petsc_matrix_printout

!-------------------------------------------------------------------------------
!> clear the petsc matrix
!-------------------------------------------------------------------------------
  subroutine petsc_matrix_clear(this)
    class(petsc_matrix_type),intent(inout) :: this
#ifdef WITH_PETSC
    PetscErrorCode :: ierr

    call MatDestroy(this%matrix,ierr)

    this%is_created = .false.
#endif
  endsubroutine petsc_matrix_clear

!-------------------------------------------------------------------------------
!> initialize petsc vector type
!-------------------------------------------------------------------------------
  subroutine petsc_vector_init(this,size)
    class(petsc_vector_type),intent(inout) :: this
    intknd,                  intent(in   ) :: size !< vector size

#ifdef WITH_PETSC
    PetscErrorCode :: ierr

    call VecCreateMPI(PETSC_COMM_WORLD,PETSC_DECIDE,size,this%vector,ierr)
    call VecSet(this%vector,1.0_krp,ierr)

    this%is_created = .true.
#endif
  endsubroutine petsc_vector_init

!-------------------------------------------------------------------------------
!> set value in the petsc vector
!-------------------------------------------------------------------------------
  subroutine petsc_vector_set_value(this,isize,value)
    class(petsc_vector_type),intent(inout) :: this
    intknd,                  intent(in   ) :: isize !< position
    realkd,                  intent(in   ) :: value

#ifdef WITH_PETSC
    PetscErrorCode :: ierr

    call VecSetValues(this%vector,1,isize,value,INSERT_VALUES,ierr)
#endif

  endsubroutine petsc_vector_set_value

!-------------------------------------------------------------------------------
!> assemble vector
!-------------------------------------------------------------------------------
  subroutine petsc_vector_assemble(this)
    class(petsc_vector_type),intent(inout) :: this

#ifdef WITH_PETSC
    PetscErrorCode :: ierr

    call VecAssemblyBegin(this%vector,ierr)
    call VecAssemblyEnd(this%vector,ierr)

    this%is_assembled = .true.
#endif
  endsubroutine petsc_vector_assemble

!-------------------------------------------------------------------------------
!> print petsc matrix on the screen
!-------------------------------------------------------------------------------
  subroutine petsc_vector_printout(this,message)
    class(petsc_vector_type),intent(inout) :: this
    charSt,                  intent(in   ) :: message !< message for printing

#ifdef WITH_PETSC
    PetscErrorCode :: ierr

    call PetscObjectSetName(this%vector, message ,ierr)
    call VecView(this%vector,Petsc_VIEWER_STDOUT_SELF,ierr)
#endif
  endsubroutine petsc_vector_printout

!-------------------------------------------------------------------------------
!> clear the petsc vector
!-------------------------------------------------------------------------------
  subroutine petsc_vector_clear(this)
    class(petsc_vector_type),intent(inout) :: this
#ifdef WITH_PETSC
    PetscErrorCode :: ierr

    call VecDestroy(this%vector,ierr)

    this%is_created = .true.
#endif
  endsubroutine petsc_vector_clear

!-------------------------------------------------------------------------------
!> initialize linear solver with petsc type
!>
!> nrow - in, row number(y direction) for matrix
!> ncol - in, column number(x direct) for matrix
!-------------------------------------------------------------------------------
  subroutine linear_solver_petsc_matrix_init(this,para_env,nrow,ncol)
    class(linear_solver_petsc_matrix_type),intent(inout) :: this
    type(parallel_env_type),pointer,       intent(in   ) :: para_env
    intknd,                 intent(in   ) :: nrow
    intknd,                 intent(in   ) :: ncol

    type(petsc_matrix_type) :: petsc_matrix_a
    type(petsc_vector_type) :: petsc_vector_b
    type(petsc_vector_type) :: petsc_vector_x

#ifdef WITH_PETSC
    PetscErrorCode :: ierr
    PC :: pc

    !> set convergence criterion
    this%tolerance = 1.0e-7

    !> PETSc initialized
    call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
    call MPI_Comm_size(PETSC_COMM_WORLD,this%n_proc,ierr)
    call MPI_Comm_rank(PETSC_COMM_WORLD,this%rank,ierr)

    !> matrix initialized
    allocate(petsc_matrix_type :: this%a)
    call this%a%init(nrow,ncol)

    !> vector initialized
    allocate(petsc_vector_type :: this%b)
    allocate(petsc_vector_type :: this%x)

    call this%b%init(nrow)  !< initial and set initialized value
    call this%x%init(nrow)  !< initial and set initialized value

    allocate(this%results(nrow))
    this%results = rzero

    call KSPCreate(PETSC_COMM_WORLD,this%ksp,ierr)

    select type(petsc_matrix_a => this%a)
    type is(petsc_matrix_type)
      call KSPSetOperators(this%ksp,petsc_matrix_a%matrix,petsc_matrix_a%matrix,ierr)
    endselect

    call KSPSetTolerances(this%ksp,this%tolerance,PETSC_DEFAULT_REAL,                &
       PETSC_DEFAULT_REAL,PETSC_DEFAULT_INTEGER,ierr)

    !> set solver method
    call KSPSetType(this%ksp,KSPGMRES,ierr)
    !call KSPSetType(this%ksp,KSPPREONLY,ierr)

    call KSPGetPC(this%ksp,this%pc,ierr)

    !> set preprocessing method
    call PCSetType(this%pc,PCJACOBI,ierr)

    call KSPSetFromOptions(this%ksp,ierr)

    call KSPSetUp(this%ksp,ierr)

#endif
    this%is_init = .true.
  endsubroutine linear_solver_petsc_matrix_init

!-------------------------------------------------------------------------------
!> clear iterative linear solver type
!-------------------------------------------------------------------------------
  subroutine linear_solver_petsc_matrix_solve(this)
    class(linear_solver_petsc_matrix_type),intent(inout) :: this

    type(petsc_matrix_type),pointer :: matrix
    type(petsc_vector_type),pointer :: vector_b
    type(petsc_vector_type),pointer :: vector_x

#ifdef WITH_PETSC
    PetscErrorCode :: ierr
#endif
    this%flag = -1

#ifdef WITH_PETSC
    !> assemble matrix and vector if necessary
    if(.not.(this%a%is_assembled)) call this%a%assemble()
    if(.not.(this%b%is_assembled)) call this%b%assemble() !< assemble right-hand
    if(.not.(this%x%is_assembled)) call this%x%assemble() !< assemble right-hand

    !> solve
    select type(vector_b => this%b)
    type is(petsc_vector_type)
      select type(vector_x => this%x)
      type is(petsc_vector_type)
        call KSPSolve(this%ksp,vector_b%vector,vector_x%vector,ierr)
      endselect
    endselect
    if(ierr == 0) this%flag = 0


#endif

  endsubroutine linear_solver_petsc_matrix_solve

!-------------------------------------------------------------------------------
!> get result from the solution vector
!-------------------------------------------------------------------------------
  subroutine linear_solver_petsc_matrix_get_result(this)
    class(linear_solver_petsc_matrix_type),intent(inout) :: this
#ifdef WITH_PETSC
    PetscErrorCode :: ierr
    type(petsc_vector_type),pointer :: vector_x

    if(this%flag /= 0) then
      call raise_error(' KSPSolve error in PETSc!')
    endif

    select type(vector_x => this%x)
    type is(petsc_vector_type)
      call VecGetArrayF90(vector_x%vector,this%results,ierr)
!write(*,*) this%results
    endselect
#endif
  endsubroutine linear_solver_petsc_matrix_get_result
!-------------------------------------------------------------------------------
!> clear iterative linear solver type
!-------------------------------------------------------------------------------
  subroutine linear_solver_petsc_matrix_clear(this)
    class(linear_solver_petsc_matrix_type),intent(inout) :: this
#ifdef WITH_PETSC
    PetscErrorCode :: ierr
    call KSPDestroy(this%ksp,ierr)
    call this%a%clear
    call this%x%clear
    call this%b%clear
#endif
  endsubroutine linear_solver_petsc_matrix_clear

!-------------------------------------------------------------------------------
endmodule linear_solver_petsc_matrix
