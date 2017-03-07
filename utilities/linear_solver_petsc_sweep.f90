!###############################################################################
!> Module for math base types
!>
!> @author Chen Zhao
!>   @date 03/18/2015
!>
!> @to do
!>   - Test: Added some comments.
!>
!> Note:
!>   1) the method with PETSc uses free-matrix
!>   2) external subroutine -- mymult
!###############################################################################
module linear_solver_petsc_sweep
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
#include <petsc/finclude/petscviewer.h>
#endif

  public :: linear_solver_petsc_sweep_type

  !-----------------------------------------------------------------------------
  !> type to get cmfd data
  !> notice:
  !>   1) get cmfd data from cmfd part
  type :: interface_data_type
    intknd :: a = 0
    intknd :: b = 0
    intknd :: c = 0

    intknd :: mg = -1
    intknd :: stt_g = 0
    intknd :: stp_g = -1
    intknd :: stt_cm = 0
    intknd :: stp_cm = -1
    intknd :: n_it_surf = -1
    intknd :: n_bc_surf = -1
    intknd :: n_inner_bc_part_xyz = 0
    intknd :: n_bc_part = -1
    intknd :: n_ghost
    intknd :: i_iter = 0

    type(parallel_env_type), pointer :: para_env => null()

    realkd,allocatable :: dtil_surf(:,:)
    realkd,allocatable :: dhat_surf(:,:)
    realkd,allocatable :: dtil_bc(:,:)
    realkd,allocatable :: dhat_bc(:,:)

    intknd,allocatable :: pos_icm(:)
    intknd,allocatable :: neg_icm(:)
    intknd,allocatable :: self_icm(:)
    intknd,allocatable :: ghost_icm(:)    !(n_ghost)
    intknd,allocatable :: self_surf_bc(:) !(n_bc_surf)
    intknd,allocatable :: ghost_surf(:)
    intknd,allocatable :: self_global(:)
    intknd,allocatable :: ghost_global(:)
    intknd,allocatable :: adja_surf(:)


    realkd,allocatable :: it_area(:)
    realkd,allocatable :: bc_area(:)
    realkd,allocatable :: crnt_bc(:,:)

    intknd,allocatable :: stt_stp_bcsurf(:,:)
    intknd,allocatable :: bc_cond(:)
    intknd,allocatable :: stt_cm_rank_1(:) !< from core_mesh_cmfd

    realkd,allocatable :: xs_tr(:,:)
    realkd,allocatable :: vol(:)
    realkd,allocatable :: xs_sc(:,:,:) !fromig,ig,icm

    realkd,allocatable :: phi_s_old(:,:)
    realkd,allocatable :: phi_ghost(:,:)      !(ng,n_ghost)
    realkd,allocatable :: phi_ghost_send(:,:) !(ng,n_ghost)

    logknd :: is_init = .false.
  endtype interface_data_type


  !-----------------------------------------------------------------------------
  ! the petsc matrix type
  type,extends(matrix_base_type) :: petsc_matrix_free_type
    intknd :: comm = -1
#ifdef WITH_PETSC
    Mat :: matrix
#endif
  contains
    procedure,pass :: init => petsc_matrix_free_init
    procedure,pass :: set_value => petsc_matrix_free_set_value
    procedure,pass :: assemble => petsc_matrix_free_assemble
    procedure,pass :: printout => petsc_matrix_free_printout
    procedure,pass :: clear => petsc_matrix_free_clear
  endtype petsc_matrix_free_type


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
    procedure,pass :: array_get
    procedure,pass :: array_restore
  endtype petsc_vector_type


  !-----------------------------------------------------------------------------
  !> the direct linear solver type with PETSC
  type,extends(linear_solver_base_type) :: linear_solver_petsc_sweep_type
    intknd :: norm = 2

    type(interface_data_type) :: indata

    realkd,pointer :: results(:)

#ifdef WITH_PETSC
    KSP :: ksp
    PC  :: pc
    intknd      :: n_proc = 0         !< total CPU number for solver
    intknd      :: rank = 0           !< rank number for the CPU
    PetscScalar :: tolerance = 1.0e-7 !< convergence criterion
    logknd      :: is_true = .true.
#endif

  contains
    procedure,pass :: init => linear_solver_petsc_sweep_init
    procedure,pass :: solve => linear_solver_petsc_sweep_solve
    procedure,pass :: clear => linear_solver_petsc_sweep_clear
    procedure,pass :: get_result => linear_solver_petsc_sweep_get_result
    procedure,pass :: get_data => linear_solver_petsc_sweep_get_data
    procedure,pass :: get_init_data => linear_solver_petsc_sweep_get_init_data
    procedure,pass :: set_b => linear_solver_petsc_sweep_set_b
  endtype linear_solver_petsc_sweep_type


!===============================================================================
    contains

#ifdef WITH_PETSC
  !---------------------------------------------------
  !interface subroutine
  subroutine mymult(A,x,y,ierr)
    Mat :: A
    Vec :: x
    Vec :: y
    PetscErrorCode :: ierr

    intknd :: i,isurf,ig,ig2,ineigh,icm,index1,index2,neg_loc,pos_loc,self_loc
    intknd :: pos_icm,neg_icm,self_icm,ghost_icm,self_surf,ghost_surf,rank_comm
    realkd :: neg_d,self_d
    intknd :: self_global,ghost_global
    intknd :: stt_surf,stp_surf,adja_surf,stt_adja_surf,stp_adja_surf
    intknd :: ipart
    intknd :: comm
    intknd :: num_surf_comm

    realkd,allocatable :: phi_ghost_send(:)
    intknd,allocatable :: send_request(:),recv_request(:)
    class(interface_data_type),pointer :: indata
    type(interface_data_type),target :: target1

    PetscScalar,pointer :: x_real(:)
    PetscScalar,pointer :: y_real(:)

    indata => target1

    call MatShellGetContext(A,indata,ierr)

    call VecGetArrayReadF90(x,x_real,ierr)
    call VecGetArrayReadF90(y,y_real,ierr)

    indata%i_iter = indata%i_iter + 1

    y_real = 0.0_krp

    allocate(send_request(indata%n_inner_bc_part_xyz))
    allocate(recv_request(indata%n_inner_bc_part_xyz))
    send_request = -1
    recv_request = -1
    ipart = 0
    do ineigh = 1, indata%n_bc_part
      !> Parallel boundary condition
      if(indata%bc_cond(ineigh) < 0) then
        rank_comm = - indata%bc_cond(ineigh) - 1
        ipart = ipart + 1
        stt_surf = indata%stt_stp_bcsurf(1,ineigh)
        stp_surf = indata%stt_stp_bcsurf(2,ineigh)
        stt_adja_surf = indata%adja_surf(stp_surf)
        stp_adja_surf = -1
        !> send along positive direction
        do isurf = stt_surf,stp_surf
          self_icm = indata%self_icm(isurf)
          adja_surf = indata%adja_surf(isurf)

          stt_adja_surf = min(adja_surf,stt_adja_surf)
          stp_adja_surf = max(adja_surf,stp_adja_surf)

          self_loc = (self_icm-1)*indata%mg
          indata%phi_ghost_send(:,adja_surf) =  x_real(self_loc+1:self_loc+indata%mg)
        enddo !isurf

        call indata%para_env%space%send(buf=indata%phi_ghost_send(:,stt_adja_surf:stp_adja_surf), &
          dest=rank_comm,tag=rank_comm+1000,  &
          request=send_request(ipart),blocking=.false.)
        call indata%para_env%space%receive(buf=indata%phi_ghost(:,stt_surf:stp_surf), &
          source=rank_comm,tag=indata%para_env%space%rank+1000, &
          request=recv_request(ipart),blocking=.false.)
      endif
    enddo !ineigh
    call indata%para_env%space%waitall(send_request(:))
    call indata%para_env%space%waitall(recv_request(:))

!-----------------------    execute Ax calculation -----------------------------
    !> deal with intersection
    do isurf = 1, indata%n_it_surf
      pos_icm = indata%pos_icm(isurf)
      neg_icm = indata%neg_icm(isurf)
      do ig = 1, indata%mg
        pos_loc = (pos_icm-1)*indata%mg+ig
        neg_loc = (neg_icm-1)*indata%mg+ig
        y_real(pos_loc) = y_real(pos_loc)                                      &
          + (indata%dtil_surf(ig,isurf) + indata%dhat_surf(ig,isurf)) *        &
          indata%it_area(isurf) * x_real(pos_loc)                              &
          + (- indata%dtil_surf(ig,isurf) + indata%dhat_surf(ig,isurf)) *      &
          indata%it_area(isurf) * x_real(neg_loc)
        y_real(neg_loc) = y_real(neg_loc)                                      &
          + (indata%dtil_surf(ig,isurf) - indata%dhat_surf(ig,isurf)) *        &
          indata%it_area(isurf) * x_real(neg_loc)                              &
          + ( - indata%dtil_surf(ig,isurf) - indata%dhat_surf(ig,isurf)) *     &
          indata%it_area(isurf) * x_real(pos_loc)
      enddo
    enddo

    !> deal with boundary
    do ineigh = 1,indata%n_bc_part
      !> reflect and vaccum boundary
      if(indata%bc_cond(ineigh) >= 0) then
        do isurf = indata%stt_stp_bcsurf(1,ineigh), &
                   indata%stt_stp_bcsurf(2,ineigh)
          self_icm = indata%self_icm(isurf)
          do ig = 1, indata%mg
            index1 = (self_icm-1)*indata%mg+ig

            y_real(index1) = y_real(index1) + indata%bc_area(isurf) *          &
              abs(indata%crnt_bc(ig,isurf)) / indata%phi_s_old(ig,self_icm) *  &
              x_real(index1)
          enddo
        enddo !isurf

      !> Parallel boundary condition
      else
        do isurf = indata%stt_stp_bcsurf(1,ineigh), &
                   indata%stt_stp_bcsurf(2,ineigh)
          self_icm = indata%self_icm(isurf)
          ghost_icm = indata%ghost_icm(isurf)
          self_surf = indata%self_surf_bc(isurf)
          ghost_surf = indata%ghost_surf(isurf)

          !> decide the negative or positive position.
          selectcase(self_surf)
          case(north)
            pos_icm = ghost_icm
            neg_icm = self_icm
          case(south)
            pos_icm = self_icm
            neg_icm = ghost_icm
          case(west)
            pos_icm = self_icm
            neg_icm = ghost_icm
          case(east)
            pos_icm = ghost_icm
            neg_icm = self_icm
          case(top)
            pos_icm = ghost_icm
            neg_icm = self_icm
          case(bottom)
            pos_icm = self_icm
            neg_icm = ghost_icm
          endselect

          do ig = 1, indata%mg
            pos_loc = (pos_icm-1)*indata%mg+ig
            neg_loc = (neg_icm-1)*indata%mg+ig

            if(self_icm == pos_icm) then
              y_real(pos_loc) = y_real(pos_loc)                                  &
                + (indata%dtil_bc(ig,isurf) + indata%dhat_bc(ig,isurf)) *        &
                indata%bc_area(isurf) * x_real(pos_loc)                          &
                + (- indata%dtil_bc(ig,isurf) + indata%dhat_bc(ig,isurf)) *      &
                indata%bc_area(isurf) * indata%phi_ghost(ig,ghost_icm)

            elseif(self_icm == neg_icm) then
              y_real(neg_loc) = y_real(neg_loc)                                  &
                + (indata%dtil_bc(ig,isurf) - indata%dhat_bc(ig,isurf)) *        &
                indata%bc_area(isurf) * x_real(neg_loc)                          &
                + ( - indata%dtil_bc(ig,isurf) - indata%dhat_bc(ig,isurf)) *     &
                indata%bc_area(isurf) * indata%phi_ghost(ig,ghost_icm)
            endif
          enddo !ig
        enddo !isurf
      endif
    enddo !ineigh

    !> Deal with group(scattering sources are on the left of equation)
    do icm = indata%stt_cm, indata%stp_cm
      do ig = indata%stt_g,indata%stp_g
        index1 = (icm-1)*indata%mg+ig
        y_real(index1) = y_real(index1) + x_real(index1) * &
          indata%xs_tr(ig,icm) * indata%vol(icm)
        do ig2 = indata%stt_g,indata%stp_g
          index2 = (icm-1)*indata%mg+ig2
          y_real(index1) = y_real(index1) - x_real(index2) * &
            indata%xs_sc(ig2,ig,icm) * indata%vol(icm)
        enddo !ig2
      enddo !ig
    enddo !icm

!-------------------- end executing Ax calculation  ----------------------------
    deallocate(send_request)
    deallocate(recv_request)

    call VecRestoreArrayF90(x,x_real,ierr)
    call VecRestoreArrayF90(y,y_real,ierr)

    return
  endsubroutine mymult

#endif

!-------------------------------------------------------------------------------
!> set values of b items on the right of equation
!> notice:
!>   1) use global id on vector for each CPU
!>   2) vector id starts from 0(PETSc)
!-------------------------------------------------------------------------------
  subroutine linear_solver_petsc_sweep_set_b(this,q_ext)
    class(linear_solver_petsc_sweep_type),intent(inout) :: this
    realkd,  intent(in) :: q_ext(:,:)

    intknd :: ig,icm,ierr,icm_global
    class(petsc_vector_type),pointer :: vector_b

    select type(vector_b => this%b)
    type is(petsc_vector_type)

      do icm = this%indata%stt_cm,this%indata%stp_cm
        icm_global = this%indata%stt_cm_rank_1(this%indata%para_env%space%rank)
        do ig = 1, this%indata%mg
          call vector_b%set_value((icm_global+icm-1)*this%indata%mg+ig-1,q_ext(ig,icm))
        enddo
      enddo

    endselect

  endsubroutine linear_solver_petsc_sweep_set_b

!-------------------------------------------------------------------------------
!> get data from other types
!-------------------------------------------------------------------------------
  subroutine linear_solver_petsc_sweep_get_init_data(this,para_env,            &
    mg,stt_g,stp_g,stt_cm,stp_cm,n_it_surf,n_bc_part,                          &
    n_inner_bc_part_xyz, n_bc_surf,                                            &
    n_ghost,pos_icm,neg_icm,self_icm,ghost_icm,self_surf_bc,ghost_surf,        &
    self_global,ghost_global,adja_surf,it_area,bc_area,                        &
    stt_stp_bcsurf,bc_cond,vol,stt_cm_rank_1)

    charSt,parameter :: myName = 'linear_solver_petsc_sweep => get_init_data ::'
    class(linear_solver_petsc_sweep_type),intent(inout) :: this
    type(parallel_env_type),      pointer,intent(in   ) :: para_env
    intknd,  intent(in) :: mg
    intknd,  intent(in) :: stt_g
    intknd,  intent(in) :: stp_g
    intknd,  intent(in) :: stt_cm
    intknd,  intent(in) :: stp_cm
    intknd,  intent(in) :: n_it_surf
    intknd,  intent(in) :: n_bc_part
    intknd,  intent(in) :: n_inner_bc_part_xyz
    intknd,  intent(in) :: n_bc_surf
    intknd,  intent(in) :: n_ghost
    intknd,  intent(in) :: pos_icm(:)
    intknd,  intent(in) :: neg_icm(:)
    intknd,  intent(in) :: self_icm(:)
    intknd,  intent(in) :: ghost_icm(:)
    intknd,  intent(in) :: self_surf_bc(:)
    intknd,  intent(in) :: ghost_surf(:)
    intknd,  intent(in) :: self_global(:)
    intknd,  intent(in) :: ghost_global(:)
    intknd,  intent(in) :: adja_surf(:)

    realkd,  intent(in) :: it_area(:)
    realkd,  intent(in) :: bc_area(:)
    realkd,  intent(in) :: stt_stp_bcsurf(:,:)
    intknd,  intent(in) :: bc_cond(:)
    realkd,  intent(in) :: vol(:)
    intknd,  intent(in) :: stt_cm_rank_1(:)

    intknd :: nError

    nError = get_nError()
    !> check para_env
    if(.not.(associated(para_env))) then
      call raise_error(myName//' para_env is not associated')
    elseif(.not.para_env%is_init)then
      call raise_error(myName//' para_env is not initialized')
    endif

    if(nError == get_nError()) then
      this%indata%para_env => para_env
      this%indata%mg        = mg
      this%indata%stt_g     = stt_g
      this%indata%stp_g     = stp_g
      this%indata%stt_cm    = stt_cm
      this%indata%stp_cm    = stp_cm

      this%indata%n_it_surf = n_it_surf
      this%indata%n_inner_bc_part_xyz = n_inner_bc_part_xyz
      this%indata%n_bc_surf = n_bc_surf
      this%indata%n_bc_part = n_bc_part

      this%indata%n_ghost   = n_ghost

      if(.not.allocated(this%indata%pos_icm))                                  &
        allocate(this%indata%pos_icm(n_it_surf))
      if(.not.allocated(this%indata%neg_icm))                                  &
        allocate(this%indata%neg_icm(n_it_surf))
      if(.not.allocated(this%indata%self_icm))                                 &
        allocate(this%indata%self_icm(n_bc_surf))
      if(.not.allocated(this%indata%ghost_icm))                                &
        allocate(this%indata%ghost_icm(n_bc_surf))
      if(.not.allocated(this%indata%self_surf_bc))                             &
        allocate(this%indata%self_surf_bc(n_bc_surf))
      if(.not.allocated(this%indata%ghost_surf))                               &
        allocate(this%indata%ghost_surf(n_bc_surf))
      if(.not.allocated(this%indata%self_global))                              &
        allocate(this%indata%self_global(n_bc_surf))
      if(.not.allocated(this%indata%ghost_global))                             &
        allocate(this%indata%ghost_global(n_bc_surf))
      if(.not.allocated(this%indata%adja_surf))                                &
        allocate(this%indata%adja_surf(n_bc_surf))

      if(.not.allocated(this%indata%it_area))                                  &
        allocate(this%indata%it_area(n_it_surf))
      if(.not.allocated(this%indata%bc_area))                                  &
        allocate(this%indata%bc_area(n_bc_surf))
      if(.not.allocated(this%indata%stt_stp_bcsurf))                           &
        allocate(this%indata%stt_stp_bcsurf(3,n_bc_part))
      if(.not.allocated(this%indata%bc_cond))                                  &
        allocate(this%indata%bc_cond(n_bc_part))
      if(.not.allocated(this%indata%vol))                                      &
        allocate(this%indata%vol(stt_cm:stp_cm))
      if(.not.allocated(this%indata%stt_cm_rank_1))                            &
        allocate(this%indata%stt_cm_rank_1(0:para_env%space%n_proc-1))

      this%indata%pos_icm   = pos_icm
      this%indata%neg_icm   = neg_icm
      this%indata%self_icm  = self_icm
      this%indata%ghost_icm  = ghost_icm
      this%indata%self_surf_bc = self_surf_bc
      this%indata%ghost_surf = ghost_surf
      this%indata%self_global = self_global
      this%indata%ghost_global = ghost_global
      this%indata%adja_surf = adja_surf

      this%indata%it_area   = it_area
      this%indata%bc_area   = bc_area

      this%indata%stt_stp_bcsurf = stt_stp_bcsurf
      this%indata%bc_cond   = bc_cond

      this%indata%vol   = vol
      this%indata%stt_cm_rank_1 = stt_cm_rank_1

      this%indata%is_init = .true.
    endif
  endsubroutine linear_solver_petsc_sweep_get_init_data
!-------------------------------------------------------------------------------
!> get data from other types
!-------------------------------------------------------------------------------
  subroutine linear_solver_petsc_sweep_get_data(this,dtil_surf,dhat_surf,      &
    dtil_bc,dhat_bc,crnt_bc,xs_tr,xs_sc,phi_s_old,phi_ghost,phi_ghost_send)

    charSt,parameter :: myName = 'linear_solver_petsc_sweep => get_data :: '
    class(linear_solver_petsc_sweep_type),intent(inout) :: this
    realkd,  intent(in) :: dtil_surf(:,:)
    realkd,  intent(in) :: dhat_surf(:,:)
    realkd,  intent(in) :: dtil_bc(:,:)
    realkd,  intent(in) :: dhat_bc(:,:)
    realkd,  intent(in) :: crnt_bc(:,:)

    realkd,  intent(in) :: xs_tr(:,:)
    realkd,  intent(in) :: xs_sc(:,:,:) !fromig,ig,icm
    realkd,  intent(in) :: phi_s_old(:,:)
    realkd,  intent(in) :: phi_ghost(:,:)
    realkd,  intent(in) :: phi_ghost_send(:,:)

    if(.not.allocated(this%indata%dtil_surf))                                  &
      allocate(this%indata%dtil_surf(this%indata%mg,this%indata%n_it_surf))
    if(.not.allocated(this%indata%dhat_surf))                                  &
      allocate(this%indata%dhat_surf(this%indata%mg,this%indata%n_it_surf))
    if(.not.allocated(this%indata%dtil_bc))                                    &
      allocate(this%indata%dtil_bc(this%indata%mg,this%indata%n_bc_surf))
    if(.not.allocated(this%indata%dhat_bc))                                    &
      allocate(this%indata%dhat_bc(this%indata%mg,this%indata%n_bc_surf))
    if(.not.allocated(this%indata%crnt_bc))                                    &
      allocate(this%indata%crnt_bc(this%indata%mg,this%indata%n_bc_surf))
    if(.not.allocated(this%indata%xs_tr))                                      &
      allocate(this%indata%xs_tr(this%indata%mg,this%indata%stt_cm:            &
      this%indata%stp_cm))
    if(.not.allocated(this%indata%xs_sc))                                      &
      allocate(this%indata%xs_sc(this%indata%mg,this%indata%mg,                &
      this%indata%stt_cm:this%indata%stp_cm))
    if(.not.allocated(this%indata%phi_s_old))                                  &
      allocate(this%indata%phi_s_old(this%indata%mg,this%indata%stt_cm:        &
      this%indata%stp_cm))
    if(.not.allocated(this%indata%phi_ghost))                                  &
      allocate(this%indata%phi_ghost(this%indata%mg,this%indata%n_ghost))
    if(.not.allocated(this%indata%phi_ghost_send))                             &
      allocate(this%indata%phi_ghost_send(this%indata%mg,this%indata%n_ghost))

    this%indata%dtil_surf = dtil_surf
    this%indata%dhat_surf = dhat_surf
    this%indata%dtil_bc   = dtil_bc

    this%indata%dhat_bc   = dhat_bc
    this%indata%crnt_bc   = crnt_bc

    this%indata%xs_tr = xs_tr
    this%indata%xs_sc = 0.0_krp
    this%indata%xs_sc = xs_sc
    this%indata%phi_s_old = phi_s_old
    this%indata%phi_ghost = phi_ghost
    this%indata%phi_ghost_send = phi_ghost_send

  endsubroutine linear_solver_petsc_sweep_get_data

!-------------------------------------------------------------------------------
!> initialize linear solver with petsc type
!>
!> nrow - in, row number(y direction) for matrix
!> ncol - in, column number(x direct) for matrix
!-------------------------------------------------------------------------------
  subroutine linear_solver_petsc_sweep_init(this,para_env,nrow,ncol)
    charSt,parameter :: myName = 'linear_solver_petsc_sweep => init :: '
    class(linear_solver_petsc_sweep_type),intent(inout) :: this
    type(parallel_env_type),pointer,      intent(in   ) :: para_env
    intknd,                               intent(in   ) :: nrow
    intknd,                               intent(in   ) :: ncol

    class(linear_solver_petsc_sweep_type),pointer :: petsc_matrix_a
    class(petsc_vector_type),pointer :: petsc_vector_b
    class(petsc_vector_type),pointer :: petsc_vector_x

    intknd :: nError,ierr

    nError = get_nError()

    !> check para_env
    if(.not.(associated(para_env))) then
      call raise_error(myName//' para_env is not associated')
    elseif(.not.para_env%is_init)then
      call raise_error(myName//' para_env is not initialized')
    endif

    if(nError == get_nError())then
#ifdef WITH_PETSC
      this%indata%para_env => para_env

      !> set convergence criterion
      this%tolerance = 1.0e-7

      PETSC_COMM_WORLD = para_env%space%comm

      !> PETSc initialized
      call PetscInitialize(PETSC_NULL_CHARACTER,ierr)


      !> matrix initialized
      allocate(petsc_matrix_free_type :: this%a)
      select type(petsc_matrix_a => this%a); type is(petsc_matrix_free_type)
        call MatCreateShell(PETSC_COMM_WORLD,nrow,ncol,                        &
           PETSC_DECIDE,PETSC_DECIDE,this%indata,petsc_matrix_a%matrix,ierr)

        call MatShellSetOperation(petsc_matrix_a%matrix,MATOP_MULT,mymult,     &
           ierr)
        petsc_matrix_a%is_created = .true.
      endselect
      !> vector initialized
      allocate(petsc_vector_type :: this%b)
      allocate(petsc_vector_type :: this%x)

      call this%b%init(nrow)  !< initial and set initialized value
      call this%x%init(nrow)  !< initial and set initialized value

      allocate(this%results(nrow))
      this%results = rzero

      !> solver initialized
      call KSPCreate(PETSC_COMM_WORLD,this%ksp,ierr)
      select type(petsc_matrix_a => this%a); type is(petsc_matrix_free_type)
        call KSPSetOperators(this%ksp,petsc_matrix_a%matrix,                   &
          petsc_matrix_a%matrix, ierr)
      endselect

      call KSPSetTolerances(this%ksp,this%tolerance,PETSC_DEFAULT_REAL,        &
         PETSC_DEFAULT_REAL,PETSC_DEFAULT_INTEGER,ierr)

      !> set solver method :: GMRES
      call KSPSetType(this%ksp,KSPGMRES,ierr)

      !> set solver method :: CHEBYSHEV
      !call KSPSetType(this%ksp,KSPCHEBYSHEV,ierr)

      !> set solver method :: CG
      !call KSPSetType(this%ksp,KSPCG,ierr)

!       !> set preprocessing method
!       call KSPGetPC(this%ksp,this%pc,ierr)
!       call PCSetType(this%pc,PCJACOBI,ierr)

      call KSPSetFromOptions(this%ksp,ierr)
      call KSPSetUp(this%ksp,ierr)

#endif
    endif
    this%is_init = .true.
  endsubroutine linear_solver_petsc_sweep_init

!-------------------------------------------------------------------------------
!> clear iterative linear solver type
!-------------------------------------------------------------------------------
  subroutine linear_solver_petsc_sweep_solve(this)
    charSt,parameter :: myName = 'linear_solver_petsc_sweep => solve :: '
    class(linear_solver_petsc_sweep_type),intent(inout) :: this

    class(linear_solver_petsc_sweep_type),pointer :: matrix
    class(petsc_vector_type),pointer :: vector_b
    class(petsc_vector_type),pointer :: vector_x

#ifdef WITH_PETSC
    PetscInt :: n_iter
    PetscErrorCode :: ierr
#endif
    this%flag = -1

#ifdef WITH_PETSC
    !> assemble matrix and vector if necessary
    if(.not.(this%a%is_assembled)) call this%a%assemble()
    if(.not.(this%b%is_assembled)) call this%b%assemble() !< assemble right-hand
    if(.not.(this%x%is_assembled)) call this%x%assemble() !< assemble right-hand

    if(.not. this%indata%is_init) then
      call raise_error(myName//'the interface date type between cmfd to petsc is not initialized')
    else
      !> solve
      select type(vector_b => this%b); type is(petsc_vector_type)
        select type(vector_x => this%x); type is(petsc_vector_type)

!call vector_b%printout('b')
!call vector_x%printout('x')
!stop 666
          call KSPSolve(this%ksp,vector_b%vector,vector_x%vector,ierr)
!call vector_x%printout("x")
        endselect
      endselect

      call KSPGetIterationNumber(this%ksp,n_iter,ierr)

      !if(this%para_env%space%rank==0) &
      !  write(*,*) 'KSP solver iteration times on CPU0',n_iter

      if(ierr == 0) this%flag = 0
    endif

    this%a%is_assembled = .false.
    this%b%is_assembled = .false.
    this%x%is_assembled = .false.
#endif

  endsubroutine linear_solver_petsc_sweep_solve

!-------------------------------------------------------------------------------
!> get result from the solution vector
!-------------------------------------------------------------------------------
  subroutine linear_solver_petsc_sweep_get_result(this)
    class(linear_solver_petsc_sweep_type),intent(inout) :: this
#ifdef WITH_PETSC
    PetscErrorCode :: ierr

    if(this%flag /= 0) then
      call raise_error(' KSPSolve error in PETSc!')
    endif

    select type(vector_x => this%x)
    type is(petsc_vector_type)
      call VecGetArrayF90(vector_x%vector,this%results,ierr)
    endselect
#endif
  endsubroutine linear_solver_petsc_sweep_get_result
!-------------------------------------------------------------------------------
!> clear iterative linear solver type
!-------------------------------------------------------------------------------
  subroutine linear_solver_petsc_sweep_clear(this)
    class(linear_solver_petsc_sweep_type),intent(inout) :: this
#ifdef WITH_PETSC
    PetscErrorCode :: ierr
    call KSPDestroy(this%ksp,ierr)
    call this%a%clear
    call this%x%clear
    call this%b%clear
#endif
  endsubroutine linear_solver_petsc_sweep_clear


!-------------------------------------------------------------------------------
!> initialize petsc matrix type
!-------------------------------------------------------------------------------
  subroutine petsc_matrix_free_init(this,nrow,ncol)
    class(petsc_matrix_free_type),intent(inout) :: this
    intknd,                  intent(in   ) :: nrow
    intknd,                  intent(in   ) :: ncol

  endsubroutine petsc_matrix_free_init

!-------------------------------------------------------------------------------
!> set value in the petsc matrix
!-------------------------------------------------------------------------------
  subroutine petsc_matrix_free_set_value(this,irow,icol,value)
    class(petsc_matrix_free_type),intent(inout) :: this
    intknd,                  intent(in   ) :: irow
    intknd,                  intent(in   ) :: icol
    realkd,                  intent(in   ) :: value

  endsubroutine petsc_matrix_free_set_value

!-------------------------------------------------------------------------------
!> assemble matrix
!-------------------------------------------------------------------------------
  subroutine petsc_matrix_free_assemble(this)
    class(petsc_matrix_free_type),intent(inout) :: this
#ifdef WITH_PETSC
    PetscErrorCode :: ierr

    call MatAssemblyBegin(this%matrix,MAT_FINAL_ASSEMBLY,ierr)
    call MatAssemblyEnd(this%matrix,MAT_FINAL_ASSEMBLY,ierr)

    this%is_assembled = .true.
#endif
  endsubroutine petsc_matrix_free_assemble

!-------------------------------------------------------------------------------
!> print petsc matrix on the screen
!-------------------------------------------------------------------------------
  subroutine petsc_matrix_free_printout(this,message)
    class(petsc_matrix_free_type),intent(inout) :: this
    charSt,                       intent(in   ) :: message !< message for printing

  endsubroutine petsc_matrix_free_printout

!-------------------------------------------------------------------------------
!> clear the petsc matrix
!-------------------------------------------------------------------------------
  subroutine petsc_matrix_free_clear(this)
    class(petsc_matrix_free_type),intent(inout) :: this
#ifdef WITH_PETSC
    PetscErrorCode :: ierr

    call MatDestroy(this%matrix,ierr)

    this%is_created = .false.
#endif
  endsubroutine petsc_matrix_free_clear

!-------------------------------------------------------------------------------
!> initialize petsc vector type
!-------------------------------------------------------------------------------
  subroutine petsc_vector_init(this,size)
    class(petsc_vector_type),intent(inout) :: this
    intknd,                  intent(in   ) :: size !< vector size

#ifdef WITH_PETSC
    PetscErrorCode :: ierr

    call VecCreateMPI(PETSC_COMM_WORLD,size,PETSC_DECIDE,this%vector,ierr)
    !call VecCreateMPI(PETSC_COMM_WORLD,size,3,this%vector,ierr)

!call VecView(this%vector,Petsc_VIEWER_STDOUT_WORLD,ierr)
!stop 555

    call VecSet(this%vector,1.0_krp,ierr)


!call VecView(this%vector,Petsc_VIEWER_STDOUT_WORLD,ierr)
!stop 555

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

!call this%assemble()
!call VecView(this%vector,Petsc_VIEWER_STDOUT_WORLD,ierr)
!STOP 111
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
    call VecView(this%vector,Petsc_VIEWER_STDOUT_WORLD,ierr)
#endif
  endsubroutine petsc_vector_printout

!-------------------------------------------------------------------------------
!> get vector and read to array
!-------------------------------------------------------------------------------
  subroutine array_get(this,array)
    class(petsc_vector_type),intent(in   ) :: this
    realkd,pointer,          intent(inout) :: array(:)
#ifdef WITH_PETSC
    PetscErrorCode :: ierr
    call VecGetArrayReadF90(this%vector,array,ierr)
#endif
  endsubroutine array_get

!-------------------------------------------------------------------------------
!> restore from array to vector
!-------------------------------------------------------------------------------
  subroutine array_restore(this,array)
    class(petsc_vector_type),intent(inout) :: this
    realkd,pointer,          intent(in   ) :: array(:)
#ifdef WITH_PETSC
    PetscErrorCode :: ierr

    call VecRestoreArrayF90(this%vector,array,ierr)
#endif
  endsubroutine array_restore

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
endmodule linear_solver_petsc_sweep


