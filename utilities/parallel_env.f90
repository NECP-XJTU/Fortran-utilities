! -*- coding: utf-8 -*-
!===============================================================================
!> @brief MPI wrapper for Fortran.
!> @author Qingming He, NECP
!> @date 2015-05-11 22:52:19
!===============================================================================
module parallel_env
  use error_warning
!$ use omp_lib                                                              !mdw
  implicit none
  private

#ifdef WITH_MPI
  include "mpif.h"
#endif


# include <kind_parameter.h>

  public :: parallel_env_type
  public :: mpi_env_type
  public :: openmp_env_type

  public :: mpi_sum
  public :: mpi_max
  public :: mpi_min

#ifndef WITH_MPI
  intknd :: mpi_comm_world = 0
  intknd :: max_comm_id = 0
  intknd,parameter :: mpi_sum = 1
  intknd,parameter :: mpi_max = 2
  intknd,parameter :: mpi_min = 3
#else

#ifdef DBL_REAL
  intknd,parameter :: mpi_krp=MPI_DOUBLE_PRECISION
#else
  intknd,parameter :: mpi_krp=MPI_REAL
#endif

#endif
  !-----------------------------------------------------------------------------
  !
  type :: openmp_env_type
    logknd :: is_init = .false.
    logknd :: master = .false. !< whether is master processor
    intknd :: n_proc = 1       !< number of processors
    intknd :: rank = 0         !< rank of the processor
  contains
    procedure :: init => init_openmp_env
    procedure :: clear => clear_openmp_env
  endtype openmp_env_type

  !-----------------------------------------------------------------------------
  !> @brief
  type :: mpi_env_type
    logknd :: is_init   = .false.
    logknd :: is_master = .false. !< whether is master processor
    intknd :: n_proc = 1          !< number of processors
    intknd :: rank   = 0          !< rank of the processor
    intknd :: comm   = -1         !< MPI communicator
    intknd :: master_rank = -1
  contains
    procedure :: init  => init_mpi_env
    procedure :: clear => clear_mpi_env
    procedure :: barrier => barrier_mpi_env
!    procedure, public :: is_master => is_master_mpi_env
!    procedure, public :: get_rank  => get_rank_mpi_env
!    procedure, public :: get_size  => get_size_mpi_env
    procedure, nopass  :: finalize => finalize_mpi_env
!    procedure, pass  :: getpid
!    procedure, pass  :: get_time

    procedure :: bcast_int_0d    => bcast_int_0d_mpi_env
    procedure :: bcast_int_1d    => bcast_int_1d_mpi_env
    procedure :: bcast_int_2d    => bcast_int_2d_mpi_env
    procedure :: bcast_int_3d    => bcast_int_3d_mpi_env
    procedure :: bcast_int_4d    => bcast_int_4d_mpi_env
    procedure :: bcast_dbl_0d    => bcast_double_0d_mpi_env
    procedure :: bcast_dbl_1d    => bcast_double_1d_mpi_env
    procedure :: bcast_dbl_2d    => bcast_double_2d_mpi_env
    procedure :: bcast_dbl_3d    => bcast_double_3d_mpi_env
    procedure :: bcast_dbl_4d    => bcast_double_4d_mpi_env
    procedure :: bcast_string    => bcast_string_mpi_env
    generic   :: bcast           => bcast_int_0d, &
                                    bcast_int_1d, &
                                    bcast_int_2d, &
                                    bcast_int_3d, &
                                    bcast_int_4d, &
                                    bcast_dbl_0d, &
                                    bcast_dbl_1d, &
                                    bcast_dbl_2d, &
                                    bcast_dbl_3d, &
                                    bcast_dbl_4d, &
                                    bcast_string

    procedure :: send_int_0d      => send_int_0d_mpi_env
    procedure :: send_int_1d      => send_int_1d_mpi_env
    procedure :: send_int_2d      => send_int_2d_mpi_env
    procedure :: send_int_3d      => send_int_3d_mpi_env
    procedure :: send_int_4d      => send_int_4d_mpi_env
    procedure :: send_dbl_0d      => send_double_0d_mpi_env
    procedure :: send_dbl_1d      => send_double_1d_mpi_env
    procedure :: send_dbl_2d      => send_double_2d_mpi_env
    procedure :: send_dbl_3d      => send_double_3d_mpi_env
    procedure :: send_dbl_4d      => send_double_4d_mpi_env
    procedure :: send_string      => send_string_mpi_env
    generic   :: send             => send_int_0d, &
                                     send_int_1d, &
                                     send_int_2d, &
                                     send_int_3d, &
                                     send_int_4d, &
                                     send_dbl_0d, &
                                     send_dbl_1d, &
                                     send_dbl_2d, &
                                     send_dbl_3d, &
                                     send_dbl_4d, &
                                     send_string

    procedure :: send_dbl_1d_pack => send_double_1d_pack_mpi_env
    generic   :: send_pack        => send_dbl_1d_pack

    procedure :: receive_int_0d      => receive_int_0d_mpi_env
    procedure :: receive_int_1d      => receive_int_1d_mpi_env
    procedure :: receive_int_2d      => receive_int_2d_mpi_env
    procedure :: receive_int_3d      => receive_int_3d_mpi_env
    procedure :: receive_int_4d      => receive_int_4d_mpi_env
    procedure :: receive_dbl_0d      => receive_double_0d_mpi_env
    procedure :: receive_dbl_1d      => receive_double_1d_mpi_env
    procedure :: receive_dbl_2d      => receive_double_2d_mpi_env
    procedure :: receive_dbl_3d      => receive_double_3d_mpi_env
    procedure :: receive_dbl_4d      => receive_double_4d_mpi_env
    procedure :: receive_string      => receive_string_mpi_env
    generic   :: receive             => receive_int_0d, &
                                        receive_int_1d, &
                                        receive_int_2d, &
                                        receive_int_3d, &
                                        receive_int_4d, &
                                        receive_dbl_0d, &
                                        receive_dbl_1d, &
                                        receive_dbl_2d, &
                                        receive_dbl_3d, &
                                        receive_dbl_4d, &
                                        receive_string

    procedure :: receive_dbl_1d_pack => receive_double_1d_pack_mpi_env
    generic   :: receive_pack        => receive_dbl_1d_pack

    procedure :: reduce  => reduce_double_0d_mpi_env

    procedure :: allreduce_dbl_0d  => allreduce_double_0d_mpi_env
    procedure :: allreduce_dbl_1d  => allreduce_double_1d_mpi_env
    procedure :: allreduce_dbl_2d  => allreduce_double_2d_mpi_env
    procedure :: allreduce_dbl_3d  => allreduce_double_3d_mpi_env
    procedure :: allreduce_dbl_4d  => allreduce_double_4d_mpi_env
    procedure :: allreduce_int_0d  => allreduce_integer_0d_mpi_env
    procedure :: allreduce_int_1d  => allreduce_integer_1d_mpi_env
    procedure :: allreduce_int_2d  => allreduce_integer_2d_mpi_env
    procedure :: allreduce_int_3d  => allreduce_integer_3d_mpi_env
    procedure :: allreduce_int_4d  => allreduce_integer_4d_mpi_env
    generic   :: allreduce         =>  allreduce_dbl_0d, &
                                       allreduce_dbl_1d, &
                                       allreduce_dbl_2d, &
                                       allreduce_dbl_3d, &
                                       allreduce_dbl_4d, &
                                       allreduce_int_0d, &
                                       allreduce_int_1d, &
                                       allreduce_int_2d, &
                                       allreduce_int_3d, &
                                       allreduce_int_4d

    procedure, nopass :: wait       => wait_mpi_env
    procedure, nopass :: waitany    => waitany_mpi_env
    procedure, nopass :: waitall    => waitall_mpi_env
    procedure, nopass :: waitall_2d => waitall_2d_mpi_env
    procedure, nopass :: waitall_3d => waitall_3d_mpi_env
    generic :: waits                => wait,    &
                                       waitany, &
                                       waitall, &
                                       waitall_2d, &
                                       waitall_3d

    procedure :: pack_dbl_1d => pack_double_1d_mpi_env
    generic :: mpi_pack      => pack_dbl_1d

    procedure :: unpack_dbl_1d => unpack_double_1d_mpi_env
    generic :: mpi_unpack      => unpack_dbl_1d

  end type mpi_env_type

  !-----------------------------------------------------------------------------
  !> @brief
  type :: parallel_env_type
    logknd :: is_init = .false.
    type(mpi_env_type) :: world
    type(mpi_env_type) :: energy
    type(mpi_env_type) :: space
    type(mpi_env_type) :: angle
    type(openmp_env_type) :: ray

    intknd :: n_radial = 0
    intknd :: n_axial  = 0
    intknd :: i_radial = 0
    intknd :: i_axial  = 0
  contains
    procedure,pass :: init => init_parallel_env_type
    procedure,pass :: cart_creat => cart_creat_parallel_env_type
    procedure,pass :: clear => clear_parallel_env_type
  endtype parallel_env_type

!===============================================================================

contains

!-------------------------------------------------------------------------------
!> pack
!-------------------------------------------------------------------------------
  subroutine pack_double_1d_mpi_env(self,input,buffer,buffer_size,pos_buff)
    class(mpi_env_type),intent(inout) :: self
    realkd,             intent(in   ) :: input(:)
    realkd,             intent(inout) :: buffer(:)
    intknd,             intent(in)    :: buffer_size
    intknd,             intent(in)    :: pos_buff

    intknd :: mpierr
#ifdef WITH_MPI
    call MPI_PACK(input(1),size(input),mpi_krp,buffer,buffer_size,pos_buff, &
      self%comm,mpierr)
#endif
  endsubroutine pack_double_1d_mpi_env

!-------------------------------------------------------------------------------
!> unpack
!-------------------------------------------------------------------------------
  subroutine unpack_double_1d_mpi_env(self,buffer,buffer_size,pos_buff,output)
    class(mpi_env_type),intent(inout) :: self
    realkd,             intent(in   ) :: buffer(:)
    intknd,             intent(in   ) :: buffer_size
    intknd,             intent(in   ) :: pos_buff
    realkd,             intent(inout) :: output(:)

    intknd :: mpierr
#ifdef WITH_MPI
    call MPI_UNPACK(buffer,buffer_size,pos_buff,output(:),size(output),mpi_krp,&
      self%comm,mpierr)
#endif
  endsubroutine unpack_double_1d_mpi_env

!-------------------------------------------------------------------------------
!> send package
!-------------------------------------------------------------------------------
  subroutine send_double_1d_pack_mpi_env(self,buf,buffer_size,dest,request,tag,blocking)
    charSt,parameter :: myname = 'parallel_env :: send - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(in) :: buf(:)              !< buffer
    intknd, intent(in) :: buffer_size
    intknd, intent(in) :: dest                !< destination processor
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking

#ifdef WITH_MPI
    if(0 <= dest .and. dest <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_SEND(buf, buffer_size, MPI_PACKED, dest, mpi_tag, self%comm,&
          & mpierr)
      else
        call MPI_ISEND(buf, buffer_size, MPI_PACKED, dest, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support send function')
#endif

  endsubroutine send_double_1d_pack_mpi_env

!-------------------------------------------------------------------------------
!> receive package
!-------------------------------------------------------------------------------
  subroutine receive_double_1d_pack_mpi_env(self,buf,buffer_size,source,request,tag,blocking)
    charSt,parameter :: myname = 'parallel_env :: receive - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(inout) :: buf(:)           !< buffer
    intknd, intent(in) :: buffer_size
    intknd, intent(in) :: source              !< rank of source
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= source .and. source <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_RECV(buf, buffer_size, MPI_PACKED, source, mpi_tag, self%comm,&
          & MPI_STATUS_IGNORE, mpierr)
      else
        call MPI_IRECV(buf, buffer_size, MPI_PACKED, source, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support receive function')
#endif
  endsubroutine receive_double_1d_pack_mpi_env

!-------------------------------------------------------------------------------
!> Initializes the openmp_env
!-------------------------------------------------------------------------------
  subroutine init_openmp_env(self,n_proc)
    class(openmp_env_type),intent(inout) :: self
    intknd,intent(in),optional :: n_proc

    self%n_proc = n_proc
    self%rank = 0
    self%master = .true.
    self%is_init = .true.
!$  if(present(n_proc)) then
!$    if(n_proc > omp_get_max_threads()) then
!$      self%n_proc = omp_get_max_threads()
!$    elseif(n_proc == 0) then
!$      self%n_proc = omp_get_max_threads()
!$    else
!$      self%n_proc = max(n_proc,1)
!$    endif
!$  endif
  endsubroutine init_openmp_env

!-------------------------------------------------------------------------------
!> Clear the openmp_env
!-------------------------------------------------------------------------------
  subroutine clear_openmp_env(self,n_proc)
    class(openmp_env_type),intent(inout) :: self
    intknd,intent(in),optional :: n_proc

    self%n_proc = 0
    self%rank = -1
    self%master = .false.
    self%is_init = .false.
  endsubroutine clear_openmp_env

!-------------------------------------------------------------------------------
!> Initializes the mpi_env
!-------------------------------------------------------------------------------
  subroutine init_mpi_env(self,comm)

    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd,intent(in),optional :: comm

    intknd :: icomm, &
              mpierr

    logknd :: is_init

    if(.not. self%is_init) then
      if(present(comm)) then
        icomm = comm
      else
        icomm = mpi_comm_world
      endif

#ifdef WITH_MPI
      call mpi_initialized(is_init,mpierr)
      if(.not. is_init) then
        call mpi_init(mpierr)
        if(mpierr /= mpi_success) call raise_error('Failed to call mpi_init')
      endif

      call mpi_comm_dup(icomm,self%comm,mpierr)
      call mpi_comm_size(self%comm,self%n_proc,mpierr)
      call mpi_comm_rank(self%comm,self%rank,mpierr)

      if(mpierr /= mpi_success) call raise_error('Failed to initialize mpi_env')
#else
      max_comm_id = max_comm_id + 1
      self%comm = max_comm_id
      self%n_proc = 1
      self%rank = 0
#endif

      if (self%rank == 0) self%is_master = .true.
      self%master_rank = 0
      self%is_init = .true.
    endif
!write(*,*)'1.7'
  endsubroutine init_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine clear_mpi_env(self)
    charSt,parameter :: myname = 'parallel_env :: clear - '
    class(mpi_env_type),intent(inout) :: self
    intknd :: mpierr
    if(self%is_init) then
#ifdef WITH_MPI
      call mpi_comm_free(self%comm,mpierr)
      if(mpierr /= mpi_success) &
        call raise_error(myname//'error returned when clear mpi env')
#endif
      self%is_init = .false.
      self%is_master = .FALSE.
      self%n_proc = 1
      self%rank = 0
      self%comm = -1
      self%master_rank = -1
    endif
  endsubroutine clear_mpi_env
  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine barrier_mpi_env(self)
    class(mpi_env_type),intent(inout) :: self
    intknd :: mpierr
#ifdef WITH_MPI
      if(self%is_init) call MPI_BARRIER(self%comm,mpierr)
#endif

  end subroutine barrier_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine finalize_mpi_env()
  !  class(mpi_env_type),intent(inout) :: self
    intknd :: mpierr
#ifdef WITH_MPI
    call MPI_FINALIZE(mpierr)
#endif

  endsubroutine finalize_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  function get_time() result(time)

    realkd :: time !< current time

#ifdef WITH_MPI
    time = MPI_WTIME()
#else
    call cpu_time(time)
#endif

  end function get_time

!  !-----------------------------------------------------------------------------
!  !> @brief Get processor ID or current MPI processor.
!  !-----------------------------------------------------------------------------
!  function getpid() result(pid)
!
!    intknd :: pid !< process identification
!
!    pid = getpid_c()
!
!  end function getpid

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine bcast_int_0d_mpi_env(self, buf, root, request, blocking)

    charSt,parameter :: myname = 'parallel_env :: bcast - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd, intent(inout) :: buf              !< buffer
    intknd, optional :: root                  !< rank of broadcast root
    intknd, optional :: request               !< communication request
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: rank         ! rank of broadcast root
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    rank = self%master_rank
    if (present(root)) rank = root
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= rank .and. rank <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_BCAST(buf, 1, MPI_INTEGER, rank, self%comm, mpierr)
      else
!        call MPI_IBCAST(buf, 1, MPI_INTEGER, rank, self%comm, mpi_req_hand,&
!          & mpierr)
!        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support bcast function')
#endif

  endsubroutine bcast_int_0d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine bcast_int_1d_mpi_env(self, buf, root, request, blocking)

    charSt,parameter :: myname = 'parallel_env :: bcast - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd, intent(inout) :: buf(:)           !< buffer
    intknd, optional :: root                  !< rank of broadcast root
    intknd, optional :: request               !< communication request
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: rank         ! rank of broadcast root
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    rank = self%master_rank
    if (present(root)) rank = root
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= rank .and. rank <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_BCAST(buf, size(buf), MPI_INTEGER, rank, self%comm, mpierr)
      else
!        call MPI_IBCAST(buf, size(buf), MPI_INTEGER, rank, self%comm,&
!          & mpi_req_hand, mpierr)
!        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support bcast function')
#endif

  endsubroutine bcast_int_1d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine bcast_int_2d_mpi_env(self, buf, root, request, blocking)

    charSt,parameter :: myname = 'parallel_env :: init - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd, intent(inout) :: buf(:, :)        !< buffer
    intknd, optional :: root                  !< rank of broadcast root
    intknd, optional :: request               !< communication request
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: rank         ! rank of broadcast root
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    rank = self%master_rank
    if (present(root)) rank = root
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= rank .and. rank <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_BCAST(buf, size(buf), MPI_INTEGER, rank, self%comm, mpierr)
      else
!        call MPI_IBCAST(buf, size(buf), MPI_INTEGER, rank, self%comm,&
!          & mpi_req_hand, mpierr)
!        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support bcast function')
#endif

  endsubroutine bcast_int_2d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine bcast_int_3d_mpi_env(self, buf, root, request, blocking)

    charSt,parameter :: myname = 'parallel_env :: bcast - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd, intent(inout) :: buf(:, :, :)     !< buffer
    intknd, optional :: root                  !< rank of broadcast root
    intknd, optional :: request               !< communication request
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: rank         ! rank of broadcast root
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    rank = self%master_rank
    if (present(root)) rank = root
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= rank .and. rank <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_BCAST(buf, size(buf), MPI_INTEGER, rank, self%comm, mpierr)
      else
!        call MPI_IBCAST(buf, size(buf), MPI_INTEGER, rank, self%comm,&
!          & mpi_req_hand, mpierr)
!        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support bcast function')
#endif

  endsubroutine bcast_int_3d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine bcast_int_4d_mpi_env(self, buf, root, request, blocking)

    charSt,parameter :: myname = 'parallel_env :: bcast - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd, intent(inout) :: buf(:, :, :, :)  !< buffer
    intknd, optional :: root                  !< rank of broadcast root
    intknd, optional :: request               !< communication request
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: rank         ! rank of broadcast root
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    rank = self%master_rank
    if (present(root)) rank = root
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= rank .and. rank <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_BCAST(buf, size(buf), MPI_INTEGER, rank, self%comm, mpierr)
      else
!        call MPI_IBCAST(buf, size(buf), MPI_INTEGER, rank, self%comm,&
!          & mpi_req_hand, mpierr)
!        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support bcast function')
#endif

  endsubroutine bcast_int_4d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine bcast_double_0d_mpi_env(self, buf, root, request, blocking)

    charSt,parameter :: myname = 'parallel_env :: bcast - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(inout) :: buf              !< buffer
    intknd, optional :: root                  !< rank of broadcast root
    intknd, optional :: request               !< communication request
    logknd, optional :: blocking              !< whether perform blocking comm.

    intknd :: rank         ! rank of broadcast root
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    rank = self%master_rank
    if (present(root)) rank = root
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= rank .and. rank <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_BCAST(buf, 1, mpi_krp, rank, self%comm, mpierr)
      else
!        call MPI_IBCAST(buf, 1, mpi_krp, rank, self%comm, mpi_req_hand,&
!          & mpierr)
!        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support bcast function')
#endif

  endsubroutine bcast_double_0d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine bcast_double_1d_mpi_env(self, buf, root, request, blocking)

    charSt,parameter :: myname = 'parallel_env :: bcast - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(inout) :: buf(:)           !< buffer
    intknd, optional :: root                  !< rank of broadcast root
    intknd, optional :: request               !< communication request
    logknd, optional :: blocking              !< whether perform blocking comm.

    intknd :: rank         ! rank of broadcast root
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    rank = self%master_rank
    if (present(root)) rank = root
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= rank .and. rank <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_BCAST(buf, size(buf), mpi_krp, rank, self%comm, mpierr)
      else
!        call MPI_IBCAST(buf, size(buf), mpi_krp, rank, self%comm,&
!          & mpi_req_hand, mpierr)
!        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support bcast function')
#endif

  endsubroutine bcast_double_1d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine bcast_double_2d_mpi_env(self, buf, root, request, blocking)

    charSt,parameter :: myname = 'parallel_env :: bcast - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(inout) :: buf(:, :)        !< buffer
    intknd, optional :: root                  !< rank of broadcast root
    intknd, optional :: request               !< communication request
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: rank         ! rank of broadcast root
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    rank = self%master_rank
    if (present(root)) rank = root
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= rank .and. rank <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_BCAST(buf, size(buf), mpi_krp, rank, self%comm, mpierr)
      else
!        call MPI_IBCAST(buf, size(buf), mpi_krp, rank, self%comm,&
!          & mpi_req_hand, mpierr)
!        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support bcast function')
#endif

  endsubroutine bcast_double_2d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine bcast_double_3d_mpi_env(self, buf, root, request, blocking)

    charSt,parameter :: myname = 'parallel_env :: bcast - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(inout) :: buf(:, :, :)     !< buffer
    intknd, optional :: root                  !< rank of broadcast root
    intknd, optional :: request               !< communication request
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: rank         ! rank of broadcast root
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    rank = self%master_rank
    if (present(root)) rank = root
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= rank .and. rank <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_BCAST(buf, size(buf), mpi_krp, rank, self%comm, mpierr)
      else
!        call MPI_IBCAST(buf, size(buf), mpi_krp, rank, self%comm,&
!          & mpi_req_hand, mpierr)
!        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support bcast function')
#endif

  endsubroutine bcast_double_3d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine bcast_double_4d_mpi_env(self, buf, root, request, blocking)

    charSt,parameter :: myname = 'parallel_env :: bcast - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(inout) :: buf(:, :, :, :)  !< buffer
    intknd, optional :: root                  !< rank of broadcast root
    intknd, optional :: request               !< communication request
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: rank         ! rank of broadcast root
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    rank = self%master_rank
    if (present(root)) rank = root
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= rank .and. rank <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_BCAST(buf, size(buf), mpi_krp, rank, self%comm, mpierr)
      else
!        call MPI_IBCAST(buf, size(buf), mpi_krp, rank, self%comm,&
!          & mpi_req_hand, mpierr)
!        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support bcast function')
#endif

  endsubroutine bcast_double_4d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine bcast_string_mpi_env(self, buf, root, request, blocking)

    charSt,parameter :: myname = 'parallel_env :: bcast - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    character(*), intent(inout) :: buf         !< buffer
    intknd, optional :: root                  !< rank of broadcast root
    intknd, optional :: request               !< communication request
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: rank         ! rank of broadcast root
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    rank = self%master_rank
    if (present(root)) rank = root
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= rank .and. rank <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_BCAST(buf, len(buf), MPI_CHARACTER, rank, self%comm, mpierr)
      else
!        call MPI_IBCAST(buf, len(buf), MPI_CHARACTER, rank, self%comm,&
!          & mpi_req_hand, mpierr)
!        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support bcast function')
#endif

  endsubroutine bcast_string_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine send_int_0d_mpi_env(self, buf, dest, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: send - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd, intent(in) :: buf                 !< buffer
    intknd, intent(in) :: dest                !< destination processor
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= dest .and. dest <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_SEND(buf, 1, MPI_INTEGER, dest, mpi_tag, self%comm, mpierr)
      else
        call MPI_ISEND(buf, 1, MPI_INTEGER, dest, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support send function')
#endif

  endsubroutine send_int_0d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine send_int_1d_mpi_env(self, buf, dest, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: send - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd, intent(in) :: buf(:)              !< buffer
    intknd, intent(in) :: dest                !< destination processor
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= dest .and. dest <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_SEND(buf, size(buf), MPI_INTEGER, dest, mpi_tag, self%comm,&
          & mpierr)
      else
        call MPI_ISEND(buf, size(buf), MPI_INTEGER, dest, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support send function')
#endif

  endsubroutine send_int_1d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine send_int_2d_mpi_env(self, buf, dest, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: send - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd, intent(in) :: buf(:, :)           !< buffer
    intknd, intent(in) :: dest                !< destination processor
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= dest .and. dest <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_SEND(buf, size(buf), MPI_INTEGER, dest, mpi_tag, self%comm,&
          & mpierr)
      else
        call MPI_ISEND(buf, size(buf), MPI_INTEGER, dest, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support send function')
#endif

  endsubroutine send_int_2d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine send_int_3d_mpi_env(self, buf, dest, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: send - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd, intent(in) :: buf(:, :, :)        !< buffer
    intknd, intent(in) :: dest                !< destination processor
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= dest .and. dest <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_SEND(buf, size(buf), MPI_INTEGER, dest, mpi_tag, self%comm,&
          & mpierr)
      else
        call MPI_ISEND(buf, size(buf), MPI_INTEGER, dest, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support send function')
#endif

  endsubroutine send_int_3d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine send_int_4d_mpi_env(self, buf, dest, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: send - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd, intent(in) :: buf(:, :, :, :)     !< buffer
    intknd, intent(in) :: dest                !< destination processor
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= dest .and. dest <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_SEND(buf, size(buf), MPI_INTEGER, dest, mpi_tag, self%comm,&
          & mpierr)
      else
        call MPI_ISEND(buf, size(buf), MPI_INTEGER, dest, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support send function')
#endif

  endsubroutine send_int_4d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine send_double_0d_mpi_env(self, buf, dest, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: send - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(in) :: buf                 !< buffer
    intknd, intent(in) :: dest                !< destination processor
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= dest .and. dest <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_SEND(buf, 1, mpi_krp, dest, mpi_tag, self%comm, mpierr)
      else
        call MPI_ISEND(buf, 1, mpi_krp, dest, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support send function')
#endif

  endsubroutine send_double_0d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine send_double_1d_mpi_env(self, buf, dest, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: send - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(in) :: buf(:)              !< buffer
    intknd, intent(in) :: dest                !< destination processor
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= dest .and. dest <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_SEND(buf, size(buf), mpi_krp, dest, mpi_tag, self%comm,&
          & mpierr)
      else
        call MPI_ISEND(buf, size(buf), mpi_krp, dest, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support send function')
#endif

  endsubroutine send_double_1d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine send_double_2d_mpi_env(self, buf, dest, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: send - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(in) :: buf(:, :)           !< buffer
    intknd, intent(in) :: dest                !< destination processor
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= dest .and. dest <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_SEND(buf, size(buf), mpi_krp, dest, mpi_tag, self%comm,&
          & mpierr)
      else
        call MPI_ISEND(buf, size(buf), mpi_krp, dest, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support send function')
#endif

  endsubroutine send_double_2d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine send_double_3d_mpi_env(self, buf, dest, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: send - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(in) :: buf(:, :, :)        !< buffer
    intknd, intent(in) :: dest                !< destination processor
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= dest .and. dest <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_SEND(buf, size(buf), mpi_krp, dest, mpi_tag, self%comm,&
          & mpierr)
      else
        call MPI_ISEND(buf, size(buf), mpi_krp, dest, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support send function')
#endif

  endsubroutine send_double_3d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine send_double_4d_mpi_env(self, buf, dest, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: send - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(in) :: buf(:, :, :, :)     !< buffer
    intknd, intent(in) :: dest                !< destination processor
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= dest .and. dest <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_SEND(buf, size(buf), mpi_krp, dest, mpi_tag, self%comm,&
          & mpierr)
      else
        call MPI_ISEND(buf, size(buf), mpi_krp, dest, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support send function')
#endif

  endsubroutine send_double_4d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine send_string_mpi_env(self, buf, dest, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: send - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    character(*), intent(in) :: buf            !< buffer
    intknd, intent(in) :: dest                !< destination processor
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= dest .and. dest <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_SEND(buf, len(buf), MPI_CHARACTER, dest, mpi_tag, self%comm,&
          & mpierr)
      else
        call MPI_ISEND(buf, len(buf), MPI_CHARACTER, dest, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support send function')
#endif

  endsubroutine send_string_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine receive_int_0d_mpi_env(self, buf, source, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: receive - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd, intent(inout) :: buf              !< buffer
    intknd, intent(in) :: source              !< rank of source
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= source .and. source <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_RECV(buf, 1, MPI_INTEGER, source, mpi_tag, self%comm,&
          & MPI_STATUS_IGNORE, mpierr)
      else
        call MPI_IRECV(buf, 1, MPI_INTEGER, source, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support receive function')
#endif

  endsubroutine receive_int_0d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine receive_int_1d_mpi_env(self, buf, source, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: receive - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd, intent(inout) :: buf(:)           !< buffer
    intknd, intent(in) :: source              !< rank of source
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= source .and. source <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_RECV(buf, size(buf), MPI_INTEGER, source, mpi_tag, self%comm,&
          & MPI_STATUS_IGNORE, mpierr)
      else
        call MPI_IRECV(buf, size(buf), MPI_INTEGER, source, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support receive function')
#endif

  endsubroutine receive_int_1d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine receive_int_2d_mpi_env(self, buf, source, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: receive - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd, intent(inout) :: buf(:, :)        !< buffer
    intknd, intent(in) :: source              !< rank of source
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= source .and. source <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_RECV(buf, size(buf), MPI_INTEGER, source, mpi_tag, self%comm,&
          & MPI_STATUS_IGNORE, mpierr)
      else
        call MPI_IRECV(buf, size(buf), MPI_INTEGER, source, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support receive function')
#endif

  endsubroutine receive_int_2d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine receive_int_3d_mpi_env(self, buf, source, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: receive - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd, intent(inout) :: buf(:, :, :)     !< buffer
    intknd, intent(in) :: source              !< rank of source
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= source .and. source <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_RECV(buf, size(buf), MPI_INTEGER, source, mpi_tag, self%comm,&
          & MPI_STATUS_IGNORE, mpierr)
      else
        call MPI_IRECV(buf, size(buf), MPI_INTEGER, source, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support receive function')
#endif

  endsubroutine receive_int_3d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine receive_int_4d_mpi_env(self, buf, source, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: receive - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    intknd, intent(inout) :: buf(:, :, :, :)  !< buffer
    intknd, intent(in) :: source              !< rank of source
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= source .and. source <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_RECV(buf, size(buf), MPI_INTEGER, source, mpi_tag, self%comm,&
          & MPI_STATUS_IGNORE, mpierr)
      else
        call MPI_IRECV(buf, size(buf), MPI_INTEGER, source, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support receive function')
#endif

  endsubroutine receive_int_4d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine receive_double_0d_mpi_env(self, buf, source, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: receive - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(inout) :: buf              !< buffer
    intknd, intent(in) :: source              !< rank of source
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= source .and. source <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_RECV(buf, 1, mpi_krp, source, mpi_tag, self%comm,&
          & MPI_STATUS_IGNORE, mpierr)
      else
        call MPI_IRECV(buf, 1, mpi_krp, source, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support receive function')
#endif

  endsubroutine receive_double_0d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine receive_double_1d_mpi_env(self, buf, source, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: receive - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(inout) :: buf(:)           !< buffer
    intknd, intent(in) :: source              !< rank of source
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= source .and. source <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_RECV(buf, size(buf), mpi_krp, source, mpi_tag, self%comm,&
          & MPI_STATUS_IGNORE, mpierr)
      else
        call MPI_IRECV(buf, size(buf), mpi_krp, source, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support receive function')
#endif

  endsubroutine receive_double_1d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine receive_double_2d_mpi_env(self, buf, source, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: receive - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(inout) :: buf(:, :)        !< buffer
    intknd, intent(in) :: source              !< rank of source
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= source .and. source <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_RECV(buf, size(buf), mpi_krp, source, mpi_tag, self%comm,&
          & MPI_STATUS_IGNORE, mpierr)
      else
        call MPI_IRECV(buf, size(buf), mpi_krp, source, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support receive function')
#endif

  endsubroutine receive_double_2d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine receive_double_3d_mpi_env(self, buf, source, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: receive - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(inout) :: buf(:, :, :)     !< buffer
    intknd, intent(in) :: source              !< rank of source
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= source .and. source <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_RECV(buf, size(buf), mpi_krp, source, mpi_tag, self%comm,&
          & MPI_STATUS_IGNORE, mpierr)
      else
        call MPI_IRECV(buf, size(buf), mpi_krp, source, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' source is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support receive function')
#endif

  endsubroutine receive_double_3d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine receive_double_4d_mpi_env(self, buf, source, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: receive - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    realkd, intent(inout) :: buf(:, :, :, :)  !< buffer
    intknd, intent(in) :: source              !< rank of source
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= source .and. source <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_RECV(buf, size(buf), mpi_krp, source, mpi_tag, self%comm,&
          & MPI_STATUS_IGNORE, mpierr)
      else
        call MPI_IRECV(buf, size(buf), mpi_krp, source, mpi_tag, self%comm,&
          & mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' rank is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support receive function')
#endif

  endsubroutine receive_double_4d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine receive_string_mpi_env(self, buf, source, request, tag, blocking)

    charSt,parameter :: myname = 'parallel_env :: receive - '
    class(mpi_env_type), intent(inout) :: self !< the mpi_env_type object
    character(*), intent(inout) :: buf         !< buffer
    intknd, intent(in) :: source              !< rank of source
    intknd, optional :: request               !< communication request
    intknd, optional :: tag                   !< message tag
    logknd, optional :: blocking              !< whether perform blocking communication

    intknd :: mpi_tag      ! MPI message tag
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    mpi_tag = 0
    if (present(tag)) mpi_tag = tag
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
#ifdef WITH_MPI
    if(0 <= source .and. source <= self%n_proc -1) then
      if (mpi_blocking) then
        call MPI_RECV(buf, len(buf), MPI_CHARACTER, source, mpi_tag, self%comm,&
          & MPI_STATUS_IGNORE, mpierr)
      else
        call MPI_IRECV(buf, len(buf), MPI_CHARACTER, source, mpi_tag, self%comm&
          &, mpi_req_hand, mpierr)
        if (present(request)) request = mpi_req_hand
      endif
    else
      call raise_error(myname//' source is out of range')
    endif
#else
    if(self%n_proc > 0) call raise_error('Does not support receive function')
#endif

  endsubroutine receive_string_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine reduce_double_0d_mpi_env(self, buf, op, root, recvbuf, request,&
    & blocking, all)

    charSt,parameter :: myname = 'parallel_env :: reduce - '
    class(mpi_env_type), intent(in) :: self   !< the mpi_env_type object

    realkd, intent(in) :: buf                 !< send buffer
    intknd, intent(in) :: op                  !< MPI operator
    intknd, optional :: root                  !< rank of root processor
    realkd, optional :: recvbuf               !< receive buffer, only significant for root
    intknd, optional :: request               !< communication request
    logknd, optional :: blocking              !< whether perform blocking communication
    logknd, optional :: all                   !< whether perform all reduce

    logknd :: mpi_blocking ! whether perform blocking communication
    logknd :: mpi_all      ! whether perform all reduce
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: rank         ! rank of root processor
    realkd :: recv_buf     ! receive buffer
    intknd :: mpierr

    rank = self%master_rank
    if (present(root)) rank = root
    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking
    mpi_all = .TRUE.
    if (present(all)) mpi_all = all
#ifdef WITH_MPI
    if (mpi_all) then
      if (mpi_blocking) then
        call MPI_ALLREDUCE(buf, recv_buf, 1, mpi_krp, op, self%comm,&
          & mpierr)
      else
        ! iallreduce is supported by MPI-3, not supported by NECP_X yet
        !call MPI_IALLREDUCE(buf, recv_buf, 1, mpi_krp, op, self%comm,&
        !  & mpi_req_hand, mpierr)
      endif
    else
      if (mpi_blocking) then
        call MPI_REDUCE(buf, recv_buf, 1, mpi_krp, op, rank, self%comm,&
          & mpierr)
      else
        ! iallreduce is supported by MPI-3, not supported by NECP_X yet
        !call MPI_IREDUCE(buf, recv_buf, 1, mpi_krp, op, rank, self%comm,&
        !  & mpi_req_hand, mpierr)
      endif
    endif
    if (present(request)) request = mpi_req_hand
    if (present(recvbuf)) recvbuf = recv_buf
#else
    recvbuf = buf
#endif
  endsubroutine reduce_double_0d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine allreduce_double_0d_mpi_env(self, buf, op, blocking, request, recvbuf)

    charSt,parameter :: myname = 'parallel_env :: reduce - '
    class(mpi_env_type), intent(in) :: self      !< the mpi_env_type object
    realkd,              intent(in) :: buf       !< send buffer
    intknd, optional,    intent(in) :: op        !< MPI operator
    logknd, optional,    intent(in) :: blocking  !< whether perform blocking communication
    intknd, optional,    intent(out) :: request  !< communication request
    realkd, optional                 :: recvbuf   !< recv buffer

    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    intknd :: mpi_op,n_data
    realkd :: recv_buf

    n_data = 1

    if (present(op)) then
      selectcase(op)
      case(mpi_sum)
        mpi_op = mpi_sum
      case(mpi_max)
        mpi_op = mpi_max
      case(mpi_min)
        mpi_op = mpi_min
      endselect
    else
      mpi_op = mpi_sum
    endif

    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking

    mpi_req_hand = -1

#ifdef WITH_MPI
    if (mpi_blocking) then
      call MPI_ALLREDUCE (buf, recv_buf, n_data, mpi_krp, mpi_op, self%comm, mpierr)
    else
      !call MPI_IALLREDUCE(buf, buf, n_data, mpi_krp, mpi_op, self%comm,&
      !  & mpi_req_hand, mpierr)
      !if(present(request)) request = mpi_req_hand
    endif
    if(present(recvbuf)) recvbuf=recv_buf
#else

    if(self%n_proc > 1) call raise_error('Does not support reduce function')
#endif

  endsubroutine allreduce_double_0d_mpi_env


  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine allreduce_double_1d_mpi_env(self, buf, op, blocking, request, recvbuf)

    charSt,parameter :: myname = 'parallel_env :: reduce - '
    class(mpi_env_type), intent(in) :: self      !< the mpi_env_type object
    realkd,              intent(in) :: buf(:)    !< send buffer
    intknd, optional,    intent(in) :: op        !< MPI operator
    logknd, optional,    intent(in) :: blocking  !< whether perform blocking communication
    intknd, optional,    intent(out):: request   !< communication request
    realkd, optional                :: recvbuf(:)!< recv buffer

    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    intknd :: mpi_op,n_data
    realkd,allocatable :: recv_buf(:)

    n_data = size(buf)
    allocate(recv_buf(n_data))

    if (present(op)) then
      selectcase(op)
      case(mpi_sum)
        mpi_op = mpi_sum
      case(mpi_max)
        mpi_op = mpi_max
      case(mpi_min)
        mpi_op = mpi_min
      endselect
    else
      mpi_op = mpi_sum
    endif

    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking

    mpi_req_hand = -1

#ifdef WITH_MPI
    if (mpi_blocking) then
      call MPI_ALLREDUCE (buf, recv_buf, n_data, mpi_krp, mpi_op, self%comm,&
        & mpierr)
    else
      !call MPI_IALLREDUCE(buf, buf, n_data, mpi_krp, mpi_op, self%comm,&
      !  & request, mpierr)
      !if(present(request)) request = mpi_req_hand

    endif
    if(present(recvbuf)) recvbuf = recv_buf
#else
    if(self%n_proc > 1) call raise_error('Does not support reduce function')
#endif

  endsubroutine allreduce_double_1d_mpi_env


  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine allreduce_double_2d_mpi_env(self, buf, op, blocking, request,recvbuf)

    charSt,parameter :: myname = 'parallel_env :: reduce - '
    class(mpi_env_type), intent(in) :: self      !< the mpi_env_type object
    realkd,             intent(in) :: buf(:,:)    !< send buffer
    intknd, optional,    intent(in) :: op        !< MPI operator
    logknd, optional,    intent(in) :: blocking  !< whether perform blocking communication
    intknd, optional,    intent(out) :: request               !< communication request
    realkd, optional                 :: recvbuf(:,:)

    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    intknd :: mpi_op,n_data
    realkd,allocatable :: recv_buf(:,:)

    n_data = size(buf)
    allocate(recv_buf(size(buf,1),size(buf,2)))

    if (present(op)) then
      selectcase(op)
      case(mpi_sum)
        mpi_op = mpi_sum
      case(mpi_max)
        mpi_op = mpi_max
      case(mpi_min)
        mpi_op = mpi_min
      endselect
    else
      mpi_op = mpi_sum
    endif

    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking

    mpi_req_hand = -1

#ifdef WITH_MPI
    if (mpi_blocking) then
      call MPI_ALLREDUCE (buf, recv_buf, n_data, mpi_krp, mpi_op, self%comm,&
        & mpierr)
    else
      !call MPI_IALLREDUCE(buf, buf, n_data, mpi_krp, mpi_op, self%comm,&
      !  & request, mpierr)
      !if(present(request)) request = mpi_req_hand

    endif
    if(present(recvbuf)) recvbuf = recv_buf
#else

    if(self%n_proc > 1) call raise_error('Does not support reduce function')
#endif

  endsubroutine allreduce_double_2d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine allreduce_double_3d_mpi_env(self, buf, op, blocking, request, recvbuf)

    charSt,parameter :: myname = 'parallel_env :: reduce - '
    class(mpi_env_type), intent(in) :: self      !< the mpi_env_type object
    realkd,              intent(in) :: buf(:,:,:)!< send buffer
    intknd, optional,    intent(in) :: op        !< MPI operator
    logknd, optional,    intent(in) :: blocking  !< whether perform blocking communication
    intknd, optional,    intent(out):: request   !< communication request
    realkd, optional                :: recvbuf(:,:,:) !< recv buffer

    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    intknd :: mpi_op,n_data
    realkd,allocatable :: recv_buf(:,:,:)

    n_data = size(buf)
    allocate(recv_buf(size(buf,1),size(buf,2),size(buf,3)))

    if (present(op)) then
      selectcase(op)
      case(mpi_sum)
        mpi_op = mpi_sum
      case(mpi_max)
        mpi_op = mpi_max
      case(mpi_min)
        mpi_op = mpi_min
      endselect
    else
      mpi_op = mpi_sum
    endif

    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking

    mpi_req_hand = -1

#ifdef WITH_MPI
    if (mpi_blocking) then
      call MPI_ALLREDUCE (buf, recv_buf, n_data, mpi_krp, mpi_op, self%comm,&
        & mpierr)
    else
      !call MPI_IALLREDUCE(buf, buf, n_data, mpi_krp, mpi_op, self%comm,&
      !  & request, mpierr)
      !if(present(request)) request = mpi_req_hand
    endif
    if(present(recvbuf)) recvbuf = recv_buf
#else

    if(self%n_proc > 1) call raise_error('Does not support reduce function')
#endif

  endsubroutine allreduce_double_3d_mpi_env


  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine allreduce_double_4d_mpi_env(self, buf, op, blocking, request, recvbuf)

    charSt,parameter :: myname = 'parallel_env :: reduce - '
    class(mpi_env_type), intent(in) :: self         !< the mpi_env_type object
    realkd,              intent(in) :: buf(:,:,:,:) !< send buffer
    intknd, optional,    intent(in) :: op           !< MPI operator
    logknd, optional,    intent(in) :: blocking     !< perform blocking communi
    intknd, optional,    intent(out):: request      !< communication request
    realkd, optional                :: recvbuf(:,:,:,:) !< recv buffer

    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    intknd :: mpi_op,n_data
    realkd,allocatable :: recv_buf(:,:,:,:)

    n_data = size(buf)
    allocate(recv_buf(size(buf,1),size(buf,2),size(buf,3),size(buf,4)))

    if (present(op)) then
      selectcase(op)
      case(mpi_sum)
        mpi_op = mpi_sum
      case(mpi_max)
        mpi_op = mpi_max
      case(mpi_min)
        mpi_op = mpi_min
      endselect
    else
      mpi_op = mpi_sum
    endif

    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking

    mpi_req_hand = -1

#ifdef WITH_MPI
    if (mpi_blocking) then
      call MPI_ALLREDUCE (buf, recv_buf, n_data, mpi_krp, mpi_op, self%comm,&
        & mpierr)
    else
      !call MPI_IALLREDUCE(buf, buf, n_data, mpi_krp, mpi_op, self%comm,&
      !  & request, mpierr)
      !if(present(request)) request = mpi_req_hand

    endif
    if(present(recvbuf)) recvbuf = recv_buf
#else

    if(self%n_proc > 1) call raise_error('Does not support reduce function')
#endif

  endsubroutine allreduce_double_4d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine allreduce_integer_0d_mpi_env(self,buf,op,blocking,request,recvbuf)

    charSt,parameter :: myname = 'parallel_env :: reduce - '
    class(mpi_env_type), intent(in) :: self      !< the mpi_env_type object
    intknd,              intent(in) :: buf       !< send buffer
    intknd, optional,    intent(in) :: op        !< MPI operator
    logknd, optional,    intent(in) :: blocking  !< perform blocking communicate
    intknd, optional,    intent(out):: request   !< communication request
    intknd, optional                :: recvbuf  !< recv buffer

    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    intknd :: mpi_op,n_data
    intknd :: recv_buf

    n_data = 1

    if (present(op)) then
      selectcase(op)
      case(mpi_sum)
        mpi_op = mpi_sum
      case(mpi_max)
        mpi_op = mpi_max
      case(mpi_min)
        mpi_op = mpi_min
      endselect
    else
      mpi_op = mpi_sum
    endif

    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking

    mpi_req_hand = -1

#ifdef WITH_MPI
    if (mpi_blocking) then
      call MPI_ALLREDUCE (buf, recv_buf, n_data, MPI_INTEGER, mpi_op, self%comm, mpierr)
    else
      !call MPI_IALLREDUCE(buf, buf, n_data, MPI_INTEGER, mpi_op, self%comm,&
      !  & mpi_req_hand, mpierr)
      !if(present(request)) request = mpi_req_hand
    endif
    if(present(recvbuf)) recvbuf = recv_buf
#else

    if(self%n_proc > 1) call raise_error('Does not support reduce function')
#endif

  endsubroutine allreduce_integer_0d_mpi_env


  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine allreduce_integer_1d_mpi_env(self,buf,op,blocking,request,recvbuf)

    charSt,parameter :: myname = 'parallel_env :: reduce - '
    class(mpi_env_type), intent(in) :: self      !< the mpi_env_type object
    intknd,              intent(in) :: buf(:)    !< send buffer
    intknd, optional,    intent(in) :: op        !< MPI operator
    logknd, optional,    intent(in) :: blocking  !< whether perform blocking communication
    intknd, optional,    intent(out):: request   !< communication request
    intknd, optional                :: recvbuf(:)
    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr
    integer,allocatable :: recv_buf(:)

    intknd :: mpi_op,n_data
!write(*,*)'buf = ',buf
    n_data = size(buf)
    allocate(recv_buf(n_data))
!write(*,*)'n_data = ',n_data
    if (present(op)) then
      selectcase(op)
      case(mpi_sum)
        mpi_op = mpi_sum
      case(mpi_max)
        mpi_op = mpi_max
      case(mpi_min)
        mpi_op = mpi_min
      endselect
    else
      mpi_op = mpi_sum
    endif

    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking

    mpi_req_hand = -1

#ifdef WITH_MPI
    if (mpi_blocking) then
      call MPI_ALLREDUCE (buf, recv_buf, n_data, MPI_INTEGER, mpi_op, self%comm,&
        & mpierr)

    else
      !call MPI_IALLREDUCE(buf, buf, n_data, MPI_INTEGER, mpi_op, self%comm,&
      !  & request, mpierr)
      !if(present(request)) request = mpi_req_hand

    endif
    if(present(recvbuf)) recvbuf = recv_buf
#else

    if(self%n_proc > 1) call raise_error('Does not support reduce function')
#endif

  endsubroutine allreduce_integer_1d_mpi_env


  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine allreduce_integer_2d_mpi_env(self,buf,op,blocking,request,recvbuf)

    charSt,parameter :: myname = 'parallel_env :: reduce - '
    class(mpi_env_type), intent(in) :: self      !< the mpi_env_type object
    intknd ,             intent(in) :: buf(:,:)  !< send buffer
    intknd, optional,    intent(in) :: op        !< MPI operator
    logknd, optional,    intent(in) :: blocking  !< whether perform blocking communication
    intknd, optional,    intent(out):: request   !< communication request
    intknd, optional                :: recvbuf(:,:)

    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr
    intknd :: mpi_op,n_data
    intknd,allocatable :: recv_buf(:,:)

    n_data = size(buf)
    allocate(recv_buf(size(buf,1),size(buf,2)))

    if (present(op)) then
      selectcase(op)
      case(mpi_sum)
        mpi_op = mpi_sum
      case(mpi_max)
        mpi_op = mpi_max
      case(mpi_min)
        mpi_op = mpi_min
      endselect
    else
      mpi_op = mpi_sum
    endif

    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking

    mpi_req_hand = -1

#ifdef WITH_MPI
    if (mpi_blocking) then
      call MPI_ALLREDUCE (buf, recv_buf, n_data, MPI_INTEGER, mpi_op, self%comm,&
        & mpierr)
    else
      !call MPI_IALLREDUCE(buf, buf, n_data, MPI_INTEGER, mpi_op, self%comm,&
      !  & request, mpierr)
      !if(present(request)) request = mpi_req_hand

    endif
    if(present(recvbuf)) recvbuf = recv_buf
#else

    if(self%n_proc > 1) call raise_error('Does not support reduce function')
#endif

  endsubroutine allreduce_integer_2d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine allreduce_integer_3d_mpi_env(self,buf,op,blocking,request,recvbuf)

    charSt,parameter :: myname = 'parallel_env :: reduce - '
    class(mpi_env_type), intent(in) :: self      !< the mpi_env_type object
    intknd,              intent(in) :: buf(:,:,:)!< send buffer
    intknd, optional,    intent(in) :: op        !< MPI operator
    logknd, optional,    intent(in) :: blocking  !< whether perform blocking communication
    intknd, optional,    intent(out):: request   !< communication request
    intknd, optional                :: recvbuf(:,:,:)

    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr
    intknd :: mpi_op,n_data
    intknd,allocatable :: recv_buf(:,:,:)

    n_data = size(buf)
    allocate(recv_buf(size(buf,1),size(buf,2),size(buf,3)))

    if (present(op)) then
      selectcase(op)
      case(mpi_sum)
        mpi_op = mpi_sum
      case(mpi_max)
        mpi_op = mpi_max
      case(mpi_min)
        mpi_op = mpi_min
      endselect
    else
      mpi_op = mpi_sum
    endif

    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking

    mpi_req_hand = -1

#ifdef WITH_MPI
    if (mpi_blocking) then
      call MPI_ALLREDUCE (buf, recv_buf, n_data, MPI_INTEGER, mpi_op, self%comm,&
        & mpierr)
    else
      !call MPI_IALLREDUCE(buf, buf, n_data, MPI_INTEGER, mpi_op, self%comm,&
      !  & request, mpierr)
      !if(present(request)) request = mpi_req_hand

    endif
    if(present(recvbuf)) recvbuf = recv_buf
#else

    if(self%n_proc > 1) call raise_error('Does not support reduce function')
#endif

  endsubroutine allreduce_integer_3d_mpi_env


  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine allreduce_integer_4d_mpi_env(self,buf,op,blocking,request,recvbuf)

    charSt,parameter :: myname = 'parallel_env :: reduce - '
    class(mpi_env_type), intent(in) :: self      !< the mpi_env_type object
    intknd,              intent(in) :: buf(:,:,:,:)    !< send buffer
    intknd, optional,    intent(in) :: op        !< MPI operator
    logknd, optional,    intent(in) :: blocking  !< whether perform blocking communication
    intknd, optional,    intent(out):: request   !< communication request
    intknd, optional                :: recvbuf(:,:,:,:)

    logknd :: mpi_blocking ! whether perform blocking communication
    intknd :: mpi_req_hand ! MPI communication request
    intknd :: mpierr

    intknd :: mpi_op,n_data

    n_data = size(buf)

    if (present(op)) then
      selectcase(op)
      case(mpi_sum)
        mpi_op = mpi_sum
      case(mpi_max)
        mpi_op = mpi_max
      case(mpi_min)
        mpi_op = mpi_min
      endselect
    else
      mpi_op = mpi_sum
    endif

    mpi_blocking = .TRUE.
    if (present(blocking)) mpi_blocking = blocking

    mpi_req_hand = -1

#ifdef WITH_MPI
    if (mpi_blocking) then
      call MPI_ALLREDUCE (buf, buf, n_data, MPI_INTEGER, mpi_op, self%comm,&
        & mpierr)
    else
      !call MPI_IALLREDUCE(buf, buf, n_data, MPI_INTEGER, mpi_op, self%comm,&
      !  & request, mpierr)
      !if(present(request)) request = mpi_req_hand

    endif
#else

    if(self%n_proc > 1) call raise_error('Does not support reduce function')
#endif

  endsubroutine allreduce_integer_4d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine wait_mpi_env(request)

    intknd, intent(in) :: request          !< communication request
    intknd :: mpierr

#ifdef WITH_MPI
    call MPI_WAIT(request, MPI_STATUS_IGNORE, mpierr)
#endif

  endsubroutine wait_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine waitany_mpi_env(requests, index)

    intknd, intent(in) :: requests(:) !< communication requests
    intknd, intent(inout) :: index    !< index of handle for operation that completed
    intknd :: mpierr

#ifdef WITH_MPI
    call MPI_WAITANY(size(requests), requests, index, MPI_STATUS_IGNORE,&
      & mpierr)
#endif

  endsubroutine waitany_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine waitall_mpi_env(requests)

    intknd, intent(in) :: requests(:) !< communication requests
    intknd :: mpierr

#ifdef WITH_MPI
    call MPI_WAITALL(size(requests), requests, MPI_STATUSES_IGNORE, mpierr)
#endif

  endsubroutine waitall_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine waitall_2d_mpi_env(requests)

    intknd, intent(in) :: requests(:,:) !< communication requests
    intknd :: mpierr

#ifdef WITH_MPI
    call MPI_WAITALL(size(requests), requests, MPI_STATUSES_IGNORE, mpierr)
#endif

  endsubroutine waitall_2d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine waitall_3d_mpi_env(requests)

    intknd, intent(in) :: requests(:,:,:) !< communication requests
    intknd :: mpierr

#ifdef WITH_MPI
    call MPI_WAITALL(size(requests), requests, MPI_STATUSES_IGNORE, mpierr)
#endif

  endsubroutine waitall_3d_mpi_env

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine clear_parallel_env_type(self)
    class(parallel_env_type),intent(inout) :: self
    intknd :: ierr
    call self%world%clear()
    call self%energy%clear()
    call self%space%clear()
    call self%angle%clear()
    call self%ray%clear()
    call self%world%finalize()
  endsubroutine clear_parallel_env_type

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine init_parallel_env_type(self)
    charSt,parameter :: myname = 'parallel_env :: init - '
    class(parallel_env_type),intent(inout) :: self

#ifdef WITH_MPI
    call self%world%init(mpi_comm_world)
#else
    call self%world%init(0_kip)
#endif
  endsubroutine init_parallel_env_type

  !-----------------------------------------------------------------------------
  !> @brief
  !-----------------------------------------------------------------------------
  subroutine cart_creat_parallel_env_type(self,n_energy,n_axial,n_radial,n_angle,n_thread)
    charSt,parameter :: myname = 'parallel_env :: init - '
    class(parallel_env_type),intent(inout) :: self
    intknd,intent(in) :: n_energy
    intknd,intent(in) :: n_axial
    intknd,intent(in) :: n_radial
    intknd,intent(in) :: n_angle
    intknd,intent(in) :: n_thread

    intknd :: ndims,nerror,n_space,dims(3),cart_comm,temp_comm,comm_cart
    intknd :: mpierr
    logknd :: periods(3)
    logknd :: remain_dims(3),reorder

    nerror = get_nerror()

    n_space = n_axial * n_radial
    if(n_energy < 1) call raise_error(myname//'energy domain should be > 1')
    if(n_space  < 1) call raise_error(myname//'space domain should be > 1')
    if(n_angle  < 1) call raise_error(myname//'angle domain should be > 1')
    if(n_thread < 1) call raise_error(myname//'# of thread should be > 1')

    if(nerror == get_nerror()) then

#ifdef WITH_MPI
    dims(1) = n_energy
    dims(2) = n_space
    dims(3) = n_angle
    ndims = 3
    periods(1:ndims) = .false.
    reorder = .true.

    if(self%world%n_proc /= (n_energy*n_axial*n_radial*n_angle)) then
      call raise_error(myname//'input.inp is not consistent with CPU number')
    endif

    call mpi_cart_create(self%world%comm,ndims,dims,periods,reorder,comm_cart,mpierr)

    if(mpierr == mpi_success) then
      remain_dims = (/.false.,.true.,.false./)
      call mpi_cart_sub(comm_cart,remain_dims,temp_comm,mpierr)
      call self%space%init(temp_comm)

      remain_dims = (/.false.,.false.,.true./)
      call mpi_cart_sub(comm_cart,remain_dims,temp_comm,mpierr)
      call self%angle%init(temp_comm)

      remain_dims = (/.true.,.false.,.false./)
      call mpi_cart_sub(comm_cart,remain_dims,temp_comm,mpierr)
      call self%energy%init(temp_comm)

      call self%ray%init(n_thread)
      self%is_init = .true.

    endif

    self%n_axial = n_axial
    self%n_radial = n_radial
    self%i_axial = ceiling((self%space%rank+0.1)/self%n_radial)
    self%i_radial = mod((self%space%rank+1),self%n_radial)

#else
    self%energy = self%world
    self%space = self%world
    self%angle = self%world
    call self%ray%init(n_thread)
    self%n_axial = 1
    self%n_radial = 1
    self%is_init = .true.
#endif
    endif
  endsubroutine cart_creat_parallel_env_type

end module parallel_env
