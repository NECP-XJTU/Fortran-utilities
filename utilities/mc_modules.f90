module mc_modules
  use geom_point

# include <kind_parameter.h>

  !-----------------------------------------------------------------------------
  !
  type :: surface_type
    intknd :: defid = 0
    realkd :: x = 0.0
  endtype

  !-----------------------------------------------------------------------------
  !
  type :: cell_type
    intknd :: defID = 0
    intknd :: id_matReg = 0
    intknd :: n_surf = 0
    intknd,allocatable :: index_surfs(:)
  contains

    procedure,pass :: init_cell

  endtype cell_type

  !-----------------------------------------------------------------------------
  !
  type  particle_type
    intknd :: defid = 0
    type(geom_point_type) :: pos
    type(geom_point_type) :: pre_pos
    type(geom_point_type) :: vector
    type(geom_point_type) :: pre_vector

    realkd :: wt = 0.0
    realkd :: energy = 0.0
    realkd :: angle  = 0.0
    intknd :: id_mat = 0
    intknd :: pre_id_mat = 0
  endtype particle_type

!===============================================================================
contains
!-------------------------------------------------------------------------------
!
  subroutine init_particle(this,defid,pos,vector,energy,angle)
    class(particle_type) :: this
    intknd,intent(in) :: defID
    type(geom_point_type),intent(in) :: pos
    type(geom_point_type),intent(in) :: vector
    realkd,intent(in) :: energy
    realkd,intent(in) :: angle

    this%defid = defid
    this%pos = pos
    this%pre_pos = pos
    this%vector = vector
    this%pre_vector = vector
    this%energy = energy
    this%angle = angle

  endsubroutine init_particle

!-------------------------------------------------------------------------------
!
  subroutine init_cell(this,defid,id_matReg,index_surfs)
    class(cell_type) :: this
    intknd,intent(in) :: defid
    intknd,intent(in) :: id_matReg
    intknd,intent(in) :: index_surfs(:)

    this%id_matReg = id_matReg
    this%index_surfs = index_surfs
    this%n_surf = size(index_surfs)
    this%defID = defID

  endsubroutine
!-------------------------------------------------------------------------------
endmodule mc_modules