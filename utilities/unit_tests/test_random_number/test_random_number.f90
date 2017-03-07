program test_random_number
  use utilities
  implicit none

# include <kind_parameter.h>

  intknd :: i
  realkd,parameter :: n_surf = 101
!  intknd,parameter :: ncell = 100
!  intknd :: index_surfs(2)
!  type(geom_point_type) :: pos
!  type(surface_type) :: surfs(101)
!  type(cell_type)    :: cells(ncell)
!  type(particle_type):: neutron

!  ! init cell geometry
!  do i = 0, n_surf
!    surfs(i)%id = i + 1
!    surfs(i)%x = i
!  enddo

!  do i = 1, ncell
!    index_surfs(1) = i -1
!    index_surfs(2) = i
!    cells(i)%init(i,i,index_surfs)
!  enddo

!  ! init partical
!  call pos%init(X=0.2)
!  call vector%init(X=(sqrt(2.0)/2),Y= (sqrt(2.0)/2))
!  call neutron%init(1, pos, vector,1.0,0.0)

!  ig = 1
!  do while(is_alive)
!    mat = mats(neutron%id_mat)

!    call find_cell(cells,neutron%pos)

!    call distance_to_boundary(neutron%pos,cell,d_bound)

!    if(mat%xs_tr(ig) == rzero) then
!      d_collision = rinfinity
!    else
!      call random_number(rdm)
!      d_collision = -log(rdm)/mat%xs_tr(ig)
!    endif

!    distance = min(d_bound,d_collision)

!    neutron%pre_pos = neutron%pos
!    neutron%pos%dims = neutron%pos%dims + neutron%vector%dims * distance

!    ! Calculates fluxes and reaction rates. it is happened at every event
!    !(crossing surface, cross lattice or collision)
!    !call trackength_tally(neutron,distance)

!    ! Update estimate of keff
!    global_counter(Eigenvalue)%value = global_counter(Eigenvalue)%value + &
!      neutron%wt * distance * mat%nu_fission(ig)

!    if(d_collision > d_bound) then
!      !find next cell
!    else
!      !collision happened
!    global_counter(K_Collision)%value = global_counter(Eigenvalue)%value + &
!      neutron%wt * mat%nu_fission(ig) / mat%xs_tr(ig)

!      call collision(neutron, mat)

!    endif

!  enddo


endprogram test_random_number