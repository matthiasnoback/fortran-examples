program tsunami
   use m_config, only: tsunami_config
   use m_init, only: t_init_gaussian, t_init_constant
   use m_diff, only: diff => diff_centered
   use m_field, only: t_field
   use m_observer, only: t_timeloop_observer, t_printing_observer, t_do_nothing_observer, &
                         t_observer_collection, t_observer_reference
   use precision, only: dp

   implicit none

   integer :: n

   type(tsunami_config) :: config

   type(t_field) :: u
   type(t_field) :: h

   real(kind=dp), parameter :: g = 9.8_dp  ! gravitational acceleration
   real(kind=dp), parameter :: hmean = 10.0_dp  ! mean water height (?)

   class(t_timeloop_observer), allocatable :: observer

   ! TODO read config from file
   config = tsunami_config(100, 5000, 0.02_dp, 1.0_dp)

   u = t_field('Water velocity in x', config%get_grid_size())
   h = t_field('Water height', config%get_grid_size())

   ! TODO implement t_init_random with itself a strategy for randomness
   call h%init(t_init_gaussian(25, 0.02_dp))
   call u%init(t_init_constant(0.0_dp))

   ! TODO make this dependent on CLI argument: --quiet?
   observer = t_observer_collection([ &
                                    !            t_observer_reference(t_do_nothing_observer()), &
                                    t_observer_reference(t_printing_observer()) &
                                    ])

   ! TODO add logging observer

   call observer%begin_timeloop(h, 0)

   print *, h

   time_loop: do n = 1, config%get_num_time_steps()
      u%data = u%data - (u%data*diff(u%data) + g*diff(h%data)) &
               /config%get_dx()*config%get_dt()
      h%data = h%data - diff(u%data*(hmean + h%data)) &
               /config%get_dx()*config%get_dt()

      call observer%after_timestep(h, n)

   end do time_loop

end program tsunami
