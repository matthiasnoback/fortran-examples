program solve_some_ode
   use ode_solvers, only: ode_solver, new_solver, ODE_EULER
   use some_ode, only: dampened_oscillator, force_or_displacement, dampening_coefficient
   use precision, only: dp

   implicit none

   real(kind=dp) :: time
   real(kind=dp) :: max_time
   real(kind=dp) :: deltt
   real(kind=dp), dimension(2) :: state
   class(ode_solver), allocatable :: solver

   force_or_displacement = 0.6_dp
   dampening_coefficient = 0.5_dp

   solver = new_solver(ODE_EULER, dampened_oscillator)
   call solver%initial([0.0_dp, 10.0_dp])
   time = 0.0_dp
   max_time = 20.0_dp
   deltt = 0.1_dp

   do while (time < max_time)
      state = solver%next(time, deltt)
      call print_state(time, state)
      time = time + deltt
   end do

contains
   subroutine print_state(time, state)
      real(kind=dp), intent(in) :: time
      real(kind=dp), dimension(:), intent(in) :: state
      print '(g0,1x,es15.8e2)', time, [state(size(state))]
   end subroutine print_state
end program solve_some_ode
