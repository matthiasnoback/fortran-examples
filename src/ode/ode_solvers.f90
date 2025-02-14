module ode_solvers
   use precision, only: dp

   implicit none

   type ode_solver
      real(kind=dp) :: time, timestep, begin, end
      real(kind=dp), dimension(:), allocatable :: state
      procedure(eval_deriv), pointer, nopass :: f
      procedure(solve_step_interface), pointer, pass(solver) :: solve
   contains
      procedure :: next => default_next_step  ! same for all solvers
      procedure :: initial => default_initial_values  ! same for all solvers
   end type ode_solver

   type ode_method
      procedure(solve_step_interface), nopass, pointer :: solve
   end type ode_method

   integer, parameter :: ODE_EULER = 1
   integer, parameter :: ODE_HEUN = 2

   abstract interface
      !
      ! Subroutine for storing the initial values
      !
      subroutine initial(solver, initial_data)
         import dp

         import ode_solver
         implicit none
         class(ode_solver), intent(in) :: solver
         real(kind=dp), dimension(:), intent(in) :: initial_data
      end subroutine initial
      !
      ! Function to compute the derivative
      !
      function eval_deriv(x, t)
         import dp

         implicit none

         real(kind=dp), dimension(:), intent(in) :: x
         real(kind=dp), dimension(size(x)) :: eval_deriv  ! Derivative is array of
         ! same size as x
         real(kind=dp), intent(in) :: t
      end function eval_deriv
      !
      ! Interface function to hide the internal workings
      !
      function next(solver, time, time_step) result(res)

         import ode_solver
         import dp

         implicit none
         class(ode_solver), intent(in) :: solver
         real(kind=dp), intent(in) :: time
         real(kind=dp), intent(in) :: time_step
         real(kind=dp), dimension(size(solver%state)) :: res
      end function next
      !
      ! Function to actually compute the new values
      !
      function solve_step_interface(solver, time, time_step) result(res)
         import ode_solver
         import dp

         implicit none

         class(ode_solver), intent(in) :: solver
         real(kind=dp), intent(in) :: time
         real(kind=dp), intent(in) :: time_step
         real(kind=dp), dimension(size(solver%state)) :: res
      end function solve_step_interface
   end interface

contains
   !
   ! General code
   !
   subroutine default_initial_values(solver, initial_data)
      class(ode_solver), intent(inout) :: solver
      real(kind=dp), dimension(:), intent(in) :: initial_data
      solver%state = initial_data
   end subroutine default_initial_values

   function new_solver(ode_type, rhs) result(res)
      integer, intent(in) :: ode_type
      procedure(eval_deriv) :: rhs
      type(ode_method), dimension(2) :: solvers
      type(ode_solver) :: res

      solvers = [ode_method(solve_euler), ode_method(solve_heun)]

      res%f => rhs
      if (ode_type >= 1 .and. ode_type <= 3) then
         res%solve => solvers(ode_type)%solve
      else
         res%solve => solvers(1)%solve  ! Use Euler’s method by default
      end if
   end function new_solver

   function default_next_step(solver, time, time_step) result(res)
      class(ode_solver), intent(inout) :: solver
      real(kind=dp), intent(in) :: time
      real(kind=dp), intent(in) :: time_step
      real(kind=dp), dimension(size(solver%state)) :: res
      res = solver%solve(time, time_step)
      solver%state = res
   end function default_next_step

   !
   ! Specific methods
   !
   function solve_euler(solver, time, time_step)
      class(ode_solver), intent(in) :: solver
      real(kind=dp), intent(in) :: time
      real(kind=dp), intent(in) :: time_step
      real(kind=dp), dimension(size(solver%state)) :: solve_euler

      solve_euler = solver%state + (time_step*solver%f(solver%state, time))
   end function solve_euler

   function solve_heun(solver, time, time_step) result(res)
      class(ode_solver), intent(in) :: solver
      real(kind=dp), intent(in) :: time
      real(kind=dp), intent(in) :: time_step
      real(kind=dp), dimension(size(solver%state)) :: res

      real(kind=dp), dimension(size(solver%state)) :: v_predictor
      real(kind=dp), dimension(size(solver%state)) :: v_corrector

      v_predictor = solver%state + time_step*solver%f(solver%state, time)
      v_corrector = solver%state + time_step* &
                    solver%f(v_predictor, time + time_step)
      res = (v_predictor + v_corrector)/2.0_dp
   end function solve_heun
end module ode_solvers
