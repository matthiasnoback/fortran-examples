module integration_with_dt
   use iso_fortran_env, only: wp => real64

   implicit none(type, external)

   private
   public :: integrate_trapezoid, user_function_t

   type, abstract :: user_function_t
   contains
      procedure(function_to_integrate), deferred :: evaluate
   end type user_function_t

   interface
      function function_to_integrate(self, x) result(y)
         import wp, user_function_t

         implicit none(type, external)

         class(user_function_t), intent(in) :: self
         real(kind=wp), intent(in) :: x
         real(kind=wp) :: y
      end function function_to_integrate
   end interface

contains

   function integrate_trapezoid(user_function, x_min, x_max, steps) result(integral)
      class(user_function_t), intent(in) :: user_function
      real(kind=wp), intent(in) :: x_min
      real(kind=wp), intent(in) :: x_max
      integer, intent(in) :: steps
      real(kind=wp) :: integral
      integer :: step
      real(kind=wp) :: x
      real(kind=wp) :: delta_x

      if (steps <= 0) then
         integral = 0.0_wp
         return
      end if

      delta_x = (x_max - x_min)/steps

      integral = user_function%evaluate(x_min) + user_function%evaluate(x_max)/2.0_wp

      do step = 1, steps - 1
         x = x_min + step*delta_x
         integral = integral + user_function%evaluate(x)
      end do

      integral = integral*delta_x
   end function integrate_trapezoid
end module integration_with_dt

module damped_oscillation
   use iso_fortran_env, only: wp => real64
   use integration_with_dt, only: user_function_t

   implicit none(type, external)

   private
   public :: damped_oscillatory_function_t

   type, extends(user_function_t) :: damped_oscillatory_function_t
      real(kind=wp) :: a
      real(kind=wp) :: b
   contains
      procedure :: evaluate => damped_oscillatory_function_evaluate
   end type damped_oscillatory_function_t

contains

   pure function damped_oscillatory_function_evaluate(self, x) result(y)
      class(damped_oscillatory_function_t), intent(in) :: self
      real(kind=wp), intent(in) :: x
      real(kind=wp) :: y

      y = exp(-self%a*x)*cos(self%b*x)
   end function damped_oscillatory_function_evaluate
end module damped_oscillation

module integration
   use iso_fortran_env, only: wp => real64

   implicit none(type, external)

   private
   public :: integrate_trapezoid

   interface
      function function_to_integrate(x) result(y)
         import wp

         implicit none(type, external)

         real(kind=wp), intent(in) :: x
         real(kind=wp):: y
      end function function_to_integrate
   end interface

contains

   function integrate_trapezoid(the_function, x_min, x_max, steps) result(integral)
      procedure(function_to_integrate), pointer, intent(in) :: the_function
      real(kind=wp), intent(in) :: x_min
      real(kind=wp), intent(in) :: x_max
      integer, intent(in) :: steps
      real(kind=wp) :: integral
      integer :: step
      real(kind=wp) :: x
      real(kind=wp) :: delta_x

      if (steps <= 0) then
         integral = 0.0_wp
         return
      end if

      delta_x = (x_max - x_min)/steps

      integral = the_function(x_min) + the_function(x_max)/2.0_wp

      do step = 1, steps - 1
         x = x_min + step*delta_x
         integral = integral + the_function(x)
      end do

      integral = integral*delta_x
   end function integrate_trapezoid
end module integration

program example
   use iso_fortran_env, only: wp => real64, int64
   use integration, only: integrate_trapezoid
   use integration_with_dt, only: integrate_trapezoid_with_dt => integrate_trapezoid
   use damped_oscillation, only: damped_oscillatory_function_t

   implicit none(type, external)

   real(kind=wp) :: integral
   real(kind=wp) :: a
   real(kind=wp) :: b
   integer :: i, iterations
   integer(int64) :: irate, tic, toc

   iterations = 10000

   a = 1.0_wp
   b = 2.0_wp

   call system_clock(count_rate=irate)

   call system_clock(count=tic)
   do i = 1, iterations
      integral = integrate_trapezoid_damped_oscillator_inline()
   end do
   call system_clock(count=toc)
   print "(es15.8e2, a)", (toc - tic)/real(irate, wp), " inline calculation"

   call system_clock(count=tic)
   do i = 1, iterations
      integral = integrate_trapezoid(damped_oscillatory_function, 0.0_wp, 10.0_wp, 100)
   end do
   call system_clock(count=toc)
   print "(es15.8e2, a)", (toc - tic)/real(irate, wp), " with procedure pointer"

   call system_clock(count=tic)
   do i = 1, iterations
      integral = integrate_trapezoid_with_dt( &
                 damped_oscillatory_function_t(1.0_wp, 2.0_wp), &
                 0.0_wp, &
                 10.0_wp, &
                 100 &
                 )
   end do
   call system_clock(count=toc)
   print "(es15.8e2, a)", (toc - tic)/real(irate, wp), " with type-bound procedure"

contains

   function damped_oscillatory_function(x) result(y)
      real(kind=wp), intent(in) :: x
      real(kind=wp):: y

      y = exp(-a*x)*cos(b*x)
   end function damped_oscillatory_function

   function integrate_trapezoid_damped_oscillator_inline() result(integral)
      real(kind=wp) :: x_min
      real(kind=wp) :: x_max
      integer :: steps
      real(kind=wp) :: integral
      integer :: step
      real(kind=wp) :: x
      real(kind=wp) :: delta_x
      real(kind=wp) :: a, b

      x_min = 0.0_wp
      x_max = 10.0_wp
      steps = 100
      a = 1.0_wp
      b = 2.0_wp

      if (steps <= 0) then
         integral = 0.0_wp
         return
      end if

      delta_x = (x_max - x_min)/steps

      integral = (exp(-a*x_min)*cos(b*x_min) + exp(-a*x_max)*cos(b*x_max))/2.0_wp

      do step = 1, steps - 1
         x = x_min + step*delta_x
         integral = integral + exp(-a*x)*cos(b*x)
      end do

      integral = integral*delta_x
   end function integrate_trapezoid_damped_oscillator_inline

end program example
