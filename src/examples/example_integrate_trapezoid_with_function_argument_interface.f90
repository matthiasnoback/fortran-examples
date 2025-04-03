module integration
   use iso_fortran_env, only: wp => real64

   implicit none(type, external)

   private
   public :: integrate_trapezoid

contains

   function integrate_trapezoid(the_function, x_min, x_max, steps) result(integral)
      interface
         function the_function(x) result(y)
            import wp

            real(kind=wp), intent(in) :: x
            real(kind=wp):: y
         end function the_function
      end interface

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

      integral = integral * delta_x
   end function integrate_trapezoid
end module integration

program example
   use iso_fortran_env, only: wp => real64
   use integration, only: integrate_trapezoid

   implicit none(type, external)

   real(kind=wp) :: integral
   real(kind=wp) :: a
   real(kind=wp) :: b

   a = 1.0_wp
   b = 2.0_wp

   integral = integrate_trapezoid(damped_oscillatory_function, 0.0_wp, 10.0_wp, 100)

   print *, 'Integral', integral

contains

   function damped_oscillatory_function(x) result(y)
      real(kind=wp), intent(in) :: x
      real(kind=wp):: y

      y = exp(-a*x)*cos(b*x)
   end function damped_oscillatory_function

end program example
