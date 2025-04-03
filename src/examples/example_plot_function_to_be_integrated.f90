program example
   use iso_fortran_env, only: wp => real64

   implicit none(type, external)

   real(kind=wp) :: integral
   real(kind=wp) :: a
   real(kind=wp) :: b

   a = 1.0_wp
   b = 2.0_wp

   call plot_function(my_function, 0.0_wp, 10.0_wp, 100)

contains

   function my_function(x) result(y)
      real(kind=wp), intent(in) :: x
      real(kind=wp):: y

      y = exp(-a*x)*cos(b*x)
   end function my_function

   subroutine plot_function(the_function, xmin, xmax, steps)
      interface
         function the_function(x) result(y)
            import wp

            real(kind=wp), intent(in) :: x
            real(kind=wp):: y
         end function the_function
      end interface

      real(kind=wp), intent(in) :: xmin
      real(kind=wp), intent(in) :: xmax
      integer, intent(in) :: steps

      integer :: i
      real(kind=wp) :: delta_x
      real(kind=wp) :: x

      if (steps <= 0) then
         return
      end if

      delta_x = (xmax - xmin)/steps

      do i = 1, steps - 1
         x = xmin + i*delta_x
         print '(es15.8e2,1x,es15.8e2)', x, the_function(x)
      end do

   end subroutine plot_function
end program example
