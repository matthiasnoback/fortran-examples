program example
   use iso_fortran_env, only: wp => real64

   implicit none

   type :: point
      real(kind=wp) :: x
      real(kind=wp) :: y
   end type point

   type(point) :: point1
   type(point) :: point2

   point1 = point(0.0_wp, 0.0_wp)
   point2 = point(3.0_wp, 4.0_wp)

   print *, distance(point1, point2)  ! OUTPUT 5.00000000000000

contains

   pure function distance(point1, point2) result(res)
      type(point), intent(in) :: point1
      type(point), intent(in) :: point2

      real(kind=wp) :: res

      res = sqrt((point1%y - point2%y)**2 + (point1%x - point2%x)**2)
   end function distance

end program example
