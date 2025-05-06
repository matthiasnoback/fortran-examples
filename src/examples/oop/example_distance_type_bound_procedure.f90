module m_point
   use iso_fortran_env, only: wp => real64

   implicit none

   type :: point
      real(kind=wp) :: x
      real(kind=wp) :: y
   contains
      procedure :: distance_to => point_distance_to
   end type point
contains

   pure function point_distance_to(this, that) result(res)
      class(point), intent(in) :: this
      class(point), intent(in) :: that

      real(kind=wp) :: res

      res = sqrt((this%y - that%y)**2 + (this%x - that%x)**2)
   end function point_distance_to

end module m_point

program example
   use iso_fortran_env, only: wp => real64

   use m_point, only: point

   implicit none

   type(point) :: point1
   type(point) :: point2

   point1 = point(0.0_wp, 0.0_wp)
   point2 = point(3.0_wp, 4.0_wp)

   print *, point1%distance_to(point2)  ! OUTPUT 5.00000000000000
end program example
