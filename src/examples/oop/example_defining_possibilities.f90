module m_point
   use iso_fortran_env, only: wp => real64

   implicit none

   type :: point
      real(kind=wp) :: x
      real(kind=wp) :: y
   end type point
end module m_point

program example
   use iso_fortran_env, only: wp => real64

   use m_point, only: point

   implicit none

   real(kind=wp) :: sum
   real(kind=wp), dimension(2) :: n1
   type(point) :: point_1, point_2, point_sum

   n1 = [3.0_wp, 4.0_wp]

   point_1 = point(0.0_wp, 0.0_wp)
   point_2 = point(n1(1), n1(2))

   sum = n1(1) + n1(2)

   point_sum = point_1 + point_2
end program example
