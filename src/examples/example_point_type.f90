program example
   use iso_fortran_env, only: wp => real64

   implicit none

   type :: point
      real(kind=wp) :: x
      real(kind=wp) :: y
   end type point

   type(point) :: a_point

   a_point = point(3.0_wp, 4.0_wp)
   print *, a_point%x, a_point%y

end program example
