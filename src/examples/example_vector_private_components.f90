module m_vector
   use iso_fortran_env, only: wp => real64

   implicit none

   type :: t_2d_vector
      ! You can also make all components private at once:
      ! private
      real(kind=wp), private :: v1
      real(kind=wp), private :: v2
   end type t_2d_vector

end module m_vector

program example
   use iso_fortran_env, only: wp => real64

   use m_vector, only: t_2d_vector

   implicit none

   type(t_2d_vector) :: point_1
   type(t_2d_vector) :: point_2

   ! Does not work
   point_1 = t_2d_vector(2.0_wp, 3.0_wp)

   ! Does not work either
   point_2%v1 = 2.0_wp
   point_2%v2 = 3.0_wp

   ! Also doesn't work
   print *, point_1%v1, point_1%v2
   print *, point_2%v1, point_2%v2

end program example
