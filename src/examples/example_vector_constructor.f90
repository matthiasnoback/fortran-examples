module m_vector
   use iso_fortran_env, only: wp => real64

   implicit none

   type :: t_2d_vector
      private
      real(kind=wp) :: v1
      real(kind=wp) :: v2
   end type t_2d_vector

   interface t_2d_vector
      module procedure vector_constructor
   end interface

contains

   pure function vector_constructor(v1, v2) result(vector)
      real(kind=wp), intent(in) :: v1
      real(kind=wp), intent(in) :: v2
      type(t_2d_vector) :: vector

      vector%v1 = v1
      vector%v2 = v2
   end function vector_constructor

end module m_vector

program example
   use iso_fortran_env, only: wp => real64

   use m_vector, only: t_2d_vector

   implicit none

   type(t_2d_vector) :: point_1
   type(t_2d_vector) :: point_2

   ! Works now!
   point_1 = t_2d_vector(2.0_wp, 3.0_wp)

   ! Does not work
   point_2%v1 = 2.0_wp
   point_2%v2 = 3.0_wp

   ! Also doesn't work
   print *, point_1%v1, point_1%v2
   print *, point_2%v1, point_2%v2

end program example
