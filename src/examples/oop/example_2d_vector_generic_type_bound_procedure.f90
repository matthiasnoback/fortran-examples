module m_vector
   use iso_fortran_env, only: wp => real64

   implicit none(type,external)

   type :: t_2d_vector
      real(kind=wp) :: v1
      real(kind=wp) :: v2
   contains
      procedure :: add => add_2d_vectors
      generic :: operator(+) => add
   end type t_2d_vector

contains
   pure function add_2d_vectors(vector1, vector2) result(sum_vector)
      class(t_2d_vector), intent(in) :: vector1
      class(t_2d_vector), intent(in) :: vector2
      type(t_2d_vector) :: sum_vector

      sum_vector = t_2d_vector(vector1%v1 + vector2%v1, vector1%v2 + vector2%v2)
   end function add_2d_vectors
end module m_vector

program example
   use iso_fortran_env, only: wp => real64

   use m_vector, only: t_2d_vector

   implicit none(type,external)

   type(t_2d_vector) :: point
   type(t_2d_vector) :: displacement
   type(t_2d_vector) :: new_position

   point = t_2d_vector(2.0_wp, 3.0_wp)
   displacement = t_2d_vector(3.0_wp, 4.0_wp)

   new_position = point + displacement
   new_position = point%add(displacement)
   print *, new_position%v1, new_position%v2

end program example
