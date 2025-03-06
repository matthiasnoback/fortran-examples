module m_vector
   use iso_fortran_env, only: wp => real64

   implicit none

   type :: t_2d_vector
      private
      real(kind=wp) :: v1
      real(kind=wp) :: v2
   contains
      procedure :: get_v1 => vector_get_v1
      procedure :: set_v1 => vector_set_v1
      procedure :: get_v2 => vector_get_v2
      procedure :: set_v2 => vector_set_v2
   end type t_2d_vector

   interface t_2d_vector
      module procedure :: vector_constructor
   end interface

contains

   pure function vector_constructor(v1, v2) result(vector)
      real(kind=wp), intent(in) :: v1
      real(kind=wp), intent(in) :: v2
      type(t_2d_vector) :: vector

      vector%v1 = v1
      vector%v2 = v2
   end function vector_constructor

   pure function vector_get_v1(self) result(v1)
      class(t_2d_vector), intent(in) :: self
      real(kind=wp) :: v1

      v1 = self%v1
   end function vector_get_v1

   subroutine vector_set_v1(self, v1)
      class(t_2d_vector), intent(inout) :: self
      real(kind=wp), intent(in) :: v1

      self%v1 = v1
   end subroutine vector_set_v1

   pure function vector_get_v2(self) result(v2)
      class(t_2d_vector), intent(in) :: self
      real(kind=wp) :: v2

      v2 = self%v2
   end function vector_get_v2

   subroutine vector_set_v2(self, v2)
      class(t_2d_vector), intent(inout) :: self
      real(kind=wp), intent(in) :: v2

      self%v2 = v2
   end subroutine vector_set_v2

end module m_vector

program example
   use iso_fortran_env, only: wp => real64

   use m_vector, only: t_2d_vector

   implicit none

   type(t_2d_vector) :: point_1
   type(t_2d_vector) :: point_2

   ! Works now!
   point_1 = t_2d_vector(2.0_wp, 3.0_wp)

   ! It works now, using setters
   call point_2%set_v1(2.0_wp)
   call point_2%set_v2(3.0_wp)

   ! It works now, using getters
   print *, point_1%get_v1(), point_1%get_v2()
   print *, point_2%get_v1(), point_2%get_v2()

   print *, sqrt((point_1%get_v1() - point_2%get_v1())**2 + &
      (point_2%get_v2() - point_2%get_v2())**2)

end program example
