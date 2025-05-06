module m_vector
   use iso_fortran_env, only: wp => real64

   implicit none

   private

   public :: new_vector, t_abstract_2d_vector

   type, abstract :: t_abstract_2d_vector
   contains
      procedure(get_v1_interface), deferred :: get_v1
      procedure(set_v1_interface), deferred :: set_v1
      procedure(get_v2_interface), deferred :: get_v2
      procedure(set_v2_interface), deferred :: set_v2
   end type t_abstract_2d_vector

   type, extends(t_abstract_2d_vector) :: t_2d_vector
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
      module procedure :: new_vector
   end interface

   interface
      pure function get_v1_interface(self) result(v1)
         import t_abstract_2d_vector, wp

         implicit none

         class(t_abstract_2d_vector), intent(in) :: self
         real(kind=wp) :: v1
      end function get_v1_interface

      pure function get_v2_interface(self) result(v2)
         import t_abstract_2d_vector, wp

         implicit none

         class(t_abstract_2d_vector), intent(in) :: self
         real(kind=wp) :: v2
      end function get_v2_interface

      subroutine set_v1_interface(self, v1)
         import t_abstract_2d_vector, wp

         implicit none

         class(t_abstract_2d_vector), intent(inout) :: self
         real(kind=wp), intent(in) :: v1
      end subroutine set_v1_interface

      subroutine set_v2_interface(self, v2)
         import t_abstract_2d_vector, wp

         implicit none

         class(t_abstract_2d_vector), intent(inout) :: self
         real(kind=wp), intent(in) :: v2
      end subroutine set_v2_interface
   end interface

contains

   pure function new_vector(v1, v2) result(vector)
      real(kind=wp), intent(in) :: v1
      real(kind=wp), intent(in) :: v2
      type(t_2d_vector) :: vector

      vector%v1 = v1
      vector%v2 = v2
   end function new_vector

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

   use m_vector, only: new_vector, t_abstract_2d_vector

   implicit none

   ! An entity declared with the CLASS keyword shall be a dummy argument or have the ALLOCATABLE or POINTER attribute.
   class(t_abstract_2d_vector), allocatable :: point_1
   class(t_abstract_2d_vector), allocatable :: point_2

   ! We are now forced to call the new_vector factory
   ! -> "The derived type specifier shall not specify an abstract type."
   ! -> "There are more component specifications in a structure constructor than components in the derived type."
   point_1 = new_vector(2.0_wp, 3.0_wp)

   ! This code would compile, but we are calling a method on a non-allocated point_2

   ! call point_2%set_v1(2.0_wp)
   ! call point_2%set_v2(3.0_wp)

   ! -> "forrtl: severe (157): Program Exception - access violation"

   print *, point_1%get_v1(), point_1%get_v2()

end program example
