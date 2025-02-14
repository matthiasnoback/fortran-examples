module m_observer
   use precision, only: dp
   use m_field, only: t_field

   implicit none

   character(*), parameter :: print_format = '(i0,*(1x,es15.8e2))'

   type, abstract :: t_timeloop_observer
   contains
      procedure(observer_function), deferred :: begin_timeloop
      procedure(observer_function), deferred :: after_timestep
   end type t_timeloop_observer

   interface
      subroutine observer_function(self, field, timestep)
         import :: t_timeloop_observer, t_field

         implicit none

         class(t_timeloop_observer), intent(in) :: self
         class(t_field), intent(in) :: field
         integer, intent(in) :: timestep
      end subroutine observer_function
   end interface

   type, extends(t_timeloop_observer) :: t_printing_observer
   contains
      procedure :: begin_timeloop => printing_observer_print_timestep_and_data
      procedure :: after_timestep => printing_observer_print_timestep_and_data
   end type t_printing_observer

   type, extends(t_timeloop_observer) :: t_do_nothing_observer
   contains
      procedure :: begin_timeloop => observer_do_nothing
      procedure :: after_timestep => observer_do_nothing
   end type t_do_nothing_observer

   type, extends(t_timeloop_observer) :: t_observer_collection
      class(t_observer_reference), dimension(:), allocatable :: observer_references
   contains
      procedure :: begin_timeloop => observer_collection_begin_timeloop
      procedure :: after_timestep => observer_collection_after_timestep
   end type t_observer_collection

   type :: t_observer_reference
      class(t_timeloop_observer), allocatable :: observer
   end type t_observer_reference
contains
   subroutine printing_observer_print_timestep_and_data(self, field, timestep)
      class(t_printing_observer), intent(in) :: self
      class(t_field), intent(in) :: field
      integer, intent(in) :: timestep

      print print_format, timestep, field%data
   end subroutine printing_observer_print_timestep_and_data

   subroutine observer_do_nothing(self, field, timestep)
      class(t_do_nothing_observer), intent(in) :: self
      class(t_field), intent(in) :: field
      integer, intent(in) :: timestep

      print *, 'Doing nothing'
      ! do nothing
   end subroutine observer_do_nothing

   subroutine observer_collection_begin_timeloop(self, field, timestep)
      class(t_observer_collection), intent(in) :: self
      class(t_field), intent(in) :: field
      integer, intent(in) :: timestep

      integer :: i

      do i = 1, size(self%observer_references)
         call self%observer_references(i)%observer%begin_timeloop(field, timestep)
      end do
   end subroutine observer_collection_begin_timeloop

   subroutine observer_collection_after_timestep(self, field, timestep)
      class(t_observer_collection), intent(in) :: self
      class(t_field), intent(in) :: field
      integer, intent(in) :: timestep

      integer :: i

      do i = 1, size(self%observer_references)
         call self%observer_references(i)%observer%after_timestep(field, timestep)
      end do
   end subroutine observer_collection_after_timestep

end module m_observer
