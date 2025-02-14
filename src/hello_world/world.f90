module m_world
   use m_hello, only: person

   implicit none

   type, extends(person) :: world
   contains
      procedure :: get_name => world_get_name
   end type world

contains
   pure function world_get_name(this) result(res)
      character(:), allocatable :: res
      class(world), intent(in) :: this
      res = 'world'
   end function world_get_name

end module m_world
