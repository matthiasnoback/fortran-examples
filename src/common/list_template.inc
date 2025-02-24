! list_template.inc
type :: list_node
   type(value_type) :: value
   type(list_node), pointer :: next => null()
end type list_node

type :: list_type
   type(list_node), pointer :: head => null()
contains
   procedure :: add => list_add
   procedure :: get => list_get
   procedure :: remove => list_remove
   procedure :: foreach => list_foreach
end type list_type

abstract interface
    subroutine foreach_procedure(value)
        import :: value_type
        type(value_type), intent(inout) :: value
    end subroutine foreach_procedure
end interface

contains

subroutine list_add(this, value)
   class(list_type), intent(inout) :: this
   type(value_type), intent(in) :: value
   type(list_node), pointer :: new_node

   allocate (new_node)
   new_node%value = value
   new_node%next => this%head
   this%head => new_node
end subroutine list_add

function list_get(this, index, found) result(value)
   class(list_type), intent(in) :: this
   integer, intent(in) :: index
   logical, intent(out) :: found
   type(value_type) :: value
   type(list_node), pointer :: current
   integer :: i

   found = .false.
   current => this%head
   do i = 1, index
      if (.not. associated(current)) return
      current => current%next
   end do

   if (associated(current)) then
      value = current%value
      found = .true.
   end if
end function list_get

subroutine list_remove(this, index)
   class(list_type), intent(inout) :: this
   integer, intent(in) :: index
   type(list_node), pointer :: current, previous
   integer :: i

   if (index == 1) then
      if (associated(this%head)) then
         current => this%head
         this%head => this%head%next
         deallocate (current)
      end if
      return
   end if

   previous => null()
   current => this%head
   do i = 1, index - 1
      if (.not. associated(current)) return
      previous => current
      current => current%next
   end do

   if (associated(current)) then
      previous%next => current%next
      deallocate (current)
   end if
end subroutine list_remove

subroutine list_foreach(this, proc)
    class(list_type), intent(in) :: this
    procedure(foreach_procedure), pointer, intent(in) :: proc

    type(list_node), pointer :: current

    current => this%head
    do while (associated(current))
        call proc(current%value)
        current => current%next
    end do
end subroutine list_foreach
