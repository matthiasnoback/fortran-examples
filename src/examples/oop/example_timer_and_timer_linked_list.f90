module stopwatch_timer
   use iso_fortran_env, only: wp => real64, int64

   implicit none(type, external)

   private
   public :: timer_t

   type :: timer_t
      character(len=:), allocatable :: label
      integer(int64) :: system_clock_start_time
      integer(int64), allocatable :: system_clock_end_time
   contains
      procedure :: print => timer_print
      procedure :: end => timer_end
   end type timer_t

   interface timer_t
      module procedure :: timer_constructor
   end interface

contains
   function timer_constructor(label) result(timer)
      character(len=*), intent(in) :: label
      type(timer_t) :: timer

      integer(kind=int64) :: start_time

      call system_clock(start_time)

      timer = timer_t(label, start_time)
   end function timer_constructor

   subroutine timer_print(self)
      class(timer_t), intent(inout) :: self

      if (allocated(self%system_clock_end_time)) then
         print *, self%label, time_difference_in_seconds( &
            self%system_clock_start_time, &
            self%system_clock_end_time &
            )
      else
         print *, self%label//' (only started)'
      end if
   end subroutine timer_print

   subroutine timer_end(self)
      class(timer_t), intent(inout) :: self

      integer(kind=int64) :: end_time

      call system_clock(end_time)

      self%system_clock_end_time = end_time
   end subroutine timer_end

   function time_difference_in_seconds(start_time, end_time) result(difference)
      integer(kind=int64), intent(in) :: start_time
      integer(kind=int64), intent(in) :: end_time
      real(kind=wp) :: difference

      real(kind=int64) :: clock_rate

      call system_clock(count_rate=clock_rate)  ! # of clock ticks per second

      difference = (end_time - start_time)/real(clock_rate, kind=wp)
   end function time_difference_in_seconds

end module stopwatch_timer

module stopwatch_timer_list
   use stopwatch_timer, only: timer_t

   implicit none(type, external)

   private
   public :: timer_list_t

   type :: timer_list_node_t
      type(timer_t) :: timer
      type(timer_list_node_t), pointer :: next => null()
   end type timer_list_node_t

   type :: timer_list_t
      type(timer_list_node_t), pointer :: head => null()
   contains
      procedure :: add => timer_list_add
      procedure :: get => timer_list_get
      procedure :: print_all => timer_list_print_all
   end type timer_list_t

contains

   subroutine timer_list_add(self, timer)
      class(timer_list_t), intent(inout) :: self
      type(timer_list_node_t), pointer :: new_node
      type(timer_t), intent(in) :: timer

      if (.not. associated(self%head)) then
         allocate (self%head)
         self%head%timer = timer
      else
         allocate (new_node)
         new_node%timer = timer
         new_node%next => self%head
         self%head => new_node
      end if
   end subroutine timer_list_add

   function timer_list_get(self, label) result(timer)
      class(timer_list_t), intent(inout), target :: self
      character(len=*), intent(in) :: label
      type(timer_t), pointer :: timer

      type(timer_list_node_t), pointer :: current

      current => self%head
      do while (associated(current))
         if (current%timer%label == label) then
            timer => current%timer
            return
         end if
         current => current%next
      end do

      timer => null()
   end function timer_list_get

   subroutine timer_list_print_all(self)
      class(timer_list_t), intent(inout) :: self

      type(timer_list_node_t), pointer :: current

      current => self%head
      do while (associated(current))
         call current%timer%print()
         current => current%next
      end do
   end subroutine timer_list_print_all

end module stopwatch_timer_list

module stopwatch_facade
   implicit none(type, external)

   private
   public :: stopwatch_start, stopwatch_end, stopwatch_print_timers

   interface
      module subroutine stopwatch_start(label)
         implicit none(type, external)

         character(len=*), intent(in) :: label
      end subroutine stopwatch_start

      module subroutine stopwatch_end(label)
         implicit none(type, external)

         character(len=*), intent(in) :: label
      end subroutine stopwatch_end

      module subroutine stopwatch_print_timers()
         implicit none(type, external)

      end subroutine stopwatch_print_timers
   end interface
end module stopwatch_facade

submodule(stopwatch_facade) stopwatch_facade_implementation
   use stopwatch_timer_list, only: timer_list_t
   use stopwatch_timer, only: timer_t

   implicit none(type, external)

   type(timer_list_t), allocatable :: timer_list

contains

   module subroutine stopwatch_start(label)
      character(len=*), intent(in) :: label

      if (.not. allocated(timer_list)) then
         allocate (timer_list)
      end if

      call timer_list%add(timer_t(label))
   end subroutine stopwatch_start

   module subroutine stopwatch_end(label)
      character(len=*), intent(in) :: label

      type(timer_t), pointer :: found_timer

      found_timer => timer_list%get(label)
      if (associated(found_timer)) then
         call found_timer%end()
      end if
   end subroutine stopwatch_end

   module subroutine stopwatch_print_timers()
      if (allocated(timer_list)) then
         call timer_list%print_all()
      end if
   end subroutine stopwatch_print_timers

end submodule stopwatch_facade_implementation

program example
   use stopwatch_facade, only: stopwatch_start, stopwatch_end, stopwatch_print_timers

   implicit none(type, external)

   integer, parameter :: m = 10000, n = m
   integer :: array(m, n)
   integer :: i, j, tot

   array = 0

   ! FASTER: Looping consecutively through columns
   call stopwatch_start('Array loop column-first')
   tot = 0
   do j = 1, size(array, dim=2)
      do i = 1, size(array, dim=1)
         tot = tot + array(i, j)
      end do
   end do
   call stopwatch_end('Array loop column-first')

   ! SLOWER: Looping consecutively through rows
   call stopwatch_start('Array loop row-first')

   tot = 0
   do i = 1, size(array, dim=1)
      do j = 1, size(array, dim=2)
         tot = tot + array(i, j)
      end do
   end do
   call stopwatch_end('Array loop row-first')

   call stopwatch_print_timers()
end program example
