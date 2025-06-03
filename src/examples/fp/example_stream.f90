module stream
   implicit none(type, external)

   public

   type, abstract :: integer_stream_t
      integer, allocatable, private :: current_value
   contains
      procedure(integer_stream_current_interface), deferred :: current
      procedure(integer_stream_next_interface), deferred :: next
      procedure(integer_stream_is_end_of_stream_interface), deferred :: is_end_of_stream
   end type integer_stream_t

   type, extends(integer_stream_t) :: integer_range_t
      integer :: from
      integer :: to
   contains
      procedure :: current => integer_range_current
      procedure :: next => integer_range_next
      procedure :: is_end_of_stream => integer_range_is_end_of_stream
   end type integer_range_t

   type, abstract, extends(integer_stream_t) :: filter_integers_t
      class(integer_stream_t), allocatable :: integers
   contains
      procedure :: current => filter_integers_current
      procedure :: next => filter_integers_next
      procedure :: is_end_of_stream => filter_integers_is_end_of_stream
      procedure(filter_integer_interface), deferred :: matches
   end type filter_integers_t

   type, extends(filter_integers_t) :: even_integers_t
   contains
      procedure :: matches => matches_even_integer
   end type even_integers_t

   type, extends(filter_integers_t) :: odd_integers_t
   contains
      procedure :: matches => matches_odd_integer
   end type odd_integers_t

   interface integer_range_t
      module procedure :: range
   end interface integer_range_t

   interface even_integers_t
      module procedure :: even_integers
   end interface even_integers_t

   interface odd_integers_t
      module procedure :: odd_integers
   end interface odd_integers_t

   interface
      function integer_stream_current_interface(self) result(int_value)
         import integer_stream_t
         implicit none(type, external)
         class(integer_stream_t), intent(inout) :: self
         integer :: int_value
      end function integer_stream_current_interface

      subroutine integer_stream_next_interface(self)
         import integer_stream_t
         implicit none(type, external)
         class(integer_stream_t), intent(inout) :: self
      end subroutine integer_stream_next_interface

      function integer_stream_is_end_of_stream_interface(self) result(is_end_of_stream)
         import integer_stream_t
         implicit none(type, external)
         class(integer_stream_t), intent(inout) :: self
         logical :: is_end_of_stream
      end function integer_stream_is_end_of_stream_interface

      function filter_integer_interface(self, value) result(match)
         import filter_integers_t
         implicit none(type, external)
         class(filter_integers_t), intent(in) :: self

         integer, intent(in) :: value
         logical :: match
      end function filter_integer_interface
   end interface

contains

   function integer_range_current(self) result(int_value)
      class(integer_range_t), intent(inout) :: self
      integer :: int_value

      int_value = self%current_value
   end function integer_range_current

   subroutine integer_range_next(self)
      class(integer_range_t), intent(inout) :: self
      if (.not. allocated(self%current_value)) then
         self%current_value = self%from
      else
         self%current_value = self%current_value + 1
      end if
   end subroutine integer_range_next

   function integer_range_is_end_of_stream(self) result(is_end_of_stream)
      class(integer_range_t), intent(inout) :: self
      logical :: is_end_of_stream

      is_end_of_stream = self%current_value > self%to
   end function integer_range_is_end_of_stream

   function filter_integers_current(self) result(even_integer)
      class(filter_integers_t), intent(inout) :: self
      integer :: even_integer

      even_integer = self%integers%current_value
   end function filter_integers_current

   subroutine filter_integers_next(self)
      class(filter_integers_t), intent(inout) :: self

      do
         call self%integers%next()
         if (self%matches(self%integers%current())) then
            exit
         end if
         if (self%integers%is_end_of_stream()) then
            exit
         end if
      end do
   end subroutine filter_integers_next

   function filter_integers_is_end_of_stream(self) result(is_end_of_stream)
      class(filter_integers_t), intent(inout) :: self
      logical :: is_end_of_stream

      is_end_of_stream = self%integers%is_end_of_stream()
   end function filter_integers_is_end_of_stream

   pure function range(from, to) result(res)
      integer, intent(in) :: from
      integer, intent(in) :: to
      type(integer_range_t) :: res

      res%from = from
      res%to = to
   end function range

   pure function even_integers(integers) result(res)
      class(integer_stream_t), intent(in) :: integers
      type(even_integers_t) :: res

      res%integers = integers
   end function even_integers

   pure function odd_integers(integers) result(res)
      class(integer_stream_t), intent(in) :: integers
      type(odd_integers_t) :: res

      res%integers = integers
   end function odd_integers

   pure function matches_even_integer(self, value) result(match)
      class(even_integers_t), intent(in) :: self
      integer, intent(in) :: value
      logical :: match

      match = mod(value, 2) == 0
   end function matches_even_integer

   pure function matches_odd_integer(self, value) result(match)
      class(odd_integers_t), intent(in) :: self
      integer, intent(in) :: value
      logical :: match

      match = mod(value, 2) == 1
   end function matches_odd_integer
end module stream

program example
   use stream, only: integer_range_t, even_integers_t, odd_integers_t
   use benchmark_facade, only: start_benchmark, stop_benchmark, print_benchmark_results

   implicit none(type, external)

   type(integer_range_t), allocatable :: range
   type(even_integers_t), allocatable :: even_integers
   type(odd_integers_t), allocatable :: odd_integers

   integer :: max

   max = 10000

   range = integer_range_t(1, max)
   even_integers = even_integers_t(integer_range_t(1, max))
   odd_integers = odd_integers_t(integer_range_t(1, max))

   call start_benchmark('Range')
   print *, 'Range'
   do
      call range%next()
      if (range%is_end_of_stream()) then
         exit
      end if
      print *, range%current()
   end do
   call stop_benchmark('Range')

   print *, 'Even numbers'
   call start_benchmark('Even numbers')
   do
      call even_integers%next()
      if (even_integers%is_end_of_stream()) then
         exit
      end if
      print *, even_integers%current()
   end do
   call stop_benchmark('Even numbers')

   print *, 'Odd numbers'
   call start_benchmark('Odd numbers')
   do
      call odd_integers%next()
      if (odd_integers%is_end_of_stream()) then
         exit
      end if
      print *, odd_integers%current()
   end do
   call stop_benchmark('Odd numbers')

   call print_benchmark_results()
end program example
