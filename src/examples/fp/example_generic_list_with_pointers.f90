module lists
   use iso_fortran_env, only: wp => real64

   implicit none(type, external)

   public

   type :: int_list_t
      integer, dimension(:), allocatable :: values
   contains
      procedure :: filter => int_list_filter
      procedure, private :: int_list_map_to_int
      procedure, private :: int_list_map_to_real
      procedure, private :: int_list_curried_map_to_real
      generic :: map => int_list_map_to_int, int_list_map_to_real, int_list_curried_map_to_real
      procedure, private :: int_list_write
      generic :: write (formatted) => int_list_write
   end type int_list_t

   type :: real_list_t
      real, dimension(:), allocatable :: values
   contains
      procedure, private :: real_list_write
      generic :: write (formatted) => real_list_write
   end type real_list_t

   interface list
      procedure :: list_int
      procedure :: list_real
   end interface

   type, abstract :: int_to_real_map_function_t
   contains
      procedure(map_int_to_real_evaluate), deferred :: evaluate
   end type int_to_real_map_function_t

   type, extends(int_to_real_map_function_t) :: divide_by_t
      real :: divisor
   contains
      procedure :: evaluate => divide_by_evaluate
   end type divide_by_t

   interface
      pure function map_int_to_real_evaluate(self, old_value) result(new_value)
         import int_to_real_map_function_t

         class(int_to_real_map_function_t), intent(in) :: self
         integer, intent(in) :: old_value
         real :: new_value
      end function map_int_to_real_evaluate
   end interface

contains
   pure function divide_by_evaluate(self, old_value) result(new_value)
      class(divide_by_t), intent(in) :: self
      integer, intent(in) :: old_value
      real :: new_value

      new_value = real(old_value) / self%divisor
   end function divide_by_evaluate

   pure function list_int(values) result(res)
      integer, dimension(:), intent(in) :: values
      type(int_list_t), pointer :: res

      allocate (res)
      res%values = values
   end function list_int

   pure function list_real(values) result(res)
      real, dimension(:), intent(in) :: values
      type(real_list_t), pointer :: res

      allocate (res)
      res%values = values
   end function list_real

   pure function int_list_filter(self, int_filter_func) result(res)
      class(int_list_t), intent(in) :: self
      type(int_list_t) :: res

      interface
         pure function int_filter_func(value) result(keep)
            integer, intent(in) :: value
            logical :: keep
         end function int_filter_func
      end interface

      integer :: i

      res%values = pack(self%values, [(int_filter_func(self%values(i)), i=1, size(self%values))])
   end function int_list_filter

   pure function int_list_map_to_int(self, int_to_int_map_func) result(res)
      class(int_list_t), intent(in) :: self
      type(int_list_t), pointer :: res

      interface
         pure function int_to_int_map_func(old_value) result(new_value)
            integer, intent(in) :: old_value
            integer :: new_value
         end function int_to_int_map_func
      end interface

      integer :: i

      allocate (res)
      allocate (res%values(size(self%values)))

      do i = 1, size(self%values)
         res%values(i) = int_to_int_map_func(self%values(i))
      end do
   end function int_list_map_to_int

   pure function int_list_map_to_real(self, int_to_real_map_func) result(res)
      class(int_list_t), intent(in) :: self
      type(real_list_t), pointer :: res

      interface
         pure function int_to_real_map_func(old_value) result(new_value)
            integer, intent(in) :: old_value
            real :: new_value
         end function int_to_real_map_func
      end interface

      integer :: i

      allocate (res)
      allocate (res%values(size(self%values)))

      do i = 1, size(self%values)
         res%values(i) = int_to_real_map_func(self%values(i))
      end do
   end function int_list_map_to_real

   pure function int_list_curried_map_to_real(self, int_to_real_map_function) result(res)
      class(int_list_t), intent(in) :: self
      class(int_to_real_map_function_t), intent(in) :: int_to_real_map_function
      type(real_list_t), pointer :: res

      integer :: i

      allocate (res)
      allocate (res%values(size(self%values)))

      do i = 1, size(self%values)
         res%values(i) = int_to_real_map_function%evaluate(self%values(i))
      end do
   end function int_list_curried_map_to_real

   subroutine int_list_write(self, unit, iotype, v_list, iostat, iomsg)
      class(int_list_t), intent(in) :: self
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      iostat = 0
      write (unit, *) self%values
   end subroutine int_list_write

   subroutine real_list_write(self, unit, iotype, v_list, iostat, iomsg)
      class(real_list_t), intent(in) :: self
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      iostat = 0
      write (unit, *) self%values
   end subroutine real_list_write
end module lists

program example
   use lists, only: list, int_list_t, real_list_t, divide_by_t

   implicit none(type, external)

   type(int_list_t), allocatable :: integers
   type(int_list_t), allocatable :: doubled
   type(real_list_t), allocatable :: one_thirds
   type(real_list_t), allocatable :: quarters

   type(int_list_t), pointer :: result

   integers = list([1, 2, 3, 4])
   print *, integers

   integers = list([1, 2, 3, 4])%filter(is_even)
   print *, integers

   doubled = list([1, 2, 3, 4])%map(double)
   print *, doubled

   result => list([1, 2, 3, 4])
   result => result%map(double)
   print *, result%map(one_third)

   quarters = integers%map(divide_by_t(4))
   print *, quarters
contains
   pure function is_even(value) result(keep)
      integer, intent(in) :: value
      logical :: keep

      keep = mod(value, 2) == 0
   end function is_even

   pure function double(old_value) result(new_value)
      integer, intent(in) :: old_value
      integer :: new_value

      new_value = old_value * 2
   end function double

   pure function one_third(old_value) result(new_value)
      integer, intent(in) :: old_value
      real :: new_value

      new_value = real(old_value) / 3
   end function one_third
end program example

! Assignments:
!
! 1. Create a DT that represents a list of integers. Add a function to it, to get a new list with only the even numbers
! 2. Extract the check to its own function, and allow passing that function as a procedure pointer to the filter function.
! 3. Add a double() function to the list, that returns a new list but with every number doubled.
! 4. Extract the transformation function and modify the map() function to allow any "int to int"
!    transformation function to be passed to it.
! 5. Add a divide_by_3() function to the list, that returns another type of list, a "real" list, with all the original numbers,
!    but divided by 3.
! 6. Make the map() function generic, so we can also pass a mapping function to it that transforms an int to a real, returning a
!    list of reals.
! Problem: how can we pass another value to the map function? E.g. divide by 3, 4, 5, etc.
! 7. Generalize: add another map implementation that accepts an abstract map_int_to_real DT that has an evaluate function on it.
! 8. Specialize: add a subtype of map_int_to_real DT that we can pass a divisor to, so we can modify the logic dynamically.
! 9. Add several examples, of dividing by 3, 4, 5, etc.
! 10. Let's keep the values inside, when printing, we can just print the whole list,
!     by defining a generic write(formatted) procedure.
