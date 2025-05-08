module lists
   use iso_fortran_env, only: wp => real64

   implicit none(type, external)

   public

   type :: int_list_t
      integer, dimension(:), allocatable :: values
   contains
      procedure, public :: filter => int_list_filter
      generic, public :: map => &
         int_list_map_to_int, &
         int_list_map_to_real, &
         int_list_curried_map_to_real
      generic, public :: write (formatted) => int_list_write
      procedure, private :: int_list_map_to_int
      procedure, private :: int_list_map_to_real
      procedure, private :: int_list_curried_map_to_real
      procedure, private :: int_list_write
      procedure, private :: int_list_reduce_to_int
      generic, public :: reduce => &
         int_list_reduce_to_int
      procedure, public :: average => int_list_average
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

         implicit none(type, external)

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
      type(int_list_t) :: res

      res%values = values
   end function list_int

   pure function list_real(values) result(res)
      real, dimension(:), intent(in) :: values
      type(real_list_t) :: res

      res%values = values
   end function list_real

   pure function int_list_filter(self, int_filter_func) result(res)
      class(int_list_t), intent(in) :: self
      type(int_list_t) :: res

      interface
         pure function int_filter_func(value) result(keep)
            implicit none(type, external)

            integer, intent(in) :: value
            logical :: keep
         end function int_filter_func
      end interface

      integer :: i

      res%values = pack(self%values, [(int_filter_func(self%values(i)), i=1, size(self%values))])
   end function int_list_filter

   pure function int_list_map_to_int(self, int_to_int_map_func) result(res)
      class(int_list_t), intent(in) :: self
      type(int_list_t) :: res

      interface
         pure function int_to_int_map_func(old_value) result(new_value)
            implicit none(type, external)

            integer, intent(in) :: old_value
            integer :: new_value
         end function int_to_int_map_func
      end interface

      integer :: i

      allocate (res%values(size(self%values)))

      do i = 1, size(self%values)
         res%values(i) = int_to_int_map_func(self%values(i))
      end do
   end function int_list_map_to_int

   pure function int_list_map_to_real(self, int_to_real_map_func) result(res)
      class(int_list_t), intent(in) :: self
      type(real_list_t) :: res

      interface
         pure function int_to_real_map_func(old_value) result(new_value)
            implicit none(type, external)

            integer, intent(in) :: old_value
            real :: new_value
         end function int_to_real_map_func
      end interface

      integer :: i

      allocate (res%values(size(self%values)))

      do i = 1, size(self%values)
         res%values(i) = int_to_real_map_func(self%values(i))
      end do
   end function int_list_map_to_real

   pure function int_list_curried_map_to_real(self, int_to_real_map_function) result(res)
      class(int_list_t), intent(in) :: self
      class(int_to_real_map_function_t), intent(in) :: int_to_real_map_function
      type(real_list_t) :: res

      integer :: i

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

   pure function int_list_reduce_to_int(self, int_to_int_reduction_function, initial_carry) result(res)
      class(int_list_t), intent(in) :: self
      integer, intent(in) :: initial_carry
      integer :: res

      interface
         pure function int_to_int_reduction_function(value, carry) result(new_carry)
            implicit none(type, external)

            integer, intent(in) :: value
            integer, intent(in) :: carry
            integer :: new_carry
         end function int_to_int_reduction_function
      end interface

      integer :: i

      res = initial_carry

      do i = 1, size(self%values)
         res = int_to_int_reduction_function(self%values(i), res)
      end do
   end function int_list_reduce_to_int

   pure function int_list_average(self) result(average)
      class(int_list_t), intent(in) :: self
      real :: average

      average = real(self%reduce(sum_integers, 0)) / real(size(self%values))
   end function int_list_average

   pure function sum_integers(value, carry) result(new_carry)
      integer, intent(in) :: value
      integer, intent(in) :: carry
      integer:: new_carry

      new_carry = value + carry
   end function sum_integers

   pure function maximum_integer(value, carry) result(new_carry)
      integer, intent(in) :: value
      integer, intent(in) :: carry
      integer:: new_carry

      if (value > carry) then
         new_carry = value
      else
         new_carry = carry
      end if
   end function maximum_integer

   pure function minimum_integer(value, carry) result(new_carry)
      integer, intent(in) :: value
      integer, intent(in) :: carry
      integer:: new_carry

      if (value < carry) then
         new_carry = value
      else
         new_carry = carry
      end if
   end function minimum_integer
end module lists

program example
   use lists, only: list, int_list_t, real_list_t, divide_by_t, &
                    maximum_integer, minimum_integer, sum_integers

   implicit none(type, external)

   type(int_list_t), allocatable :: integers
   type(int_list_t), allocatable :: doubled
   type(real_list_t), allocatable :: one_thirds
   type(real_list_t), allocatable :: quarters

   integers = list([1, 2, 3, 4])
   print *, integers

   integers = integers%filter(is_even)
   print *, 'Even integers:', integers
   print *, 'Sum:', integers%reduce(sum_integers, 0)
   print *, 'Max:', integers%reduce(maximum_integer, -huge(0))
   print *, 'Min:', integers%reduce(minimum_integer, huge(0))
   print *, 'Avg:', integers%average()

   doubled = integers%map(double)
   print *, 'Doubled even integers:', doubled

   one_thirds = doubled%map(one_third)
   print *, 'One-thirds of even integers:', one_thirds

   quarters = integers%map(divide_by_t(4))
   print *, 'Quarters of integers:', quarters
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

! Assignment 1: fold, total, max and min
!
! See example_generic_list_filter_and_map.f90
!
! Generalize the given sum() function:
! Define a function that receives a single value and the sum up to this point. Call this function instead
! Now allow this function to be passed to the function as an argument
! Rename the function to fold(): this means, from left to right in the array, build up a single value,
! based on the elements inside the array
!
! Try to implement another reduction function: max, and min
! Hint: for the initial carry you need the highest and the lowest integer value,use huge(0) or -huge(0) for that
!
! Assignment 2: average of list
!
! Add an average function to the list, which returns the average value of the list
!
! Problem: what if the list is empty?
!
! max, min, average, should reduce to an *optional* int or real
! Two ideas: you *have* to provide an element when creating an int_list
! Another idea: define two types: empty_int_list, non_empty_int_list; you can only get an average for a non-empty list.
! We can enforce non-emptyness, by passing the first element separately, then the rest of the elements (head/tail)
! If we want both lists to share the same API, then we still need to indicate that average may not give a correct answer.
! Same for max, min. It may return a value, or maybe not. We have to deal with that when using that value
