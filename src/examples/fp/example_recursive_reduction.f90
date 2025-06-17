module recursivity
   implicit none(type, external)

   public

contains

   pure recursive function reduce(func, numbers, start) result(res)
      interface
         pure function func(number, carry) result(new_carry)
            implicit none(type, external)

            integer, intent(in) :: number
            integer, intent(in) :: carry
            integer :: new_carry
         end function func
      end interface

      integer, dimension(:), intent(in) :: numbers
      integer, intent(in) :: start
      integer :: res

      if (size(numbers) == 0) then
         res = start
         return
      end if

      res = reduce(func, numbers(2:), func(numbers(1), start))
   end function reduce

   function reduce_with_pointer(func, numbers, start) result(res)
      interface
         pure function func(number, carry) result(new_carry)
            implicit none(type, external)

            integer, intent(in) :: number
            integer, intent(in) :: carry
            integer :: new_carry
         end function func
      end interface

      integer, dimension(:), intent(in), target :: numbers
      integer, intent(in) :: start
      integer, dimension(:), pointer :: temp
      integer :: res

      res = start
      temp => numbers

      do while (size(temp) > 0)
         res = func(temp(1), res)
         temp => temp(2:)
      end do

   end function reduce_with_pointer

   pure function sum(number, carry) result(new_carry)
      integer, intent(in) :: number
      integer, intent(in) :: carry
      integer :: new_carry
      new_carry = number + carry
   end function sum

end module recursivity

program example
   use recursivity, only: reduce, sum, reduce_with_pointer

   implicit none(type, external)

   integer, dimension(:), allocatable :: numbers
   integer :: i

   numbers = [(i, i=1, 10)]

   print *, 'Sum: ', reduce(sum, numbers, 0)
   print *, 'Sum: ', reduce_with_pointer(sum, numbers, 0)

end program example
