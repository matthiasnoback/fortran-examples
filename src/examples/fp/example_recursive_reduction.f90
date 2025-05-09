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
      else
         res = reduce(func, numbers(2:), func(numbers(1), start))
      end if
   end function reduce

   pure function sum(number, carry) result(new_carry)
      integer, intent(in) :: number
      integer, intent(in) :: carry
      integer :: new_carry
      new_carry = number + carry
   end function sum

end module recursivity

program example
   use recursivity, only: reduce, sum

   implicit none(type, external)

   integer, dimension(:), allocatable :: numbers
   integer :: i

   numbers = [(i, i=1, 100)]

   print *, 'Sum: ', reduce(sum, numbers, 0)

end program example
