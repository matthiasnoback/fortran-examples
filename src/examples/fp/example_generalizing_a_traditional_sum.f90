module integers
   implicit none(type, external)

   public
contains
   pure function sum_function(carry, value) result(new_carry)
      integer, intent(in) :: carry
      integer, intent(in) :: value
      integer :: new_carry

      new_carry = carry + value
   end function sum_function

   pure function reduce_integers(numbers, reduction_function, initial_carry) result(reduced)
      integer, dimension(:), intent(in) :: numbers

      interface
         pure function reduction_function(carry, value) result(new_carry)
            implicit none(type, external)

            integer, intent(in) :: carry
            integer, intent(in) :: value
            integer :: new_carry
         end function reduction_function
      end interface

      integer, intent(in) :: initial_carry
      integer :: reduced

      integer :: i

      reduced = initial_carry
      do i = 1, size(numbers)
         reduced = reduction_function(reduced, numbers(i))
      end do
   end function reduce_integers
end module integers

program example
   use integers, only: sum_function, reduce_integers

   implicit none(type, external)

   integer :: i, sum, carry
   integer, dimension(:), allocatable :: numbers

   numbers = [1, 2, 3, 4]
   print *, numbers

   ! Traditional: do loop with an accumulator
   sum = 0
   do i = 1, size(numbers)
      sum = sum + numbers(i)
   end do
   print *, sum

   ! Extract the logic for deterining the next carry value from the do loop
   carry = 0
   do i = 1, size(numbers)
      carry = sum_function(carry, numbers(i))
   end do
   sum = carry
   print *, sum

   ! Generalize the loop function itself
   sum = reduce_integers(numbers, sum_function, 0)
   print *, sum

end program example
