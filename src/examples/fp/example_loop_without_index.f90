module recursion
   implicit none(type, external)
   public

contains

   subroutine print_numbers(numbers)
      integer, dimension(:), intent(in) :: numbers
      integer :: i

      do i = 1, size(numbers)
         print *, numbers(i)
      end do
   end subroutine print_numbers

   recursive subroutine print_numbers_recursively(numbers)
      integer, dimension(:), intent(in) :: numbers

      if (size(numbers) == 0) then
         return
      end if

      print *, numbers(1)
      call print_numbers_recursively(numbers(2:))
   end subroutine print_numbers_recursively
end module recursion

program example
   use recursion, only: print_numbers, print_numbers_recursively

   implicit none(type, external)

   integer, dimension(:), allocatable :: numbers
   integer :: i

   numbers = [1, 2, 3]

   do i = 1, size(numbers)
      print *, numbers(i)
   end do

   call print_numbers(numbers)

   call print_numbers_recursively(numbers)

end program example
