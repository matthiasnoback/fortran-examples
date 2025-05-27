module error_handling_options
   use, intrinsic :: iso_fortran_env, only: wp => real64

   implicit none(type, external)

   public

   integer, parameter :: ERROR_NO_ERROR = 0
   integer, parameter :: ERROR_AVERAGE_NO_NUMBERS = 1

contains

   function average_naive(numbers) result(res)
      real(kind=wp), dimension(:), intent(in) :: numbers
      real(kind=wp) :: res

      res = sum(numbers) / size(numbers)
   end function average_naive

   function average_default_0(numbers) result(res)
      real(kind=wp), dimension(:), intent(in) :: numbers
      real(kind=wp) :: res

      if (size(numbers) == 0) then
         res = 0.0_wp
      else
         res = sum(numbers) / size(numbers)
      end if
   end function average_default_0

   function average_with_success_result(numbers, average) result(success)
      real(kind=wp), dimension(:), intent(in) :: numbers
      real(kind=wp), intent(out) :: average
      logical :: success

      if (size(numbers) == 0) then
         success = .false.
      else
         success = .true.
         average = sum(numbers) / size(numbers)
      end if
   end function average_with_success_result

   function average_with_error_result(numbers, average) result(error)
      real(kind=wp), dimension(:), intent(in) :: numbers
      real(kind=wp), intent(out) :: average
      integer :: error

      if (size(numbers) == 0) then
         error = ERROR_AVERAGE_NO_NUMBERS
      else
         error = ERROR_NO_ERROR
         average = sum(numbers) / size(numbers)
      end if
   end function average_with_error_result

   function average_error_stop(numbers) result(res)
      real(kind=wp), dimension(:), intent(in) :: numbers
      real(kind=wp) :: res

      if (size(numbers) == 0) then
         error stop 'Error stop: No numbers provided for average calculation.'
      else
         res = sum(numbers) / size(numbers)
      end if
   end function average_error_stop

end module error_handling_options

program example
   use, intrinsic :: iso_fortran_env, only: wp => real64

   use error_handling_options, only: average_error_stop, average_default_0, &
                                     average_with_success_result, average_naive, &
                                     average_with_error_result, &
                                     ERROR_AVERAGE_NO_NUMBERS, ERROR_NO_ERROR
   implicit none(type, external)

   real(kind=wp), dimension(:), allocatable :: numbers
   real(kind=wp) :: average
   logical :: success
   integer :: ierr

   ! Example usage of the average function with error handling
   allocate (numbers(0))  ! Allocating an empty array to trigger the error

   average = average_default_0(numbers)
   print *, 'Average with default 0.0:', average

   success = average_with_success_result(numbers, average)
   if (.not. success) then
      print *, 'Error: No numbers provided for average calculation.'
   else
      print *, 'Average with success result:', average
   end if

   ierr = average_with_error_result(numbers, average)
   if (ierr == ERROR_NO_ERROR) then
      print *, 'Average with success result:', average
   else if (ierr == ERROR_AVERAGE_NO_NUMBERS) then
      print *, 'Error: No numbers provided for average calculation.'
   end if

   average = average_naive(numbers)
   print *, 'Average naive:', average

   if (size(numbers) == 0) then
      print *, 'We should not calculate the average.'
   else
      average = average_naive(numbers)
      print *, 'Average:', average
   end if

   ! This will stop execution with an error message if numbers is empty
   average = average_error_stop(numbers)

end program example
