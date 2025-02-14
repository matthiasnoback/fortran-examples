module test_tsunami
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use m_diff, only: diff_upwind
   use precision, only: dp
   implicit none
   private

   public :: collect_tsunami_tests

contains

   !> Collect all exported unit tests
   subroutine collect_tsunami_tests(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [new_unittest("test_diff", test_diff_upwind), &
                   new_unittest( &
                   "test_check_empty_1d_arrays_are_equal", &
                   test_check_empty_1d_arrays_are_equal &
                   ), &
                   new_unittest( &
                   "test_check_one_element_1d_arrays_are_equal", &
                   test_check_one_element_1d_arrays_are_equal &
                   ), &
                   new_unittest( &
                   "test_check_1d_arrays_size_is_not_equal", &
                   test_check_1d_arrays_size_is_not_equal, &
                   .true. &
                   ), &
                   new_unittest( &
                   "test_check_1d_arrays_values_are_not_equal", &
                   test_check_1d_arrays_values_are_not_equal, &
                   .true. &
                   ) &
                   ]

   end subroutine collect_tsunami_tests

   subroutine test_diff_upwind(error)
      type(error_type), allocatable, intent(out) :: error

      call check_1d_arrays_are_equal([-3.0_dp, 1.0_dp, 2.0_dp], &
                                     diff_upwind([1.0_dp, 2.0_dp, 4.0_dp]), error)
   end subroutine test_diff_upwind

   subroutine test_check_empty_1d_arrays_are_equal(error)
      type(error_type), allocatable, intent(out) :: error

      real(kind=dp), dimension(0) :: no_elements_array

      type(error_type), allocatable :: sub_test_error

      call check_1d_arrays_are_equal(no_elements_array, no_elements_array, sub_test_error)
      if (allocated(sub_test_error)) then
         call test_failed(error, "Expected empty arrays to be equal")
         return
      end if
   end subroutine test_check_empty_1d_arrays_are_equal

   subroutine test_check_one_element_1d_arrays_are_equal(error)
      type(error_type), allocatable, intent(out) :: error

      real(kind=dp), dimension(1) :: single_element_array

      type(error_type), allocatable :: sub_test_error

      call check_1d_arrays_are_equal(single_element_array, single_element_array, sub_test_error)
      if (allocated(sub_test_error)) then
         call test_failed(error, "Expected single-element arrays to be equal")
         return
      end if
   end subroutine test_check_one_element_1d_arrays_are_equal

   subroutine test_check_1d_arrays_size_is_not_equal(error)
      type(error_type), allocatable, intent(out) :: error
      real(kind=dp), dimension(0) :: no_elements_array
      call check_1d_arrays_are_equal([1.0_dp], no_elements_array, error)
   end subroutine test_check_1d_arrays_size_is_not_equal

   subroutine test_check_1d_arrays_values_are_not_equal(error)
      type(error_type), allocatable, intent(out) :: error
      call check_1d_arrays_are_equal([1.0_dp], [2.0_dp], error)
   end subroutine test_check_1d_arrays_values_are_not_equal

   subroutine check_1d_arrays_are_equal(expected, actual, error)
      real(kind=dp), dimension(:), intent(in) :: expected
      real(kind=dp), dimension(:), intent(in) :: actual
      real(kind=dp), parameter :: epsilon = 0.000001_dp

      type(error_type), allocatable, intent(out) :: error
      integer :: i
      character(len=10) :: i_as_string

      call check(error, size(expected) == size(actual), 'Array size is not equal')
      if (allocated(error)) return

      do i = 1, size(expected)
         if (abs(expected(i) - actual(i)) > epsilon) then
            write (i_as_string, '(i0)') i
            call test_failed(error, "Difference between values at index "//trim(i_as_string) &
                             //" is more than allowed")
         end if
      end do
   end subroutine check_1d_arrays_are_equal

end module test_tsunami
