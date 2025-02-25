! my_tests.f90
module my_tests_mod
   use iso_c_binding, only: c_int
   use m_hello, only: hello, anyone
   use fassert, only: assert_equal
   implicit none
contains

   integer(c_int) function my_test_success() result(ret) bind(C)

      ret = 0
   end function my_test_success

   integer(c_int) function test_hello_anyone() result(ret) bind(C)
      call assert_equal(0 - 1, 1, 'Expected integers to be the same')
      ret = 0
   end function test_hello_anyone

   integer(c_int) function my_test_fail() result(ret) bind(C)
      ret = 1
   end function my_test_fail

end module my_tests_mod
