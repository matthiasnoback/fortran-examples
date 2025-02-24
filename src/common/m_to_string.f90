module m_to_string
   use iso_fortran_env, only: real64

   implicit none

   interface to_string
      module procedure :: int_to_string
      module procedure :: real64_to_string
   end interface to_string
contains

   pure function int_to_string(integer_value) result(res)
      integer, intent(in) :: integer_value
      character(len=:), allocatable :: res
      character(len=255) :: temp

      write (temp, '(I0)') integer_value
      res = temp
   end function int_to_string

   pure function real64_to_string(real_value) result(res)
      real(kind=real64), intent(in) :: real_value
      character(len=:), allocatable :: res
      character(len=255) :: temp

      write (temp, '(g0.5)') real_value
      res = temp
   end function real64_to_string

end module m_to_string
