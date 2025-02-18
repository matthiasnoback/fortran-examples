module m_to_string
   implicit none

   interface to_string
      module procedure :: int_to_string
   end interface to_string
contains

   pure function int_to_string(integer_value) result(res)
      integer, intent(in) :: integer_value
      character(len=:), allocatable :: res
      character(len=255) :: temp

      write (temp, '(I0)') integer_value
      res = trim(temp)
   end function int_to_string

end module m_to_string
