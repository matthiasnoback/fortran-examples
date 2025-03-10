module custom_operators
   use iso_fortran_env, only: wp => real64
   implicit none
   interface operator(.concat.)
      module procedure concat_real_string
   end interface
contains
   function concat_real_string(string_number, real_number) result(concatenated)
      character(len=*), intent(in) :: string_number
      real(kind=wp), intent(in) :: real_number
      character(len=100) :: concatenated
      character(len=20) :: real_string

      write (real_string, '(F6.2)') real_number  ! Convert real to string with formatting
      concatenated = trim(string_number)//trim(real_string)
   end function concat_real_string
end module custom_operators

program test_concatenation
   use iso_fortran_env, only: wp => real64
   use custom_operators, only: operator(.concat.)
   implicit none
   real(kind=wp) :: num
   character(len=20) :: str
   character(len=100) :: result

   num = 123.456_wp
   str = 'The number is: '
   result = str.concat.num

   print *, result
end program test_concatenation
