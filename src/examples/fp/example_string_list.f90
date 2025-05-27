module strings
   implicit none(type, external)

   private
   public :: string_t, string_list_t

   type :: string_t
      character(len=:), allocatable :: value
   end type string_t

   type :: string_list_t
      type(string_t), dimension(:), allocatable :: strings
   contains
      procedure, private :: map_to_integer_array
      generic :: map => map_to_string_lengths
   end type string_list_t
end module strings

program example
   use strings, only: string_list_t, string_t
   implicit none(type, external)

   type(string_list_t) :: my_strings
   integer :: i

   my_strings = string_list_t([string_t('1.0 2.567546'), string_t('3.0 4.2345')])
   do i = 1, size(my_strings%strings)
      print *, my_strings%strings(i)%value
   end do
end program example
