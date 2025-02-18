program error_handling
   use m_error, only: t_error, t_abstract_error
   use m_files, only: file_get_contents
   use m_to_string, only: to_string

   implicit none

   call test_t_error()

   call test_file_error()

   call test_file_read()

contains
   subroutine test_t_error()
      type(t_error) :: my_error

      my_error = t_error("Help in "//__FILE__//' on line '//to_string(__LINE__))
      print *, my_error%get_message()
   end subroutine test_t_error

   subroutine test_file_error()
      class(t_abstract_error), allocatable :: file_error
      character(len=:), allocatable :: contents

      call file_get_contents('nofile.txt', contents, file_error)
      if (allocated(file_error)) then
         print *, file_error%get_message()
      end if
   end subroutine test_file_error

   subroutine test_file_read()
      class(t_abstract_error), allocatable :: file_error
      character(len=:), allocatable :: contents
      character(len=:), allocatable :: path

      path = 'file.txt'

      call file_get_contents(path, contents, file_error)

      if (allocated(file_error)) then
         print *, "We should not have received an error, but we got ", file_error%get_message()
      else
         print *, 'Contents of '//path//' is '//contents
      end if

   end subroutine test_file_read

end program error_handling
