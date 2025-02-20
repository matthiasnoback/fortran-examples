program error_handling
   use m_error, only: t_error, t_abstract_error, t_error_location
   use m_files, only: file_get_contents, t_file_error
   use m_to_string, only: to_string

   implicit none

   call test_t_error()

   call test_t_error_with_no_file_and_line()

   call test_file_error()

   call test_file_read()

   call test_nested_error()

contains
   subroutine test_t_error()
      type(t_error) :: my_error

      my_error = t_error('Help', t_error_location(__FILE__, __LINE__))
      print *, my_error%to_string()
   end subroutine test_t_error

   subroutine test_t_error_with_no_file_and_line()
      type(t_error) :: my_error

      my_error = t_error('Help no file and line')
      print *, my_error%to_string()
   end subroutine test_t_error_with_no_file_and_line

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

   subroutine test_nested_error()
      type(t_file_error) :: previous_error
      type(t_error) :: current_error

      previous_error = t_file_error('Failed to open file', 'file.txt', 'internal error')
      current_error = t_error('Failed to read config', &
                              t_error_location(__FILE__, __LINE__), previous_error)

      print *, current_error%to_string()
   end subroutine test_nested_error

end program error_handling
