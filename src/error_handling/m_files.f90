module m_files
   use m_error, only: t_error, t_abstract_error
   use m_to_string, only: to_string

   implicit none

   private
   public :: file_get_contents, t_file_error

   integer, parameter :: end_of_file_status = -1
   integer, parameter :: read_successful_status = 0

   type, extends(t_error) :: t_file_error
      character(len=:), allocatable :: path
      character(len=:), allocatable :: io_message
   contains
      procedure :: get_message => file_error_get_message
   end type t_file_error

contains

   pure function file_error_get_message(error) result(res)
      class(t_file_error), intent(in) :: error
      character(len=:), allocatable :: res

      res = error%message_string//', path: '//trim(error%path)// &
            ', IO message: '//trim(error%io_message)// &
            ', in file: '//error%file//' on line '//to_string(error%line)
   end function file_error_get_message

   subroutine file_get_contents(path, contents, error)
      character(len=*), intent(in) :: path
      character(len=:), allocatable, intent(out) :: contents
      class(t_abstract_error), allocatable, intent(out) :: error

      integer :: file_unit
      integer :: io_status
      character(len=255) :: io_message
      character(len=255) :: line_read

      character, parameter :: new_line = char(10)

      open (newunit=file_unit, file=trim(path), action='read', status='old', &
            iostat=io_status, iomsg=io_message)

      if (io_status > 0) then
         error = t_file_error(__FILE__, __LINE__, 'Failed to open file', path, io_message)
         goto 100
      end if

      contents = ''

      do
         read (file_unit, '(A)', iostat=io_status, iomsg=io_message) line_read
         if (is_end_of_file(io_status)) then
            exit
         end if

         if (.not. is_read_successful(io_status)) then
            ! TODO file and line optional, at the end?
            error = t_file_error(__FILE__, __LINE__, 'Failed to read from file', path, io_message)
            goto 100
         end if

         contents = contents//trim(line_read)//new_line
      end do

100   continue
      close (file_unit)

   end subroutine file_get_contents

   pure function is_end_of_file(io_status) result(res)
      integer, intent(in) :: io_status
      logical :: res

      res = io_status == end_of_file_status
   end function is_end_of_file

   pure function is_read_successful(io_status) result(res)
      integer, intent(in) :: io_status
      logical :: res

      res = io_status == read_successful_status
   end function is_read_successful
end module m_files
