module m_logging

   use m_files, only: open_file_for_appending
   use m_error, only: t_abstract_error

   implicit none

   private

   public :: &
      t_abstract_logger, &
      t_stdout_logger, &
      t_file_logger, &
      t_multi_logger, &
      t_logger_reference

   type, abstract :: t_abstract_logger
   contains
      procedure(log_interface), deferred :: log
   end type t_abstract_logger

   interface
      subroutine log_interface(this, message)
         import t_abstract_logger

         implicit none

         class(t_abstract_logger), intent(in) :: this
         character(len=*), intent(in) :: message
      end subroutine log_interface
   end interface

   type, extends(t_abstract_logger) :: t_stdout_logger
   contains
      procedure :: log => stdout_logger_log
   end type t_stdout_logger

   type, extends(t_abstract_logger) :: t_file_logger
      character(len=:), allocatable :: path
      integer :: file_unit
   contains
      procedure :: log => file_logger_log
      procedure :: terminate => file_logger_terminate
   end type t_file_logger

   interface t_file_logger
      module procedure :: file_logger_constructor
   end interface

   type :: t_logger_reference
      class(t_abstract_logger), allocatable :: logger
   end type t_logger_reference

   type, extends(t_abstract_logger) :: t_multi_logger
      type(t_logger_reference), dimension(:), allocatable :: logger_references
   contains
      procedure :: log => multi_logger_log
   end type t_multi_logger
contains

   subroutine stdout_logger_log(this, message)
      class(t_stdout_logger), intent(in) :: this
      character(len=*), intent(in) :: message

      print *, message
   end subroutine stdout_logger_log

   function file_logger_constructor(path, error) result(res)
      character(len=*), intent(in) :: path
      class(t_abstract_error), intent(out), allocatable :: error

      type(t_file_logger) :: res

      res%path = path

      call open_file_for_appending(path, res%file_unit, error)
   end function file_logger_constructor

   subroutine file_logger_log(this, message)
      class(t_file_logger), intent(in) :: this
      character(len=*), intent(in) :: message

      write (this%file_unit, fmt=*) message
   end subroutine file_logger_log

   subroutine file_logger_terminate(this)
      class(t_file_logger), intent(in) :: this

      close (this%file_unit)
   end subroutine file_logger_terminate

   subroutine multi_logger_log(this, message)
      class(t_multi_logger), intent(in) :: this
      character(len=*), intent(in) :: message

      integer :: i

      do i = 1, size(this%logger_references)
         call this%logger_references(i)%logger%log(message)
      end do
   end subroutine multi_logger_log

end module m_logging
