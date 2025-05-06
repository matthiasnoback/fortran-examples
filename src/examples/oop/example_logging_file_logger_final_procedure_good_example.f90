module m_logging

   implicit none

   type, abstract :: t_abstract_logger
   contains
      procedure(log_function_interface), deferred :: log
   end type t_abstract_logger

   interface
      subroutine log_function_interface(self, message)
         import t_abstract_logger
         implicit none

         class(t_abstract_logger), intent(inout) :: self
         character(len=*), intent(in) :: message
      end subroutine log_function_interface
   end interface

   type, extends(t_abstract_logger) :: t_file_logger
      integer :: log_file_unit
      integer :: message_counter = 0
   contains
      procedure :: log => file_logger_log
      final :: file_logger_destructor
   end type t_file_logger

   interface t_file_logger
      procedure :: file_logger_constructor
   end interface

   class(t_abstract_logger), allocatable, target :: the_logger

contains

   function file_logger_constructor(log_file_path) result(file_logger)
      character(len=*), intent(in) :: log_file_path
      type(t_file_logger), pointer :: file_logger

      integer :: open_status
      integer :: log_file_unit

      print *, 'Constructing file logger'
      open (file=log_file_path, newunit=log_file_unit, status='unknown', &
            position='append', action='write', iostat=open_status)

      if (open_status /= 0) then
         error stop "Could not open log file for writing"
      end if

      allocate (file_logger)

      file_logger%log_file_unit = log_file_unit
   end function file_logger_constructor

   subroutine file_logger_destructor(self)
      type(t_file_logger) :: self
      print *, 'Destructing file logger'
   end subroutine file_logger_destructor

   subroutine file_logger_log(self, message)
      class(t_file_logger), intent(inout) :: self
      character(len=*), intent(in) :: message

      self%message_counter = self%message_counter + 1
      print *, 'Message counter', self%message_counter
      write (self%log_file_unit, fmt=*) message
   end subroutine file_logger_log

   function get_logger() result(logger)
      class(t_abstract_logger), pointer :: logger

      if (.not. allocated(the_logger)) then
         the_logger = t_file_logger('debug.log')
      end if

      logger => the_logger
   end function get_logger
end module m_logging

program example
   use m_logging, only: t_abstract_logger, get_logger

   implicit none

   class(t_abstract_logger), pointer :: logger

   logger => get_logger()

   call logger%log('A message')
   call logger%log('Another message')

   deallocate (logger)

end program example
