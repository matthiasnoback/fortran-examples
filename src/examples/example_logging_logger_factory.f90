module m_logging
   implicit none(type, external)

   logical :: debug
   logical :: quiet
   logical :: should_log_to_file
   integer :: log_file

   private
   public :: get_logger, t_abstract_logger

   type, abstract :: t_abstract_logger
   contains
      procedure(log_function_interface), deferred :: log
   end type t_abstract_logger

   interface
      subroutine log_function_interface(self, message)
         import t_abstract_logger
         implicit none(type, external)

         class(t_abstract_logger), intent(inout) :: self
         character(len=*), intent(in) :: message
      end subroutine log_function_interface
   end interface

   type, extends(t_abstract_logger) :: t_do_everything_logger
      integer :: message_counter = 0
   contains
      procedure :: log => do_everytying_logger_log
   end type t_do_everything_logger

   class(t_abstract_logger), allocatable :: the_logger

contains
   function get_logger() result(logger)
      class(t_abstract_logger), allocatable :: logger

      if (.not. allocated(the_logger)) then
         call init_logging()

         the_logger = t_do_everything_logger()
      end if

      logger = the_logger
   end function get_logger

   subroutine do_everytying_logger_log(self, message)
      class(t_do_everything_logger), intent(inout) :: self
      character(len=*), intent(in) :: message

      self%message_counter = self%message_counter + 1

      if (debug) then
         if (should_log_to_file) then
            write (log_file, fmt=*) current_time()//message
         end if

         if (.not. quiet) then
            print *, current_time()//' '//message
         end if
      end if

      print *, 'Message counter: ', self%message_counter
   end subroutine do_everytying_logger_log

   subroutine init_logging()
      integer :: open_status
      print *, 'Initialize logging'
      ! We could read this from CLI arguments
      ! --debug, --quiet, and --log-file
      debug = .true.
      quiet = .false.
      open (file='debug.log', newunit=log_file, status='unknown', &
            position='append', action='write', iostat=open_status)
      should_log_to_file = open_status == 0
   end subroutine init_logging

   function current_time() result(iso_time)
      character(len=8) :: date
      character(len=10) :: time
      character(len=5) :: zone
      character(len=24) :: iso_time

      call date_and_time(date, time, zone)

      iso_time = date(1:4)//'-'//date(5:6)//'-'//date(7:8)//'T'// &
                 time(1:2)//':'//time(3:4)//':'//time(5:6)//zone
   end function current_time
end module m_logging

program example
   use m_logging, only: get_logger, t_abstract_logger

   implicit none(type, external)

   class(t_abstract_logger), allocatable :: logger

   logger = get_logger()

   call logger%log('Message')

   call logger%log('Another message')

end program example
