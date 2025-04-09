module common_time
   implicit none(type, external)

   private
   public :: current_time

contains
   !> Returns the current time as ISO 8601-formatted timestamp
   function current_time() result(iso_time)
      character(len=8) :: date
      character(len=10) :: time
      character(len=5) :: zone
      character(len=24) :: iso_time

      call date_and_time(date, time, zone)

      iso_time = date(1:4)//'-'//date(5:6)//'-'//date(7:8)//'T'// &
                 time(1:2)//':'//time(3:4)//':'//time(5:6)//zone
   end function current_time
end module common_time

module logging_abstract

   implicit none(type, external)

   private
   public :: abstract_logger_t

   type, abstract :: abstract_logger_t
   contains
      procedure(log_function_interface), deferred :: log
   end type abstract_logger_t

   interface
      subroutine log_function_interface(self, message)
         import abstract_logger_t
         implicit none(type, external)

         class(abstract_logger_t), intent(inout) :: self
         character(len=*), intent(in) :: message
      end subroutine log_function_interface
   end interface

end module logging_abstract

module common_command_line
   implicit none(type, external)

   private

   public :: has_cli_argument
   public :: get_cli_argument

contains

   function has_cli_argument(argument) result(res)
      character(len=*), intent(in) :: argument

      logical :: res
      integer :: index

      do index = 1, command_argument_count()
         if (get_cli_argument(index) == argument) then
            res = .true.
            return
         end if
      end do

      res = .false.
   end function has_cli_argument

   function get_cli_argument(index) result(value)
      integer, intent(in) :: index
      character(len=:), allocatable :: value

      integer :: argument_length
      integer :: status

      call get_command_argument(number=index, length=argument_length)
      allocate (character(len=argument_length) :: value)

      call get_command_argument(index, value, status=status)

      if (status /= 0) then
         error stop "Could not retrieve command-line argument"
      end if
   end function get_cli_argument

end module common_command_line

module logging_everything
   use logging_abstract, only: abstract_logger_t
   use logging_file, only: file_logger_t
   use logging_stdout, only: stdout_logger_t

   implicit none(type, external)

   private
   public :: do_everything_logger_t

   type, extends(abstract_logger_t) :: do_everything_logger_t
      private

      logical :: debug
      logical :: quiet

      class(file_logger_t), allocatable :: file_logger
      class(stdout_logger_t), allocatable :: stdout_logger
   contains
      procedure :: log => do_everything_logger_log
      final :: do_everything_logger_destructor
   end type do_everything_logger_t

   interface do_everything_logger_t
      procedure :: do_everything_logger_constructor
   end interface
contains
   function do_everything_logger_constructor(debug, quiet) result(logger)
      logical, intent(in) :: debug
      logical, intent(in) :: quiet
      type(do_everything_logger_t), pointer :: logger

      print *, 'Constructor of do_everything_logger_t'

      allocate (logger)

      logger%debug = debug
      logger%quiet = quiet
      logger%file_logger = file_logger_t('debug.log')
      logger%stdout_logger = stdout_logger_t()
   end function do_everything_logger_constructor

   subroutine do_everything_logger_destructor(self)
      type(do_everything_logger_t), intent(in) :: self

      print *, 'Destructor of do_everything_logger_t called'
   end subroutine do_everything_logger_destructor

   subroutine do_everything_logger_log(self, message)
      class(do_everything_logger_t), intent(inout) :: self
      character(len=*), intent(in) :: message

      if (self%debug) then
         call self%file_logger%log(message)

         if (.not. self%quiet) then
            call self%stdout_logger%log(message)
         end if
      end if
   end subroutine do_everything_logger_log
end module logging_everything

module logging_file
   use common_time, only: current_time
   use logging_abstract, only: abstract_logger_t

   implicit none(type, external)

   private
   public :: file_logger_t

   type, extends(abstract_logger_t) :: file_logger_t
      integer :: log_file_unit
   contains
      procedure :: log => file_logger_log
      final :: file_logger_destructor
   end type file_logger_t

   interface file_logger_t
      procedure :: file_logger_constructor
   end interface

contains

   function file_logger_constructor(log_file_path) result(file_logger)
      character(len=*), intent(in) :: log_file_path
      type(file_logger_t), pointer :: file_logger

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
      type(file_logger_t), intent(in) :: self
      print *, 'Destructor of file logger called'
   end subroutine file_logger_destructor

   subroutine file_logger_log(self, message)
      class(file_logger_t), intent(inout) :: self
      character(len=*), intent(in) :: message

      write (self%log_file_unit, fmt=*) current_time()//' '//message
   end subroutine file_logger_log

end module logging_file

module logging_stdout
   use common_time, only: current_time
   use logging_abstract, only: abstract_logger_t

   implicit none(type, external)

   private
   public :: stdout_logger_t

   type, extends(abstract_logger_t) :: stdout_logger_t
   contains
      procedure :: log => stdout_logger_log
   end type stdout_logger_t

contains

   subroutine stdout_logger_log(self, message)
      class(stdout_logger_t), intent(inout) :: self
      character(len=*), intent(in) :: message

      print *, current_time()//' '//message
   end subroutine stdout_logger_log

end module logging_stdout

module logging_facade
   use logging_abstract, only: abstract_logger_t

   implicit none(type, external)

   private
   public :: get_logger

   interface
      module function get_logger() result(logger)
         import abstract_logger_t

         implicit none(type, external)

         class(abstract_logger_t), pointer :: logger
      end function get_logger
   end interface

end module logging_facade

module logging_multi_logger
   use logging_abstract, only: abstract_logger_t

   type, extends(abstract_logger_t) :: multi_logger_t
      class(abstract_logger_t), pointer :: inner_logger
   contains
      procedure :: log => multi_logger_log
   end type multi_logger_t

contains

   subroutine multi_logger_log(self, message)
      class(multi_logger_t), intent(inout) :: self
      character(len=*), intent(in) :: message

      call self%inner_logger%log(message)

   end subroutine multi_logger_log

end module logging_multi_logger

submodule(logging_facade) logging_facade_implementation
   use common_command_line, only: has_cli_argument
   use logging_abstract, only: abstract_logger_t
   use logging_file, only: file_logger_t
   use logging_stdout, only: stdout_logger_t
   use logging_everything, only: do_everything_logger_t
   use logging_multi_logger, only: multi_logger_t

   implicit none(type, external)

   class(abstract_logger_t), pointer :: the_logger

contains

   module function get_logger() result(logger)
      class(abstract_logger_t), pointer :: logger

      logical :: debug
      logical :: quiet

      type(do_everything_logger_t), allocatable, target :: do_everything_logger
      type(multi_logger_t), allocatable, target :: multi_logger

      if (.not. associated(the_logger)) then
         debug = .not. has_cli_argument('--no-debug')
         quiet = has_cli_argument('--quiet')

         allocate (do_everything_logger)
         do_everything_logger = do_everything_logger_t(debug, quiet)
         allocate (multi_logger)
         multi_logger%inner_logger => do_everything_logger
         the_logger => multi_logger
      end if

      logger => the_logger
   end function get_logger

end submodule logging_facade_implementation

program logging_demo
   use logging_facade, only: get_logger
   use logging_abstract, only: abstract_logger_t

   implicit none(type, external)

   class(abstract_logger_t), pointer :: logger

   logger => get_logger()
   call logger%log('A message')

   call logger%log('Another message')

   ! Only to demonstrate that the final procedures will be invoked
   deallocate (logger)

end program logging_demo
