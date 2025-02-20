module m_error
   use m_to_string, only: to_string

   implicit none

   type, abstract :: t_abstract_error
   contains
      procedure(error_message), deferred, pass(error) :: get_message
      procedure(error_message), deferred, pass(error) :: to_string
   end type t_abstract_error

   type :: t_error_location
      character(len=:), allocatable :: file
      integer :: line
   contains
      procedure :: to_string => error_location_to_string
   end type t_error_location

   type, extends(t_abstract_error) :: t_error
      character(len=:), allocatable :: message
      class(t_error_location), allocatable :: location
      class(t_abstract_error), allocatable :: previous_error
   contains
      procedure :: get_message => error_get_message
      procedure :: to_string => error_to_string
   end type t_error

   interface t_error
      module procedure :: error_constructor
   end interface t_error

   interface
      pure function error_message(error) result(message)
         import :: t_abstract_error
         implicit none
         class(t_abstract_error), intent(in) :: error
         character(len=:), allocatable :: message
      end function error_message
   end interface
contains

   pure function error_constructor(message, location, previous_error) result(res)
      character(len=*), intent(in) :: message
      class(t_error_location), optional, intent(in) :: location
      class(t_abstract_error), optional, intent(in) :: previous_error
      type(t_error) :: res

      res%message = message
      if (present(location)) then
         res%location = location
      end if

      if (present(previous_error)) then
         res%previous_error = previous_error
      end if
   end function error_constructor

   pure function error_get_message(error) result(res)
      class(t_error), intent(in) :: error
      character(len=:), allocatable :: res

      res = error%message
   end function error_get_message

   pure function error_to_string(error) result(res)
      class(t_error), intent(in) :: error
      character(len=:), allocatable :: res

      character, parameter :: new_line = char(10)

      ! TODO add more info: previous error
      res = 'ERROR: "'//error%get_message()//'"'
      if (allocated(error%location)) then
         res = res//' in '//error%location%to_string()
      end if

      if (allocated(error%previous_error)) then
         res = res//new_line//error%previous_error%to_string()
      end if

   end function error_to_string

   pure function error_location_to_string(location) result(res)
      class(t_error_location), intent(in) :: location
      character(len=:), allocatable :: res

      res = location%file//' on line '//to_string(location%line)
   end function error_location_to_string

end module m_error
