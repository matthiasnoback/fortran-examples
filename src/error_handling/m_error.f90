module m_error
   implicit none

   type, abstract :: t_abstract_error
      character(len=:), allocatable :: file
      integer :: line = 0
   contains
      procedure(error_message), deferred, pass(error) :: get_message
      procedure :: to_string => abstract_error_to_string
   end type t_abstract_error

   type, extends(t_abstract_error) :: t_error
      character(len=:), allocatable :: message_string
   contains
      procedure :: get_message => get_message_string
   end type t_error

   interface
      pure function error_message(error) result(message)
         import :: t_abstract_error
         implicit none
         class(t_abstract_error), intent(in) :: error
         character(len=:), allocatable :: message
      end function error_message
   end interface
contains
   pure function get_message_string(error) result(res)
      class(t_error), intent(in) :: error
      character(len=:), allocatable :: res

      res = error%message_string
   end function get_message_string

   pure function abstract_error_to_string(error) result(res)
      class(t_abstract_error), intent(in) :: error
      character(len=:), allocatable :: res

      res = error%get_message()
   end function abstract_error_to_string

end module m_error
