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

module logging_counter
   use logging_abstract, only: abstract_logger_t

   implicit none(type, external)

   private
   public :: counting_logger_t

   type, extends(abstract_logger_t) :: counting_logger_t
      integer :: counter
   contains
      procedure :: log => counting_logger_log
      final :: counting_logger_destructor
   end type counting_logger_t

   interface counting_logger_t
      module procedure counting_logger_constructor
   end interface
contains

   function counting_logger_constructor() result (new_logger)
      type(counting_logger_t), pointer :: new_logger

      allocate(new_logger)

      print *, 'Constructing counting logger'

      new_logger%counter = 1000000
   end function counting_logger_constructor

   subroutine counting_logger_log(self, message)
      class(counting_logger_t), intent(inout) :: self
      character(len=*), intent(in) :: message

      print *, message
      self%counter = self%counter + 1
      print *, 'Counter is ', self%counter
   end subroutine counting_logger_log

   subroutine counting_logger_destructor(self)
      type(counting_logger_t), intent(inout) :: self

      print *, 'Destructing counting logger'
   end subroutine counting_logger_destructor
end module logging_counter

module logging_decoration
   use logging_abstract, only: abstract_logger_t

   implicit none(type, external)

   private
   public :: decorating_logger_t

   type, extends(abstract_logger_t) :: decorating_logger_t
      class(abstract_logger_t), pointer :: decorated_logger => null()
   contains
      procedure :: log => decorating_logger_log
      final :: decorating_logger_destructor
   end type decorating_logger_t

   interface decorating_logger_t
      module procedure decorating_logger_constructor
   end interface decorating_logger_t

contains

   function decorating_logger_constructor(decorated_logger) result (new_logger)
      class(abstract_logger_t), target, intent(in) :: decorated_logger

      type(decorating_logger_t), pointer :: new_logger

      print *, 'Constructing decorating logger'
      allocate(new_logger)

      new_logger%decorated_logger => decorated_logger
   end function decorating_logger_constructor

   subroutine decorating_logger_log(self, message)
      class(decorating_logger_t), intent(inout) :: self
      character(len=*), intent(in) :: message

      call self%decorated_logger%log('Prefix '//message)
   end subroutine decorating_logger_log

   subroutine decorating_logger_destructor(self)
      type(decorating_logger_t), intent(inout) :: self

      print *, 'Destructing decorating logger'
      deallocate(self%decorated_logger)
   end subroutine decorating_logger_destructor

end module logging_decoration

module logging_facade
   use logging_abstract, only: abstract_logger_t
   use logging_decoration, only: decorating_logger_t
   use logging_counter, only: counting_logger_t

   implicit none(type, external)

   private
   public :: get_logger, cleanup

   class(abstract_logger_t), pointer :: shared_logger_instance

contains

   function get_logger() result(result)
      class(abstract_logger_t), pointer :: result

      type(decorating_logger_t), pointer :: decorating_logger
      class(abstract_logger_t), pointer :: decorated_logger

      if (.not. associated(shared_logger_instance)) then
         decorated_logger => counting_logger_t()
         decorating_logger => decorating_logger_t(decorated_logger)
         shared_logger_instance => decorating_logger
      end if

      result => shared_logger_instance
   end function get_logger

   subroutine cleanup()
      deallocate(shared_logger_instance)
   end subroutine cleanup

end module logging_facade

program example
   use logging_facade, only: get_logger, cleanup

   implicit none(type, external)

   call get_logger()%log('Test 1')
   call get_logger()%log('Test 2')

   call cleanup()
end program example
