module m_hello
   implicit none

   private

   type, abstract :: person
   contains
      procedure(name_function), deferred, pass(this) :: get_name
   end type person

   abstract interface
      function name_function(this) result(res)
         import :: person
         implicit none
         class(person), intent(in) :: this
         character(:), allocatable :: res
      end function name_function
   end interface

   type, extends(person) :: anyone
      character(:), allocatable :: name_attr
   contains
      procedure :: get_name => anyone_get_name
   end type anyone

   public :: person, hello, anyone

contains
   function hello(who) result(res)
      class(person), intent(in) :: who
      character(:), allocatable :: res
      res = 'Hello, '//who%get_name()//'!'
   end function hello

   pure function anyone_get_name(this) result(res)
      class(anyone), intent(in) :: this
      character(:), allocatable :: res

      res = this%name_attr
   end function anyone_get_name
end module m_hello
