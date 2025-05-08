module curry_module
   implicit none
contains

   pure function outer_function(x) result(f)
      real, intent(in) :: x
      interface
         pure function f(y) result(res)
            real, intent(in) :: y
            real :: res
         end function f
      end interface

      ! Define an internal function
      real function inner_function(y)
         real, intent(in) :: y
         inner_function = x * y
      end function inner_function

      ! Assign the internal function to the procedure pointer
      f => inner_function
   end function outer_function

end module curry_module

program test
   use curry_module
   implicit none
   real :: result
   procedure(f), pointer :: p_proc

   ! Create a curried function that multiplies by 5
   p_proc => outer_function(5.0)

   ! Use the procedure pointer
   result = p_proc(3.0)
   print *, "5 * 3 =", result
end program test
