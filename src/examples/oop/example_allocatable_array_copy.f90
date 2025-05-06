program example
   implicit none(type, external)

   integer, dimension(:), allocatable :: return_value

   print *, 'Calling create_int_array()'
   return_value = create_int_array()

   print *, 'Memory location of captured return value:', loc(return_value)

contains

   function create_int_array() result(res)
      integer, dimension(:), allocatable :: res

      allocate (res(1000))

      res = 1
      print *, 'Memory location of return value before returning', loc(res)
   end function create_int_array

end program example
