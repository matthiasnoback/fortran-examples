program example
   use iso_fortran_env, only: wp => real64

   implicit none

   real(kind=wp) :: x
   real(kind=wp) :: y

   x = 3.0_wp
   y = 4.0_wp

   print *, x, y

end program example
