module m_diff
   use precision, only: dp

   implicit none
   private

   public :: diff_upwind, diff_centered

contains
   function diff_upwind(x) result(dx)
      real(kind=dp), intent(in), dimension(:) :: x
      real(kind=dp), dimension(size(x)) :: dx
      integer :: im
      im = size(x)
      dx(1) = x(1) - x(im)  ! periodic
      dx(2:im) = x(2:im) - x(1:im - 1)
   end function diff_upwind

   function diff_centered(x) result(dx)
      real(kind=dp), intent(in), dimension(:) :: x
      real(kind=dp), dimension(size(x)) :: dx
      integer :: im

      im = size(x)

      dx(1) = x(2) - x(im)
      dx(im) = x(1) - x(im - 1)
      dx(2:im - 1) = x(3:im) - x(1:im - 2)
      dx = 0.5_dp*dx
   end function diff_centered
end module m_diff
