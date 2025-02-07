module m_diff
    use precision, only : dp

    implicit none
    private

    public :: diff

contains
    function diff(x) result (dx)
        real(kind = dp), intent(in), dimension(:) :: x
        real(kind = dp), dimension(size(x)) :: dx
        integer :: im
        im = size(x)
        dx(1) = x(1) - x(im)  ! periodic
        dx(2:im) = x(2:im) - x(1:im - 1)
    end function diff
end module m_diff
