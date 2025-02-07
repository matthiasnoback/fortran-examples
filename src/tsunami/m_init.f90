module m_init
    use precision, only : dp

    implicit none

    private

    public :: set_gaussian

contains
    subroutine set_gaussian(x, icenter, decay)
        real(kind = dp), dimension(:), intent(out) :: x
        integer, intent(in) :: icenter
        real(kind = dp), intent(in) :: decay

        integer :: i

        ! initialize water height to a Gaussian shape
        do concurrent(i = 1:size(x))
            x(i) = exp(-decay * (i - icenter)**2)  ! * abs(sin(real(i)))
        end do

    end subroutine set_gaussian
end module m_init
