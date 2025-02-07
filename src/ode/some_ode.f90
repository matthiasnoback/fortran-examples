module some_ode
    use precision, only : dp

    implicit none

    !
    ! We use module variables here, out of laziness
    ! Better would be to use a derived type with all necessary
    ! data and procedure pointers
    !
    real(kind = dp) :: force_or_displacement
    real(kind = dp) :: dampening_coefficient
contains
    function dampened_oscillator(x, t) result (res)
        real(kind = dp), dimension(:), intent(in) :: x
        real(kind = dp), dimension(size(x)) :: res
        real(kind = dp), intent(in) :: t
        res(1) = x(2)  ! Velocity
        res(2) = -force_or_displacement * x(1) - dampening_coefficient * x(2)  ! Force
    end function dampened_oscillator
end module some_ode
