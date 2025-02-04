module some_ode
    !
    ! We use module variables here, out of laziness
    ! Better would be to use a derived type with all necessary
    ! data and procedure pointers
    !
    real :: force_or_displacement
    real :: dampening_coefficient
contains
    function dampened_oscillator(x, t)
        real, dimension(:) :: x
        real, dimension(size(x)) :: dampened_oscillator
        real :: t
        dampened_oscillator(1) = x(2) ! Velocity
        dampened_oscillator(2) = -force_or_displacement * x(1) - dampening_coefficient * x(2) ! Force
    end function dampened_oscillator
end module some_ode
