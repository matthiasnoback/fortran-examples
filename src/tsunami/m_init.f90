module m_init
    use precision, only : dp

    use m_field, only : t_field, t_init_function

    implicit none

    private

    public :: t_init_gaussian, t_init_constant

    type, extends(t_init_function) :: t_init_gaussian
        integer :: icenter
        real(kind = dp) :: decay
    contains
        procedure :: init => init_gaussian
    end type t_init_gaussian

    type, extends(t_init_function) :: t_init_constant
        real(kind = dp) :: value
    contains
        procedure :: init => init_constant
    end type t_init_constant

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

    subroutine init_gaussian(init_function, field)
        class(t_field), intent(inout) :: field
        class(t_init_gaussian), intent(in) :: init_function

        call set_gaussian(field%data, init_function%icenter, init_function%decay)
    end subroutine init_gaussian

    subroutine init_constant(init_function, field)
        class(t_field), intent(inout) :: field
        class(t_init_constant), intent(in) :: init_function

        field%data = init_function%value
    end subroutine init_constant
end module m_init
