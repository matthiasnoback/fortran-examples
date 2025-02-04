module integration_library
    implicit none
    private
    public :: integrate_trapezoid, user_function

    type, abstract :: user_function
    contains
        procedure(function_evaluation), deferred, pass(params) :: eval
    end type user_function

    abstract interface
        real function function_evaluation(x, params)
            import :: user_function
            real :: x
            class(user_function) :: params
        end function function_evaluation
    end interface
contains
    subroutine integrate_trapezoid(params, xmin, xmax, steps, result)
        class(user_function) :: params
        real, intent(in) :: xmin, xmax
        integer, intent(in) :: steps
        real, intent(out) :: result
        integer :: i
        real :: x
        real :: deltx

        if (steps <= 0) then
            result = 0.0
            return
        end if

        deltx = (xmax - xmin) / steps

        result = (params%eval(xmin) + params%eval(xmax)) / 2.0

        do i = 1, steps - 1
            x = xmin + i * deltx
            result = result + params%eval(x)
        end do

    end subroutine integrate_trapezoid
end module
