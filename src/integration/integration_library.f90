module integration_library
    use precision, only : dp

    implicit none

    private
    public :: integrate_trapezoid, user_function

    type, abstract :: user_function
    contains
        procedure(function_evaluation), deferred, pass(params) :: eval
    end type user_function

    abstract interface
        function function_evaluation(x, params) result (res)
            import :: user_function, dp

            implicit none

            real(kind = dp), intent(in) :: x
            class(user_function), intent(in) :: params
            real(kind = dp) :: res
        end function function_evaluation
    end interface
contains
    function integrate_trapezoid(params, xmin, xmax, steps) result (result)
        class(user_function), intent(in) :: params
        real(kind = dp), intent(in) :: xmin, xmax
        integer, intent(in) :: steps
        real(kind = dp) :: result
        integer :: i
        real(kind = dp) :: x
        real(kind = dp) :: deltx

        if (steps <= 0) then
            result = 0.0_dp
            return
        end if

        deltx = (xmax - xmin) / steps

        result = (params%eval(xmin) + params%eval(xmax)) / 2.0_dp

        do i = 1, steps - 1
            x = xmin + i * deltx
            result = result + params%eval(x)
        end do

    end function integrate_trapezoid
end module integration_library
