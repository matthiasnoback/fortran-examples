program integration
    use integration_library, only : integrate_trapezoid
    use iso_fortran_env, only : error_unit
    use functions, only : my_function
    use precision, only : dp

    implicit none

    type(my_function) :: the_function = my_function(2.0_dp, 3.0_dp)

    real(kind = dp) :: xmin = 0.0_dp
    real(kind = dp) :: xmax = 1.0_dp

    integer :: steps = 10

    select case (get_task_cli_argument('integrate'))
    case('integrate')
        print *, integrate_trapezoid(the_function, xmin, xmax, steps)
    case('plot')
        call plot_function(the_function, xmin, xmax, steps)
    case default
        error stop "Unknown task"
    end select

contains
    function get_task_cli_argument(default_task) result (res)
        character(len = *), intent(in) :: default_task
        character(len = 100) :: res
        integer :: status
        integer :: length

        call get_command_argument(1, res, length, status)

        if (status > 0) then
            res = default_task
        end if

        res = trim(res)
    end function get_task_cli_argument

    subroutine plot_function(params, xmin, xmax, steps)
        type(my_function), intent(in) :: params
        real(kind = dp), intent(in) :: xmin
        real(kind = dp), intent(in) :: xmax
        integer, intent(in) :: steps

        integer :: i
        real(kind = dp) :: delta_x
        real(kind = dp) :: x

        if (steps <= 0) then
            return
        end if

        delta_x = (xmax - xmin) / steps

        do i = 1, steps - 1
            x = xmin + i * delta_x
            print '(es15.8e2,1x,es15.8e2)', x, params%eval(x)
        end do

    end subroutine plot_function

end program integration
