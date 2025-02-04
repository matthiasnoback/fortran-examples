program integration
    use integration_library, only : integrate_trapezoid
    use iso_fortran_env, only : error_unit
    use functions, only : my_function

    character(len = 100) :: task_name
    type(my_function) :: the_function = my_function(2.0, 3.0)

    real :: xmin = 0.0
    real :: xmax = 1.0

    integer :: steps = 10
    real :: integration_result

    call get_task_cli_argument(task_name, 'integrate')

    write(error_unit, *) "Requested task: " // task_name

    if (task_name == 'plot') then
        call plot_function(the_function, xmin, xmax, steps)
    elseif (task_name == 'integrate') then
        call integrate_trapezoid(the_function, xmin, xmax, steps, integration_result)

        print *, integration_result
    else
        error stop "Unknown task name: " // task_name
    end if

contains
    subroutine get_task_cli_argument(task_name, default_task)
        character(len = *), intent(out) :: task_name
        character(len = *), intent(in) :: default_task
        integer :: status
        integer :: length

        call get_command_argument(1, task_name, length, status)

        if (status > 0) then
            task_name = default_task
        end if

        task_name = trim(task_name)
    end subroutine get_task_cli_argument

    subroutine plot_function(params, xmin, xmax, steps)
        type(my_function), intent(in) :: params
        real, intent(in) :: xmin
        real, intent(in) :: xmax
        integer, intent(in) :: steps

        integer :: i
        real :: delta_x
        real :: x

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
