program solve_some_ode
    use ode_solvers, only : ode_solver, new_solver, ODE_EULER
    use some_ode, only : dampened_oscillator, force_or_displacement, dampening_coefficient

    real :: time
    real :: max_time
    real :: deltt
    real, dimension(2) :: state
    class(ode_solver), allocatable :: solver

    force_or_displacement = 0.6
    dampening_coefficient = 0.5

    solver = new_solver(ODE_EULER, dampened_oscillator)
    call solver%initial((/ 0.0, 10.0 /))
    time = 0.0
    max_time = 20.0
    deltt = 0.1

    do while(time < max_time)
        state = solver%next(time, deltt)
        call print_state(time, state)
        time = time + deltt
    enddo

contains
    subroutine print_state(time, state)
        real :: time
        real, dimension(:) :: state
        print '(g0,1x,es15.8e2)', time, [state(size(state))]
    end subroutine print_state
end program
