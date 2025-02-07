module m_config
    use precision, only : dp

    implicit none

    type :: tsunami_config
        private

        integer :: grid_size = 100  ! grid size in x
        integer :: num_time_steps = 100  ! number of time steps

        real(kind = dp) :: dt = 1.0_dp  ! time step [s]
        real(kind = dp) :: dx = 1.0_dp  ! grid spacing [m]
        real(kind = dp) :: c = 1.0_dp  ! background flow speed [m/s]
    contains
        procedure :: get_grid_size => config_get_grid_size
        procedure :: get_num_time_steps => config_get_num_time_steps
        procedure :: get_dt => config_get_dt
        procedure :: get_dx => config_get_dx
        procedure :: get_c => config_get_c
    end type tsunami_config

    interface tsunami_config
        module procedure :: tsunami_config_constructor
    end interface tsunami_config
contains
    function tsunami_config_constructor(grid_size, num_time_steps, dt, dx, c) result (res)
        integer, intent(in) :: grid_size
        integer, intent(in) :: num_time_steps
        real(kind = dp) :: dt
        real(kind = dp) :: dx
        real(kind = dp) :: c

        type(tsunami_config) :: res

        if (grid_size <= 0.0_dp) then
            error stop 'grid_size must be > 0'
        end if

        if (num_time_steps <= 0) then
            error stop 'grid_size must be > 0'
        end if

        if (dt <= 0.0_dp) then
            error stop 'time step dt must be > 0'
        end if

        if (dx <= 0.0_dp) then
            error stop 'grid spacing dx must be > 0'
        end if

        if (c <= 0.0_dp) then
            error stop 'background flow speed c must be > 0'
        end if

        res%grid_size = grid_size
        res%num_time_steps = num_time_steps
        res%dt = dt
        res%dx = dx
        res%c = c
    end function tsunami_config_constructor

    function config_get_grid_size(config) result (res)
        class(tsunami_config) :: config
        integer :: res

        res = config%grid_size
    end function config_get_grid_size

    function config_get_num_time_steps(config) result (res)
        class(tsunami_config) :: config
        integer :: res

        res = config%num_time_steps
    end function config_get_num_time_steps

    function config_get_dt(config) result (res)
        class(tsunami_config) :: config
        real(kind = dp) :: res

        res = config%dt
    end function config_get_dt

    function config_get_dx(config) result (res)
        class(tsunami_config) :: config
        real(kind = dp) :: res

        res = config%dx
    end function config_get_dx

    function config_get_c(config) result (res)
        class(tsunami_config) :: config
        real(kind = dp) :: res

        res = config%c
    end function config_get_c
end module m_config
