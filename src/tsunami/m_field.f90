module m_field
    use precision, only : dp

    implicit none

    type t_field
        character(len = :), allocatable :: name
        integer :: dim
        real(kind = dp), dimension(:), allocatable :: data
    contains
        ! type-bound procedures go here
        procedure :: init => field_init
        ! for use in write() (or print?)
        procedure :: field_write_formatted
        generic :: write(formatted) => field_write_formatted
    end type t_field

    interface t_field
        ! custom constructors, operators go here
        module procedure :: field_constructor
    end interface t_field

    type, abstract :: t_init_function
    contains
        procedure(init_function_interface), deferred, pass(init_function) :: init
    end type t_init_function

    interface
        subroutine init_function_interface(init_function, field)
            import t_field
            import t_init_function
            implicit none

            class(t_init_function), intent(in) :: init_function
            class(t_field), intent(inout) :: field
        end subroutine init_function_interface
    end interface

contains
    pure function field_constructor(name, dim) result (res)
        character(len = *), intent(in) :: name
        integer, intent(in) :: dim
        type(t_field) :: res

        res%name = name
        res%dim = dim
        allocate(res%data(dim))
    end function field_constructor

    subroutine field_init(field, init_func)
        class(t_field), intent(inout) :: field
        class(t_init_function), intent(in) :: init_func

        call init_func%init(field)
    end subroutine field_init

    subroutine field_write_formatted(field, unit, iotype, v_list, iostat, iomsg)
        class(t_field), intent(in) :: field
        integer, intent(in) :: unit
        character(len = *), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len = 100), intent(inout) :: iomsg

        write (unit, *, iostat = iostat, iomsg = iomsg) field%name, field%data
    end subroutine field_write_formatted

end module m_field
