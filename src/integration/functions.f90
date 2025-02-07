module functions
    use integration_library, only : user_function
    use precision, only : dp

    implicit none

    type, extends(user_function) :: my_function
        real(kind = dp) :: a
        real(kind = dp) :: b
    contains
        procedure, pass(params) :: eval => f
    end type my_function

contains
    function f(x, params) result (res)
        real(kind = dp), intent(in) :: x
        class(my_function), intent(in) :: params
        real(kind = dp) :: res

        res = exp(-params%a * x) * cos(params%b * x)
    end function f

end module functions
