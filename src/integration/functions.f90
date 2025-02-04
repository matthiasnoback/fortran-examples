module functions
    use integration_library, only : user_function

    implicit none

    type, extends(user_function) :: my_function
        real :: a
        real :: b
    contains
        procedure, pass(params) :: eval => f
    end type my_function

contains
    function f(x, params) result (res)
        real :: x
        real :: res
        class(my_function) :: params
        res = exp(-params%a * x) * cos(params%b * x)
    end function f

end module functions
