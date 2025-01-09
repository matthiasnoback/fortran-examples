module my_tests_mod
    use iso_fortran_env
    use iso_c_binding
    use m_hello, only : hello, anyone
    implicit none
contains

    integer function test_success() result(ret) bind(C)
        character(len = 255) :: actual
        actual = hello(anyone('test'))
        if (actual /= 'Hello, test!') then
            ret = 1
        else
            ret = 0
        end if
    end function test_success

    integer function test_fail() result(ret) bind(C)
        ret = 1
    end function test_fail

end module my_tests_mod
