module functions
   implicit none(type, external)

   public

   integer :: default_exp = 2
   integer :: module_exp = 3
   integer :: last_used_exp = 2

contains

   pure function square(x) result(squared)
      integer, intent(in) :: x
      integer :: squared

      squared = x**2
   end function square

   pure function power(x, exp) result(res)
      integer, intent(in) :: x
      integer, intent(in) :: exp
      integer :: res

      res = x**exp
   end function power

   pure function power_external_exponent(x) result(res)
      integer, intent(in) :: x
      integer :: res

      res = x**module_exp
   end function power_external_exponent

   pure function power_optional_exp(x, optional_exp) result(res)
      integer, intent(in) :: x
      integer, optional, intent(in) :: optional_exp
      integer :: res

      integer :: exp

      if (present(optional_exp)) then
         exp = optional_exp
      else
         exp = default_exp
      end if

      res = x**exp
   end function power_optional_exp

   function increase_counter(c, i) result(p)
      integer, intent(inout) :: c
      integer, intent(in) :: i
      integer :: p

      p = c
      c = c + i
   end function increase_counter

   function average(numbers, ierr) result(res)
      integer, dimension(:), intent(in) :: numbers
      integer, intent(inout) :: ierr
      real :: res

      if (size(numbers) == 0) then
         ierr = 1
         return
      end if

      ierr = 0
      res = sum(numbers) / size(numbers)
   end function average

   function first_line(path) result(res)
      character(len=*), intent(in) :: path
      character(len=:), allocatable :: res

      integer :: file_unit
      integer :: io_status
      character(len=255) :: line_read

      res = ''

      open (newunit=file_unit, file=trim(path), action='read', status='old', &
            position='rewind', iostat=io_status)

      if (io_status > 0) then
         return
      end if

      read (file_unit, '(A)', iostat=io_status) line_read
      res = trim(line_read)
   end function first_line

   subroutine log(message)
      character(len=*), intent(in) :: message

      print *, 'DEBUG: '//message
   end subroutine log

   function generate_id(prefix) result(res)
      character(len=*), intent(in) :: prefix
      character(len=len(prefix) + 6) :: res

      real :: number
      call random_seed()
      call random_number(number)

      write (res, fmt='(A,A,I5.5)') prefix, '_', int(10000 * number)
   end function generate_id

   function add_timestamp(message) result(res)
      character(len=*), intent(in) :: message
      character(len=10) :: time
      character(len=:), allocatable :: res

      call date_and_time(time=time)

      res = time(1:2)//':'//time(3:4)//':'//time(5:6)//' - '//message
   end function add_timestamp

end module functions

program main
   use functions, only: square, increase_counter, power, &
                        power_optional_exp, average, first_line, &
                        power_external_exponent, log, generate_id, &
                        add_timestamp

   implicit none(type, external)

   integer :: counter, ierr
   integer, dimension(:), allocatable :: empty_array
   allocate (empty_array(0))

   print *, square(10)

   counter = 0

   print *, increase_counter(counter, 1)
   print *, counter

   print *, power(10, 3)
   print *, power_external_exponent(5)
   print *, power_optional_exp(10)

   print *, average([1, 4], ierr)
   print *, ierr

   print *, average(empty_array, ierr)
   print *, ierr

   print *, first_line('debug.log')
   print *, first_line('not_found.log')

   call log('A debug message')

   print *, generate_id('test')

   print *, add_timestamp('Hello')

end program main
