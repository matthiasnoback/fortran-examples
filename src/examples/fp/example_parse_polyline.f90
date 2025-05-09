module polylines
   implicit none(type, external)

   public

   type :: point_t
      real :: x
      real :: y
   end type point_t

   type :: polyline_t
      type(point_t), dimension(:), allocatable :: points
   contains
      procedure, private :: write_polyline
      generic, public :: write (formatted) => write_polyline
   end type polyline_t

   type, extends(polyline_t) :: closed_polyline_t
   end type closed_polyline_t

   type :: point_or_error_t
      type(error_t), allocatable :: error
      type(point_t), allocatable :: point
   end type point_or_error_t

   type :: polyline_or_error_t
      type(error_t), allocatable :: error
      type(polyline_t), allocatable :: polyline
   contains
      procedure, private :: map_to_closed_polyline_or_error
      generic, public :: map => map_to_closed_polyline_or_error
   end type polyline_or_error_t

   type :: error_t
      character(len=:), allocatable :: message
   end type error_t

   type :: closed_polyline_or_error_t
      type(error_t), allocatable :: error
      type(closed_polyline_t), allocatable :: polyline
   end type closed_polyline_or_error_t

contains

   subroutine write_polyline(self, unit, iotype, v_list, iostat, iomsg)
      class(polyline_t), intent(in) :: self
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      integer :: i

      iostat = 0

      associate (polyline => self)
         print *, "Polyline with ", size(polyline%points), " points."
         do i = 1, size(polyline%points)
            print *, "Point ", i, ": (", polyline%points(i)%x, ", ", polyline%points(i)%y, "); "
         end do
      end associate
   end subroutine write_polyline

   pure function parse_point(line) result(point_or_error)
      character(len=*), intent(in) :: line
      integer :: ios
      type(point_or_error_t) :: point_or_error

      real :: x
      real :: y

      read (line, *, iostat=ios) x, y
      if (ios /= 0) then
         point_or_error%error = error_t("Failed to parse point from line: "//line)
      else
         point_or_error%point = point_t(x, y)
      end if
   end function parse_point

   pure function parse_polyline(lines) result(polyline_or_error)
      character(len=*), intent(in) :: lines(:)
      type(polyline_t) :: polyline
      type(polyline_or_error_t) :: polyline_or_error
      type(point_or_error_t) :: point_or_error

      integer :: i

      allocate (polyline%points(size(lines)))

      do i = 1, size(lines)
         point_or_error = parse_point(trim(lines(i)))
         if (allocated(point_or_error%error)) then
            polyline_or_error%error = &
               error_t("Failed to parse polyline"//point_or_error%error%message)
            return
         else
            polyline%points(i) = point_or_error%point
         end if
      end do

      polyline_or_error%polyline = polyline
   end function parse_polyline

   pure function map_to_closed_polyline_or_error(self, map_function) result(closed_polyline_or_error)
      class(polyline_or_error_t), intent(in) :: self

      type(closed_polyline_or_error_t) :: closed_polyline_or_error

      interface
         pure function map_function(polyline) result(mapped)
            import :: polyline_t, closed_polyline_or_error_t
            class(polyline_t), intent(in) :: polyline
            type(closed_polyline_or_error_t) :: mapped
         end function map_function
      end interface

      if (allocated(self%error)) then
         closed_polyline_or_error%error = self%error
         return
      end if

      closed_polyline_or_error = map_function(self%polyline)
   end function map_to_closed_polyline_or_error

   pure function closed_polyline(polyline) result(closed_polyline_or_error)
      class(polyline_t), intent(in) :: polyline
      type(closed_polyline_or_error_t) :: closed_polyline_or_error

      if (size(polyline%points) < 3) then
         closed_polyline_or_error%error = &
            error_t("A closed polyline must have at least 3 points.")
         return
      end if

      if (polyline%points(1)%x /= polyline%points(size(polyline%points))%x .or. &
          polyline%points(1)%y /= polyline%points(size(polyline%points))%y) then
         closed_polyline_or_error%error = &
            error_t("The first and last points of a closed polyline must be the same.")
         return
      end if

      closed_polyline_or_error%polyline = closed_polyline_t(polyline%points)
   end function closed_polyline

end module polylines

program example
   use polylines, only: polyline_t, parse_polyline, polyline_or_error_t, &
                        closed_polyline_or_error_t, closed_polyline

   implicit none(type, external)

   type(polyline_or_error_t) :: polyline_or_error
   type(closed_polyline_or_error_t) :: closed_polyline_or_error
   integer :: i

   polyline_or_error = parse_polyline(["1.0 2.0", "3.0 4.0", "5.0 6.0"])
   closed_polyline_or_error = polyline_or_error%map(closed_polyline)

   if (allocated(closed_polyline_or_error%error)) then
      print *, "Error: ", closed_polyline_or_error%error%message
   else
      print *, "Closed polyline: ", closed_polyline_or_error%polyline
   end if

   polyline_or_error = parse_polyline(["1.0 2.0", "3.0 4.0", "5.0 6.0", "1.0 2.0"])
   closed_polyline_or_error = polyline_or_error%map(closed_polyline)

   if (allocated(closed_polyline_or_error%error)) then
      print *, "Error: ", closed_polyline_or_error%error%message
   else
      print *, "Closed polyline: ", closed_polyline_or_error%polyline
   end if

   polyline_or_error = parse_polyline(["1.0 2.0", "3.0 4.0", "5.0 6.0"])

   if (allocated(polyline_or_error%error)) then
      print *, "Error: ", polyline_or_error%error%message
   else
      print *, "Polyline:", polyline_or_error%polyline
   end if
end program example

! module parser_module
!    implicit none

!    type :: TwoReals
!       real :: x, y
!    end type TwoReals

!    type :: ParseError
!       character(len=:), allocatable :: message
!    end type ParseError

!    type :: ParseErrorOnLine
!       type(ParseError) :: error
!       integer :: line_number
!    end type ParseErrorOnLine

!    type :: Either
!       class(*), allocatable :: value
!    end type Either

! contains

!    function parse_string(input) result(output)
!       character(len=*), intent(in) :: input
!       type(Either) :: output
!       real :: x, y
!       integer :: ios

!       read (input, *, iostat=ios) x, y
!       if (ios == 0) then
!          allocate (TwoReals :: output%value)
!          output%value = TwoReals(x, y)
!       else
!          allocate (ParseError :: output%value)
!          output%value = ParseError("Failed to parse input string")
!       end if
!    end function parse_string

!    function parse_file(filename) result(output_list)
!       character(len=*), intent(in) :: filename
!       type(Either), allocatable :: output_list(:)
!       character(len=256) :: line
!       integer :: unit, ios, line_number
!       type(Either) :: parsed_line
!       integer :: count

!       ! Open the file
!       open (newunit=unit, file=filename, status='old', action='read', iostat=ios)
!       if (ios /= 0) then
!          allocate (output_list(1))
!          allocate (ParseError :: output_list(1)%value)
!          output_list(1)%value = ParseError("Failed to open file")
!          return
!       end if

!       ! Count the number of lines in the file
!       count = 0
!       do
!          read (unit, '(A)', iostat=ios) line
!          if (ios /= 0) exit
!          count = count + 1
!       end do

!       rewind (unit)
!       allocate (output_list(count))

!       ! Parse each line
!       line_number = 0
!       do
!          read (unit, '(A)', iostat=ios) line
!          if (ios /= 0) exit
!          line_number = line_number + 1
!          parsed_line = parse_string(trim(line))
!          if (allocated(parsed_line%value)) then
!             allocate (ParseErrorOnLine :: parsed_line%value)
!             parsed_line%value = ParseErrorOnLine(ParseError(parsed_line%value%message), line_number)
!          end if
!          output_list(line_number) = parsed_line
!       end do

!       close (unit)
!    end function parse_file

! end module parser_module
