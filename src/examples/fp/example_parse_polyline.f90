module printing
   implicit none(type, external)

   public

   interface to_string
      module procedure int_to_string, real_to_string
   end interface

   type, abstract :: to_string_t
   contains
      procedure(derived_type_to_string_interface), public, deferred :: to_string
      procedure, private :: to_string_write
      generic, public :: write (formatted) => to_string_write
   end type to_string_t

   interface
      pure function derived_type_to_string_interface(self) result(str)
         import :: to_string_t
         implicit none(type, external)
         class(to_string_t), intent(in) :: self
         character(len=:), allocatable :: str
      end function derived_type_to_string_interface
   end interface

contains

   subroutine to_string_write(self, unit, iotype, v_list, iostat, iomsg)
      class(to_string_t), intent(in) :: self
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      ! Reference `iotype` to avoid unused variable warning;
      ! `iotype` is a scalae of intent `in` and type `character(*)`. Its value is 'LISTDIRECTED',
      ! 'NAMELIST', or 'DT'//string, where string is the charcater string from the `dt` edit descriptor.
      associate (dummy => iotype)
      end associate

      ! Reference `v_list` to avoid unused variable warning;
      ! `v_list` is a rank-one assumed-shape array of intent `in` and type default `integer`.
      ! Its value comes from the parenthetical list of the edit descriptor.
      associate (dummy => v_list)
      end associate

      write (unit, fmt=*, iomsg=iomsg, iostat=iostat) self%to_string()
   end subroutine to_string_write

   pure function int_to_string(self) result(str)
      integer, intent(in) :: self
      character(len=32) :: temp
      character(len=:), allocatable :: str

      write (temp, '(I0)') self
      str = trim(adjustl(temp))
   end function int_to_string

   pure function real_to_string(self) result(str)
      real, intent(in) :: self
      character(len=32) :: temp
      character(len=:), allocatable :: str

      write (temp, '(F6.4)') self
      str = trim(adjustl(temp))
   end function real_to_string
end module printing

module polylines
   use printing, only: to_string, to_string_t

   implicit none(type, external)

   public

   type, extends(to_string_t) :: point_t
      real :: x
      real :: y
   contains
      procedure, public :: to_string => point_to_string
   end type point_t

   type, extends(to_string_t) :: polyline_t
      type(point_t), dimension(:), allocatable :: points
   contains
      procedure, public :: to_string => polyline_to_string
   end type polyline_t

   type, extends(polyline_t) :: polygon_t
   end type polygon_t

   type, extends(to_string_t) :: point_or_error_t
      type(error_t), allocatable :: error
      type(point_t), allocatable :: point
   contains
      procedure, public :: to_string => point_or_error_to_string
   end type point_or_error_t

   type, extends(to_string_t) :: polyline_or_error_t
      type(error_t), allocatable :: error
      type(polyline_t), allocatable :: polyline
   contains
      procedure, private :: map_to_polygon_or_error
      generic, public :: map => map_to_polygon_or_error
      procedure, public :: to_string => polyline_or_error_to_string
   end type polyline_or_error_t

   type, extends(to_string_t) :: error_t
      character(len=:), allocatable :: message
   contains
      procedure, public :: to_string => error_to_string
   end type error_t

   type, extends(to_string_t) :: polygon_or_error_t
      type(error_t), allocatable :: error
      type(polygon_t), allocatable :: polygon
   contains
      procedure, public :: to_string => polygon_or_error_to_string
   end type polygon_or_error_t

contains

   pure function error_to_string(self) result(str)
      class(error_t), intent(in) :: self
      character(len=:), allocatable :: str

      str = "Error: "//self%message
   end function error_to_string

   pure function polyline_or_error_to_string(self) result(str)
      class(polyline_or_error_t), intent(in) :: self
      character(len=:), allocatable :: str

      if (allocated(self%error)) then
         str = self%error%to_string()
      else
         str = self%polyline%to_string()
      end if
   end function polyline_or_error_to_string

   pure function polygon_or_error_to_string(self) result(str)
      class(polygon_or_error_t), intent(in) :: self
      character(len=:), allocatable :: str

      if (allocated(self%error)) then
         str = self%error%to_string()
      else
         str = self%polygon%to_string()
      end if
   end function polygon_or_error_to_string

   pure function polyline_to_string(self) result(str)
      class(polyline_t), intent(in) :: self
      character(len=:), allocatable :: str
      integer :: i

      str = "Polyline with "//to_string(size(self%points))//" points: "
      do i = 1, size(self%points)
         str = str//"("//to_string(self%points(i)%x)//", "// &
               to_string(self%points(i)%y)//") "
      end do
   end function polyline_to_string

   pure function point_or_error_to_string(self) result(str)
      class(point_or_error_t), intent(in) :: self
      character(len=:), allocatable :: str

      if (allocated(self%error)) then
         str = self%error%to_string()
      else
         str = self%point%to_string()
      end if
   end function point_or_error_to_string

   pure function point_to_string(self) result(str)
      class(point_t), intent(in) :: self
      character(len=:), allocatable :: str

      str = "("//to_string(self%x)//", "//to_string(self%y)//")"
   end function point_to_string

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
               error_t("Failed to parse polyline - "//point_or_error%error%message)
            return
         else
            polyline%points(i) = point_or_error%point
         end if
      end do

      polyline_or_error%polyline = polyline
   end function parse_polyline

   pure function map_to_polygon_or_error(self, map_function) result(polygon_or_error)
      class(polyline_or_error_t), intent(in) :: self

      type(polygon_or_error_t) :: polygon_or_error

      interface
         pure function map_function(polyline) result(mapped)
            import :: polyline_t, polygon_or_error_t
            class(polyline_t), intent(in) :: polyline
            type(polygon_or_error_t) :: mapped
         end function map_function
      end interface

      if (allocated(self%error)) then
         polygon_or_error%error = self%error
         return
      end if

      polygon_or_error = map_function(self%polyline)
   end function map_to_polygon_or_error

   pure function to_polygon(polyline) result(polygon_or_error)
      class(polyline_t), intent(in) :: polyline
      type(polygon_or_error_t) :: polygon_or_error

      if (size(polyline%points) < 3) then
         polygon_or_error%error = &
            error_t("A closed polyline must have at least 3 points.")
         return
      end if

      if (polyline%points(1)%x /= polyline%points(size(polyline%points))%x .or. &
          polyline%points(1)%y /= polyline%points(size(polyline%points))%y) then
         polygon_or_error%error = &
            error_t("The first and last points of a closed polyline must be the same.")
         return
      end if

      polygon_or_error%polygon = polygon_t(polyline%points)
   end function to_polygon

end module polylines

program example
   use polylines, only: polyline_t, parse_polyline, polyline_or_error_t, &
                        polygon_or_error_t, to_polygon

   implicit none(type, external)

   type(polyline_or_error_t) :: polyline_or_error
   type(polygon_or_error_t) :: polygon_or_error

   polyline_or_error = parse_polyline(["1.0 2.0", "3.0 4.0", "5.0 6.0"])
   point_t(1.0, 2.0)
   print *, polyline_or_error

   polyline_or_error = parse_polyline(["1.0 2.0", "a 4.0  ", "5.0    "])
   print *, polyline_or_error

   polyline_or_error = parse_polyline(["1.0 2.0", "3.0 4.0", "5.0 6.0"])
   polygon_or_error = polyline_or_error%map(to_polygon)

   print *, polygon_or_error

   polyline_or_error = parse_polyline(["1.0 2.0", "3.0 4.0", "5.0 6.0", "1.0 2.0"])
   polygon_or_error = polyline_or_error%map(to_polygon)

   print *, polygon_or_error

   polyline_or_error = parse_polyline(["1.0 2.0", "3.0 4.0", "5.0 6.0"])

   print *, polyline_or_error
end program example

! Idea; factory point() that returns point_or_error_t if requested for transparency?

