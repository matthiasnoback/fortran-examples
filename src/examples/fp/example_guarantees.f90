module common_error_handling
   implicit none(type, external)
   private

   public :: error_t

   type :: error_t
      character(len=:), allocatable :: message
   end type error_t

end module common_error_handling

module geometry_point
   implicit none(type, external)

   private
   public :: point_t

   type :: point_t
      real :: x
      real :: y
   contains
   end type point_t

end module geometry_point

module geometry_polyline
   use common_error_handling, only: error_t
   use geometry_point, only: point_t

   implicit none(type, external)

   private
   public :: polyline_t
   public :: polyline_or_error_t
   public :: create_polyline

   type :: polyline_t
      private
      type(point_t), dimension(:), allocatable :: points
   contains
      procedure :: count_points
   end type polyline_t

   type polyline_or_error_t
      type(polyline_t), allocatable :: polyline
      type(error_t), allocatable :: error
   end type polyline_or_error_t

   interface error
      module procedure polyline_or_error_error
   end interface error

   interface polyline
      module procedure polyline_or_error_polyline
   end interface polyline
contains

   pure function create_polyline(points) result(res)
      type(point_t), dimension(:), intent(in) :: points
      type(polyline_or_error_t) :: res

      if (size(points) < 2) then
         res = error('A polyline must have at least two points.')
         return
      end if

      res = polyline(points)
   end function create_polyline

   pure function polyline_or_error_error(message) result (res)
      character(len=*), intent(in) :: message
      type(polyline_or_error_t) :: res

      res%error = error_t(message)
   end function polyline_or_error_error

   pure function polyline_or_error_polyline(points) result (res)
      type(point_t), dimension(:), intent(in) :: points
      type(polyline_or_error_t) :: res

      res%polyline = polyline_t(points)
   end function polyline_or_error_polyline

   pure function count_points(this) result(count)
      class(polyline_t), intent(in) :: this
      integer :: count

      count = size(this%points)
   end function count_points

end module geometry_polyline

program example
   use geometry_point, only: point_t
   use geometry_polyline, only: create_polyline, polyline_or_error_t, polyline_t

   implicit none(type, external)

   type(polyline_or_error_t), allocatable :: polyline

   type(polyline_t) :: bad_polyline
   print *, bad_polyline%count_points()

   polyline = create_polyline([point_t(0.0, 0.0)])

   if (allocated(polyline%error)) then
      print *, 'Error: ', polyline%error%message
   else
      print *, 'Polyline created successfully.'
   end if

   polyline = create_polyline([point_t(0.0, 0.0), point_t(1.0, 1.0)])

   if (allocated(polyline%error)) then
      print *, 'Error: ', polyline%error%message
   else
      print *, 'Polyline created successfully.'
   end if

end program example
