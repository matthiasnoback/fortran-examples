
module model
   use iso_fortran_env, only: wp => real64

   implicit none(type, external)

   private
   public :: waterlevel_at_station_t, fill_waterlevels, &
             filter, station_is_1, map_to_waterlevel

   type :: waterlevel_at_station_t
      integer :: timestep
      integer :: station_id
      real(kind=wp) :: waterlevel
   end type waterlevel_at_station_t

   interface filter
      module procedure :: filter_waterlevels_at_station
   end interface

   interface
      pure function filter_waterlevel_at_station(item) result(res)
         import waterlevel_at_station_t

         implicit none(type, external)

         type(waterlevel_at_station_t), intent(in) :: item
         logical :: res
      end function filter_waterlevel_at_station
   end interface
contains
   subroutine fill_waterlevels(waterlevels, timesteps, stations)
      type(waterlevel_at_station_t), dimension(:), allocatable, intent(inout) :: waterlevels
      integer, intent(in) :: timesteps
      integer, intent(in) :: stations

      integer :: timestep, station
      real(kind=wp) :: waterlevel

      call random_seed()

      allocate (waterlevels(timesteps * stations))

      do timestep = 1, timesteps
         do station = 1, stations
            call random_number(waterlevel)
            waterlevels((timestep - 1) * stations + station) = &
               waterlevel_at_station_t(timestep, station, waterlevel)
         end do
      end do
   end subroutine fill_waterlevels

   pure function station_is_1(waterlevel_at_station) result(res)
      type(waterlevel_at_station_t), intent(in) :: waterlevel_at_station
      logical :: res
      res = waterlevel_at_station%station_id == 1
   end function station_is_1

   pure function filter_waterlevels_at_station(filter_func, list) result(filtered)
      procedure(filter_waterlevel_at_station), pointer, intent(in) :: filter_func
      type(waterlevel_at_station_t), dimension(:), intent(in) :: list
      type(waterlevel_at_station_t), dimension(:), allocatable :: filtered
      integer :: i

      filtered = pack(list, [(filter_func(list(i)), i=1, size(list))])
   end function filter_waterlevels_at_station

   pure elemental function map_to_waterlevel(waterlevel_at_station) result(waterlevel)
      type(waterlevel_at_station_t), intent(in) :: waterlevel_at_station
      real(kind=wp) :: waterlevel
      waterlevel = waterlevel_at_station%waterlevel
   end function map_to_waterlevel

end module model

module reduce
   use iso_fortran_env, only: wp => real64

   implicit none(type, external)

   private
   public :: foldl, sum_two_reals_wp, difference_from_mean, average

   interface foldl
      module procedure foldl_real_wp
   end interface

   interface
      pure function real_fold_function(left, right) result(res)
         import wp

         implicit none(type, external)

         real(kind=wp), intent(in) :: left
         real(kind=wp), intent(in) :: right
         real(kind=wp) :: res
      end function real_fold_function
   end interface
contains

   pure recursive function foldl_real_wp(f, start, x) result(res)
      procedure(real_fold_function) :: f !! Folding function
      real(kind=wp), intent(in) :: start !! Accumulator start value
      real(kind=wp), dimension(:), intent(in) :: x !! Input array
      real(kind=wp) :: res
      if (size(x) < 1) then
         res = start
      else
         res = foldl_real_wp(f, f(start, x(1)), x(2:))
      end if
   end function foldl_real_wp

   pure function sum_two_reals_wp(left, right) result(res)
      implicit none(type, external)

      real(kind=wp), intent(in) :: left
      real(kind=wp), intent(in) :: right
      real(kind=wp) :: res

      res = left + right
   end function sum_two_reals_wp

   pure function sum_reals_wp(reals) result(sum)
      real(kind=wp), dimension(:), intent(in) :: reals
      real(kind=wp) :: sum

      sum = foldl(sum_two_reals_wp, 0.0_wp, reals)
   end function sum_reals_wp

   pure elemental function difference_from_mean(value, mean) result(difference)
      real(kind=wp), intent(in) :: value
      real(kind=wp), intent(in) :: mean
      real(kind=wp) :: difference

      difference = value - mean
   end function difference_from_mean

   pure function average(numbers) result(res)
      real(kind=wp), dimension(:), intent(in) :: numbers
      real(kind=wp) :: res

      if (size(numbers) == 0) then
         res = 0.0_wp
      else
         res = sum(numbers) / size(numbers)
      end if
   end function average

end module reduce

module statistics
   use iso_fortran_env, only: wp => real64
   use reduce, only: foldl, sum_two_reals_wp, difference_from_mean, average

   implicit none(type, external)

   private
   public :: mean_and_standard_deviation, stddev_t, moving_average_t, map

   interface map
      procedure :: map_integers_to_reals
   end interface map

   type :: stddev_t
      real(kind=wp) :: mean
      real(kind=wp) :: standard_deviation
   end type stddev_t

   type :: moving_average_t
      real(kind=wp), dimension(:), allocatable :: samples
      integer :: window_size
   contains
      procedure :: moving_average => moving_average_moving_average
      procedure :: window_for_index => window_for_index
      procedure :: map_to_indices => moving_average_map_to_indices
   end type moving_average_t

   interface
      pure function integer_to_real(value) result(res)
         import wp

         implicit none(type, external)
         integer, intent(in) :: value
         real(kind=wp) :: res
      end function integer_to_real
   end interface

contains

   pure function map_integers_to_reals(f, x) result(res)
      procedure(integer_to_real), pointer, intent(in) :: f
      integer, dimension(:), intent(in) :: x
      real(kind=wp), dimension(size(x)) :: res

      integer :: i

      res = [(f(x(i)), i=1, size(x))]
   end function map_integers_to_reals

   pure function curried_window_average(samples) result(f)
      real(kind=wp), dimension(:), intent(in) :: samples
      procedure(integer_to_real), pointer :: f

      f => window_average(samples)
   end function curried_window_average

   pure function window_average(samples) result(f)
      real(kind=wp), dimension(:), intent(in) :: samples

      interface
         pure function f(index) result(res)
            import wp
            implicit none(type, external)
            integer, intent(in) :: index
            real(kind=wp) :: res
         end function f
      end interface

      f => average_for_index
   contains
      pure function average_for_index(index) result(res)
         integer, intent(in) :: index
         integer :: start_index, end_index
         real(kind=wp), dimension(:), allocatable :: window
         real(kind=wp) :: res

         start_index = max(current_index - 3, 1)
         end_index = min(current_index + 3, size(samples))
         window = samples(start_index:end_index)
         res = average(window)
      end function average_for_index

   end function window_average

   pure elemental function moving_average_moving_average(self, index) result(result)
      class(moving_average_t), intent(in) :: self
      integer, intent(in) :: index
      real(kind=wp) :: result

      result = average(self%window_for_index(index))
   end function moving_average_moving_average

   pure function window_for_index(self, current_index) result(window)
      class(moving_average_t), intent(in) :: self
      integer, intent(in) :: current_index
      integer :: start_index, end_index
      real(kind=wp), dimension(:), allocatable :: window

      start_index = max(current_index - self%window_size, 1)
      end_index = min(current_index + self%window_size, size(self%samples))
      window = self%samples(start_index:end_index)
   end function window_for_index

   function moving_average_map_to_indices(self) result(indices)
      class(moving_average_t), intent(in) :: self
      integer, dimension(:), allocatable :: indices

      integer :: i

      indices = [(i, i=1, size(self%samples))]
   end function moving_average_map_to_indices

   ! Note: not pure, but should be! No IO needed, but error stop is used
   function mean_and_standard_deviation(samples) result(res)
      real(kind=wp), dimension(:), intent(in) :: samples
      type(stddev_t) :: res

      if (size(samples) == 0) then
         error stop "Expected size of samples to be > 0"
      end if

      res%mean = foldl(sum_two_reals_wp, 0.0_wp, samples) / size(samples)
      res%standard_deviation = sqrt(sum(difference_from_mean(samples, res%mean)**2) / &
                                    size(samples))
   end function mean_and_standard_deviation

end module statistics

program example
   use model, only: waterlevel_at_station_t, fill_waterlevels, &
                    filter, station_is_1, map_to_waterlevel
   use iso_fortran_env, only: wp => real64
   use reduce, only: foldl, sum_two_reals_wp, difference_from_mean, average
   use statistics, only: mean_and_standard_deviation, stddev_t, moving_average_t, map

   implicit none(type, external)

   type(waterlevel_at_station_t), dimension(:), allocatable :: waterlevels

   integer :: index
   integer :: timesteps

   ! For mean and standard deviation
   type(stddev_t), allocatable :: new_stddev

   ! For moving average
   type(moving_average_t) :: new_moving_average

   integer :: start_index, end_index
   real(kind=wp), dimension(:), allocatable :: window
   real(kind=wp), dimension(:), allocatable :: moving_average, waterlevels_for_moving_average
   integer :: window_size

   timesteps = 10

   call fill_waterlevels(waterlevels, timesteps, stations=3)

   ! Step 6: Clean up, get rid of most of the local and intermediate variables
   new_stddev = mean_and_standard_deviation(map_to_waterlevel(filter(station_is_1, waterlevels)))
   print *, 'Standard deviation:', new_stddev%standard_deviation
   print *, 'Mean:', new_stddev%mean

   allocate (moving_average(timesteps))
   allocate (waterlevels_for_moving_average(timesteps))

   ! Note: repeated filtering and mapping
   do index = 1, size(waterlevels)
      if (waterlevels(index)%station_id == 1) then
         waterlevels_for_moving_average(waterlevels(index)%timestep) = waterlevels(index)%waterlevel
      end if
   end do

   new_moving_average = moving_average_t(map_to_waterlevel(filter(station_is_1, waterlevels)), 3)
   print *, 'Moving average:', &
      new_moving_average%moving_average(new_moving_average%map_to_indices())
   print *, 'Moving average with map:', &
      map(new_moving_average%moving_average, new_moving_average%map_to_indices())

   window_size = 3
   do index = 1, size(waterlevels_for_moving_average)
      start_index = max(index - window_size, 1)
      end_index = min(index + window_size, size(waterlevels_for_moving_average))
      window = waterlevels_for_moving_average(start_index:end_index)
      moving_average(index) = average(window)
   end do
   print *, 'New moving average is same as old moving average:', &
      all(moving_average == new_moving_average%moving_average(new_moving_average%map_to_indices()))

end program example
