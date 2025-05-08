
module model
   use iso_fortran_env, only: wp => real64

   implicit none(type, external)

   private
   public :: waterlevel_at_station_t, fill_waterlevels, average, &
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

      allocate (waterlevels(timesteps*stations))

      do timestep = 1, timesteps
         do station = 1, stations
            call random_number(waterlevel)
            waterlevels((timestep - 1)*stations + station) = &
               waterlevel_at_station_t(timestep, station, waterlevel)
         end do
      end do
   end subroutine fill_waterlevels

   pure function average(numbers) result(res)
      real(kind=wp), dimension(:), intent(in) :: numbers
      real(kind=wp) :: res

      if (size(numbers) == 0) then
         res = 0.0_wp
      else
         res = sum(numbers)/size(numbers)
      end if
   end function average

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

program example
   use model, only: waterlevel_at_station_t, fill_waterlevels, &
                    average, filter, station_is_1, map_to_waterlevel
   use iso_fortran_env, only: wp => real64

   implicit none(type, external)

   type(waterlevel_at_station_t), dimension(:), allocatable :: waterlevels
   type(waterlevel_at_station_t), dimension(:), allocatable :: waterlevels_for_station_1, &
                                                               waterlevels_for_station_1_with_pack
   integer :: index
   integer :: timesteps

   ! For mean and standard deviation
   real(kind=wp) :: total
   integer :: number_of_observations
   real(kind=wp) :: mean
   real(kind=wp), dimension(:), allocatable :: differences
   real(kind=wp), dimension(:), allocatable :: waterlevels_for_mean

   ! For moving average
   integer :: start_index, end_index
   real(kind=wp), dimension(:), allocatable :: window
   real(kind=wp), dimension(:), allocatable :: moving_average, waterlevels_for_moving_average
   integer :: window_size

   timesteps = 10

   call fill_waterlevels(waterlevels, timesteps, stations=3)

   ! Step 1: wrap pack inside a filter to which we can pass a procedure pointer
   waterlevels_for_station_1 = filter(station_is_1, waterlevels)
   waterlevels_for_station_1_with_pack = pack(waterlevels, waterlevels%station_id == 1)
   print *, 'Filtering waterlevels is the same as using pack directly', &
      all((waterlevels_for_station_1%waterlevel - &
           waterlevels_for_station_1_with_pack%waterlevel) == 0.0_wp)
   total = 0.0_wp
   number_of_observations = 0
   ! Step 2: map list of waterlevel_for_station_t to list of real waterlevels
   waterlevels_for_mean = map_to_waterlevel(waterlevels_for_station_1)

   ! Note potential risk of dividing by 0
   mean = sum(waterlevels_for_mean)/size(waterlevels_for_mean)

   print *, 'Mean:', mean

   ! Note: repeating
   allocate (differences(timesteps))
   do index = 1, size(waterlevels)
      if (waterlevels(index)%station_id == 1) then
         differences(waterlevels(index)%timestep) = waterlevels(index)%waterlevel - mean
      end if
   end do

   print *, 'Standard deviation:', sqrt(sum(differences**2)/size(differences))

   allocate (moving_average(timesteps))
   allocate (waterlevels_for_moving_average(timesteps))

   do index = 1, size(waterlevels)
      if (waterlevels(index)%station_id == 1) then
         waterlevels_for_moving_average(waterlevels(index)%timestep) = waterlevels(index)%waterlevel
      end if
   end do

   window_size = 3
   do index = 1, size(waterlevels_for_moving_average)
      start_index = max(index - window_size, 1)
      end_index = min(index + window_size, size(waterlevels_for_moving_average))
      window = waterlevels_for_moving_average(start_index:end_index)
      moving_average(index) = average(window)
   end do
   print *, 'Moving average:', moving_average

end program example
