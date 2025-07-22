module test_pump
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use hydraulic_structure_pump, only: next_pump_state, dp, pump_specification_t, pump_state_t

   implicit none(type, external)

   private
   public :: collect_pump_tests

   interface check
      procedure :: check_pump_state, check_pump_state_and_discharge
   end interface

contains

   subroutine check_pump_state(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      type(pump_state_t), intent(in) :: actual
      type(pump_state_t), intent(in) :: expected

      call check(error, actual%discharge, expected%discharge)
      if (allocated(error)) then
         return
      end if

      call check(error, actual%is_running, expected%is_running)
      if (allocated(error)) then
         return ! optional, if it's the last call to `check`
      end if
   end subroutine check_pump_state

   subroutine check_pump_state_and_discharge(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      type(pump_state_t), intent(in) :: actual
      real(kind=dp), intent(in) :: expected

      call check(error, actual%discharge, expected)
   end subroutine check_pump_state_and_discharge

   !> Collect all exported unit tests
   subroutine collect_pump_tests(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_discharge_of_a_pump_that_is_turned_off", &
                               test_discharge_of_a_pump_that_is_turned_off), &
                  new_unittest("test_discharge_of_a_pump_that_is_always_on", &
                               test_discharge_of_a_pump_that_is_always_on), &
                  new_unittest("test_water_level_suction_side_is_at_start_level", &
                               test_water_level_suction_side_is_at_start_level), &
                  new_unittest("test_water_level_suction_side_is_below_start_level", &
                               test_water_level_suction_side_is_below_start_level), &
                  new_unittest("test_water_level_suction_side_is_above_start_level", &
                               test_water_level_suction_side_is_above_start_level), &
                  new_unittest("test_water_level_suction_side_is_at_stop_level", &
                               test_water_level_suction_side_is_at_stop_level), &
                  new_unittest("test_water_level_suction_side_is_below_stop_level", &
                               test_water_level_suction_side_is_below_stop_level), &
                  new_unittest("test_water_level_suction_side_is_above_stop_level", &
                               test_water_level_suction_side_is_above_stop_level), &
                  new_unittest("test_water_level_suction_side_is_between_start_and_stop_level", &
                               test_water_level_suction_side_is_between_start_and_stop_level) &
                  ]
   end subroutine collect_pump_tests

   subroutine test_discharge_of_a_pump_that_is_turned_off(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      type(pump_state_t) :: previous_state

      pump_specification = pump_specification_t(0.0_dp, 0.0_dp, 0.0_dp)

      call check(error, next_pump_state(pump_specification, previous_state, 0.0_dp), 0.0_dp)
   end subroutine test_discharge_of_a_pump_that_is_turned_off

   subroutine test_discharge_of_a_pump_that_is_always_on(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      type(pump_state_t) :: previous_state

      pump_specification = pump_specification_t(10.0_dp, 0.0_dp, 0.0_dp)

      call check(error, next_pump_state(pump_specification, previous_state, 0.0_dp), 10.0_dp)
   end subroutine test_discharge_of_a_pump_that_is_always_on

   subroutine test_water_level_suction_side_is_at_start_level(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump
      type(pump_state_t) :: previous_state

      pump%capacity = 10.0_dp
      pump%water_level_start = 2.0_dp

      call check(error, next_pump_state(pump, previous_state, 2.0_dp), 10.0_dp)
   end subroutine test_water_level_suction_side_is_at_start_level

   pure function pump_with_capacity(capacity) result(pump)
      real(kind=dp), intent(in) :: capacity
      type(pump_specification_t) :: pump
      pump%capacity = capacity
   end function pump_with_capacity

   subroutine pump_start_at(pump, start_level)
      type(pump_specification_t), intent(inout) :: pump
      real(kind=dp), intent(in) :: start_level

      pump%water_level_start = start_level
      pump%water_level_stop = 0.0_dp ! irrelevant
   end subroutine pump_start_at

   subroutine test_water_level_suction_side_is_below_start_level(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      type(pump_state_t) :: previous_state

      pump_specification = pump_specification_t(10.0_dp, 2.0_dp, 0.0_dp)

      call check(error, next_pump_state(pump_specification, previous_state, 1.0_dp), 0.0_dp)
   end subroutine test_water_level_suction_side_is_below_start_level

   subroutine test_water_level_suction_side_is_above_start_level(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      type(pump_state_t) :: previous_state

      pump_specification = pump_specification_t(10.0_dp, 2.0_dp, 0.0_dp)

      call check(error, next_pump_state(pump_specification, previous_state, 3.0_dp), 10.0_dp)
   end subroutine test_water_level_suction_side_is_above_start_level

   subroutine test_water_level_suction_side_is_at_stop_level(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      type(pump_state_t) :: previous_state

      pump_specification = pump_specification_t(10.0_dp, 2.0_dp, 1.0_dp)

      call check(error, next_pump_state(pump_specification, previous_state, 1.0_dp), 0.0_dp)
   end subroutine test_water_level_suction_side_is_at_stop_level

   subroutine test_water_level_suction_side_is_below_stop_level(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      type(pump_state_t) :: previous_state

      pump_specification = pump_specification_t(10.0_dp, 2.0_dp, 1.0_dp)

      call check(error, next_pump_state(pump_specification, previous_state, 0.5_dp), 0.0_dp)
      call check(error, next_pump_state(pump_specification, previous_state, 0.5_dp), &
                 pump_state_t(0.0_dp, .false.))
   end subroutine test_water_level_suction_side_is_below_stop_level

   subroutine test_water_level_suction_side_is_between_start_and_stop_level(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      type(pump_state_t) :: previous_state
      previous_state%is_running = .true.

      pump_specification = pump_specification_t(10.0_dp, 2.0_dp, 1.0_dp)

      call check(error, next_pump_state(pump_specification, previous_state, 1.5_dp), 10.0_dp)
   end subroutine test_water_level_suction_side_is_between_start_and_stop_level

   subroutine test_water_level_suction_side_is_above_stop_level(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      type(pump_state_t) :: previous_state

      pump_specification = pump_specification_t(10.0_dp, 2.0_dp, 1.0_dp)

      call check(error, next_pump_state(pump_specification, previous_state, 2.0_dp), 10.0_dp)
   end subroutine test_water_level_suction_side_is_above_stop_level

end module test_pump
