module m_logging

   implicit none

   logical :: debug
   logical :: quiet
   logical :: should_log_to_file
   integer :: log_file

contains
   subroutine init_logging()
      integer :: open_status
      ! We could read this from CLI arguments
      ! --debug, --quiet, and --log-file
      debug = .true.
      quiet = .false.
      open (file='debug.log', newunit=log_file, status='unknown', &
            position='append', action='write', iostat=open_status)
      should_log_to_file = open_status == 0
   end subroutine init_logging

   function current_time() result(iso_time)
      character(len=8) :: date
      character(len=10) :: time
      character(len=5) :: zone
      character(len=26) :: iso_time

      call date_and_time(date, time, zone)

      iso_time = trim(date(1:4))//'-'//trim(date(5:6))//'-'//trim(date(7:8))//'T'// &
                 trim(time(1:2))//':'//trim(time(3:4))//':'//trim(time(5:6))//zone
   end function current_time
end module m_logging

program example
   use m_logging, only: debug, quiet, should_log_to_file, log_file, init_logging, current_time

   implicit none

   call init_logging()

   if (debug) then
      if (should_log_to_file) then
         write (log_file, fmt=*) current_time()//'[config] Loaded'
      end if

      if (.not. quiet) then
         print *, current_time()//'[config] Loaded'
      end if
   end if

end program example
