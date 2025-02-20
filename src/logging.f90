program logging
   use m_logging, only: t_abstract_logger, &
                        t_stdout_logger, &
                        t_file_logger, &
                        t_multi_logger, &
                        t_logger_reference
   use m_error, only: t_abstract_error
   use m_files, only: file_get_contents

   implicit none

   class(t_stdout_logger), allocatable :: stdout_logger
   class(t_file_logger), allocatable :: file_logger
   class(t_multi_logger), allocatable :: multi_logger
   class(t_logger_reference), allocatable :: reference

   stdout_logger = create_stdout_logger()
   call test_stdout_logger(stdout_logger)

   file_logger = create_file_logger('dev.log')
   call test_file_logger(file_logger)

   multi_logger = t_multi_logger([ &
                                 t_logger_reference(stdout_logger), &
                                 t_logger_reference(file_logger) &
                                 ])

   call test_multi_logger(multi_logger)

   call file_logger%terminate()

contains

   subroutine test_stdout_logger(logger)

      class(t_abstract_logger), intent(in) :: logger

      call logger%log('Should appear in STDOUT')

   end subroutine test_stdout_logger

   subroutine test_file_logger(logger)
      class(t_file_logger), intent(in) :: logger
      class(t_abstract_error), allocatable :: error

      character(len=:), allocatable :: log_contents

      call logger%log('Should appear in a file')

      call file_get_contents(logger%path, log_contents, error)
      print *, 'Current log file contents: '//log_contents
   end subroutine test_file_logger

   subroutine test_multi_logger(logger)
      class(t_abstract_logger), intent(in) :: logger

      call logger%log('Should appear in both the file and STDOUT')
   end subroutine test_multi_logger

   function create_stdout_logger() result(res)
      class(t_stdout_logger), allocatable :: res

      res = t_stdout_logger()
   end function create_stdout_logger

   function create_file_logger(path) result(res)
      character(len=*), intent(in) :: path
      type(t_file_logger), allocatable :: res
      class(t_abstract_error), allocatable :: error

      res = t_file_logger(path, error)
      if (allocated(error)) then
         error stop error%to_string()
      end if
   end function create_file_logger

end program logging
