! Inspired by https://zmoon.github.io/FortranTipBrowser/tips/049.html
program performance

   use iso_fortran_env, only: wp => real64, int64
   use m_stopwatch_facade, only: stopwatch_start, stopwatch_end, stopwatch_print_all

   implicit none

   integer, parameter :: m = 10000, n = m
   integer :: array(m, n)
   integer :: i, j, tot

   array = 0

   ! FASTER: Looping consecutively through columns
   call stopwatch_start('Array loop column-first')
   tot = 0
   do j = 1, size(array, dim=2)
      do i = 1, size(array, dim=1)
         tot = tot + array(i, j)
      end do
   end do
   call stopwatch_end('Array loop column-first')

   ! SLOWER: Looping consecutively through rows
   call stopwatch_start('Array loop row-first')

   tot = 0
   do i = 1, size(array, dim=1)
      do j = 1, size(array, dim=2)
         tot = tot + array(i, j)
      end do
   end do
   call stopwatch_end('Array loop row-first')

   call stopwatch_print_all()

end program performance
