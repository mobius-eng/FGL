#include "get_cmd_arg_inc.f90"
program cmd_arg_demo

implicit none

integer, parameter :: wp = selected_real_kind(15)

real(wp) :: x, y
integer :: n, iostat

namelist /params/ x, y, n

! run program as: cmdarg_test x = 1.5 y = 2.4 n = 30
_GET_CMD_ARGS_(params, iostat)

if (iostat == 0) then
  write (*, *) x, y, n
else
   write (*,*) 'Error in arguments parsing'
end if

end program