#include "get_cmd_arg_inc.f90"
PROGRAM cmd_arg_demo

IMPLICIT NONE

INTEGER, PARAMETER :: wp = selected_real_kind(15)

REAL(wp) :: x, y
INTEGER :: n, iostat

NAMELIST /params/ x, y, n

! run PROGRAM as: cmdarg_test x = 1.5 y = 2.4 n = 30
_GET_CMD_ARGS_(params, iostat)

WRITE (*, '(T20, A)') 'FGL: Parse command arguments using namelist'
WRITE (*, '(A,/)') REPEAT('=', 80)
WRITE (*, '(T2, A,/)') 'Run program as cmdarg_test x= 1.5 y = 2.5 n = 30'

IF (iostat == 0) THEN
  WRITE(*, '(T2, A)') 'Arguments were parsed correctly'
  WRITE (*, '(T4, A, F8.2, A, ES9.2, A, I6)') 'X = ', x, ', Y = ', y, ', N = ', n
ELSE
  WRITE (*,*) 'Error in arguments parsing'
END IF

END PROGRAM