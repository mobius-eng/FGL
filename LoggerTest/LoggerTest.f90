! Which messages are printed are controlled by
! definition of macros LOGINFO =< LOGDEBUG =< LOGERROR
! Here =< means " right is included in left"
! This definition needs to be done BEFORE including logger_inc.f90
#define LOGINFO
#include "logger_inc.f90"
program logger_test
    
    use logger_m
    implicit none
    
    _ERROR_(fmt('My error with value is ', 3.4, '!'))
    _DEBUG_('Debugging message')
    _INFO_(fmt('Info message with integer', 12, '.'))
    _DEBUG_IF_(3 > 1, "This debug message will be printed")
    _INFO_IF_(3 < 1, "This info message won't be printed")
    
end program