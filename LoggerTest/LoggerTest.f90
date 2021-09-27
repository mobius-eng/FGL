#define LOGINFO
#include "logger_inc.f90"
program logger_test
    
    use logger_m
    implicit none
    
    _ERROR_(fmt('My error with value', 3.4, '!'))
    _DEBUG_('Debugging message')
    _INFO_(fmt('Info message with integer', 12, '.'))
    
end program