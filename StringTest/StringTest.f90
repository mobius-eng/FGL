#include "get_cmd_arg_inc.f90"
program StringTest
    
    use string_m
    
    implicit none
    
    interface
        ! dest is returned
        function strncpy(dest, source, num) bind (c, name = 'strncpy')
            use iso_c_binding
            type(c_ptr), value :: dest, source
            type(c_ptr) :: strncpy
            integer(c_size_t), value :: num
        end function
    end interface
    
    character(len=256) :: s1, s2
    type(c_ptr) :: cs1, cs2, ctmp
    integer(c_size_t) :: n
    
    s1 = 'Hello World!'
    cs1 = associate_c_string(s1, .true.)
    cs2 = associate_c_string(s2)
    n = len_trim(s1) + 10
    ctmp = strncpy(cs2, cs1, n)
    call rm_null_from_fstring(s2)
    write (*,*) trim(s2)
    
end program