program exception_test
    
    use error_m
    implicit none
    
    type, extends(exception) :: myerror
        real :: x
    end type
    
    type(myerror) :: merr
    character(len=64) :: msg
    real x
    class(exception), pointer :: perr
    
    call set_error(IO_OUTPUT_ERROR, 'Some other error')
    call get_last_error_message(msg)
    write (*,*) get_last_error_code(), trim(msg)
    merr = myerror(VAL_DOMAIN_ERROR, 'Some error', 1.0)
    call set_error(merr)
    call get_last_error_message(msg)
    call get_last_error_object(perr)
    select type(perr)
        type is (exception)
            write (*,*) 'Only exception type is detected'
        type is (myerror)
            write (*,*) 'myerror type is detected', perr%x
    end select
    write (*,*) get_last_error_code(), trim(msg)
    call release_error()
    write (*,*) get_last_error_code()
    
end program