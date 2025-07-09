program error_test
    
use error_m
implicit none

character(len=64) :: msg

call set_error(IO_OUTPUT_ERROR, 'Some other error')
msg = get_last_error_message()
write (*, '(A, I8, TR1, A, A)') 'Error set to IO_OUTPUT_ERROR = ', io_output_error, 'with message: ', 'Some error'
write (*, '(A)') 'Recorded error:'
write (*,'(I8, TR1,A)') get_last_error_code(), trim(msg)

call release_error()
write (*, '(A)') 'The error has been released. Checking records:'
msg = get_last_error_message()
write (*,'(I8, TR1, A)') get_last_error_code(), msg

end program