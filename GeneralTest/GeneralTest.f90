program GeneralTest
    
    implicit none
    
    integer :: n
    
    n = -1
    n = ishft(n, -1)
    write (*,*) n
    n = ishft(n, -1)
    write (*,*) n
    n = ishft(n, -1)
    write (*,*) n
    n = -8
    n = mod(n, 3)
    write (*,*) '-8 mod 3', n
    
end program