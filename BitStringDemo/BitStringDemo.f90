program bitstring_demo
    
    use bitstring_m
    
    implicit none
    
    type(bitstring_t) :: bs, bs2
    logical, dimension(:), allocatable :: larray
    integer :: i
    bs = make_zero_bitstring(35)
    write (*,*) 'Zero: ', bs
    call bs%set(1, 1)
    call bs%set(4, 1)
    call bs%set(34, 1)
    write (*,*) '1 in positions 1, 4 & 34: ', bs
    larray = bs%convert_to_logical_array()
    write (*,*) bs%convert_to_logical_array()
    larray(5) = .true.
    bs2 = make_bitstring_from_logical_array(larray(1:16))
    write (*,'(dt)') bs2
    
end program