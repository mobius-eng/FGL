program queue_demo
    
    use queue_m
    
    implicit none
    
    integer :: dummy, n
    integer, parameter :: nint = sizeof(dummy)
    type(queue_t(nint)) :: queue
    
    call queue%init(10)
    call queue%push_back(1)
    call queue%push_back(2)
    call queue%pop_front(n)
    print *, n
    call queue%pop_front(n)
    print *, n
    call queue%push_back(3)
    call queue%peek_head(n)
    print *, n
    n = -10
    call queue%pop_front(n)
    print *, n
    
end program