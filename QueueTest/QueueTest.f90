program queue_demo
    
    use queue_m
    
    implicit none
    
    integer :: n
    integer, parameter :: nint = sizeof(n)
    type(queue_t(nint)) :: queue
    
    call queue%init(10)
    call queue%push_back(1)
    call queue%push_back(2)
    call queue%pop_front(n)
    print *, 'Pushed to back 1, 2, popped from front: ', n
    call queue%pop_front(n)
    print *, 'Popped from front: ', n
    call queue%push_back(3)
    call queue%peek_head(n)
    print *, 'Pushed back 3, peek head: ', n
    n = -10
    call queue%pop_front(n)
    print *, 'Popped from front: ', n
    
end program