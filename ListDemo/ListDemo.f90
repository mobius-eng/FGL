program ListDemo
    
    use list_m
    
    implicit none
    
    type point_t
        real :: x, y
    end type
    
    integer, parameter :: point_size = sizeof(point_t(0.0, 0.0))
    
    type(list_t(point_size)) :: lst
    type(point_t) :: p, q
    integer :: i
    
    call lst%init(10)
    
    write (*,*) 'List capacity', lst%capacity(), 'List size', lst%size()
    
    do i = 1, 15
        p%x = i * 1.0
        p%y = i * 2.0
        call lst%push(p)
        write (*,*) 'Pushed', i, 'Capacity', lst%capacity(), 'Size', lst%size()
    end do
    
    do i = 1, 15
        call lst%get(i, q)
        write (*,*) 'Get', i, q%x, q%y
    end do
    
    do i = 1, 15
        call lst%pop(q)
        write (*,*) 'Pop', i, q%x, q%y, 'Size', lst%size()
    end do
        
end program