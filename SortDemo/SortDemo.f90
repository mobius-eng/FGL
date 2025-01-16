program sort_demo
    
    use, intrinsic :: iso_c_binding, only : c_ptr, c_f_pointer
    use sort_m
    
    implicit none
    
    integer, dimension(10) :: a = [3, 4, 1, 9, 7, 10, 5, 2, 8, 6]
    integer :: xx
    print *, a
    call sort_insert(a, storage_size(a(1)) / 8, int_cmp)
    print *, a
contains

    integer function int_cmp(x, y)
        
        type(c_ptr), intent(in) :: x, y        
        
        integer, pointer :: px, py
        
        int_cmp = 0

        call c_f_pointer(x, px)
        call c_f_pointer(y, py)
        
        int_cmp = px - py
        
    end function

end program