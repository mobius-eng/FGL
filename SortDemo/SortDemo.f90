program sort_demo
    
    use sort_m
    
    implicit none
    
    integer, dimension(10) :: a = [3, 4, 1, 9, 7, 10, 5, 2, 8, 6]
    integer :: xx
    print *, a
    call sort_insert(a, xx, int_cmp)
    print *, a
contains

    integer function int_cmp(x, y)
        class(*), intent(in) :: x, y
        int_cmp = 0
        select type (xx => x)
        type is (integer)
            select type (yy => y)
            type is (integer)
                int_cmp = xx - yy
            end select
        end select
    end function
end program