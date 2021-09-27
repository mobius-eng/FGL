module sort_m
    
    use iso_c_binding
    implicit none

contains
    
    subroutine sort_insert(items, item, cmp)
        class(*), dimension(:), target :: items
        class(*), intent(inout), target :: item
        interface
            integer function cmp(x, y)
                class(*), intent(in) :: x, y
            end function
        end interface
        integer :: i, j, nitems, n, iloc
        type(c_ptr) :: pitem
        integer(c_int8_t), dimension(:), pointer :: item_bits, xbits
        ! Get access to the storage of ITEM through BITS array
        n = sizeof(item)
        pitem = c_loc(item)
        call c_f_pointer(pitem, item_bits, [n])
        nitems = size(items)
        do i = 2, nitems
            item_bits(1:n) = transfer(items(i), item_bits)
            iloc = i
            j = i - 1
            inner: do while (j >= 1)
                if (cmp(items(j), item) <= 0) exit inner
                ! Update ITEM candidate position ILOC
                iloc = j
                ! Shift ITEMS(J) one position backwards
                pitem = c_loc(items(j+1))
                call c_f_pointer(pitem, xbits, [n])
                xbits(1:n) = transfer(items(j), xbits)
                ! Try next J
                j = j - 1
            end do inner
            ! At this point ILOC is the position of ITEM in partially sorted array
            ! Write ITEM into this position
            pitem = c_loc(items(iloc))
            call c_f_pointer(pitem, xbits, [n])
            xbits(1:n) = transfer(item, xbits)
        end do
    end subroutine
    
end module