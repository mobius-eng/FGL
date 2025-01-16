module sort_m
    
    use iso_c_binding, only : c_loc, c_f_pointer, c_ptr
    use, intrinsic :: iso_fortran_env, only : int8
    implicit none

    abstract interface
    
        function compare_fn(a, b) result(y)
            import c_ptr
            type(c_ptr), intent(in) :: a, b
            integer :: y
        
        end function
    
    end interface
    
contains
    
    subroutine sort_insert(items, item_size, cmp)
        type(*), dimension(:), target :: items
        integer, intent(in) :: item_size
        procedure(compare_fn) :: cmp
        
        integer :: i, j, nitems, iloc, jloc, ntot
        type(c_ptr) :: pitem, p1, p2
        integer(int8), dimension(:), pointer :: item_bits
        integer(int8), dimension(item_size), target :: tmp
        
        ! Get access to the storage of ITEM through BITS array
        nitems = size(items)
        ntot = nitems * item_size
        pitem = c_loc(items)
        call c_f_pointer(pitem, item_bits, [ntot])
        p2 = c_loc(tmp)
        
        ! At each iteration, items(1:(i-1)) is sorted
        outer: do i = 2, nitems
        
            iloc = (i-1)*item_size+1
            tmp(1:item_size) = item_bits(iloc:iloc - 1 + item_size)
            j = i
            
            inner: do while (j > 1)
            
                jloc = (j-1) * item_size + 1
                ! candidate is >= to the last element of the sorted array -- leave it
                p1 = c_loc(items(j-1))
                if (cmp(p1, p2) <= 0) exit inner
                
                ! Shift ITEMS(J) one position towards the end
                item_bits(jloc: jloc - 1 + item_size) = item_bits(jloc - item_size:jloc-1)
                ! Try next J
                j = j - 1
            end do inner
            
            jloc = (j-1) * item_size + 1
            ! need to place TMP into items J
            item_bits(jloc:jloc-1+item_size) = tmp(1:item_size)
            
        end do outer
        
    end subroutine
    
end module