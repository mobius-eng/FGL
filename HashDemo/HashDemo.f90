program HashDemo
    
    use hashtable_m
    use point_m

    implicit none
    
    character(len=64) :: str
    type(point_t) :: p, q
    type(point_entry_t) :: pe
    class(hashtable_entry_t), pointer :: ppe
    type(point_key_t) :: k
    type(hashtable_t) :: htbl
    integer :: htbl_err
    
    write (*,*) 'Hashtable demo'
    htbl = hashtable_create(entry = pe, key = k, init_size = 10)
    
    p%x = 1.0
    p%y = 2.0
    str = 'Point_1'
    call htbl%set(str, point_entry_create(p))
    p%x = 10.0
    p%y = 20.0
    str = 'Point_2'
    call htbl%set(str, point_entry_create(p))
    ppe => null()
    ppe => htbl%get('Point_1')
    if (.not. associated(ppe)) then
        write (*,*) 'Hashtable GET error Point_1'
    else
        select type(x => ppe)
            type is (point_entry_t)
                write (*,*) 'Point_1', x%p
        end select
    end if
    ppe => null()
    ppe => htbl%get('Point_2')
    if (.not. associated(ppe)) then
        write (*,*) 'Hashtable GET error Point_2'
    else
        select type(x => ppe)
            type is (point_entry_t)
                pe = ppe
        end select
        write (*,*) 'Point_2', pe%p
    end if
    ppe => null()
    ppe => htbl%get('Point_3')
    if (.not. associated(ppe)) then
        write (*,*) 'Hashtable GET error Point_3'
    else
        select type(x => ppe)
            type is (point_entry_t)
                pe = ppe
        end select
        write (*,*) 'Point_3', pe%p
    end if    
    
end program