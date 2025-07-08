module hashtable_m
    
    use ustorage_m
    use hash_m
    
    implicit none
    
    type hashtable_entry_t(slen, dlen)
        integer, len :: slen, dlen
        logical :: is_occupied
        character(len=slen) :: key
        integer(ihash) :: hash
        type(ustorage_t(dlen)) :: value
    contains
        procedure, pass(self) :: clear => hashtable_entry_clear
    end type
    
    integer, parameter ::   hashtable_default_size = 1024,  &
        &                   hashtable_default_seed = 1
    real, parameter ::      hashtable_default_growth = 1.5
    
    type hashtable_t(slen, dlen)
        integer, len :: slen, dlen
        integer(ihash) :: seed
        real :: growth_factor
        integer :: size
        type(hashtable_entry_t(slen, dlen)), dimension(:), allocatable :: entries
    contains
        procedure, pass(self) :: init => hashtable_init
        procedure, pass(self) :: set => hashtable_set
        procedure, pass(self) :: get => hashtable_get
        procedure, pass(self) :: grow => hashtable_grow
    end type
    
contains
    
    subroutine hashtable_entry_clear(self)
        class(hashtable_entry_t(*,*)), intent(inout) :: self
        self%is_occupied = .false.
        self%key = ''
        self%hash = 0
    end subroutine
    
    subroutine hashtable_init(self, init_size, seed, growth_factor)
        class(hashtable_t(*,*)), intent(inout) :: self
        integer, intent(in), optional :: init_size
        integer(ihash), intent(in), optional :: seed
        real, intent(in), optional :: growth_factor
        type(hashtable_entry_t(self%slen, self%dlen)) :: dummy_entry
        integer :: n, i
        integer(ihash) :: s
        real :: f
        n = hashtable_default_size
        s = hashtable_default_seed
        f = hashtable_default_growth
        if (present(init_size)) n = init_size
        if (present(seed)) s = seed
        if (present(growth_factor)) f = growth_factor
        self%seed = s
        self%growth_factor = f
        self%size = 0
        allocate(self%entries(n), source = dummy_entry)
        do i = 1, n
            call self%entries(i)%clear()
        end do
    end subroutine
    
    subroutine hashtable_grow(self)
        class(hashtable_t(*,*)), intent(inout) :: self
        integer :: capacity, new_capacity, i, j
        type(hashtable_entry_t(self%slen, self%dlen)), dimension(:), allocatable :: temp
        capacity = size(self%entries)
        new_capacity = int(capacity * self%growth_factor)
        call move_alloc(self%entries, temp)
        call self%init(init_size = new_capacity, seed = self%seed, growth_factor = self%growth_factor)
        ! Find new places for each item
        do i = 1, capacity
            j = hashtable_find_position(self, temp(i)%hash, temp(i)%key)
            self%entries(j) = temp(i)
        end do
    end subroutine
    
    recursive integer function hashtable_find_position(self, hash_key, key) result(i)
        class(hashtable_t(*,*)), intent(in) :: self
        integer, intent(in) :: hash_key
        character(len=*), intent(in) :: key
        integer :: capacity, istart, j
        capacity = size(self%entries)
        ! Keep track of 0-based indices -- easier this way
        istart = mod(hash_key, capacity)
        ! We know that we won't besearching for more than capacity times
        searching: block
            do j = 0, capacity-1
                i = modulo(istart+j, capacity)
                if (.not. self%entries(i+1)%is_occupied) exit searching
                if (self%entries(i+1)%hash == hash_key) then
                    if (self%entries(i+1)%key == key) exit searching
                end if
            end do
            ! Only get here if couldn't find the index
            i = -1
            return
        end block searching
        ! Get here if found the index
        ! Make it 1-based index
        i = i + 1
    end function
    
    subroutine hashtable_set(self, key, value)
        class(hashtable_t(*,*)), intent(inout) :: self
        character(len=*), intent(in) :: key
        type(*), intent(in) :: value
        ! integer, intent(out) :: err
        integer(ihash) :: key_hash
        integer :: ipos
        ! err = 0
        key_hash = hash(key, self%seed)
        ipos = hashtable_find_position(self, key_hash, key)
        if (ipos < 1) then
            ! hashtable is full
            call self%grow()
            ipos = hashtable_find_position(self, key_hash, key)
        end if
        if (.not. self%entries(ipos)%is_occupied) self%size = self%size + 1
        self%entries(ipos)%is_occupied = .true.
        self%entries(ipos)%hash = key_hash
        self%entries(ipos)%key = key
        call self%entries(ipos)%value%store(value)
    end subroutine
    
    subroutine hashtable_get(self, key, value, err)
        class(hashtable_t(*,*)), intent(in) :: self
        character(len=*), intent(in) :: key
        type(*), intent(inout) :: value
        integer, intent(out) :: err
        integer(ihash) :: key_hash
        integer :: ipos
        err = 0
        key_hash = hash(key, self%seed)
        ipos = hashtable_find_position(self, key_hash, key)
        if (ipos > 1) then
            if (self%entries(ipos)%is_occupied) then
                call self%entries(ipos)%value%retrieve(value)
            else
                err = -1
            end if
        else
            err = -1
        end if
    end subroutine
    
    
    
end module
    