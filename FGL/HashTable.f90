MODULE hashtable_m

USE ustorage_m
USE hash_m

IMPLICIT NONE
PRIVATE
PUBLIC :: hashtable_t, hashtable_entry_t

TYPE hashtable_entry_t(slen, dlen)
  INTEGER, len           :: slen, dlen
  LOGICAL                :: filled
  ! Small hack as generic length on CHARACTER doesn't work in ifx
  ! NB. It used to work in ifort
  ! But it does work with an array
  CHARACTER(1)           :: key(1:slen)
  ! Actual length of the key (slen is max length)
  INTEGER                :: nkey
  INTEGER(ihash)         :: hash
  TYPE(ustorage_t(dlen)) :: value
  CONTAINS
  PROCEDURE, PASS(self)  :: clear        => hashtable_entry_clear
  PROCEDURE, PASS(self)  :: cmp_key_char => hashtable_entry_cmp_key_char
  PROCEDURE, PASS(self)  :: cmp_key_arr  => hashtable_entry_cmp_key_arr
  GENERIC                :: cmp_key      => cmp_key_char, cmp_key_arr
END TYPE

INTEGER, parameter ::   hashtable_default_size = 1024,  &
    &                   hashtable_default_seed = 1
REAL, parameter ::      hashtable_default_growth = 1.5

TYPE hashtable_t(slen, dlen)
  INTEGER, len :: slen, dlen
  INTEGER(ihash) :: seed
  REAL :: growth_factor
  INTEGER :: size
  TYPE(hashtable_entry_t(slen, dlen)), ALLOCATABLE :: entries(:)
  CONTAINS
  PROCEDURE, PASS(self) :: init => hashtable_init
  PROCEDURE, PASS(self) :: set => hashtable_set
  PROCEDURE, PASS(self) :: get => hashtable_get
  PROCEDURE, PASS(self) :: grow => hashtable_grow
  PROCEDURE, PASS(self) :: find_pos_char => hashtable_find_position_char
  PROCEDURE, PASS(self) :: find_pos_arr => hashtable_find_position_arr
  GENERIC :: find_position => find_pos_arr, find_pos_char
END TYPE



CONTAINS


SUBROUTINE hashtable_entry_clear(self)
  CLASS(hashtable_entry_t(*,*)), INTENT(INOUT) :: self
  self%filled = .false.
  self%key(:) = ''
  self%nkey   = 0
  self%hash   = 0
END SUBROUTINE

FUNCTION hashtable_entry_cmp_key_char(self, key) RESULT(iseq)
  CLASS(hashtable_entry_t(*,*)), INTENT(IN) :: self
  CHARACTER(*), INTENT(IN) :: key
  LOGICAL :: iseq

  INTEGER :: n, i

  n = len_trim(key)
  iseq = .FALSE.

  IF (self%filled .AND. n == self%nkey) THEN
    DO i = 1, n
      IF (self%key(i) /= key(i:i)) RETURN
    END DO

    iseq = .TRUE.

  END IF

END FUNCTION

FUNCTION hashtable_entry_cmp_key_arr(self, key, nkey) RESULT(iseq)
  CLASS(hashtable_entry_t(*,*)), INTENT(IN) :: self
  CHARACTER(1), INTENT(IN) :: key(:)
  INTEGER, INTENT(IN)      :: nkey
  LOGICAL                  :: iseq

  INTEGER                  :: m, i

  iseq = .FALSE.

  IF (.NOT. self%filled) RETURN

  m = self%nkey

  IF (m /= nkey) RETURN

  DO i = 1, nkey
    IF (self%key(i) /= key(i)) RETURN
  END DO

  iseq = .TRUE.

END FUNCTION hashtable_entry_cmp_key_arr



SUBROUTINE hashtable_init(self, init_size, seed, growth_factor)
  CLASS(hashtable_t(*,*)), INTENT(INOUT)        :: self
  INTEGER, INTENT(IN), OPTIONAL                 :: init_size
  INTEGER(ihash), INTENT(IN), OPTIONAL          :: seed
  REAL, INTENT(IN), OPTIONAL                    :: growth_factor

  ! No need for "source" argument anymore. And no need for dummy
  ! But its declaration causes error in ifx
  !TYPE(hashtable_entry_t(self%slen, self%dlen)) :: dummy_entry
  INTEGER                                       :: n, i
  INTEGER(ihash)                                :: s
  REAL                                          :: f

  n = hashtable_default_size
  s = hashtable_default_seed
  f = hashtable_default_growth
  IF (present(init_size))     n = init_size
  IF (present(seed))          s = seed
  IF (present(growth_factor)) f = growth_factor
  self%seed          = s
  self%growth_factor = f
  self%size          = 0
  ! Left for history: this is how it used to be done
  ! ALLOCATE(self%entries(n), source = dummy_entry)
  ALLOCATE(self%entries(n))
  
  DO i = 1, n
    CALL self%entries(i)%clear()
  END DO

END SUBROUTINE


SUBROUTINE hashtable_grow(self)
  CLASS(hashtable_t(*,*)), INTENT(INOUT)      :: self
  INTEGER                                     :: capacity, new_capacity, i, j
  TYPE(hashtable_entry_t(self%slen, self%dlen)), ALLOCATABLE :: temp(:)
  
  capacity     = size(self%entries)
  new_capacity = int(capacity * self%growth_factor)
  
  CALL move_alloc(self%entries, temp)
  CALL self%init(init_size=new_capacity, seed=self%seed, growth_factor=self%growth_factor)
  
  ! Find new places for each item
  DO i = 1, capacity
    j = self%find_position(temp(i)%hash, temp(i)%key, temp(i)%nkey)
    self%entries(j) = temp(i)
  END DO

END SUBROUTINE


INTEGER FUNCTION hashtable_find_position_char(self, hash_key, key) RESULT(i)
  CLASS(hashtable_t(*,*)), INTENT(IN) :: self
  INTEGER(ihash), INTENT(IN)          :: hash_key
  CHARACTER(len=*), INTENT(IN)        :: key
  
  INTEGER                             :: capacity, istart, j
  
  capacity = size(self%entries)
  ! Keep track of 0-based indices -- easier this way
  istart   = mod(hash_key, capacity)
  
  ! We know that we won't besearching for more than capacity times
  searching: BLOCK
    DO j = 0, capacity-1
      i = modulo(istart+j, capacity)
      
      IF (.not. self%entries(i+1)%filled) EXIT searching
      
      IF (self%entries(i+1)%hash == hash_key) THEN

        IF (self%entries(i+1)%cmp_key(key)) EXIT searching

      END IF

    END DO

    ! Only get here IF couldn't find the index
    i = -1
    RETURN

  END BLOCK searching

  ! Get here IF found the index
  ! Make it 1-based index
  i = i + 1

END FUNCTION hashtable_find_position_char


INTEGER FUNCTION hashtable_find_position_arr(self, hash_key, key, nkey) RESULT(i)
  CLASS(hashtable_t(*,*)), INTENT(IN) :: self
  INTEGER(ihash), INTENT(IN)          :: hash_key
  CHARACTER(len=1), INTENT(IN)        :: key(:)
  INTEGER, INTENT(IN)                 :: nkey
  
  INTEGER                             :: capacity, istart, j
  
  capacity = size(self%entries)
  ! Keep track of 0-based indices -- easier this way
  istart   = mod(hash_key, capacity)
  
  ! We know that we won't besearching for more than capacity times
  searching: BLOCK
    DO j = 0, capacity-1
      i = modulo(istart+j, capacity)
      
      IF (.not. self%entries(i+1)%filled) EXIT searching
      
      IF (self%entries(i+1)%hash == hash_key) THEN

        IF (self%entries(i+1)%cmp_key(key, nkey)) EXIT searching

      END IF

    END DO

    ! Only get here IF couldn't find the index
    i = -1
    RETURN

  END BLOCK searching

  ! Get here IF found the index
  ! Make it 1-based index
  i = i + 1

END FUNCTION hashtable_find_position_arr


SUBROUTINE hashtable_set(self, key, value)
  CLASS(hashtable_t(*,*)), INTENT(INOUT), TARGET :: self
  CHARACTER(len=*), INTENT(IN)           :: key
  TYPE(*), INTENT(IN)                    :: value
  
  INTEGER(ihash)                         :: key_hash
  INTEGER                                :: ipos, nkey, i
  TYPE(hashtable_entry_t(self%slen,self%dlen)), POINTER :: pentry

  key_hash = hash(key, self%seed)
  ipos = self%find_position(key_hash, key)
  
  IF (ipos < 1) THEN
    ! hashtable is full
    CALL self%grow()
    ipos = self%find_position(key_hash, key)
  END IF

  IF (.not. self%entries(ipos)%filled) self%size = self%size + 1
  

  nkey = len_trim(key)
  nkey = min(nkey, self%slen)

  pentry => self%entries(ipos)
  pentry%filled = .true.
  pentry%hash   = key_hash
  pentry%nkey = nkey
  DO i = 1, nkey
    pentry%key(i) = key(i:i)
  END DO

  !self%entries(ipos)%is_occupied = .true.
  !self%entries(ipos)%hash        = key_hash
  !self%entries(ipos)%key(1:nkey) = key(1:nkey)
  CALL self%entries(ipos)%value%store(value)

END SUBROUTINE
    

SUBROUTINE hashtable_get(self, key, value, err)
  CLASS(hashtable_t(*,*)), INTENT(IN) :: self
  CHARACTER(len=*), INTENT(IN)        :: key
  TYPE(*), INTENT(INOUT)              :: value
  INTEGER, INTENT(OUT)                :: err

  INTEGER(ihash)                      :: key_hash
  INTEGER                             :: ipos
  
  err = 0
  key_hash = hash(key, self%seed)
  ipos = self%find_position(key_hash, key)
  IF (ipos > 1) THEN
    
    IF (self%entries(ipos)%filled) THEN
      CALL self%entries(ipos)%value%retrieve(value)
    ELSE
      err = -1
    END IF

  ELSE

    err = -1

  END IF
  
END SUBROUTINE


END MODULE
    