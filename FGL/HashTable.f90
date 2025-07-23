MODULE hashtable_m

USE, INTRINSIC :: iso_c_binding, ONLY: c_int8_t, c_loc, c_f_pointer, c_ptr
USE hash_m
USE error_m

IMPLICIT NONE
PRIVATE
PUBLIC :: hashtable_t

INTEGER, PARAMETER :: hashtable_default_size = 1024,  &
                      hashtable_default_seed = 1,     &
                      hashtable_noentry_error = -100
REAL, PARAMETER    :: hashtable_default_growth = 1.5

TYPE hashtable_t
  INTEGER                        :: keylen
  INTEGER                        :: dsize
  INTEGER(ihash)                 :: seed
  REAL                           :: growth_factor
  INTEGER                        :: capacity, size
  INTEGER(c_int8_t), ALLOCATABLE :: entries(:)
  INTEGER(ihash),    ALLOCATABLE :: hash(:)
  CHARACTER(:),      ALLOCATABLE :: keys(:)
  CONTAINS
  PROCEDURE, PASS(self) :: init => hashtable_init
  PROCEDURE, PASS(self) :: set => hashtable_set
  PROCEDURE, PASS(self) :: get => hashtable_get
  PROCEDURE, PASS(self) :: grow => hashtable_grow
  PROCEDURE, PASS(self) :: find_position => hashtable_find_position
END TYPE


CONTAINS


SUBROUTINE hashtable_init(self, keylen, dsize, init_size, seed, growth_factor)
  CLASS(hashtable_t), INTENT(INOUT)    :: self
  INTEGER, INTENT(IN)                  :: keylen, dsize
  INTEGER, INTENT(IN), OPTIONAL        :: init_size
  INTEGER(ihash), INTENT(IN), OPTIONAL :: seed
  REAL, INTENT(IN), OPTIONAL           :: growth_factor
  INTEGER                              :: n, totsize
  INTEGER(ihash)                       :: s
  REAL                                 :: f

  n = hashtable_default_size
  s = hashtable_default_seed
  f = hashtable_default_growth
  IF (present(init_size))     n = init_size
  IF (present(seed))          s = seed
  IF (present(growth_factor)) f = growth_factor
  self%keylen        = keylen
  self%dsize         = dsize
  self%capacity      = n
  self%seed          = s
  self%growth_factor = f
  self%size          = 0

  totsize = dsize * n
  ALLOCATE(CHARACTER(len=keylen)::self%keys(n))
  ALLOCATE(self%entries(totsize), self%hash(n))
  
  self%hash(:) = 0
  self%keys(:) = ''

END SUBROUTINE


SUBROUTINE hashtable_set(self, key, value)
  CLASS(hashtable_t), INTENT(INOUT), TARGET :: self
  CHARACTER(len=*), INTENT(IN)              :: key
  TYPE(*), INTENT(IN), TARGET               :: value

  INTEGER(ihash)                            :: key_hash
  INTEGER                                   :: ipos, nkey, i
  TYPE(c_ptr)                               :: cpval
  INTEGER(c_int8_t), POINTER                :: pval(:)

  key_hash = hash(key, self%seed)
  ipos = self%find_position(key_hash, key)
  
  IF (ipos < 1) THEN
    ! hashtable is full
    CALL self%grow()
    ipos = self%find_position(key_hash, key)
  END IF

  self%size = self%size + 1
  
  nkey = LEN_TRIM(key)
  nkey = MIN(nkey, self%keylen)

  self%keys(ipos) = ''
  self%keys(ipos)(1:nkey) = key(1:nkey)
  self%hash(ipos) = key_hash

  cpval = c_loc(value)
  CALL c_f_pointer(cpval, pval, [self%dsize])
  i = (ipos - 1) * self%dsize
  self%entries(i+1:i+self%dsize) = pval(1:self%dsize)

END SUBROUTINE


SUBROUTINE hashtable_get(self, key, value, err)
  CLASS(hashtable_t), INTENT(IN) :: self
  CHARACTER(len=*), INTENT(IN)   :: key
  TYPE(*), INTENT(INOUT), TARGET :: value
  INTEGER, INTENT(OUT)           :: err

  INTEGER(ihash)                 :: key_hash
  INTEGER                        :: ipos, i
  TYPE(c_ptr)                    :: cpval
  INTEGER(c_int8_t), POINTER     :: pval(:)
  
  err = no_error
  key_hash = hash(key, self%seed)
  ipos = self%find_position(key_hash, key)
  
  IF (ipos > 1) THEN
    IF (self%hash(ipos) == key_hash) THEN
      
      cpval = c_loc(value)
      CALL c_f_pointer(cpval, pval, [self%dsize])
      i = (ipos - 1) * self%dsize
      pval(1:self%dsize) = self%entries(i+1:i+self%dsize)
      RETURN
    END IF
  END IF
  
  err = hashtable_noentry_error

END SUBROUTINE


SUBROUTINE hashtable_grow(self)
  CLASS(hashtable_t), INTENT(INOUT)      :: self
  INTEGER                                :: capacity, new_capacity, i, j, jpos, ipos
  INTEGER(c_int8_t), ALLOCATABLE, TARGET :: entries(:)
  CHARACTER(:),      ALLOCATABLE         :: keys(:)
  INTEGER(ihash),    ALLOCATABLE         :: hashval(:)
  TYPE(c_ptr)                            :: cpval
  INTEGER(c_int8_t), POINTER             :: pval(:)

  
  capacity     = self%capacity
  new_capacity = int(capacity * self%growth_factor)
  
  CALL move_alloc(self%entries, entries)
  CALL move_alloc(self%keys, keys)
  CALL move_alloc(self%hash, hashval)

  CALL self%init(keylen=self%keylen,     dsize=self%dsize,    &
                 init_size=new_capacity, seed=self%seed,      &
                 growth_factor=self%growth_factor)
  
  ! Find new places for each item
  DO ipos = 1, capacity
    IF (hashval(ipos) > 0) THEN
      jpos = self%find_position(hashval(ipos), keys(ipos))
      
      self%hash(jpos) = hashval(ipos)
      self%keys(jpos) = keys(ipos)
      
      i = (ipos - 1) * self%dsize
      j = (jpos - 1) * self%dsize
      cpval = c_loc(entries(i+1))
      CALL c_f_pointer(cpval, pval, [self%dsize])
      self%entries(j+1:j+self%dsize) = pval(1:self%dsize)
    END IF
  END DO

END SUBROUTINE


INTEGER FUNCTION hashtable_find_position(self, hash_key, key) RESULT(i)
  CLASS(hashtable_t), INTENT(IN) :: self
  INTEGER(ihash), INTENT(IN)     :: hash_key
  CHARACTER(*), INTENT(IN)       :: key
  INTEGER                        :: capacity, istart, j
  
  capacity = self%capacity
  ! Keep track of 0-based indices -- easier this way
  istart   = mod(hash_key, capacity)
  
  ! We know that we won't besearching for more than capacity times
  searching: BLOCK
    DO j = 0, capacity-1
      i = modulo(istart+j, capacity)
      
      IF (self%hash(i+1) == 0) EXIT searching
      
      IF (self%hash(i+1) == hash_key) THEN

        IF (self%keys(i+1) == key) EXIT searching

      END IF

    END DO

    ! Only get here IF couldn't find the index
    i = -1
    RETURN

  END BLOCK searching

  ! Get here IF found the index
  ! Make it 1-based index
  i = i + 1

END FUNCTION hashtable_find_position


END MODULE
    