SUBROUTINE isort(items)

  DATA_TYPE, INTENT(INOUT) :: items(:)
  DATA_TYPE :: tmp
  
  INTEGER :: i, j
  
  ! At each iteration, items(1:(i-1)) are sorted
  iloop: DO i = 2, size(items)
    ! need to keep it as items(i) may be overwritten
    tmp = items(i)
    j = i - 1
    ! Find the place for items(i) among 1:I
    ! initially "vacant" spot is position I (its current)
    ! On each cycle: J+1 is vacant
    jloop: DO WHILE (j >= 1)
      ! ITEMS(I) is IN right order relative to
      ! ITEMS(1:J) - exit
      IF (tmp >= items(j)) EXIT jloop
      ! Shift ITEMS(J) one position towards the END
      ! J+1 position is currently vacant
      items(j+1) = items(j)
      ! Now J is vacant
      ! TMP  < ITEMS(J+1), try next J
      j = j - 1
    END DO jloop
    ! On exit: J+1 is vacant position for TMP
    items(j+1) = tmp
    ! ITEMS(1:I) here are sorted
  END DO iloop
     
END SUBROUTINE
