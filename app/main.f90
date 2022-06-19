! Advent of code 2021 - Day 21 Dirac dice
!
  program main 
    call main_part1
    call main_part2
  end program main



  subroutine main_part2
    use dirac_universe_mod
    implicit none
    type(board_t) :: board
    integer(I8) :: nwins(NO_OF_PLAYERS)

    print *, '*** PART 2 ***'
    ! test case
    call board % start([4, 8])
    ! real problem case
!   call board % start([4, 2])

    nwins = count_wins(board, 1_I8, NO_OF_PLAYERS)
    print '("Player 1 wins ",i18)', nwins(1)
    print '("Player 2 wins ",i18)', nwins(2)
    print *, 'Correct answer? ', &
        maxval(nwins)==444356092776315_I8 .or. maxval(nwins)==91559198282731_I8
print *, 'called =',counter
  end subroutine main_part2



  subroutine main_part1
    use day21
    implicit none
    type(board_t) :: board
    integer :: j, answer
    logical :: iswin

    print *, '*** PART 1 ***'
    ! test case
    call board % start([4, 8])
    ! real problem case
!   call board % start([4, 2])

    LOOP: do 
      do j=1,2
        call board % move(j,iswin)
        print '("Player ",i0,"  pos=",i2,"  score=",i4)',j,board % position(1), board % score(1)
        if (iswin) exit LOOP
      end do
    end do LOOP
    answer = minval(board%score) * board%dice%nrolled
    print *, 'Player 1 final score =', board % score(1)
    print *, 'Player 2 final score =', board % score(2)
    print *, 'Dice rolled =', board % dice % nrolled
    print *, 'Answer =', answer
    print *, 'Correct answer? ',answer==908595 .or. answer==739785
    print *
  end subroutine main_part1
