  module dirac_universe_mod
    implicit none
    private
    public board_t, count_wins


    integer, parameter, public :: I8 = selected_int_kind(18), &
    & NO_OF_PLAYERS=2, SCORE_TO_WIN=21, NO_OF_ROLLS=3, DICE_SIZE=3, BOARD_SIZE=10

    ! The sum from 3 rolls can be between 3 and 9.
    ! If at each roll, the universe splits into 3, "roll_freq" contains
    ! the number of universes, with the same sum.
    integer(I8), save :: roll_freq(NO_OF_ROLLS : DICE_SIZE*NO_OF_ROLLS) = 0

    type board_t
      private
      integer :: position(NO_OF_PLAYERS)
      integer :: score(NO_OF_PLAYERS) = 0
    contains 
      procedure, non_overridable :: move => board_move
      procedure :: start => board_start
    end type

 integer(I8), public, protected :: counter=0

  contains

    subroutine global_init()
      integer :: i, j, k ! works for 3 rolls only

      roll_freq = 0
      do i=1, DICE_SIZE
      do j=1, DICE_SIZE
      do k=1, DICE_SIZE
        roll_freq(i+j+k) = roll_freq(i+j+k) + 1_I8
      end do
      end do
      end do
      if (sum(roll_freq) /= DICE_SIZE**NO_OF_ROLLS) &
          error stop 'initialize global - defensive check failed'
    end subroutine



    subroutine board_start(this, initpos)
      class(board_t), intent(out) :: this
      integer, intent(in) :: initpos(:)

      if (size(initpos) /= NO_OF_PLAYERS) &
          error stop 'board_start - array size must be same as no of players'
      this % position = initpos
      this % score = 0
      if (any(roll_freq==0)) call global_init
    end subroutine



    pure function board_move(this, who, rollsum) result(newboard)
      class(board_t), intent(in) :: this
      integer, intent(in) :: who, rollsum
      type(board_t) :: newboard

      newboard = this
      associate(pos => newboard % position(who), score => newboard % score(who))
        pos = mod(pos+rollsum-1, BOARD_SIZE) + 1
        score = score + pos
      end associate
    end function



    recursive function count_wins(board, freq, who) result(nwins)
      type(board_t), intent(in) :: board
      integer(I8), intent(in) :: freq
      integer, intent(in) :: who
      integer(I8) :: nwins(NO_OF_PLAYERS)

      type(board_t) :: newboard
      integer :: i, next
  counter = counter + 1

      ! return easy answer if possible...
      nwins = 0
      do i=1, NO_OF_PLAYERS
        if (board % score(i) >= SCORE_TO_WIN) then
          nwins(i) = freq
          return
        end if
      end do

      ! ...or make one move (and split the universe)
      next = mod(who+1-1, NO_OF_PLAYERS) + 1
      do i=lbound(roll_freq,dim=1), ubound(roll_freq,dim=1)
        newboard = board % move(next, i)
        nwins = nwins + count_wins(newboard, roll_freq(i), next)
      end do
      nwins = nwins * freq
    end function

  end module dirac_universe_mod
