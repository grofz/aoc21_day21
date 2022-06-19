  module day21
    implicit none
    private
    public board_t

    integer, parameter :: DICE_SIZE = 100, BOARD_SIZE = 10
    integer, parameter :: NO_OF_PLAYERS = 2, NO_OF_ROLLS = 3
    integer, parameter :: SCORE_TO_WIN = 1000

    type dice_t
      integer :: seed = 0
      integer :: nrolled = 0
    contains
      procedure :: roll => dice_roll
      procedure :: mroll => dice_mroll
      procedure :: init => dice_init
    end type

    type board_t
      integer :: position(NO_OF_PLAYERS)
      integer :: score(NO_OF_PLAYERS) = 0
      type(dice_t) :: dice
    contains
      procedure :: move => board_move
      procedure :: start => board_start
    end type

  contains

    subroutine dice_init(this)
      class(dice_t), intent(out) :: this
      this % seed = 0
    end subroutine

    function dice_roll(this) result(outcome)
      class(dice_t), intent(inout) :: this
      integer :: outcome
      this % seed = this % seed + 1
      if (this % seed > DICE_SIZE) this % seed = 1
      outcome = this % seed
      this % nrolled = this % nrolled + 1
    end function

    function dice_mroll(this,n) result(nsum)
      class(dice_t), intent(inout) :: this
      integer, intent(in) :: n
      integer :: nsum
      integer :: i
      nsum = 0
      do i=1,n
        nsum = nsum + this % roll()
      end do
    end function

    subroutine board_move(this, ip, iswin)
      class(board_t), intent(inout) :: this
      integer, intent(in) :: ip
      logical, intent(out) :: iswin

      integer :: nsum
      nsum = this % dice % mroll(NO_OF_ROLLS)
      associate(pos => this % position(ip))
        pos = mod(pos+nsum-1, BOARD_SIZE) + 1
        this % score(ip) = this % score(ip) + pos
      end associate
      iswin = this % score(ip) >= SCORE_TO_WIN
    end subroutine

    subroutine board_start(this, pos)
      class(board_t), intent(out) :: this
      integer, intent(in) :: pos(:)
      if (size(pos) /= NO_OF_PLAYERS) &
        error stop 'board_start - size must be same as no_of_players'
      this % position = pos
      this % score = 0
      call this % dice % init()
    end subroutine 

  end module day21
