module parser_list
    use parser_listitem
    implicit none
    private

    public :: token_t

    type, public :: token_list
        type(token_t), allocatable :: items(:)
        integer, private :: num_items = 0
    contains
        procedure :: get
        procedure :: pop
        procedure :: peek
        procedure :: append
        procedure :: is_empty
        procedure, private :: write
        generic, public :: write(formatted) => write
    end type

    interface size
        module procedure list_size
    end interface
    public :: size

contains

    subroutine append(self, item)
        class(token_list) :: self
        type(token_t) :: item

        if (.not. allocated(self % items)) then
            self % items = [ item ]
        else
            self % items = [ self % items, item ]
        end if

        self % num_items = self % num_items + 1
    end subroutine

    function get(self, idx)
        class(token_list) :: self
        type(token_t) :: get
        integer, intent(in) :: idx
        get = self % items(idx)
    end function

    function pop(self)
        class(token_list) :: self
        type(token_t) :: pop

        if (self % is_empty()) error stop

        pop = self % peek()
        self % num_items = self % num_items - 1
        self % items = self % items(:self % num_items)
    end function

    function peek(self)
        class(token_list) :: self
        type(token_t), allocatable :: peek
        if (self % is_empty()) error stop
        peek = self % items(self % num_items)
    end function

    logical function is_empty(self)
        class(token_list) :: self
        is_empty = self % num_items == 0
    end function

    integer function list_size(self)
        type(token_list) :: self
        list_size = self % num_items
    end function

    subroutine clear(self)
        class(token_list) :: self
        deallocate(self % items)
    end subroutine

    subroutine write(self, unit, iotype, v_list, iostat, iomsg)
        class(token_list), intent(in) :: self
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in)  :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        integer :: i

        write(unit,'("[")')
        if (size(self) > 0) then
            do i=1,size(self)-1
                if (allocated(self % items(i) % object)) then
                    write(unit,'(DT,", ")') self % items(i)
                else
                    write(unit,'( A,", ")') self % items(i) % string
                end if
            end do

            if (allocated(self % items(i) % object)) then
                write(unit,'(DT)') self % items(i)
            else
                write(unit,'(A)')  self % items(i) % string
            end if
        end if
        write(unit,'("]")')
    end subroutine
end module
