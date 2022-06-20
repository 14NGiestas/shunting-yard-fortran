module parser_list
    use parser_listitem
    implicit none
    private

    type, public :: List
        type(ListItem), allocatable :: items(:)
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
        class(List) :: self
        class(*) :: item

        if (.not. allocated(self % items)) then
            self % items = [ ListItem(item) ]
        else
            self % items = self % items(:self % num_items)
            self % items = [ self % items, ListItem(item) ]
        end if

        self % num_items = self % num_items + 1
    end subroutine

    function get(self, idx)
        class(List) :: self
        class(*), allocatable :: get
        integer, intent(in) :: idx
        get = self % items(idx) % content
    end function

    function pop(self)
        use iso_fortran_env
        class(List) :: self
        class(*), allocatable :: pop

        if (self % is_empty()) then
            pop = None()
            return
        end if

        pop = self % peek()
        self % num_items = self % num_items - 1
    end function

    function peek(self)
        class(List) :: self
        class(*), allocatable :: peek
        if (self % is_empty()) then
            peek = None()
            return
        end if
        peek = self % items(self % num_items) % content
    end function

    logical function is_empty(self)
        class(List) :: self
        is_empty = self % num_items == 0
    end function

    integer function list_size(self)
        class(List) :: self
        list_size = self % num_items
    end function

    subroutine clear(self)
        class(List) :: self
        deallocate(self % items)
    end subroutine

    subroutine write(self, unit, iotype, v_list, iostat, iomsg)
        class(List), intent(in) :: self
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in)  :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        integer :: i

        write(unit,'("[")')
        if (size(self) > 0) then
            do i=1,size(self)-1
                write(unit,'(DT,", ")') self % items(i)
            end do
            write(unit,'(DT)') self % items(i)
        end if
        write(unit,'("]")')
    end subroutine
end module
