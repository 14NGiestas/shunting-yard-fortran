module parser_listitem
    implicit none
    private

    type, public :: None
    end type

    type, public :: ListItem
        class(*), allocatable :: content
    contains
        procedure :: write
        generic, public :: write(formatted) => write
    end type

contains

    subroutine write(self, unit, iotype, v_list, iostat, iomsg)
        class(ListItem), intent(in) :: self
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in)  :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        select type(it => self % content)
        type is (character(*))
            write(unit,'("''",g0,"''")') it
        type is (real)
            write(unit,'(g0)') it
        type is (complex)
            write(unit,'(g0)') it
        type is (integer)
            write(unit,'(g0)') it
        class is (None)
            write(unit,'("None")')
        !class is (ListItem)
        !    write(unit,'(g0)') ListItem(it % content)
        class default
            write(unit,'("[DERIVED TYPE ",I0,"]")') sizeof(it)
        end select
    end subroutine

end module
