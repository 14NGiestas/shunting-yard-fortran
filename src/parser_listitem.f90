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
        character(200) :: buffer

        select type(it => self % content)
        type is (character(*))
            print '(A)', it
        type is (real)
            write(buffer,*) it
            print '(A)', trim(adjustl(trim(buffer)))
        type is (complex)
            write(buffer,*) it
            print '(A)', trim(adjustl(trim(buffer)))
        type is (integer)
            print '(I0)', it
        class default
            print '("[DERIVED TYPE ",I0,"]")', sizeof(it)
        end select
    end subroutine

end module
