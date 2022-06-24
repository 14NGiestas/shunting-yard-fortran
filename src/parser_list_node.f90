module parser_list_node
    implicit none
    private

    type, public :: list_node
        type(list_node), pointer :: next => null()
        type(list_node), pointer :: prev => null()
        class(*), allocatable :: content
    contains
        procedure :: clear => destroy_node
        procedure, private :: clear_all => destroy_all_nodes
        procedure :: write
        generic, public :: write(formatted) => write
    end type

    interface list_node
        module procedure initialize_node
    end interface

contains

    !> Creates a list_node that contains 'new_item' as its child
    !>
    !> Returns the new parent node
    pure function initialize_node( new_item ) result( new_node )
        type(list_node) :: new_node
        class(*), intent(in) :: new_item

        ! allocating new_item to the new node's item
        allocate(new_node%content, source=new_item)
    end function

    pure subroutine destroy_node(this_node)
        class(list_node), intent(inout) :: this_node

        !Deallocate it's item
        if (allocated(this_node%content)) deallocate(this_node%content)

        !Nullify it's pointers
        nullify(this_node%next)
        nullify(this_node%prev)
    end subroutine

    pure subroutine destroy_all_nodes(this_node)
        class(list_node), intent(inout) :: this_node
        type(list_node), pointer :: current_node
        type(list_node), pointer :: next_node
        !Deallocate it's item
        current_node = this_node
        next_node => current_node%next
        do
            deallocate(current_node)
            call current_node%clear()
            if (.not. associated(next_node)) exit
            current_node => next_node
            next_node => current_node%next
        end do
    end subroutine

    subroutine write(self, unit, iotype, v_list, iostat, iomsg)
        class(list_node), intent(in) :: self
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
        class is (list_node)
            write(unit,'(g0)') list_node(it % content)
        class default
            write(unit,'("[DERIVED TYPE ",I0,"]")') sizeof(it)
        end select
    end subroutine

end module
