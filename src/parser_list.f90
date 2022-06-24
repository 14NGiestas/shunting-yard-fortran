module parser_list
    use parser_list_node
    implicit none
    private

    type, public :: linked_list
        integer, private :: num_nodes = 0
        type(list_node), pointer :: head => null()
        type(list_node), pointer :: tail => null()
    contains
        procedure :: push     => push_at_tail
        procedure :: insert   => insert_at_index
        procedure :: pop      => pop_node_at_tail
        procedure :: peek     => get_node_at_tail
        procedure :: remove   => remove_node_at_index
        procedure :: get      => get_node_at_index
        procedure :: size     => get_length
        !procedure :: set_size => set_length ! what? why?
        procedure :: replace  => replace_at_index
        procedure :: reverse  => reverse_child_list
        procedure :: clear    => destroy_whole_child_list
        procedure, private :: write
        generic, public :: write(formatted) => write
    end type

    interface size
        module procedure get_length
    end interface
    public :: size

contains

    !> Insert 'content' at the tail of the input child list
    pure subroutine push_at_tail(this_child_list, content)

        class(linked_list), intent(inout) :: this_child_list
        class(*), intent(in) :: content

        ! Finding if its a first node or the child_list already have a node
        if (associated(this_child_list%tail)) then
            allocate(this_child_list%tail%next, source=list_node(content))
            this_child_list%tail%next%prev => this_child_list%tail
            this_child_list%tail => this_child_list%tail%next
        else
            allocate(this_child_list%head, source=list_node(content))
            this_child_list%tail => this_child_list%head
        end if

        this_child_list%num_nodes = this_child_list%num_nodes + 1
    end subroutine push_at_tail

    !> Insert 'content' at the given 'node_index' of the input child list
    pure subroutine insert_at_index( this_child_list, content ,node_index )
        class(linked_list), intent(inout) :: this_child_list
        integer, intent(in)        :: node_index
        class(*), intent(in)       :: content
        type(list_node), pointer        :: current_node
        type(list_node), pointer        :: next_node

        integer :: index

        ! This index will be used for iteraing
        index  = node_index-1

        ! will insert after tail when the input is more than size of the child list
        if(index >=this_child_list%num_nodes) then
            call this_child_list%push(content)
            return
        else if(index <=0) then
            ! will insert after tail when the input is more than size of the child list
            current_node => this_child_list%head
            allocate(this_child_list%head, source=list_node(content))
            this_child_list%head%next => current_node
            current_node%prev   => this_child_list%head
        else
            current_node => this_child_list%head
            do while(index >1)
            index  = index -1
            current_node => current_node%next
            end do
            next_node => current_node%next
            allocate(current_node%next, source=list_node(content))
            current_node%next%prev => current_node
            current_node%next%next => next_node
            current_node => current_node%next
            current_node%next%prev => current_node
        end if
        this_child_list%num_nodes = this_child_list%num_nodes + 1
    end subroutine insert_at_index

    !> Removing the node at the given 'node_index' from the input child list
    pure subroutine remove_node_at_index( this_child_list, node_index )

        class(linked_list), intent(inout) :: this_child_list
        integer, intent(in):: node_index
        type(list_node), pointer:: current_node

        ! This index will be reference for child list
        integer:: index

        !iterating through the child_list to reach the nth node
        current_node => this_child_list%head

        ! return if the given node index is not in range of 1 to size of linked list
        if(node_index<=0) return
        if(node_index>this_child_list%num_nodes) return
        index = 1
        do while ( associated(current_node) )
            if (index==node_index) then
            if (associated(current_node%prev).and.associated(current_node%next)) then
                !child_list Node is in mid
                current_node%next%prev => current_node%prev
                current_node%prev%next => current_node%next

            else if (associated(current_node%prev)) then
                !child_list tail
                nullify(current_node%prev%next)
                this_child_list%tail => current_node%prev

            else if (associated(current_node%next)) then
                !child_list head
                nullify(current_node%next%prev)
                this_child_list%head => current_node%next
            else
                !only node in list
                nullify(this_child_list%head)
                nullify(this_child_list%tail)
            end if

            !Destroy node content and Free it's memory
            call current_node%clear()
            deallocate(current_node)

            !Reduce the index by 1
            this_child_list%num_nodes = this_child_list%num_nodes - 1
            return
            end if
            current_node => current_node%next
            index = index+1
        end do
    end subroutine remove_node_at_index

    !> Removing the last node from the input child list
    pure subroutine pop_node_at_tail( this_child_list )

        class(linked_list), intent(inout) :: this_child_list

        type(list_node), pointer:: current_node

        ! return if the size of the child list is 0
        if(this_child_list%num_nodes == 0) return


        ! poping the last node of the child list
        current_node => this_child_list%tail
        if (associated(current_node%prev).and.associated(current_node%next)) then
            !child_list Node is in mid
            current_node%next%prev => current_node%prev
            current_node%prev%next => current_node%next

        else if (associated(current_node%prev)) then
            !child_list tail
            nullify(current_node%prev%next)
            this_child_list%tail => current_node%prev

        else if (associated(current_node%next)) then
            !child_list head
            nullify(current_node%next%prev)
            this_child_list%head => current_node%next
        else
            nullify(this_child_list%head)
            nullify(this_child_list%tail)
        end if

        !Destroy node content and Free it's memory
        call current_node%clear()
        deallocate(current_node)

        !Reduce the count by 1
        this_child_list%num_nodes = this_child_list%num_nodes - 1
    end subroutine pop_node_at_tail

    !> Returns the last node from the input child list
    function get_node_at_tail(this_child_list) result (content_return)
        class(linked_list), intent(in) :: this_child_list
        class(*), pointer :: content_return

        content_return => get_node_at_index(this_child_list, this_child_list % num_nodes)
    end function

    !> Returns the pointer to the content stored at 'node_index' in the input child list
    !>
    !> Returns a pointer
    function get_node_at_index( this_child_list, node_index ) result (return_content)

        class(linked_list), intent(in) :: this_child_list
        integer, intent(in):: node_index
        class(*), pointer :: return_content
        type(list_node), pointer:: current_node
        integer:: index

        !iterating through the child_list to reach the nth node
        current_node => this_child_list%head
        index = 1
        do while ( associated(current_node) )

            if (index == node_index) then
                ! Return the pointer to content stored at specified index
                return_content => current_node%content
                nullify(current_node)
                return
            end if
            current_node => current_node%next
            index = index+1

        end do
        nullify(current_node)
        nullify(return_content)
    end function get_node_at_index


    !> Returns the total number of nodes in the input child list
    !>
    !> Returns an integer
    pure function get_length ( this_child_list ) result ( length )
        class(linked_list), intent(in)  :: this_child_list
        integer                     :: length

        length = this_child_list%num_nodes
    end function get_length



    !> Replaces the content stored in node at 'node_index' of the input child list with 'new_content'
    pure subroutine replace_at_index( this_child_list, content ,node_index )
        class(linked_list), intent(inout) :: this_child_list
        integer, intent(in)        :: node_index
        class(*), intent(in)       :: content
        type(list_node), pointer        :: current_node
        integer :: index


        ! This index will be reference for child list
        index = node_index

        ! return if the given node index is not in range of 1 to size of child list
        if(index<1 .or. index>this_child_list%num_nodes) return


        ! Iterating through parent nodes while size of the child list is smaller than index
        current_node => this_child_list%head
        do while(index>1)
            index = index-1
            current_node => current_node%next
        end do
        current_node%content = content
    end subroutine replace_at_index

        !> Reverses the input child list
    pure subroutine reverse_child_list (this_child_list)
        class(linked_list), intent(inout) :: this_child_list
        type(list_node), pointer        :: temp_node
        type(list_node), pointer        :: curr_node

        nullify(temp_node)

        ! Swapping head of the child node with tail of the child node
        curr_node => this_child_list%head
        do while (associated(curr_node))
            temp_node => curr_node%prev
            curr_node%prev => curr_node%next
            curr_node%next => temp_node
            curr_node => curr_node%prev
        end do

        temp_node=> this_child_list%head
        this_child_list%head => this_child_list%tail
        this_child_list%tail => temp_node
    end subroutine reverse_child_list

       !> Destroy the whole given linked list
    !> Free  the allocated memory
    !> Nullify all the variables
    pure subroutine destroy_whole_child_list( this_child_list )
        !Entrada:
        class(linked_list), intent(inout) :: this_child_list
        !Local:
        type(list_node), pointer:: current_node

        do while (this_child_list%num_nodes>0)
            current_node => this_child_list%head
            if (associated(current_node%next)) then
            nullify(current_node%next%prev)
            this_child_list%head => current_node%next
            end if
            call current_node%clear()
            deallocate(current_node)
            this_child_list%num_nodes = this_child_list%num_nodes - 1
        end do
    end subroutine destroy_whole_child_list


    subroutine write(self, unit, iotype, v_list, iostat, iomsg)
        class(linked_list), intent(in) :: self
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in)  :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        integer :: i

        write(unit,'("[")')
        write(unit,'("]")')
    end subroutine
end module
