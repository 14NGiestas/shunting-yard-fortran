module parser_module
    use iso_fortran_env
    use parser_list
    use parser_listitem
    implicit none
    private

    type, public :: Parser
        procedure(EventInterface), pointer :: on_function => null()
        procedure(EventInterface), pointer :: on_operator => null()
        procedure(EventInterface), pointer :: on_operand  => null()
    contains
        procedure :: parse
        procedure, nopass :: register_function
        procedure, nopass :: register_operator
        procedure, nopass :: is_enclosed
        procedure, private :: tokenize_input
        procedure, private :: convert_to_RPN
        procedure, private :: eval_expression
    end type

    abstract interface
        function EventInterface(self, lhs, opr, rhs) result(ans)
            import :: Parser
            class(Parser)  :: self
            class(*) :: opr
            class(*), optional :: lhs
            class(*), optional :: rhs
            class(*), allocatable :: ans
        end function
    end interface

    character(:), allocatable :: REGISTERED_FUNCTIONS(:)
    character(:), allocatable :: REGISTERED_OPERATORS(:)
    character(:), allocatable :: REGISTERED_RIGHT_ASSOC(:)

contains

    subroutine register_function(fun_names)
        character(*) :: fun_names(:)
        if (allocated(REGISTERED_FUNCTIONS)) then
            REGISTERED_FUNCTIONS = [REGISTERED_FUNCTIONS, fun_names]
        else
            REGISTERED_FUNCTIONS = [fun_names]
        end if
    end subroutine

    subroutine register_operator(opr_names, is_right_assoc)
        character(*) :: opr_names(:)
        logical, optional :: is_right_assoc
        if (allocated(REGISTERED_OPERATORS)) then
            REGISTERED_OPERATORS = [REGISTERED_OPERATORS, opr_names]
        else
            REGISTERED_OPERATORS = [opr_names]
        end if

        if (present(is_right_assoc)) then
            if (.not. is_right_assoc) return
            if (allocated(REGISTERED_RIGHT_ASSOC)) then
                REGISTERED_RIGHT_ASSOC = [REGISTERED_RIGHT_ASSOC, opr_names]
            else
                REGISTERED_RIGHT_ASSOC = [opr_names]
            end if
        end if
    end subroutine

    subroutine parse(self, infix)
        class(Parser) :: self
        character(*) :: infix
        type(List) :: tokens
        type(List) :: postfix
        type(List) :: output
        class(*), allocatable :: ans

        call self % tokenize_input(infix, tokens)
        if (is_enclosed(tokens)) then
            call self % convert_to_RPN(tokens, postfix)
            call self % eval_expression(postfix, output)
        else
            write(error_unit,*) 'Unmatched parenthesis on: '//infix
            stop
        end if
    end subroutine

    subroutine tokenize_input(self, from, tokens)
        class(Parser) :: self
        character(*), intent(in)  :: from
        type(List),   intent(in out) :: tokens
        character(:), allocatable :: infix
        character(:), allocatable :: slice, next_char
        logical :: is_token, is_next_token
        integer :: i, delta

        ! Sanitization
        infix = replace(from,'**','^')
        infix = replace(infix,' ','')

        delta = 0
        i = 1
        do while(i+delta < len(infix))
            slice     = infix(i:i+delta)
            next_char = infix(i+delta+1:i+delta+1)
            is_token = any(REGISTERED_OPERATORS == slice) &
                  .or. scan(slice,'()') > 0
            is_next_token = any(REGISTERED_OPERATORS == next_char) &
                  .or. scan(next_char,'()') > 0
            if (is_token .or. is_next_token) then
                call tokens % append(slice)
                i = i + 1
                if (is_next_token) i = i + delta
                delta = 0
            else
                delta = delta + 1
            end if
        end do
        slice = infix(i:)
        call tokens % append(slice)

    contains

        function replace(string, text, repl) result(output)
            character(*) :: string, text, repl
            character(:), allocatable :: output
            integer :: i, nt, nr

            output = string
            nt = len(text)
            nr = len(repl)
            do
                i = index(output,text(:nt))
                if (i == 0) exit
                output = output(:i-1) // repl(:nr) // output(i+nt:)
            end do
        end function

    end subroutine

    subroutine convert_to_RPN(self, tokens, postfix)
        class(Parser) :: self
        type(List), intent(in)  :: tokens
        type(List), intent(out) :: postfix
        type(List) :: operators
        class(*), allocatable :: token, top
        integer :: i

        do i=1,size(tokens)
            token = tokens % get(i)
            ! Is a registered function?
            if (is_function(token)) then
                ! Push into operator stack
                call operators % append(token)
            ! Is a valid operand: number or variable name?
            else if (is_operand(token)) then
                ! Push into output queue
                print*, "add to output", ListItem(token)
                call postfix % append(token)
            ! Is a open paren.
            else if (is_open_paren(token)) then
                print*, "push parenthesis"
                call operators % append(token)
            ! Is a close paren.
            else if (is_close_paren(token)) then
                print*, 'popping to parenthesis'
                do
                    top = operators % peek()
                    if (.not. is_open_paren(top)) then
                        call postfix % append(top)
                        top = operators % pop()
                        print*, "... popped ", ListItem(top), "to output"
                    else
                        exit
                    end if
                end do

                top = operators % pop()
                print*, '... found parenthesis'
            ! Is a registered operator
            else !if (is_operator(token)) then
                print*, "adding operator: ", ListItem(token)
                do while (size(operators) > 0)
                    top = operators % peek()
                    if (.not. is_open_paren(top) &
                        .and. ( &
                            (is_left_assoc(token) .and. priority(token) <= priority(top)) &
                            .or. &
                            (is_right_assoc(token) .and. priority(token) < priority(top)) &
                        ) &
                    ) then
                        call postfix % append(top)
                        top = operators % pop()
                        print*, "... popped operator ", ListItem(top), " to output"
                    else
                        exit
                    end if
                end do
                call operators % append(token)
            end if
            print*, '   output: ', postfix
            print*, '   stack:  ', operators
        end do

        print*, "transfering tokens from stack to output"
        ! Pop all remaining operators
        do while (size(operators) > 0)
            top = operators % pop()
            call postfix % append(top)
        end do

        print*, postfix

    end subroutine

    subroutine eval_expression(self, postfix, output)
        class(Parser) :: self
        type(List), intent(in out) :: postfix
        type(List), intent(out) :: output
        class(*), allocatable :: token
        class(*), allocatable :: op1, op2
        integer :: i

        do i=1,size(postfix)
            token = postfix % get(i)
            if (is_function(token)) then
                ! Pop the first operand
                op1 = output % pop()
                ! Put result back in the stack
                call output % append(self % on_function(opr=token,rhs=op1))
            else if (is_operand(token)) then
                ! Convert operand string into a actual operand
                call output % append(self % on_operand(opr=token))
            else if (is_operator(token)) then
                ! Pop the first operand
                op1 = output % pop()
                ! Pop the second operand
                op2 = output % pop()
                ! Perform operation
                call output % append(self % on_operator(lhs=op2,opr=token,rhs=op1))
            end if
            print*, 'token: ', ListItem(token), 'output: ', output
        end do
        token = output % pop()
    end subroutine

    integer function priority(input)
        class(*) :: input
        select type(input); type is (character(*))
            if (is_function(input) &
                .or. is_open_paren(input) &
                .or. is_close_paren(input) &
            ) then
                priority = 5
            else if (input == '^') then
                priority = 4
            else if (input == '*' .or. input == '/') then
                priority = 3
            else if (input == '+' .or. input == '-') then
                priority = 2
            else
                priority = -1
            end if
        end select
    end function

    logical function is_open_paren(input)
        class(*) :: input
        select type(input); type is (character(*))
            is_open_paren = scan(input, '(') > 0
        end select
    end function

    logical function is_close_paren(input)
        class(*) :: input
        select type(input); type is (character(*))
            is_close_paren = scan(input, ')') > 0
        end select
    end function

    logical function is_operand(input)
        class(*) :: input
        select type(input); type is (character(*))
            is_operand = scan(input, "abcdefghijklmnopqrstuvwxyz_"//&
                                     "ABCDEFGUIJKLMNOPQRSTUVWXYZ."//&
                                     "0123456789") > 0
        end select
    end function

    logical function is_left_assoc(input)
        class(*) :: input
        is_left_assoc = .not. is_right_assoc(input)
    end function

    logical function is_right_assoc(input)
        class(*) :: input
        select type(input); type is (character(*))
            is_right_assoc = any(REGISTERED_RIGHT_ASSOC == input)
        end select
    end function

    logical function is_function(input)
        class(*) :: input
        select type(input); type is (character(*))
            is_function = any(REGISTERED_FUNCTIONS == input)
        end select
    end function

    logical function is_operator(input)
        class(*) :: input
        select type(input); type is (character(*))
            is_operator = any(REGISTERED_OPERATORS == input)
        end select
    end function

    logical function is_enclosed(tokens)
        type(List) :: tokens
        class(*), allocatable :: token
        integer :: i,k
        is_enclosed = .false.
        k = 0
        do i = 1, size(tokens)
            token = tokens % get(i)
            if (is_open_paren(token)) then
                k = k + 1
            else if (is_close_paren(token)) then
                k = k - 1
            end if
            if (k < 0) exit
        end do
        if (k == 0) is_enclosed = .true.
    end function

end module
