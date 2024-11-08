module parser_module
    use iso_fortran_env
    use fortran_tokenizer
    implicit none
    private

    public :: token_t

    type, public :: parser_t
        type(tokenizer_t) :: tokenizer
        procedure(interface_on_unary),   pointer :: on_unary   => null()
        procedure(interface_on_binary),  pointer :: on_binary  => null()
        procedure(interface_on_operand), pointer :: on_operand => null()
    contains
        procedure :: parse
        procedure, nopass :: register_unary
        procedure, nopass :: register_function => register_unary
        procedure, nopass :: register_binary
        procedure, nopass :: register_operator => register_binary
        procedure, nopass :: ignore_tokens
        procedure, nopass :: is_enclosed
        procedure, private :: convert_to_RPN
        procedure, private :: eval_expression
    end type

    abstract interface
        function interface_on_binary(self, lhs, opr, rhs) result(ans)
            import :: parser_t, token_t
            class(parser_t) :: self
            type(token_t) :: lhs
            type(token_t) :: opr
            type(token_t) :: rhs
            type(token_t) :: ans
        end function

        function interface_on_operand(self, opr) result(ans)
            import :: parser_t, token_t
            class(parser_t) :: self
            type(token_t) :: opr
            type(token_t) :: ans
        end function

        function interface_on_unary(self, opr, arg) result(ans)
            import :: parser_t, token_t
            class(parser_t)  :: self
            type(token_t) :: opr
            type(token_t) :: arg
            type(token_t) :: ans
        end function
    end interface

    character(:), allocatable :: REGISTERED_IGNORED(:)
    character(:), allocatable :: REGISTERED_UNARY(:)
    character(:), allocatable :: REGISTERED_BINARY(:)
    character(:), allocatable :: REGISTERED_RIGHT_ASSOC(:)

contains

    subroutine register_unary(names)
        character(*) :: names(:)

        if (allocated(REGISTERED_UNARY)) then
            REGISTERED_UNARY = [REGISTERED_UNARY, names]
        else
            REGISTERED_UNARY = [names]
        end if

        if (allocated(REGISTERED_RIGHT_ASSOC)) then
            REGISTERED_RIGHT_ASSOC = [REGISTERED_RIGHT_ASSOC, names]
        else
            REGISTERED_RIGHT_ASSOC = [names]
        end if
    end subroutine

    subroutine register_binary(names, is_right_assoc)
        character(*) :: names(:)
        logical, optional :: is_right_assoc

        if (allocated(REGISTERED_BINARY)) then
            REGISTERED_BINARY = [REGISTERED_BINARY, names]
        else
            REGISTERED_BINARY = [names]
        end if

        if (present(is_right_assoc)) then
            if (.not. is_right_assoc) return
            if (allocated(REGISTERED_RIGHT_ASSOC)) then
                REGISTERED_RIGHT_ASSOC = [REGISTERED_RIGHT_ASSOC, names]
            else
                REGISTERED_RIGHT_ASSOC = [names]
            end if
        end if

    end subroutine

    subroutine ignore_tokens(ignored_names)
        character(*) :: ignored_names(:)
        if (allocated(REGISTERED_IGNORED)) then
            REGISTERED_IGNORED = [REGISTERED_IGNORED, ignored_names]
        else
            REGISTERED_IGNORED = [ignored_names]
        end if
    end subroutine

    function parse(self, infix) result(ans)
        class(parser_t) :: self
        character(*) :: infix
        type(token_list) :: tokens
        type(token_list) :: postfix
        type(token_list) :: output
        type(token_t)    :: ans

        self % tokenizer % validate_token => by_expression
        tokens = self % tokenizer % tokenize(infix)

        if (is_enclosed(tokens)) then
            call self % convert_to_RPN(tokens, postfix)
            call self % eval_expression(postfix, output)
        else
            write(error_unit,*) 'Unmatched parenthesis on: '//infix
            stop
        end if
        ans = output % pop()
    contains
        logical function by_expression(token)
            character(*), intent(in) :: token
            by_expression = .true. &
            .and. .not. is_ignored(token) &
            .and. ( &
                     is_unary(token) &
                .or. is_binary(token) &
                .or. is_operand(token) &
                .or. is_open_paren(token) &
                .or. is_close_paren(token) &
            )
        end function
    end function

    subroutine convert_to_RPN(self, tokens, postfix)
        class(parser_t) :: self
        type(token_list), intent(in)  :: tokens
        type(token_list), intent(out) :: postfix
        type(token_list) :: operators
        type(token_t) :: token, prev_token, top
        integer :: i

        prev_token % type = 'null'
        

        do i=1,size(tokens)
            token = tokens % get(i)
            ! Is a registered function?
            if (is_unary(token % string) .and. ( &
                prev_token % type == 'null'       .or. &
                prev_token % type == 'open_paren' .or. &
                prev_token % type == 'unary'      .or. &
                prev_token % type == 'binary'          &
               ) &
            ) then
                ! Push into operator stack
                token % type = "unary"
                call operators % append(token)
            ! Is a open paren.
            else if (is_open_paren(token % string)) then
                ! Push parenthesis
                token % type = "open_paren"
                call operators % append(token)
            ! Is a close paren.
            else if (is_close_paren(token % string)) then
                token % type = "close_paren"
                ! Pop parenthesis
                do
                    top = operators % pop()
                    if (top % type == 'open_paren') exit
                    call postfix % append(top)
                end do
            else if (is_binary(token % string)) then
                token % type = "binary"
                ! Is a registered operator
                do while (size(operators) > 0)
                    top = operators % peek()
                    if (.not. is_open_paren(top%string) &
                        .and. ( &
                            (is_left_assoc(token%string) .and. priority(token) <= priority(top)) &
                            .or. &
                            (is_right_assoc(token%string) .and. priority(token) < priority(top)) &
                        ) &
                    ) then
                        ! Pop operator to output"
                        call postfix % append(top)
                        top = operators % pop()
                    else
                        exit
                    end if
                end do
                call operators % append(token)
            ! Is a valid operand: number or variable name?
            else if (is_operand(token % string)) then
                ! Push into output queue
                token % type = "operand"
                call postfix % append(token)
            end if

            prev_token = token
        end do

        ! Pop all remaining operators
        do i=1,size(operators)
            top = operators % pop()
            call postfix % append(top)
        end do
    end subroutine

    subroutine eval_expression(self, postfix, output)
        class(parser_t) :: self
        type(token_list), intent(in out) :: postfix
        type(token_list), intent(out) :: output
        type(token_t) :: token, new_token
        type(token_t) :: op1, op2, ret
        integer :: i

        do i=1,size(postfix)
            token = postfix % get(i)
            select case(token % type)
            case ("unary")
                ! Pop the first operand
                op1 = output % pop()
                ! Evaluate the return
                ret = self % on_unary(token, op1)
                ! Put result back in the stack
                call output % append(ret)
            case ("binary")
                ! Pop the first operand
                op1 = output % pop()
                ! Pop the second operand
                op2 = output % pop()
                !
                ret = self % on_binary(op2,token,op1)
                ! Perform operation
                call output % append(ret)
            case ("operand")
                ret = self % on_operand(token)
                ! Convert operand string into a actual operand
                call output % append(ret)
            end select
        end do
    end subroutine

    integer function priority(token)
        type(token_t), intent(in) :: token 
        if (token % type == 'unary'      .or. &
            token % type == 'open_paren' .or. &
            token % type == 'close_paren') then
            priority = 5
        else if (token % string == '^' .or. token % string == '**') then
            priority = 4
        else if (token % string == '*' .or. token % string == '/') then
            priority = 3
        else if (token % string == '+' .or. token % string == '-') then
            priority = 2
        else
            priority = -1
        end if
    end function

    logical function is_open_paren(input)
        character(*), intent(in) :: input
        is_open_paren = input == '('
    end function

    logical function is_close_paren(input)
        character(*), intent(in) :: input
        is_close_paren = input == ')'
    end function

    logical function is_operand(input)
        character(*), intent(in) :: input
        integer :: i
        is_operand = all([( is_alphanum(input(i:i)), i = 1, len(input) )])
    end function

    logical function is_alphanum(input)
        character, intent(in) :: input
        is_alphanum = scan(input, "abcdefghijklmnopqrstuvwxyz_"//&
                                  "ABCDEFGUIJKLMNOPQRSTUVWXYZ."//&
                                  "0123456789") > 0
    end function

    logical function is_left_assoc(input)
        character(*), intent(in) :: input
        is_left_assoc = .not. is_right_assoc(input)
    end function

    logical function is_right_assoc(input)
        character(*), intent(in) :: input
        if (allocated(REGISTERED_RIGHT_ASSOC)) then
            is_right_assoc = any(REGISTERED_RIGHT_ASSOC == input)
        else
            is_right_assoc = .false.
        end if
    end function

    logical function is_ignored(input)
        character(*), intent(in) :: input
        if (allocated(REGISTERED_IGNORED)) then
            is_ignored = any(REGISTERED_IGNORED == input)
        else
            is_ignored = .false.
        end if
    end function

    logical function is_unary(input)
        character(*), intent(in) :: input
        if (allocated(REGISTERED_UNARY)) then
            is_unary = any(REGISTERED_UNARY == input)
        else
            is_unary = .false.
        end if
    end function

    logical function is_binary(input)
        character(*), intent(in) :: input
        if (allocated(REGISTERED_BINARY)) then
            is_binary = any(REGISTERED_BINARY == input)
        else
            is_binary = .false.
        end if
    end function

    logical function is_enclosed(tokens)
        type(token_list) :: tokens
        type(token_t)    :: token
        integer :: i,k
        is_enclosed = .false.
        k = 0
        do i = 1, size(tokens)
            token = tokens % get(i)
            if (is_open_paren(token % string)) then
                k = k + 1
            else if (is_close_paren(token % string)) then
                k = k - 1
            end if
            if (k < 0) exit
        end do
        if (k == 0) is_enclosed = .true.
    end function

end module
