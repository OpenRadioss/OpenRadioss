        program main

            use INTER7_CANDIDATE_PAIRS_MOD
            implicit none
            interface
             subroutine set_stack_unlimited() bind(C, name="set_stack_unlimited")
             end subroutine set_stack_unlimited
            end interface
            character(len=256) :: filename
            call set_stack_unlimited() 
            call get_command_argument(1, filename)
            call test_candidates(filename) 
        
        end program main 
