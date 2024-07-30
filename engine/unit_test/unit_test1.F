      !||====================================================================
      !||    main                         ../engine/unit_test/unit_test1.F
      !||--- calls      -----------------------------------------------------
      !||    set_stack_unlimited          ../engine/unit_test/unlimit_stack.cpp
      !||    test_candidates              ../engine/source/interfaces/intsort/inter7_candidate_pairs.F
      !||--- uses       -----------------------------------------------------
      !||    inter7_candidate_pairs_mod   ../engine/source/interfaces/intsort/inter7_candidate_pairs.F
      !||====================================================================
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
