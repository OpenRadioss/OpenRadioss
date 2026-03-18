# OpenRadioss Element mockup

Simplified mockup for ELEMENT computation extracted from OpenRadioss

## How to build

This code is intended to be compiled on Linux only.

Several scripts are available for different compilers / architecture.

## Source layout

        RADIOSS_TEST
             |
             | call 
             |------->  INIT
             |
             | call 
             |------->  INIT_DATA
             |
             | call      
             |------->  GROUP_COMPUTATION
                                 |
                                 | call 
                                 |------->  SHELL_COMPUTATION
                                 |                   |
                                 |                   | call 
                                 |                   |------->  LAW2_COMPUTATION
                                 |                                      |
                                 |                                      | call 
                                 |                                      |------->  M2CPLR_COMPUTATION
                                 |
                                 |
                                 | call 
                                 |------->  UPDATE_FOR
                                 |
                                 |


        RADIOSS_TEST
             |
             |  
             |------->  INIT
             |
             |  
             |------->  INIT_DATA
             |
             | // OpenMP parallel treatment
             |------->  GROUP_COMPUTATION
               // end of OpenMP parallel treatment



        GROUP_COMPUTATION
                |
                |
        loop over the element groups, 1 OpenMP thread per group
                |------->  SHELL_COMPUTATION



        SHELL_COMPUTATION
                |
          initialization of local array with global data
                | 
                |       material computation
                |------->  LAW2_COMPUTATION
                |
                |       update of force/displacement...
                |------->  UPDATE_FOR
