      MODULE DT_MOD
      
        IMPLICIT NONE
#include      "my_real.inc"
        
        !TYPE definition
        TYPE DT_
          INTEGER IDEL_BRICK
          MY_REAL BRICK_CST_DEFV_MIN,BRICK_CST_COL_MIN   ! /DT/BRICK/CST/1
          MY_REAL BRICK_DEL_DEFV_MIN,BRICK_DEL_DEFV_MAX, ! /DT/BRICK/DEL/1
     .            BRICK_DEL_ASP_MAX,BRICK_DEL_COL_MIN
        END TYPE DT_
        
        CONTAINS        
          
C INITIALISATION : CALL DT%INIT_IDEL_BRICK()
          SUBROUTINE INIT_IDEL_BRICK(THIS)		
            IMPLICIT NONE
#include    "constant.inc" 
           
            TYPE (DT_), INTENT(INOUT) :: THIS
			
            THIS%IDEL_BRICK = 0
            IF ((THIS%BRICK_DEL_DEFV_MIN+THIS%BRICK_DEL_DEFV_MAX+
     .          THIS%BRICK_DEL_ASP_MAX+THIS%BRICK_DEL_COL_MIN)>ZERO) THIS%IDEL_BRICK = 1 
                                                            
          END SUBROUTINE    
               
      END MODULE DT_MOD

