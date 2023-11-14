Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  DT_MOD                        share/modules/dt_mod.F        
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        ENG_QAPRINT_DRIVER            source/output/qaprint/eng_qaprint_driver.F
Chd|        ENG_QAPRINT_DTINPUT           source/output/qaprint/eng_qaprint_dtinput.F
Chd|        FORINT                        source/elements/forint.F      
Chd|        FREFORM                       source/input/freform.F        
Chd|        LECINP                        source/input/lecinp.F         
Chd|        LECTUR                        source/input/lectur.F         
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S10MALLGEO3                   source/elements/solid/solide10/s10mallgeo3.F
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SGEODEL3                      source/elements/solid/solide/sgeodel3.F
Chd|        SMALLGEO3                     source/elements/solid/solide/smallgeo3.F
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|        TSHGEODEL3                    source/elements/thickshell/solidec/tshgeodel3.F
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  INIT_IDEL_BRICK               share/modules/dt_mod.F        
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|====================================================================
          SUBROUTINE INIT_IDEL_BRICK(THIS)
            IMPLICIT NONE
#include    "constant.inc" 
           
            TYPE (DT_), INTENT(INOUT) :: THIS

            THIS%IDEL_BRICK = 0
            IF ((THIS%BRICK_DEL_DEFV_MIN+THIS%BRICK_DEL_DEFV_MAX+
     .          THIS%BRICK_DEL_ASP_MAX+THIS%BRICK_DEL_COL_MIN)>ZERO) THIS%IDEL_BRICK = 1 
                                                            
          END SUBROUTINE    
               
      END MODULE DT_MOD

