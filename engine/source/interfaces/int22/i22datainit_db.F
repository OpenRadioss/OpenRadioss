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
Chd|  I22DATAINIT_DB                source/interfaces/int22/i22datainit_db.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE I22DATAINIT_DB()
      USE I22TRI_MOD
      USE I22BUFBRIC_MOD   
C=======================================================================   
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandoned, and is not an official option.
C
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "subvolumes.inc"
#include      "i22edges.inc"
#include      "inter22.inc"
#include      "task_c.inc"
C-----------------------------------------------


C=======================================================================  
C               D E B U G   F L A G S
C=======================================================================  
C Debug without inter22 - necessary to compare numerical solution w & w/o inter22

      IF(INT22==0)THEN
        IBUG22              = 0
        IBUG22_ccBufList    = 0
        IBUG22_antidiff     = 0
        IBUG22_upwind       = 0               
        IBUG22_tri          = 0
        IBUG22_trivox       = 0
        IBUG22_intersect    = 0
        IBUG22_subvol       = 0
        IBUG22_aera         = 0
        IBUG22_ident        = 0
        IBUG22_convec       = 0
        IBUG22_spe_convec   = 0       
        IBUG22_eos          = 0
        IBUG22_dvol         = 0      
        IBUG22_sigeps51     = 0      
        IBUG22_ssp          = 0   
        IBUG22_flux         = 0
        IBUG22_flux22       = 0
        IBUG22_volfrac      = 0      
        IBUG22_conti        = 0   
        IBUG22_fint         = 0     
        IBUG22_sinit        = 0                          
        IBUG22_nodalp       = 0
        IBUG22_amouil       = 0                                   
        IBUG22_fcont        = 0
        IBUG22_fcontASS     = 0        
        IBUG22_Swet         = 0
        IBUG22_destroy      = 0         
        IBUG22_merge        = 0  
        IBUG22_link_switch  = 0
        IBUG22_prediction   = 0        
        IBUG22_UndirectLink = 0      
        IBUG22_Truss        = 0          
        IBUG22_OrphanNodes  = 0                  
        IBUG22_DtMin        = 0        
        IBUG22_OUTP_CCbuffer= 0    !enables to output bricks in cut cell buffer (HyperMesh tcl script)         
        IBUG22_OUTP_IntPoint= 0    !enables to output intersection points (HyperMesh tcl script)               
        IBUG22_NVAR         = 0   !-1: all, 1:mass, 2:energy           
        IBUG22_ITRIMAT      = 0   !-1: all, 1:submat1, 2:submat2, 3:submat3, 4:submat4        
        IBUG22_CC_LENGTH    = 0
      ENDIF


      RETURN
      END


