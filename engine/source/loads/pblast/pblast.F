Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  PBLAST                        source/loads/pblast/pblast.F  
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        PBLAST_1                      source/loads/pblast/pblast_1.F
Chd|        PBLAST_2                      source/loads/pblast/pblast_2.F
Chd|        PBLAST_3                      source/loads/pblast/pblast_3.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        PBLAST_MOD                    ../common_source/modules/pblast_mod.F
Chd|====================================================================
      SUBROUTINE PBLAST (ILOADP  ,FAC     ,A      ,
     2                   V       ,X       ,
     3                   TFEXC   ,IADC    ,
     4                   FSKY    ,FSKYV   ,LLOADP ,FEXT    ,ITAB   ,
     5                   H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD 
      USE PBLAST_MOD
      USE GROUPDEF_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "mvsiz_p.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLOADP(SLLOADP)
      INTEGER ILOADP(SIZLOADP,NLOADP)
      INTEGER  IADC(*)
      INTEGER, INTENT(IN) :: ITAB(NUMNOD)
      my_real
     .   FAC(LFACLOAD,NLOADP), A(3,NUMNOD), V(3,NUMNOD),
     .   X(3,NUMNOD), TFEXC,
     .   FSKY(8,SFSKY/8), FSKYV(SFSKY/8,8),FEXT(3,NUMNOD)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER NL, ABAC_ID, NDT, NDT0, USER_ID, ID
       my_real :: T0INF_LOC
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C this subroutines is applying pressure load to
C a segment submitted to a blast wave which
C characteristics are built from TM5-1300 abacuses which are in unit system {cm g mus, bar},  pressure must also be converted into Mbar before switching from unit system.
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
       IF(NLOADP_B==0)THEN
         T0INF_PBLAST = EP20
         DT_PBLAST    = EP20
         IDT_PBLAST   = 0
         RETURN 
       ENDIF     
C-----------------------------------------------,
C   S o u r c e   C o d e
C-----------------------------------------------
       TFEXC     = ZERO
       T0INF_LOC = EP20
       USER_ID = 0

       !-----------------------------------------------
       !   LOOP OVER all /LOAD/PBLAST options
       !----------------------------------------------- 
       DO NL=NLOADP_F+1, NLOADP_F+NLOADP_B

         ABAC_ID = ILOADP(07,NL)
         ID      = ILOADP(08,NL) !user_id                
         NDT     = PBLAST_NDT                            
         NDT0    = ILOADP(10,NL)                         
         IF(NDT0/=0) NDT = NDT0                  
                   
         
         SELECT CASE(ABAC_ID)
           CASE(1)
             !--- LOADING MODEL : FREE AIR, SPHERICAL CHARGE
             CALL PBLAST_1(
     1                   ILOADP  ,FAC     ,A        ,
     2                   V       ,X       ,
     3                   TFEXC   ,IADC    ,
     4                   FSKY    ,FSKYV   ,LLOADP   ,FEXT   ,ITAB ,
     5                   H3D_DATA,NL      ,T0INF_LOC)
     
           CASE(2)
             !--- LOADING MODEL : GROUND REFLECTION, HEMISPHERICAL CHARGE
             CALL PBLAST_2(
     1                   ILOADP  ,FAC     ,A        ,
     2                   V       ,X       ,
     3                   TFEXC   ,IADC    ,
     4                   FSKY    ,FSKYV   ,LLOADP   ,FEXT   ,ITAB ,
     5                   H3D_DATA,NL      ,T0INF_LOC)           
           CASE(3)
             !--- LOADING MODEL : SURFACE BURST, SPHERICAL CHARGE
             CALL PBLAST_3(
     1                   ILOADP  ,FAC     ,A        ,
     2                   V       ,X       ,
     3                   TFEXC   ,IADC    ,
     4                   FSKY    ,FSKYV   ,LLOADP   ,FEXT   ,ITAB ,
     5                   H3D_DATA,NL      ,T0INF_LOC) 
     
#include "lockon.inc"
      IF(T0INF_LOC<T0INF_PBLAST) USER_ID   = ID
#include "lockoff.inc"

           END SELECT 
     
         ENDDO !next NL
         
      !this is required to compute pblast Time step : DT=inf(T0)/N(parameter)
#include "lockon.inc"
      T0INF_PBLAST = MIN(T0INF_LOC,T0INF_PBLAST)
      DT_PBLAST    = T0INF_PBLAST/NDT
      IDT_PBLAST   = USER_ID
#include "lockoff.inc"                 

      END SUBROUTINE
