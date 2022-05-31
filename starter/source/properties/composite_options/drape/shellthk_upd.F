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
Chd|  SHELLTHK_UPD                  source/properties/composite_options/drape/shellthk_upd.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE SHELLTHK_UPD( 
     .                     DRAPE    , STACK, THK ,IXC     ,IXTG ,
     .                     IGEO     ,IWORKSH    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SUBMODEL_MOD
      USE STACK_MOD
      USE DRAPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "sphcom.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "drape_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IXC(NIXC,*),
     .        IXTG(NIXTG,*),IGEO(NPROPGI,*),IWORKSH(3,*)
      my_real :: 
     .      THK(*)
C-----------------------------------------------
      TYPE (DRAPE_)  :: DRAPE(*)
      TYPE (STACK_PLY) :: STACK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II,NPT,PID, IGTYP,IPOS,IPPID,IPMAT,IPANG, IPTHK,
     .           IPPOS, NTHK,ISUBS,J,I3,I4,ISH3N
      my_real :: THINNING, THKLY , POS, DT,TMIN,TMAX,THICKT

C-----------------------------------------------
C=======================================================================
!!###########################################################
C         
          DO II=1,NUMELC 
            NPT = IWORKSH(1,II)
            PID = IXC(6,II)
            IGTYP = IGEO(11,PID)
            IF(IGTYP /=17 .AND. IGTYP /= 51 .AND. IGTYP /= 52) CYCLE
            IPOS = IGEO(99,PID) 
C---
C  update the shell thickness if /DRAPE defined          
C---            
C  re-set the thickness of shell according to /DRAPE layer thinning
             TMIN   = EP20                  
             TMAX   = -EP20                 
             THICKT = ZERO                
             IPPID   = 2                  
             IPMAT   = IPPID + NPT        
             IPANG  =  1                  
             IPTHK  =  IPANG + NPT        
             IPPOS  =  IPTHK + NPT        
             NTHK   =  IPPOS + NPT        
             ISUBS  =  IWORKSH(3,II)         
             THICKT = STACK%GEO(1 ,ISUBS) 
C
              IF (IPOS == 1) THEN
                  DO J=1,NPT
                    I3 = IPTHK + J
                    I4 = IPPOS + J
                    THINNING = ONE
                    IF (DRAPE(II)%IDRAPE(J) > 0) THINNING = DRAPE(II)%RDRAPE(J,1)
                    THKLY  = STACK%GEO(I3 ,ISUBS)*THICKT
                    POS    = STACK%GEO(I4 ,ISUBS)*THICKT
                    DT   = HALF*THKLY*THINNING
                    TMIN = MIN(TMIN,POS - DT)		  
                    TMAX = MAX(TMAX,POS + DT)		  
                  ENDDO                                                             
                  THICKT = TMAX - TMIN
                  IF (THK(II) == ZERO) THK(II) = THICKT ! 
              ELSE 
                  DO J=1,NPT
                    THINNING = ONE 
                    I3 = IPTHK + J
                    I4 = IPPOS + J
                    IF (DRAPE(II)%IDRAPE(J) > 0) THINNING = DRAPE(II)%RDRAPE(J,1)
                    THKLY  = STACK%GEO(I3 ,ISUBS)*THICKT
                    THICKT = THICKT + THKLY*THINNING
                  ENDDO
                  IF (THK(II) == ZERO) THK(II) = THICKT
              ENDIF ! IF (IPOS == 1)
            ENDDO ! numelc
C --- T3
          DO II=1,NUMELTG 
            ISH3N = NUMELC + II
            NPT = IWORKSH(1,ISH3N)
            PID = IXTG(5,II)
            IGTYP = IGEO(11,PID)
            IF(IGTYP /=17 .AND. IGTYP /= 51 .AND. IGTYP /= 52) CYCLE
            IPOS = IGEO(99,PID) 
C---
C  update the shell thickness if /DRAPE defined          
C---            
C  re-set the thickness of shell according to /DRAPE layer thinning
             TMIN = EP20                  
             TMAX = -EP20                 
             THICKT = ZERO                
             IPPID   = 2                  
             IPMAT   = IPPID + NPT        
             IPANG  =  1                  
             IPTHK  =  IPANG + NPT        
             IPPOS  =  IPTHK + NPT        
             NTHK   =  IPPOS + NPT        
             ISUBS =IWORKSH(3,ISH3N)         
             THICKT = STACK%GEO(1 ,ISUBS) 
C
              IF (IPOS == 1) THEN
                  DO J=1,NPT
                    I3 = IPTHK + J
                    I4 = IPPOS + J
                    THINNING = ONE
                    IF (DRAPE(ISH3N)%IDRAPE(J) > 0) THINNING = DRAPE(ISH3N)%RDRAPE(J,1)
                    THKLY  = STACK%GEO(I3 ,ISUBS)*THICKT
                    POS    = STACK%GEO(I4 ,ISUBS)*THICKT
                    DT   = HALF*THKLY*THINNING
                    TMIN = MIN(TMIN,POS - DT)		  
                    TMAX = MAX(TMAX,POS + DT)		  
                  ENDDO                                                             
                  THICKT = TMAX - TMIN
                  IF (THK(ISH3N) == ZERO) THK(ISH3N) = THICKT ! 
              ELSE                              
                  DO J=1,NPT
                    THINNING = ONE
                    IF (DRAPE(ISH3N)%IDRAPE(J) > 0) THINNING = DRAPE(ISH3N)%RDRAPE(J,1)
                    THKLY  = STACK%GEO(I3 ,ISUBS)*THICKT
                    THICKT = THICKT + THKLY*THINNING
                  ENDDO
                  IF (THK(ISH3N) == ZERO) THK(ISH3N) = THICKT
              ENDIF ! IF (IPOS == 1)
            ENDDO ! numelc           
C============================================================================
       
       RETURN
       END
