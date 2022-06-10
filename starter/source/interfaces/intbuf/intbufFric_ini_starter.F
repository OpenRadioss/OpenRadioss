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
Chd|  INTBUF_FRIC_INI_STARTER       source/interfaces/intbuf/intbufFric_ini_starter.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INTBUF_FRIC_INI_STARTER(INTBUF_FRIC_TAB  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUF_FRIC_MOD                     
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   M y  A l l o c a t e                         
C----------------------------------------------- 
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
c      my_real
c     .   
      TYPE(INTBUF_FRIC_STRUCT_) :: INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NINF , NSET ,S_TABPARTS_FRIC ,IORTH ,MFROT ,LENC
C-----------------------------------------------

      DO NINF = 1,NINTERFRIC 

        NSET = INTBUF_FRIC_TAB(NINF)%NSETPRTS       
        S_TABPARTS_FRIC = INTBUF_FRIC_TAB(NINF)%S_TABPARTS_FRIC 
        IORTH = INTBUF_FRIC_TAB(NINF)%IORTHFRIC
        MFROT = INTBUF_FRIC_TAB(NINF)%FRICMOD
        IF(MFROT ==0 ) THEN   
           LENC =2  
        ELSE
           LENC = 8
        ENDIF 
C=======================================================================
c       ALLOCATIONS
C=======================================================================

          MY_ALLOCATE(INTBUF_FRIC_TAB(NINF)%TABCOUPLEPARTS_FRIC,NSET) 
          INTBUF_FRIC_TAB(NINF)%TABCOUPLEPARTS_FRIC(1:NSET) = 0 

          MY_ALLOCATE(INTBUF_FRIC_TAB(NINF)%TABPARTS_FRIC,S_TABPARTS_FRIC) 
          INTBUF_FRIC_TAB(NINF)%TABPARTS_FRIC(1:S_TABPARTS_FRIC) = 0 

          MY_ALLOCATE(INTBUF_FRIC_TAB(NINF)%ADPARTS_FRIC,S_TABPARTS_FRIC+1) 
          INTBUF_FRIC_TAB(NINF)%ADPARTS_FRIC(1:S_TABPARTS_FRIC+1) = 0 

          MY_ALLOCATE(INTBUF_FRIC_TAB(NINF)%IFRICORTH,NSET) 
          INTBUF_FRIC_TAB(NINF)%IFRICORTH(1:NSET) = 0 

          IF (IORTH == 0 ) THEN
             MY_ALLOCATE(INTBUF_FRIC_TAB(NINF)%TABCOEF_FRIC,LENC*(NSET+1)) 
              INTBUF_FRIC_TAB(NINF)%TABCOEF_FRIC(1:LENC*(NSET+1)) = ZERO 
          ELSE
             MY_ALLOCATE(INTBUF_FRIC_TAB(NINF)%TABCOEF_FRIC,LENC+2*LENC*NSET) 
              INTBUF_FRIC_TAB(NINF)%TABCOEF_FRIC(1:2*LENC*NSET+LENC) = ZERO 
          ENDIF

C=======================================================================
c       END ALLOCATIONS
C=======================================================================
      ENDDO     ! NINF  = 1,NINTERFRIC
C-----
      RETURN

      END SUBROUTINE INTBUF_FRIC_INI_STARTER

Chd|====================================================================
Chd|  INTFRIC_WRESTI                source/interfaces/intbuf/intbufFric_ini_starter.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|====================================================================
      SUBROUTINE INTFRIC_WRESTI(INTBUF_FRIC_TAB, LENI)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUF_FRIC_MOD                     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENI
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, J, NSET
C--------------------------------------
C     ECRITURE DES DIMENSIONS
C--------------------------------------
      LENI=0
      DO N=1,NINTERFRIC
        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%NSETPRTS,LEN)
        LENI = LENI +1

        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%FRICMOD,LEN)
        LENI = LENI +1

        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%FRICFORM,LEN)
        LENI = LENI +1

        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%IFFILTER,LEN)
        LENI = LENI +1

        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%IORTHFRIC,LEN)
        LENI = LENI +1

        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%S_TABPARTS_FRIC ,LEN)
        LENI = LENI +1

        NSET = INTBUF_FRIC_TAB(N)%NSETPRTS
        LEN    = NSET
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%TABCOUPLEPARTS_FRIC,LEN)
        LENI = LENI + LEN

        LEN    = INTBUF_FRIC_TAB(N)%S_TABPARTS_FRIC
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%TABPARTS_FRIC,LEN)
        LENI = LENI + LEN

        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%ADPARTS_FRIC,LEN+1)
        LENI = LENI + LEN + 1

        LEN    = NSET
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%IFRICORTH,LEN)
        LENI = LENI + LEN

      END DO
      RETURN
      END SUBROUTINE INTFRIC_WRESTI
Chd|====================================================================
Chd|  INTFRIC_WRESTR                source/interfaces/intbuf/intbufFric_ini_starter.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|====================================================================
      SUBROUTINE INTFRIC_WRESTR(INTBUF_FRIC_TAB, LENR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUF_FRIC_MOD                     
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENR ,NSET ,IORTH ,MFROT ,LENC
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, J
C--------------------------------------
      LENR=0
      DO N=1,NINTERFRIC
        LEN    =1
        CALL WRITE_DB(INTBUF_FRIC_TAB(N)%XFILTR_FRIC,LEN)
        LENR   =LENR+1

        NSET = INTBUF_FRIC_TAB(N)%NSETPRTS
        IORTH = INTBUF_FRIC_TAB(N)%IORTHFRIC
        MFROT = INTBUF_FRIC_TAB(N)%FRICMOD
        IF(MFROT ==0 ) THEN   
           LENC =2  
        ELSE
           LENC = 8
        ENDIF 
        IF (IORTH == 0 ) THEN
           LEN    =LENC*(NSET+1)
           CALL WRITE_DB(INTBUF_FRIC_TAB(N)%TABCOEF_FRIC,LEN)
        ELSE
           LEN    = LENC +2*LENC*NSET
           CALL WRITE_DB(INTBUF_FRIC_TAB(N)%TABCOEF_FRIC,LEN)
        ENDIF
        LENR   =LENR+LEN

      END DO
      RETURN
      END SUBROUTINE  INTFRIC_WRESTR    
