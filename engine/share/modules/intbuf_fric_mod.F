Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  INTBUF_FRIC_MOD               share/modules/intbuf_fric_mod.F
Chd|-- called by -----------
Chd|        I11MAINF                      source/interfaces/int11/i11mainf.F
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|        INTFRIC_RRESTI                share/modules/intbuf_fric_mod.F
Chd|        INTFRIC_RRESTR                share/modules/intbuf_fric_mod.F
Chd|        INTFRIC_WRESTI                share/modules/intbuf_fric_mod.F
Chd|        INTFRIC_WRESTR                share/modules/intbuf_fric_mod.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|        INTBUFFRIC_MOD                share/modules/restart_mod.F   
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INTBUF_FRIC_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
      TYPE INTBUF_FRIC_STRUCT_  

      INTEGER ::    NSETPRTS       ! number of couple of parts 
      INTEGER ::    FRICMOD        ! the friction model
      INTEGER ::    FRICFORM       ! the friction formulation ( viscous or incremental)
      INTEGER ::    IFFILTER       !Friction filtering flag 
      INTEGER ::    S_TABPARTS_FRIC  ! number of parts  
      INTEGER ::    IORTHFRIC          ! Flag for orthotropic friction
       my_real :: XFILTR_FRIC        !Filtering coefficient

      INTEGER, DIMENSION(:), ALLOCATABLE :: TABCOUPLEPARTS_FRIC    ! Table of couple of parts
      INTEGER, DIMENSION(:), ALLOCATABLE :: TABPARTS_FRIC          ! Table of parts
      INTEGER, DIMENSION(:), ALLOCATABLE :: ADPARTS_FRIC           ! Table of adress of couple of parts
      INTEGER, DIMENSION(:), ALLOCATABLE :: IFRICORTH              ! Table of Orthotropic type of couple of parts 
      my_real,DIMENSION(:), ALLOCATABLE :: TABCOEF_FRIC            ! Table of friction coefficients

      END TYPE INTBUF_FRIC_STRUCT_

      END MODULE INTBUF_FRIC_MOD



Chd|====================================================================
Chd|  INTFRIC_WRESTI                share/modules/intbuf_fric_mod.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        INTBUF_FRIC_MOD               share/modules/intbuf_fric_mod.F
Chd|====================================================================
      SUBROUTINE INTFRIC_WRESTI(INTBUF_FRIC_TAB)
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
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, J, NSET
C--------------------------------------
C     ECRITURE DES DIMENSIONS
C--------------------------------------
      DO N=1,NINTERFRIC
        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%NSETPRTS,LEN)

        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%FRICMOD,LEN)

        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%FRICFORM,LEN)

        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%IFFILTER,LEN)

        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%IORTHFRIC,LEN)

        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%S_TABPARTS_FRIC ,LEN)

        NSET = INTBUF_FRIC_TAB(N)%NSETPRTS
        LEN    = NSET
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%TABCOUPLEPARTS_FRIC,LEN)

        LEN    = INTBUF_FRIC_TAB(N)%S_TABPARTS_FRIC 
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%TABPARTS_FRIC,LEN)

        LEN    = INTBUF_FRIC_TAB(N)%S_TABPARTS_FRIC +1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%ADPARTS_FRIC,LEN)

        LEN  = INTBUF_FRIC_TAB(N)%NSETPRTS
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%IFRICORTH,LEN)
      END DO
      RETURN
      END SUBROUTINE INTFRIC_WRESTI
Chd|====================================================================
Chd|  INTFRIC_WRESTR                share/modules/intbuf_fric_mod.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        INTBUF_FRIC_MOD               share/modules/intbuf_fric_mod.F
Chd|====================================================================
      SUBROUTINE INTFRIC_WRESTR(INTBUF_FRIC_TAB)
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
      INTEGER NSET 
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, J, IORTH ,MFROT ,LENC
C--------------------------------------
      DO N=1,NINTERFRIC
        LEN    =1
        CALL WRITE_DB(INTBUF_FRIC_TAB(N)%XFILTR_FRIC,LEN)

        NSET = INTBUF_FRIC_TAB(N)%NSETPRTS
        IORTH = INTBUF_FRIC_TAB(N)%IORTHFRIC
        MFROT = INTBUF_FRIC_TAB(N)%FRICMOD
        IF(MFROT ==0 ) THEN   
           LENC =2  
        ELSE
           LENC = 8
        ENDIF 
        IF(IORTH == 0) THEN
           LEN    =LENC*(NSET+1)
           CALL WRITE_DB(INTBUF_FRIC_TAB(N)%TABCOEF_FRIC,LEN)
        ELSE
           LEN    =LENC+2*LENC*NSET
           CALL WRITE_DB(INTBUF_FRIC_TAB(N)%TABCOEF_FRIC,LEN)
        ENDIF
      END DO
      RETURN
      END SUBROUTINE INTFRIC_WRESTR

Chd|====================================================================
Chd|  INTFRIC_RRESTI                share/modules/intbuf_fric_mod.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_I_C                      ../common_source/tools/input_output/write_routtines.c
Chd|        INTBUF_FRIC_MOD               share/modules/intbuf_fric_mod.F
Chd|====================================================================
      SUBROUTINE INTFRIC_RRESTI(INTBUF_FRIC_TAB)
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
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, J, NSET,LENI
C--------------------------------------
C     ECRITURE DES DIMENSIONS
C--------------------------------------
      LENI=0
      DO N=1,NINTERFRIC
        LEN    =1
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%NSETPRTS,LEN)
        LENI = LENI + LEN

        LEN    =1
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%FRICMOD,LEN)
        LENI = LENI + LEN

        LEN    =1
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%FRICFORM,LEN)
        LENI = LENI + LEN

        LEN    =1
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%IFFILTER,LEN)
        LENI = LENI + LEN

        LEN    =1
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%IORTHFRIC,LEN)
        LENI = LENI + LEN

        LEN    =1
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%S_TABPARTS_FRIC,LEN)
        LENI = LENI + LEN

        LEN = INTBUF_FRIC_TAB(N)%NSETPRTS
        ALLOCATE(INTBUF_FRIC_TAB(N)%TABCOUPLEPARTS_FRIC(LEN))
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%TABCOUPLEPARTS_FRIC,LEN)
        LENI = LENI + LEN

        LEN    = INTBUF_FRIC_TAB(N)%S_TABPARTS_FRIC
        ALLOCATE(INTBUF_FRIC_TAB(N)%TABPARTS_FRIC(LEN))
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%TABPARTS_FRIC,LEN)
        LENI = LENI + LEN

        LEN = INTBUF_FRIC_TAB(N)%S_TABPARTS_FRIC +1
        ALLOCATE(INTBUF_FRIC_TAB(N)%ADPARTS_FRIC(LEN))
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%ADPARTS_FRIC,LEN)
        LENI = LENI + LEN

        LEN = INTBUF_FRIC_TAB(N)%NSETPRTS
        ALLOCATE(INTBUF_FRIC_TAB(N)%IFRICORTH(LEN))
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%IFRICORTH,LEN)
        LENI = LENI + LEN

      END DO
      RETURN
      END SUBROUTINE INTFRIC_RRESTI

Chd|====================================================================
Chd|  INTFRIC_RRESTR                share/modules/intbuf_fric_mod.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        INTBUF_FRIC_MOD               share/modules/intbuf_fric_mod.F
Chd|====================================================================
      SUBROUTINE INTFRIC_RRESTR(INTBUF_FRIC_TAB)
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
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, J ,NSET ,IORTH ,MFROT ,LENC
C--------------------------------------
      DO N=1,NINTERFRIC
        LEN    =1
        CALL READ_DB(INTBUF_FRIC_TAB(N)%XFILTR_FRIC,LEN)
        NSET = INTBUF_FRIC_TAB(N)%NSETPRTS
        IORTH = INTBUF_FRIC_TAB(N)%IORTHFRIC
        MFROT = INTBUF_FRIC_TAB(N)%FRICMOD
        IF(MFROT ==0 ) THEN   
           LENC =2  
        ELSE
           LENC = 8
        ENDIF

        IF(IORTH == 0) THEN
           LEN    =LENC*(NSET+1)
        ELSE
           LEN    =LENC+2*LENC*NSET
        ENDIF

        IF(LEN>0)THEN
           ALLOCATE(INTBUF_FRIC_TAB(N)%TABCOEF_FRIC(LEN))
           CALL READ_DB(INTBUF_FRIC_TAB(N)%TABCOEF_FRIC,LEN)
        ENDIF

      END DO
      RETURN
      END SUBROUTINE INTFRIC_RRESTR
