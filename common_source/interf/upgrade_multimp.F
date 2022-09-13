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
Chd|  UPGRADE_MULTIMP               interf/upgrade_multimp.F      
Chd|-- called by -----------
Chd|        ININTR                        starter/source/interfaces/interf1/inintr.F
Chd|        ININTR_THKVAR                 starter/source/interfaces/interf1/inintr_thkvar.F
Chd|        I10MAIN_TRI                   engine/source/interfaces/intsort/i10main_tri.F
Chd|        I11MAIN_TRI                   engine/source/interfaces/intsort/i11main_tri.F
Chd|        I20MAIN_TRI                   engine/source/interfaces/intsort/i20main_tri.F
Chd|        I21MAIN_TRI                   engine/source/interfaces/intsort/i21main_tri.F
Chd|        I22MAIN_TRI                   engine/source/interfaces/intsort/i22main_tri.F
Chd|        I23MAIN_TRI                   engine/source/interfaces/intsort/i23main_tri.F
Chd|        I24MAIN_TRI                   engine/source/interfaces/intsort/i24main_tri.F
Chd|        I25MAIN_TRI                   engine/source/interfaces/intsort/i25main_tri.F
Chd|        I7MAIN_TRI                    engine/source/interfaces/intsort/i7main_tri.F
Chd|        INTER_SORT_07                 engine/source/interfaces/int07/inter_sort_07.F
Chd|-- calls ---------------
Chd|        ARRET                         starter/source/system/arret.F 
Chd|        ARRET                         engine/source/system/arret.F  
Chd|        ARRET_MESSAGE                 starter/source/interfaces/int24/arret_message.F
Chd|        ARRET_MESSAGE                 engine/source/system/arret_message.F
Chd|        INTBUFDEF_MOD                 modules/intbufdef_mod.F       
Chd|        RESTMOD                       starter/share/modules1/restart_mod.F
Chd|        RESTMOD                       engine/share/modules/restart_mod.F
Chd|====================================================================
      SUBROUTINE UPGRADE_MULTIMP(NI,MULTIMP_PARAMETER,INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RESTMOD
      USE INTBUFDEF_MOD
C-------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NI, MULTIMP_PARAMETER

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITY,IFQ,INACTI,MFROT,NCONT,ITIED,
     .        NCONTE,NSN,NRTM,IEDGE1,FLAGREMNODE
      INTEGER I,N,OLD_SIZE,NEW_SIZE,MULTIMP,
     .        OLD_SIZE2,NEW_SIZE2,IERR,
     .        OLD_SIZE_OPT, NEW_SIZE_OPT

      INTEGER, DIMENSION (:), ALLOCATABLE :: OLD_TAB_I, OLD_TAB2_I, OLD_TAB3_I
      my_real, DIMENSION (:), ALLOCATABLE :: OLD_TAB_R, OLD_TAB2_R
C======================================================================|
      IERR=0

      MULTIMP  = IPARI(NPARI*(NI-1)+23)
      NCONT    = IPARI(NPARI*(NI-1)+18)
      OLD_SIZE = MULTIMP * NCONT
      NEW_SIZE = MULTIMP_PARAMETER * NCONT

      !use for type20
      NCONTE    = NCONT
      OLD_SIZE2 = MULTIMP * NCONTE
      NEW_SIZE2 = MULTIMP_PARAMETER * NCONTE

C Set the new MULTIMP parameter for the given Interface
      IPARI(NPARI*(NI-1)+23) = MULTIMP_PARAMETER
C Set Global parameters
      ITY    =     IPARI(NPARI*(NI-1)+7)
      INACTI = ABS(IPARI(NPARI*(NI-1)+22))
      MFROT  = IPARI(NPARI*(NI-1)+30)
      IFQ    =     IPARI(NPARI*(NI-1)+31)
      NSN    =     IPARI(NPARI*(NI-1)+5)
      NRTM   =     IPARI(NPARI*(NI-1)+4)
      IEDGE1 =     IPARI(NPARI*(NI-1)+58)
      FLAGREMNODE= IPARI(NPARI*(NI-1)+63)
      ITIED  =     IPARI(NPARI*(NI-1)+85)

C=======================================================================
      IF (ITY == 7)THEN
C=======================================================================
        ALLOCATE (OLD_TAB_I(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        ALLOCATE (OLD_TAB_R(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        IF(ITIED/=0)THEN
          ALLOCATE (OLD_TAB2_R(8*OLD_SIZE),STAT=IERR)
          IF(IERR/=0)GOTO 999
        ELSE
          ALLOCATE (OLD_TAB2_R(0),STAT=IERR)
          IF(IERR/=0)GOTO 999
        END IF
        
        !-----------!
        ! CAND_E
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_E(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_E)
        INTBUF_TAB%S_CAND_E = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_E(INTBUF_TAB%S_CAND_E),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_E(1:INTBUF_TAB%S_CAND_E)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_E(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! CAND_N
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_N(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_N)
        INTBUF_TAB%S_CAND_N = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_N(INTBUF_TAB%S_CAND_N),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_N(1:INTBUF_TAB%S_CAND_N)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_N(I) = OLD_TAB_I(I)
        ENDDO   
   
        IF (IFQ /= 0)THEN
          !-----------!
          ! IFPEN
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_I(I)=INTBUF_TAB%IFPEN(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%IFPEN)
          INTBUF_TAB%S_IFPEN = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%IFPEN(INTBUF_TAB%S_IFPEN),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%IFPEN(1:INTBUF_TAB%S_IFPEN)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%IFPEN(I) = OLD_TAB_I(I)
          ENDDO   

          !-----------!
          ! FTSAVX
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%FTSAVX(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%FTSAVX)
          INTBUF_TAB%S_FTSAVX = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%FTSAVX(INTBUF_TAB%S_FTSAVX),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%FTSAVX(1:INTBUF_TAB%S_FTSAVX)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%FTSAVX(I) = OLD_TAB_R(I)
          ENDDO    

          !-----------!
          ! FTSAVY
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%FTSAVY(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%FTSAVY)
          INTBUF_TAB%S_FTSAVY = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%FTSAVY(INTBUF_TAB%S_FTSAVY),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%FTSAVY(1:INTBUF_TAB%S_FTSAVY)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%FTSAVY(I) = OLD_TAB_R(I)
          ENDDO   

          !-----------!
          ! FTSAVZ
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%FTSAVZ(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%FTSAVZ)
          INTBUF_TAB%S_FTSAVZ = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%FTSAVZ(INTBUF_TAB%S_FTSAVZ),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%FTSAVZ(1:INTBUF_TAB%S_FTSAVZ)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%FTSAVZ(I) = OLD_TAB_R(I)
          ENDDO   
        ENDIF

        IF(ITIED/=0)THEN
          !-----------! 
          ! CAND_F 
          !-----------!
          DO I=1,8*OLD_SIZE
            OLD_TAB2_R(I)=INTBUF_TAB%CAND_F(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%CAND_F)
          INTBUF_TAB%S_CAND_F = 8 * MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%CAND_F(INTBUF_TAB%S_CAND_F),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%CAND_F(1:INTBUF_TAB%S_CAND_F)=0
          DO I=1,8*OLD_SIZE
            INTBUF_TAB%CAND_F(I) = OLD_TAB2_R(I)
          ENDDO           
        END IF

        IF(INACTI == 5 .OR. INACTI == 6 .OR. INACTI == 7) THEN 
          !-----------! 
          ! CAND_P
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%CAND_P(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%CAND_P)
          INTBUF_TAB%S_CAND_P = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%CAND_P(INTBUF_TAB%S_CAND_P),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%CAND_P(1:INTBUF_TAB%S_CAND_P)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%CAND_P(I) = OLD_TAB_R(I)
          ENDDO           
        ENDIF

        DEALLOCATE (OLD_TAB_I,OLD_TAB_R,OLD_TAB2_R)

C=======================================================================
      ELSEIF(ITY == 10)THEN
C=======================================================================
        ALLOCATE (OLD_TAB_I(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        ALLOCATE (OLD_TAB_R(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        ALLOCATE (OLD_TAB2_R(6*OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999 ! specific CAND_F array

        !-----------!
        ! CAND_E
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_E(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_E)
        INTBUF_TAB%S_CAND_E = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_E(INTBUF_TAB%S_CAND_E),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_E(1:INTBUF_TAB%S_CAND_E)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_E(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! CAND_N
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_N(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_N)
        INTBUF_TAB%S_CAND_N = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_N(INTBUF_TAB%S_CAND_N),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_N(1:INTBUF_TAB%S_CAND_N)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_N(I) = OLD_TAB_I(I)
        ENDDO   
   
        !-----------!
        ! CAND_F
        !-----------!
        DO I=1,6*OLD_SIZE
          OLD_TAB2_R(I)=INTBUF_TAB%CAND_F(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_F)
        INTBUF_TAB%S_CAND_F = 6*MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_F(INTBUF_TAB%S_CAND_F),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_F(1:INTBUF_TAB%S_CAND_F)=0
        DO I=1,6*OLD_SIZE
          INTBUF_TAB%CAND_F(I) = OLD_TAB2_R(I)
        ENDDO           

        DEALLOCATE (OLD_TAB_I,OLD_TAB_R,OLD_TAB2_R)

C=======================================================================
      ELSEIF(ITY == 11)THEN
C=======================================================================
        ALLOCATE (OLD_TAB_I(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        ALLOCATE (OLD_TAB2_I(2*OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999 !specific CHAIN array (2*MUTIMP*NCONT)
        ALLOCATE (OLD_TAB_R(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999

        !-----------!
        ! CAND_E
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_E(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_E)
        INTBUF_TAB%S_CAND_E = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_E(INTBUF_TAB%S_CAND_E),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_E(1:INTBUF_TAB%S_CAND_E)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_E(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! CAND_N
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_N(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_N)
        INTBUF_TAB%S_CAND_N = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_N(INTBUF_TAB%S_CAND_N),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_N(1:INTBUF_TAB%S_CAND_N)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_N(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! CHAIN
        !-----------!
        DO I=1,2*OLD_SIZE
          OLD_TAB2_I(I)=INTBUF_TAB%CHAIN(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CHAIN)
        INTBUF_TAB%S_CHAIN = 2*MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CHAIN(INTBUF_TAB%S_CHAIN),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CHAIN(1:INTBUF_TAB%S_CHAIN)=0
        DO I=1,2*OLD_SIZE
          INTBUF_TAB%CHAIN(I) = OLD_TAB2_I(I)
        ENDDO   

        IF (MFROT == 2) THEN 
          !-----------!
          ! IFPEN
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_I(I)=INTBUF_TAB%IFPEN(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%IFPEN)
          INTBUF_TAB%S_IFPEN = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%IFPEN(INTBUF_TAB%S_IFPEN),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%IFPEN(1:INTBUF_TAB%S_IFPEN)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%IFPEN(I) = OLD_TAB_I(I)
          ENDDO    

          !-----------!
          ! FTSAVX
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%FTSAVX(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%FTSAVX)
          INTBUF_TAB%S_FTSAVX = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%FTSAVX(INTBUF_TAB%S_FTSAVX),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%FTSAVX(1:INTBUF_TAB%S_FTSAVX)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%FTSAVX(I) = OLD_TAB_R(I)
          ENDDO    

          !-----------!
          ! FTSAVY
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%FTSAVY(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%FTSAVY)
          INTBUF_TAB%S_FTSAVY = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%FTSAVY(INTBUF_TAB%S_FTSAVY),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%FTSAVY(1:INTBUF_TAB%S_FTSAVY)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%FTSAVY(I) = OLD_TAB_R(I)
          ENDDO   

          !-----------!
          ! FTSAVZ
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%FTSAVZ(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%FTSAVZ)
          INTBUF_TAB%S_FTSAVZ = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%FTSAVZ(INTBUF_TAB%S_FTSAVZ),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%FTSAVZ(1:INTBUF_TAB%S_FTSAVZ)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%FTSAVZ(I) = OLD_TAB_R(I)
          ENDDO   
        ENDIF
 
        DEALLOCATE (OLD_TAB_I,OLD_TAB2_I,OLD_TAB_R)

C=======================================================================
      ELSEIF(ITY == 16)THEN
C=======================================================================
        ALLOCATE (OLD_TAB_I(OLD_SIZE))
        ALLOCATE (OLD_TAB_R(OLD_SIZE))

        !-----------!
        ! CAND_E
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_E(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_E)
        INTBUF_TAB%S_CAND_E = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_E(INTBUF_TAB%S_CAND_E),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_E(1:INTBUF_TAB%S_CAND_E)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_E(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! CAND_N
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_N(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_N)
        INTBUF_TAB%S_CAND_N = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_N(INTBUF_TAB%S_CAND_N),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_N(1:INTBUF_TAB%S_CAND_N)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_N(I) = OLD_TAB_I(I)
        ENDDO   

        DEALLOCATE (OLD_TAB_I,OLD_TAB_R)

C=======================================================================
      ELSEIF(ITY == 17)THEN
C=======================================================================
        ALLOCATE (OLD_TAB_I(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        ALLOCATE (OLD_TAB_R(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999

        !-----------!
        ! CAND_E
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_E(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_E)
        INTBUF_TAB%S_CAND_E = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_E(INTBUF_TAB%S_CAND_E),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_E(1:INTBUF_TAB%S_CAND_E)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_E(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! CAND_N
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_N(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_N)
        INTBUF_TAB%S_CAND_N = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_N(INTBUF_TAB%S_CAND_N),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_N(1:INTBUF_TAB%S_CAND_N)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_N(I) = OLD_TAB_I(I)
        ENDDO   

        DEALLOCATE (OLD_TAB_I,OLD_TAB_R)

C=======================================================================
      ELSEIF(ITY == 20)THEN
C=======================================================================
        ALLOCATE (OLD_TAB_I(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        ALLOCATE (OLD_TAB2_I(OLD_SIZE2),STAT=IERR)
        IF(IERR/=0)GOTO 999   !specific MUTIMP*NCONTE arrays
        ALLOCATE (OLD_TAB3_I(2*OLD_SIZE2),STAT=IERR)
        IF(IERR/=0)GOTO 999 !specific CHAIN20 array (2*MUTIMP*NCONTE )
        ALLOCATE (OLD_TAB_R(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999

        !-----------!
        ! CAND_E
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_E(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_E)
        INTBUF_TAB%S_CAND_E = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_E(INTBUF_TAB%S_CAND_E),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_E(1:INTBUF_TAB%S_CAND_E)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_E(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! CAND_N
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_N(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_N)
        INTBUF_TAB%S_CAND_N = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_N(INTBUF_TAB%S_CAND_N),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_N(1:INTBUF_TAB%S_CAND_N)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_N(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! LCAND_N
        !-----------!
        DO I=1,OLD_SIZE2
          OLD_TAB2_I(I)=INTBUF_TAB%LCAND_N(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%LCAND_N)
        INTBUF_TAB%S_LCAND_N = MULTIMP_PARAMETER * NCONTE
        ALLOCATE(INTBUF_TAB%LCAND_N(INTBUF_TAB%S_LCAND_N),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%LCAND_N(1:INTBUF_TAB%S_LCAND_N)=0
        DO I=1,OLD_SIZE2
          INTBUF_TAB%LCAND_N(I) = OLD_TAB2_I(I)
        ENDDO   

        !-----------!
        ! LCAND_S
        !-----------!
        DO I=1,OLD_SIZE2
          OLD_TAB2_I(I)=INTBUF_TAB%LCAND_S(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%LCAND_S)
        INTBUF_TAB%S_LCAND_S = MULTIMP_PARAMETER * NCONTE
        ALLOCATE(INTBUF_TAB%LCAND_S(INTBUF_TAB%S_LCAND_S),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%LCAND_S(1:INTBUF_TAB%S_LCAND_S)=0
        DO I=1,OLD_SIZE2
          INTBUF_TAB%LCAND_S(I) = OLD_TAB2_I(I)
        ENDDO   

        !-----------!
        ! CHAIN20
        !-----------!
        DO I=1,2*OLD_SIZE2
          OLD_TAB3_I(I)=INTBUF_TAB%CHAIN20(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CHAIN20)
        INTBUF_TAB%S_CHAIN20 = 2*MULTIMP_PARAMETER*NCONTE
        ALLOCATE(INTBUF_TAB%CHAIN20(INTBUF_TAB%S_CHAIN20),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CHAIN20(1:INTBUF_TAB%S_CHAIN20)=0
        DO I=1,2*OLD_SIZE2
          INTBUF_TAB%CHAIN20(I) = OLD_TAB3_I(I)
        ENDDO  

        IF(INACTI == 5 .OR. INACTI == 6 .OR. INACTI == 7) THEN  
          !-----------!
          ! CAND_P
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%CAND_P(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%CAND_P)
          INTBUF_TAB%S_CAND_P = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%CAND_P(INTBUF_TAB%S_CAND_P),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%CAND_P(1:INTBUF_TAB%S_CAND_P)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%CAND_P(I) = OLD_TAB_R(I)
          ENDDO           
        ENDIF

        IF (IFQ/=0) THEN 
          !-----------!
          ! IFPEN
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_I(I)=INTBUF_TAB%IFPEN(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%IFPEN)
          INTBUF_TAB%S_IFPEN = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%IFPEN(INTBUF_TAB%S_IFPEN),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%IFPEN(1:INTBUF_TAB%S_IFPEN)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%IFPEN(I) = OLD_TAB_I(I)
          ENDDO    

          !-----------!
          ! CAND_FX
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%CAND_FX(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%CAND_FX)
          INTBUF_TAB%S_CAND_FX = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%CAND_FX(INTBUF_TAB%S_CAND_FX),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%CAND_FX(1:INTBUF_TAB%S_CAND_FX)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%CAND_FX(I) = OLD_TAB_R(I)
          ENDDO    

          !-----------!
          ! CAND_FY
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%CAND_FY(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%CAND_FY)
          INTBUF_TAB%S_CAND_FY = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%CAND_FY(INTBUF_TAB%S_CAND_FY),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%CAND_FY(1:INTBUF_TAB%S_CAND_FY)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%CAND_FY(I) = OLD_TAB_R(I)
          ENDDO   

          !-----------!
          ! CAND_FZ
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%CAND_FZ(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%CAND_FZ)
          INTBUF_TAB%S_CAND_FZ = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%CAND_FZ(INTBUF_TAB%S_CAND_FZ),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%CAND_FZ(1:INTBUF_TAB%S_CAND_FZ)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%CAND_FZ(I) = OLD_TAB_R(I)
          ENDDO   
        ENDIF

        DEALLOCATE (OLD_TAB_I,OLD_TAB2_I,OLD_TAB3_I,OLD_TAB_R)

C=======================================================================
      ELSEIF(ITY == 21)THEN
C=======================================================================
        ALLOCATE (OLD_TAB_I(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        ALLOCATE (OLD_TAB_R(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999

        !-----------!
        ! CAND_E
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_E(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_E)
        INTBUF_TAB%S_CAND_E = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_E(INTBUF_TAB%S_CAND_E),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_E(1:INTBUF_TAB%S_CAND_E)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_E(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! CAND_N
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_N(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_N)
        INTBUF_TAB%S_CAND_N = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_N(INTBUF_TAB%S_CAND_N),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_N(1:INTBUF_TAB%S_CAND_N)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_N(I) = OLD_TAB_I(I)
        ENDDO   

        DEALLOCATE (OLD_TAB_I,OLD_TAB_R)

C=======================================================================
      ELSEIF(ITY == 22)THEN
C=======================================================================
        ALLOCATE (OLD_TAB_I(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        ALLOCATE (OLD_TAB_R(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999

        !-----------!
        ! CAND_E
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_E(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_E)
        INTBUF_TAB%S_CAND_E = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_E(INTBUF_TAB%S_CAND_E),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_E(1:INTBUF_TAB%S_CAND_E)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_E(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! CAND_N
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_N(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_N)
        INTBUF_TAB%S_CAND_N = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_N(INTBUF_TAB%S_CAND_N),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_N(1:INTBUF_TAB%S_CAND_N)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_N(I) = OLD_TAB_I(I)
        ENDDO  

        IF (IFQ /= 0)THEN
          !-----------!
          ! IFPEN
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_I(I)=INTBUF_TAB%IFPEN(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%IFPEN)
          INTBUF_TAB%S_IFPEN = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%IFPEN(INTBUF_TAB%S_IFPEN),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%IFPEN(1:INTBUF_TAB%S_IFPEN)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%IFPEN(I) = OLD_TAB_I(I)
          ENDDO 
        ENDIF 

        DEALLOCATE (OLD_TAB_I,OLD_TAB_R)

C=======================================================================
      ELSEIF(ITY == 23)THEN
C=======================================================================
        ALLOCATE (OLD_TAB_I(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        ALLOCATE (OLD_TAB_R(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999

        !-----------!
        ! CAND_E
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_E(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_E)
        INTBUF_TAB%S_CAND_E = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_E(INTBUF_TAB%S_CAND_E),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_E(1:INTBUF_TAB%S_CAND_E)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_E(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! CAND_N
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_N(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_N)
        INTBUF_TAB%S_CAND_N = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_N(INTBUF_TAB%S_CAND_N),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_N(1:INTBUF_TAB%S_CAND_N)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_N(I) = OLD_TAB_I(I)
        ENDDO 

        !-----------!
        ! IFPEN
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%IFPEN(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%IFPEN)
        INTBUF_TAB%S_IFPEN = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%IFPEN(INTBUF_TAB%S_IFPEN),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%IFPEN(1:INTBUF_TAB%S_IFPEN)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%IFPEN(I) = OLD_TAB_I(I)
        ENDDO 

            !-----------! 
            ! CAND_P
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_R(I)=INTBUF_TAB%CAND_P(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_P)
        INTBUF_TAB%S_CAND_P = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_P(INTBUF_TAB%S_CAND_P),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_P(1:INTBUF_TAB%S_CAND_P)=0
        DO I=1,OLD_SIZE
           INTBUF_TAB%CAND_P(I) = OLD_TAB_R(I)
        ENDDO  

        !-----------!
        ! FTSAVX
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_R(I)=INTBUF_TAB%FTSAVX(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%FTSAVX)
        INTBUF_TAB%S_FTSAVX = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%FTSAVX(INTBUF_TAB%S_FTSAVX),STAT=IERR)
        IF(IERR/=0)GOTO 999
          INTBUF_TAB%FTSAVX(1:INTBUF_TAB%S_FTSAVX)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%FTSAVX(I) = OLD_TAB_R(I)
        ENDDO    

        !-----------!
        ! FTSAVY
        !-----------!
        DO I=1,OLD_SIZE
           OLD_TAB_R(I)=INTBUF_TAB%FTSAVY(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%FTSAVY)
        INTBUF_TAB%S_FTSAVY = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%FTSAVY(INTBUF_TAB%S_FTSAVY),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%FTSAVY(1:INTBUF_TAB%S_FTSAVY)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%FTSAVY(I) = OLD_TAB_R(I)
        ENDDO   

        !-----------!
        ! FTSAVZ
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_R(I)=INTBUF_TAB%FTSAVZ(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%FTSAVZ)
        INTBUF_TAB%S_FTSAVZ = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%FTSAVZ(INTBUF_TAB%S_FTSAVZ),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%FTSAVZ(1:INTBUF_TAB%S_FTSAVZ)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%FTSAVZ(I) = OLD_TAB_R(I)
        ENDDO  
 

        DEALLOCATE (OLD_TAB_I,OLD_TAB_R)

C=======================================================================
      ELSEIF(ITY == 24)THEN
C=======================================================================
        ALLOCATE (OLD_TAB_I(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        ALLOCATE (OLD_TAB_R(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999

        !-----------!
        ! CAND_E
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_E(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_E)
        INTBUF_TAB%S_CAND_E = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_E(INTBUF_TAB%S_CAND_E),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_E(1:INTBUF_TAB%S_CAND_E)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_E(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! CAND_N
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_N(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_N)
        INTBUF_TAB%S_CAND_N = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_N(INTBUF_TAB%S_CAND_N),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_N(1:INTBUF_TAB%S_CAND_N)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_N(I) = OLD_TAB_I(I)
        ENDDO 

        IF (IEDGE1 > 0) THEN
          !-----------!
          ! CAND_T
          !-----------!
          DO I=1,OLD_SIZE 
            OLD_TAB_I(I)=INTBUF_TAB%CAND_T(I) 
          ENDDO 
          DEALLOCATE(INTBUF_TAB%CAND_T) 
          INTBUF_TAB%S_CAND_T = MULTIMP_PARAMETER * NCONT
          ALLOCATE(INTBUF_TAB%CAND_T(INTBUF_TAB%S_CAND_T),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%CAND_T(1:INTBUF_TAB%S_CAND_T)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%CAND_T(I) = OLD_TAB_I(I)
          ENDDO 
        ENDIF

C=======================================================================
      ELSEIF(ITY == 25)THEN
C=======================================================================
C
        OLD_SIZE_OPT=INTBUF_TAB%S_CAND_OPT_N
        NEW_SIZE_OPT=MAX(MULTIMP_PARAMETER * NCONT,OLD_SIZE_OPT)

        ALLOCATE (OLD_TAB_I(MAX(OLD_SIZE,4*OLD_SIZE_OPT)),STAT=IERR)
        IF(IERR/=0)GOTO 999
        ALLOCATE (OLD_TAB_R(MAX(OLD_SIZE,4*OLD_SIZE_OPT)),STAT=IERR)
        IF(IERR/=0)GOTO 999

        !-----------!
        ! CAND_E
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_E(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_E)
        INTBUF_TAB%S_CAND_E = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_E(INTBUF_TAB%S_CAND_E),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_E(1:INTBUF_TAB%S_CAND_E)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_E(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! CAND_N
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_N(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_N)
        INTBUF_TAB%S_CAND_N = MULTIMP_PARAMETER * NCONT
        ALLOCATE(INTBUF_TAB%CAND_N(INTBUF_TAB%S_CAND_N),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_N(1:INTBUF_TAB%S_CAND_N)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_N(I) = OLD_TAB_I(I)
        ENDDO 

        !-----------!
        ! FARM
        !-----------!

        DO I=1,4*OLD_SIZE_OPT
          OLD_TAB_I(I)=INTBUF_TAB%FARM(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%FARM)
        INTBUF_TAB%S_FARM = 4 * NEW_SIZE_OPT
        ALLOCATE(INTBUF_TAB%FARM(INTBUF_TAB%S_FARM),STAT=IERR)
        IF(IERR/=0)GOTO 999
        DO I=1,4*OLD_SIZE_OPT
          INTBUF_TAB%FARM(I) = OLD_TAB_I(I)
        ENDDO 

        !-----------!
        ! CAND_OPT_N
        !-----------!
        DO I=1,OLD_SIZE_OPT
          OLD_TAB_I(I)=INTBUF_TAB%CAND_OPT_N(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_OPT_N)
        INTBUF_TAB%S_CAND_OPT_N = NEW_SIZE_OPT
        ALLOCATE(INTBUF_TAB%CAND_OPT_N(INTBUF_TAB%S_CAND_OPT_N),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_OPT_N(1:INTBUF_TAB%S_CAND_OPT_N)=0
        DO I=1,OLD_SIZE_OPT
          INTBUF_TAB%CAND_OPT_N(I) = OLD_TAB_I(I)
        ENDDO 

        !-----------!
        ! CAND_OPT_E
        !-----------!
        DO I=1,OLD_SIZE_OPT
          OLD_TAB_I(I)=INTBUF_TAB%CAND_OPT_E(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_OPT_E)
        INTBUF_TAB%S_CAND_OPT_E = NEW_SIZE_OPT
        ALLOCATE(INTBUF_TAB%CAND_OPT_E(INTBUF_TAB%S_CAND_OPT_E),STAT=IERR)
        INTBUF_TAB%CAND_OPT_E(1:INTBUF_TAB%S_CAND_OPT_E)=0
        IF(IERR/=0)GOTO 999
        DO I=1,OLD_SIZE_OPT
          INTBUF_TAB%CAND_OPT_E(I) = OLD_TAB_I(I)
        ENDDO 

        !-----------!
        ! PENM
        !-----------!
        DO I=1,4*OLD_SIZE_OPT
          OLD_TAB_R(I)=INTBUF_TAB%PENM(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%PENM)
        INTBUF_TAB%S_PENM = 4 * NEW_SIZE_OPT
        ALLOCATE(INTBUF_TAB%PENM(INTBUF_TAB%S_PENM),STAT=IERR)
        INTBUF_TAB%PENM(1:INTBUF_TAB%S_PENM)=ZERO
        IF(IERR/=0)GOTO 999
        DO I=1,4*OLD_SIZE_OPT
          INTBUF_TAB%PENM(I) = OLD_TAB_R(I)
        ENDDO 

        !-----------!
        ! DISTM
        !-----------!
        DO I=1,OLD_SIZE_OPT
          OLD_TAB_R(I)=INTBUF_TAB%DISTM(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%DISTM)
        INTBUF_TAB%S_DISTM = NEW_SIZE_OPT
        ALLOCATE(INTBUF_TAB%DISTM(INTBUF_TAB%S_DISTM),STAT=IERR)
        IF(IERR/=0)GOTO 999
        DO I=1,OLD_SIZE_OPT
          INTBUF_TAB%DISTM(I) = OLD_TAB_R(I)
        ENDDO 

        !-----------!
        ! LBM
        !-----------!
        DO I=1,4*OLD_SIZE_OPT
          OLD_TAB_R(I)=INTBUF_TAB%LBM(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%LBM)
        INTBUF_TAB%S_LBM= 4 * NEW_SIZE_OPT
        ALLOCATE(INTBUF_TAB%LBM(INTBUF_TAB%S_LBM),STAT=IERR)
        IF(IERR/=0)GOTO 999
        DO I=1,4*OLD_SIZE_OPT
          INTBUF_TAB%LBM(I) = OLD_TAB_R(I)
        ENDDO 

        !-----------!
        ! LCM
        !-----------!
        DO I=1,4*OLD_SIZE_OPT
          OLD_TAB_R(I)=INTBUF_TAB%LCM(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%LCM)
        INTBUF_TAB%S_LCM = 4 * NEW_SIZE_OPT
        ALLOCATE(INTBUF_TAB%LCM(INTBUF_TAB%S_LCM),STAT=IERR)
        IF(IERR/=0)GOTO 999
        DO I=1,4*OLD_SIZE_OPT
          INTBUF_TAB%LCM(I) = OLD_TAB_R(I)
        ENDDO 

        DEALLOCATE (OLD_TAB_I,OLD_TAB_R)

      ENDIF !end all interfaces type

c===================================================================== 
cc      print*,'----------- Upgrade MultiMP -----------'
cc      print*,'INTERFACE Number NI:',NI
cc      print*,'INTERFACE Type :',ITY
cc      print*,'NSN - NRTM :',NSN,NRTM
cc      print*,'NCONT   :',NCONT
cc      print*,'User Number : ',IPARI(NPARI*(NI-1)+15)
cc      print*,' '
cc      print*,'OLD MULTIMP:',MULTIMP
cc      print*,'NEW MULTIMP:',MULTIMP_PARAMETER
cc      print*,' '
cc      print*,'OLD SIZE MULTIMP*NCONT =',OLD_SIZE
cc      print*,'NEW SIZE MULTIMP*NCONT =',NEW_SIZE
cc      print*,'---------------------------------------'
cc      call my_flush(6)
C=======================================================================
      RETURN
 999  CONTINUE
      CALL ARRET_MESSAGE(ITY,IPARI(NPARI*(NI-1)+15),INTBUF_TAB%VARIABLES(37))
      
      CALL ARRET(2)
      END
Chd|====================================================================
Chd|  UPGRADE_CAND_OPT              interf/upgrade_multimp.F      
Chd|-- called by -----------
Chd|        I25MAIN_OPT_TRI               engine/source/interfaces/intsort/i25main_opt_tri.F
Chd|        I25MAIN_SLID                  engine/source/interfaces/int25/i25main_slid.F
Chd|-- calls ---------------
Chd|        ARRET                         starter/source/system/arret.F 
Chd|        ARRET                         engine/source/system/arret.F  
Chd|        ARRET_MESSAGE_SLID            starter/source/interfaces/int24/arret_message.F
Chd|        ARRET_MESSAGE_SLID            engine/source/system/arret_message.F
Chd|        INTBUFDEF_MOD                 modules/intbufdef_mod.F       
Chd|        RESTMOD                       starter/share/modules1/restart_mod.F
Chd|        RESTMOD                       engine/share/modules/restart_mod.F
Chd|====================================================================
      SUBROUTINE UPGRADE_CAND_OPT(NI,K_STOK,INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RESTMOD
      USE INTBUFDEF_MOD
C-------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NI, K_STOK

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITY,IFQ,INACTI,MFROT,NCONT,
     .        NCONTE,NSN,NRTM,IEDGE1,FLAGREMNODE
      INTEGER I,N,OLD_SIZE,NEW_SIZE,MULTIMP,
     .        OLD_SIZE2,NEW_SIZE2,IERR

      INTEGER, DIMENSION (:), ALLOCATABLE :: OLD_TAB_I
      my_real, DIMENSION (:), ALLOCATABLE :: OLD_TAB_R
C======================================================================|
      IERR=0

      OLD_SIZE = INTBUF_TAB%S_CAND_OPT_N
      NEW_SIZE = INTBUF_TAB%I_STOK(2)+K_STOK

C Set Global parameters
      ITY    =     IPARI(NPARI*(NI-1)+7)
      INACTI = ABS(IPARI(NPARI*(NI-1)+22))
      MFROT  = IPARI(NPARI*(NI-1)+30)
      IFQ    =     IPARI(NPARI*(NI-1)+31)
      NSN    =     IPARI(NPARI*(NI-1)+5)
      NRTM   =     IPARI(NPARI*(NI-1)+4)

C=======================================================================
      IF(ITY == 25)THEN
C=======================================================================
        ALLOCATE (OLD_TAB_I(4*OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        ALLOCATE (OLD_TAB_R(4*OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999

        !-----------!
        ! FARM
        !-----------!
        DO I=1,4*OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%FARM(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%FARM)
        INTBUF_TAB%S_FARM = 4 * NEW_SIZE
        ALLOCATE(INTBUF_TAB%FARM(INTBUF_TAB%S_FARM),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%FARM(1:INTBUF_TAB%S_FARM)=0
        DO I=1,4*OLD_SIZE
          INTBUF_TAB%FARM(I)=OLD_TAB_I(I)
        END DO

        !-----------!
        ! CAND_OPT_N
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_OPT_N(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_OPT_N)
        INTBUF_TAB%S_CAND_OPT_N = NEW_SIZE
        ALLOCATE(INTBUF_TAB%CAND_OPT_N(INTBUF_TAB%S_CAND_OPT_N),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_OPT_N(1:INTBUF_TAB%S_CAND_OPT_N)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_OPT_N(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! CAND_OPT_E
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CAND_OPT_E(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CAND_OPT_E)
        INTBUF_TAB%S_CAND_OPT_E = NEW_SIZE
        ALLOCATE(INTBUF_TAB%CAND_OPT_E(INTBUF_TAB%S_CAND_OPT_E),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CAND_OPT_E(1:INTBUF_TAB%S_CAND_OPT_E)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CAND_OPT_E(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! PENM
        !-----------!
        DO I=1,4*OLD_SIZE
          OLD_TAB_R(I)=INTBUF_TAB%PENM(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%PENM)
        INTBUF_TAB%S_PENM = 4 * NEW_SIZE
        ALLOCATE(INTBUF_TAB%PENM(INTBUF_TAB%S_PENM),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%PENM(1:INTBUF_TAB%S_PENM)=ZERO
        DO I=1,4*OLD_SIZE
          INTBUF_TAB%PENM(I) = OLD_TAB_R(I)
        ENDDO   

        !-----------!
        ! DISTM
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_R(I)=INTBUF_TAB%DISTM(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%DISTM)
        INTBUF_TAB%S_DISTM = NEW_SIZE
        ALLOCATE(INTBUF_TAB%DISTM(INTBUF_TAB%S_DISTM),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%DISTM(1:INTBUF_TAB%S_DISTM)=ZERO
        DO I=1,OLD_SIZE
          INTBUF_TAB%DISTM(I) = OLD_TAB_R(I)
        ENDDO   

        !-----------!
        ! LBM
        !-----------!
        DO I=1,4*OLD_SIZE
          OLD_TAB_R(I)=INTBUF_TAB%LBM(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%LBM)
        INTBUF_TAB%S_LBM= 4 * NEW_SIZE
        ALLOCATE(INTBUF_TAB%LBM(INTBUF_TAB%S_LBM),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%LBM(1:INTBUF_TAB%S_LBM)=ZERO
        DO I=1,4*OLD_SIZE
          INTBUF_TAB%LBM(I) = OLD_TAB_R(I)
        ENDDO   

        !-----------!
        ! LCM
        !-----------!
        DO I=1,4*OLD_SIZE
          OLD_TAB_R(I)=INTBUF_TAB%LCM(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%LCM)
        INTBUF_TAB%S_LCM = 4 * NEW_SIZE
        ALLOCATE(INTBUF_TAB%LCM(INTBUF_TAB%S_LCM),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%LCM(1:INTBUF_TAB%S_LCM)=ZERO
        DO I=1,4*OLD_SIZE
          INTBUF_TAB%LCM(I) = OLD_TAB_R(I)
        ENDDO   

        DEALLOCATE (OLD_TAB_I,OLD_TAB_R)

      ENDIF !end all interfaces type

c===================================================================== 
cc      print*,'----------- Upgrade MultiMP -----------'
cc      print*,'INTERFACE Number NI:',NI
cc      print*,'INTERFACE Type :',ITY
cc      print*,'NSN - NRTM :',NSN,NRTM
cc      print*,'NCONT   :',NCONT
cc      print*,'User Number : ',IPARI(NPARI*(NI-1)+15)
cc      print*,' '
cc      print*,'OLD MULTIMP:',MULTIMP
cc      print*,'NEW MULTIMP:',MULTIMP_PARAMETER
cc      print*,' '
cc      print*,'OLD SIZE MULTIMP*NCONT =',OLD_SIZE
cc      print*,'NEW SIZE MULTIMP*NCONT =',NEW_SIZE
cc      print*,'---------------------------------------'
cc      call my_flush(6)
C=======================================================================
      RETURN
 999  CONTINUE
      CALL ARRET_MESSAGE_SLID(ITY,IPARI(NPARI*(NI-1)+15))
      
      CALL ARRET(2)
      END
Chd|====================================================================
Chd|  UPGRADE_LCAND_EDG             interf/upgrade_multimp.F      
Chd|-- called by -----------
Chd|        ININT3                        starter/source/interfaces/inter3d1/inint3.F
Chd|        I25MAIN_TRI                   engine/source/interfaces/intsort/i25main_tri.F
Chd|-- calls ---------------
Chd|        ARRET                         starter/source/system/arret.F 
Chd|        ARRET                         engine/source/system/arret.F  
Chd|        ARRET_MESSAGE                 starter/source/interfaces/int24/arret_message.F
Chd|        ARRET_MESSAGE                 engine/source/system/arret_message.F
Chd|        INTBUFDEF_MOD                 modules/intbufdef_mod.F       
Chd|        RESTMOD                       starter/share/modules1/restart_mod.F
Chd|        RESTMOD                       engine/share/modules/restart_mod.F
Chd|====================================================================
      SUBROUTINE UPGRADE_LCAND_EDG(NI,MULTIMP_PARAMETER,INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RESTMOD
      USE INTBUFDEF_MOD
C-------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NI, MULTIMP_PARAMETER

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITY,INACTI,NCONTE
      INTEGER I,N,OLD_SIZE,NEW_SIZE,MULTIMP,IERR,IFQ

      INTEGER, DIMENSION (:), ALLOCATABLE :: OLD_TAB_I
      my_real, DIMENSION (:), ALLOCATABLE :: OLD_TAB_R
C======================================================================|
      IERR=0

      MULTIMP  = IPARI(NPARI*(NI-1)+87)
      NCONTE   = IPARI(NPARI*(NI-1)+88)
      OLD_SIZE = MULTIMP * NCONTE
      NEW_SIZE = MULTIMP_PARAMETER * NCONTE

C Set the new MULTIMP parameter for the given Interface
      IPARI(NPARI*(NI-1)+87) = MULTIMP_PARAMETER
C Set Global parameters
      ITY    =     IPARI(NPARI*(NI-1)+7)
      INACTI = ABS(IPARI(NPARI*(NI-1)+22))
      IFQ    =     IPARI(NPARI*(NI-1)+31)
C=======================================================================
      IF(ITY == 25)THEN

        ALLOCATE (OLD_TAB_I(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        ALLOCATE (OLD_TAB_R(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        !-----------!
        ! CANDM_E2E
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CANDM_E2E(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CANDM_E2E)
        INTBUF_TAB%S_CANDM_E2E = MULTIMP_PARAMETER * NCONTE
        ALLOCATE(INTBUF_TAB%CANDM_E2E(INTBUF_TAB%S_CANDM_E2E),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CANDM_E2E(1:INTBUF_TAB%S_CANDM_E2E)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CANDM_E2E(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! CANDS_E2E
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CANDS_E2E(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CANDS_E2E)
        INTBUF_TAB%S_CANDS_E2E = MULTIMP_PARAMETER * NCONTE
        ALLOCATE(INTBUF_TAB%CANDS_E2E(INTBUF_TAB%S_CANDS_E2E),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CANDS_E2E(1:INTBUF_TAB%S_CANDS_E2E)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CANDS_E2E(I) = OLD_TAB_I(I)
        ENDDO   

C       IF(INACTI == 5) THEN  
          !-----------!
          ! CAND_P
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%CAND_P(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%CAND_P)
          INTBUF_TAB%S_CAND_P = MULTIMP_PARAMETER * NCONTE
          ALLOCATE(INTBUF_TAB%CAND_P(INTBUF_TAB%S_CAND_P),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%CAND_P(1:INTBUF_TAB%S_CAND_P)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%CAND_P(I) = OLD_TAB_R(I)
          ENDDO           
C       ENDIF

        IF (IFQ /= 0)THEN
          !-----------!
          ! IFPEN_E2E
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_I(I)=INTBUF_TAB%IFPEN_E(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%IFPEN_E)
          INTBUF_TAB%S_IFPEN_E = MULTIMP_PARAMETER * NCONTE
          ALLOCATE(INTBUF_TAB%IFPEN_E(INTBUF_TAB%S_IFPEN_E),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%IFPEN_E(1:INTBUF_TAB%S_IFPEN_E)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%IFPEN_E(I) = OLD_TAB_I(I)
          ENDDO   

          !-----------!
          ! FTSAVX_E
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%FTSAVX_E(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%FTSAVX_E)
          INTBUF_TAB%S_FTSAVX_E = MULTIMP_PARAMETER * NCONTE
          ALLOCATE(INTBUF_TAB%FTSAVX_E(INTBUF_TAB%S_FTSAVX_E),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%FTSAVX_E(1:INTBUF_TAB%S_FTSAVX_E)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%FTSAVX_E(I) = OLD_TAB_R(I)
          ENDDO    

          !-----------!
          ! FTSAVY_E
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%FTSAVY_E(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%FTSAVY_E)
          INTBUF_TAB%S_FTSAVY_E = MULTIMP_PARAMETER * NCONTE
          ALLOCATE(INTBUF_TAB%FTSAVY_E(INTBUF_TAB%S_FTSAVY_E),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%FTSAVY_E(1:INTBUF_TAB%S_FTSAVY_E)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%FTSAVY_E(I) = OLD_TAB_R(I)
          ENDDO   

          !-----------!
          ! FTSAVZ_E
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%FTSAVZ_E(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%FTSAVZ_E)
          INTBUF_TAB%S_FTSAVZ_E = MULTIMP_PARAMETER * NCONTE
          ALLOCATE(INTBUF_TAB%FTSAVZ_E(INTBUF_TAB%S_FTSAVZ_E),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%FTSAVZ_E(1:INTBUF_TAB%S_FTSAVZ_E)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%FTSAVZ_E(I) = OLD_TAB_R(I)
          ENDDO   
        ENDIF


        DEALLOCATE (OLD_TAB_I,OLD_TAB_R)

C=======================================================================
      ENDIF !end all interfaces type

c===================================================================== 
cc      print*,'----------- Upgrade MultiMP -----------'
cc      print*,'INTERFACE Number NI:',NI
cc      print*,'INTERFACE Type :',ITY
cc      print*,'NSN - NRTM :',NSN,NRTM
cc      print*,'NCONT   :',NCONT
cc      print*,'User Number : ',IPARI(NPARI*(NI-1)+15)
cc      print*,' '
cc      print*,'OLD MULTIMP:',MULTIMP
cc      print*,'NEW MULTIMP:',MULTIMP_PARAMETER
cc      print*,' '
cc      print*,'OLD SIZE MULTIMP*NCONT =',OLD_SIZE
cc      print*,'NEW SIZE MULTIMP*NCONT =',NEW_SIZE
cc      print*,'---------------------------------------'
cc      call my_flush(6)
C=======================================================================
      RETURN
 999  CONTINUE
      CALL ARRET_MESSAGE(ITY,IPARI(NPARI*(NI-1)+15),INTBUF_TAB%VARIABLES(37))
      
      CALL ARRET(2)
      END
Chd|====================================================================
Chd|  UPGRADE_LCAND_E2S             interf/upgrade_multimp.F      
Chd|-- called by -----------
Chd|        ININT3                        starter/source/interfaces/inter3d1/inint3.F
Chd|        I25MAIN_TRI                   engine/source/interfaces/intsort/i25main_tri.F
Chd|-- calls ---------------
Chd|        ARRET                         starter/source/system/arret.F 
Chd|        ARRET                         engine/source/system/arret.F  
Chd|        ARRET_MESSAGE                 starter/source/interfaces/int24/arret_message.F
Chd|        ARRET_MESSAGE                 engine/source/system/arret_message.F
Chd|        INTBUFDEF_MOD                 modules/intbufdef_mod.F       
Chd|        RESTMOD                       starter/share/modules1/restart_mod.F
Chd|        RESTMOD                       engine/share/modules/restart_mod.F
Chd|====================================================================
      SUBROUTINE UPGRADE_LCAND_E2S(NI,MULTIMP_PARAMETER,INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RESTMOD
      USE INTBUFDEF_MOD
C-------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NI, MULTIMP_PARAMETER

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITY,INACTI,NCONTE
      INTEGER I,N,OLD_SIZE,NEW_SIZE,MULTIMP,IERR,IFQ

      INTEGER, DIMENSION (:), ALLOCATABLE :: OLD_TAB_I
      my_real, DIMENSION (:), ALLOCATABLE :: OLD_TAB_R
C======================================================================|
      IERR=0

      MULTIMP  = IPARI(NPARI*(NI-1)+89)
      NCONTE   = IPARI(NPARI*(NI-1)+88)
      OLD_SIZE = MULTIMP * NCONTE
      NEW_SIZE = MULTIMP_PARAMETER * NCONTE

C Set the new MULTIMP parameter for the given Interface
      IPARI(NPARI*(NI-1)+89) = MULTIMP_PARAMETER
C Set Global parameters
      ITY    =     IPARI(NPARI*(NI-1)+7)
      INACTI = ABS(IPARI(NPARI*(NI-1)+22))
      IFQ    =     IPARI(NPARI*(NI-1)+31)
C=======================================================================
      IF(ITY == 25)THEN

        ALLOCATE (OLD_TAB_I(OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        ALLOCATE (OLD_TAB_R(4*OLD_SIZE),STAT=IERR)
        IF(IERR/=0)GOTO 999
        !-----------!
        ! CANDM_E2S
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CANDM_E2S(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CANDM_E2S)
        INTBUF_TAB%S_CANDM_E2S = MULTIMP_PARAMETER * NCONTE
        ALLOCATE(INTBUF_TAB%CANDM_E2S(INTBUF_TAB%S_CANDM_E2S),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CANDM_E2S(1:INTBUF_TAB%S_CANDM_E2S)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CANDM_E2S(I) = OLD_TAB_I(I)
        ENDDO   

        !-----------!
        ! CANDS_E2S
        !-----------!
        DO I=1,OLD_SIZE
          OLD_TAB_I(I)=INTBUF_TAB%CANDS_E2S(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%CANDS_E2S)
        INTBUF_TAB%S_CANDS_E2S = MULTIMP_PARAMETER * NCONTE
        ALLOCATE(INTBUF_TAB%CANDS_E2S(INTBUF_TAB%S_CANDS_E2S),STAT=IERR)
        IF(IERR/=0)GOTO 999
        INTBUF_TAB%CANDS_E2S(1:INTBUF_TAB%S_CANDS_E2S)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%CANDS_E2S(I) = OLD_TAB_I(I)
        ENDDO   

C       IF(INACTI == 5) THEN  
          !-----------!
          ! CAND_PS
          !-----------!
          DO I=1,4*OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%CAND_PS(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%CAND_PS)
          INTBUF_TAB%S_CAND_PS = 4 * MULTIMP_PARAMETER * NCONTE
          ALLOCATE(INTBUF_TAB%CAND_PS(INTBUF_TAB%S_CAND_PS),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%CAND_PS(1:INTBUF_TAB%S_CAND_PS)=0
          DO I=1,4*OLD_SIZE
            INTBUF_TAB%CAND_PS(I) = OLD_TAB_R(I)
          ENDDO           
C       ENDIF

        IF (IFQ /= 0)THEN
          !-----------!
          ! IFPEN_E2S
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_I(I)=INTBUF_TAB%IFPEN_E2S(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%IFPEN_E2S)
          INTBUF_TAB%S_IFPEN_E2S =  MULTIMP_PARAMETER * NCONTE
          ALLOCATE(INTBUF_TAB%IFPEN_E2S(INTBUF_TAB%S_IFPEN_E2S),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%IFPEN_E2S(1:INTBUF_TAB%S_IFPEN_E2S)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%IFPEN_E2S(I) = OLD_TAB_I(I)
          ENDDO   

          !-----------!
          ! FTSAVX_E2S
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%FTSAVX_E2S(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%FTSAVX_E2S)
          INTBUF_TAB%S_FTSAVX_E2S =  4 * MULTIMP_PARAMETER * NCONTE
          ALLOCATE(INTBUF_TAB%FTSAVX_E2S(INTBUF_TAB%S_FTSAVX_E2S),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%FTSAVX_E2S(1:INTBUF_TAB%S_FTSAVX_E2S)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%FTSAVX_E2S(I) = OLD_TAB_R(I)
          ENDDO    

          !-----------!
          ! FTSAVY_E2S
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%FTSAVY_E2S(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%FTSAVY_E2S)
          INTBUF_TAB%S_FTSAVY_E2S =  4 * MULTIMP_PARAMETER * NCONTE
          ALLOCATE(INTBUF_TAB%FTSAVY_E2S(INTBUF_TAB%S_FTSAVY_E2S),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%FTSAVY_E2S(1:INTBUF_TAB%S_FTSAVY_E2S)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%FTSAVY_E2S(I) = OLD_TAB_R(I)
          ENDDO   

          !-----------!
          ! FTSAVZ
          !-----------!
          DO I=1,OLD_SIZE
            OLD_TAB_R(I)=INTBUF_TAB%FTSAVZ_E2S(I)
          ENDDO
          DEALLOCATE(INTBUF_TAB%FTSAVZ_E2S)
          INTBUF_TAB%S_FTSAVZ_E2S = 4 * MULTIMP_PARAMETER * NCONTE
          ALLOCATE(INTBUF_TAB%FTSAVZ_E2S(INTBUF_TAB%S_FTSAVZ_E2S),STAT=IERR)
          IF(IERR/=0)GOTO 999
          INTBUF_TAB%FTSAVZ_E2S(1:INTBUF_TAB%S_FTSAVZ_E2S)=0
          DO I=1,OLD_SIZE
            INTBUF_TAB%FTSAVZ_E2S(I) = OLD_TAB_R(I)
          ENDDO   
        ENDIF

        DEALLOCATE (OLD_TAB_I,OLD_TAB_R)

C=======================================================================
      ENDIF !end all interfaces type

c===================================================================== 
cc      print*,'----------- Upgrade MultiMP -----------'
cc      print*,'INTERFACE Number NI:',NI
cc      print*,'INTERFACE Type :',ITY
cc      print*,'NSN - NRTM :',NSN,NRTM
cc      print*,'NCONT   :',NCONT
cc      print*,'User Number : ',IPARI(NPARI*(NI-1)+15)
cc      print*,' '
cc      print*,'OLD MULTIMP:',MULTIMP
cc      print*,'NEW MULTIMP:',MULTIMP_PARAMETER
cc      print*,' '
cc      print*,'OLD SIZE MULTIMP*NCONT =',OLD_SIZE
cc      print*,'NEW SIZE MULTIMP*NCONT =',NEW_SIZE
cc      print*,'---------------------------------------'
cc      call my_flush(6)
C=======================================================================
      RETURN
 999  CONTINUE
      CALL ARRET_MESSAGE(ITY,IPARI(NPARI*(NI-1)+15),INTBUF_TAB%VARIABLES(37))
      
      CALL ARRET(2)
      END
