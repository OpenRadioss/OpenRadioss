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
Chd|  DAMPDTNODA                    source/general_controls/damping/dampdtnoda.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        READ_ENGINE_DTMIN             source/general_controls/damping/read_engine_dtmin.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE DAMPDTNODA(MS ,IN  ,STIFN ,STIFR ,IGRNOD   ,DAMPR )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real , DIMENSION(NUMNOD), INTENT(IN) ::MS 
      my_real , DIMENSION(NUMNOD*IRODDL), INTENT(IN) ::IN 
      my_real , DIMENSION(NUMNOD), INTENT(INOUT) ::STIFN 
      my_real , DIMENSION(NUMNOD), INTENT(INOUT) ::STIFR 
      my_real , DIMENSION(NRDAMP,NDAMP), INTENT(IN) ::DAMPR 
C-----------------------------------------------
      TYPE (GROUP_) ,DIMENSION(NGRNOD), INTENT(IN) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,I,J,ISK,IGR,ND,K,L,LL
      my_real FACTB,DAMPBR,DAMPAR,D_TSTART,D_TSTOP,DAMPT,
     .   DT2N(NUMNOD),BB,COEFF,DTN,DTN2,DAMPAI,DAMPBI,DT0,DAMPA3,
     .   ALPHA05,BETA1,DTMAX,FAC,DTMI1,FAC0    
C----------------------------------------------------------
        CALL READ_ENGINE_DTMIN(DTMI1)
        DTMAX = EP04
        DT0 = DTMAX
        DO I=1,NDAMP
          IF (DAMPR(19,I)/=0) CYCLE                                               
          IGR   = NINT(DAMPR(2,I))
          DTN = DTMAX
          DO J=1,IGRNOD(IGR)%NENTITY
            ND=IGRNOD(IGR)%ENTITY(J)
            DT2N(ND) = DTMAX
            IF(STIFN(ND)>EM20 .AND. MS(ND)>ZERO)THEN
              DT2N(ND) = SQRT(TWO*MS(ND)/STIFN(ND))
              DT2N(ND) = MIN(DTMAX,DT2N(ND))
              DTN = MIN(DT2N(ND),DTN)
            END IF                               
          END DO      
          IF (IRODDL/=0) THEN
            DO J=1,IGRNOD(IGR)%NENTITY
              ND=IGRNOD(IGR)%ENTITY(J)
              IF(STIFR(ND)>EM20 .AND. IN(ND)>ZERO)THEN
                DTN2 = SQRT(TWO*IN(ND)/STIFR(ND))
                DT2N(ND) = MIN(DT2N(ND),DTN2)
                DTN = MIN(DT2N(ND),DTN)
              END IF                               
            END DO      
          END IF                               
          DT0=MIN(DT0,DTN)
          DAMPAI = MAX(DAMPR(3,I),DAMPR(5,I),DAMPR(7,I))
          DAMPAI = MAX(DAMPAI,DAMPR(9,I),DAMPR(11,I),DAMPR(13,I))
          DAMPBI = MAX(DAMPR(4,I),DAMPR(6,I),DAMPR(8,I))
          DAMPBI = MAX(DAMPBI,DAMPR(10,I),DAMPR(12,I),DAMPR(14,I))
          DAMPT = DT0*DAMPR(16,I)
          DAMPBI = MIN(DAMPBI,DAMPT)
C--  
          DO J=1,IGRNOD(IGR)%NENTITY
            ND=IGRNOD(IGR)%ENTITY(J)
            BB=DAMPBI/DT2N(ND)+HALF*DAMPAI*DT2N(ND)
            FAC = SQRT(BB*BB+ONE) -BB
            IF (FAC*DT2N(ND) < DTMI1) THEN
              BB=DAMPBI/DTMI1+HALF*DAMPAI*DTMI1
              FAC = MAX(FAC,SQRT(BB*BB+ONE) -BB)
            END IF
            COEFF =ONE/FAC**2
            IF(STIFN(ND)>EM20 .AND. MS(ND)>ZERO)THEN
              STIFN(ND) =STIFN(ND)*COEFF
            END IF                               
            IF (IRODDL/=0) THEN
              IF(STIFR(ND)>EM20 .AND. IN(ND)>ZERO)THEN
                STIFR(ND) =STIFR(ND)*COEFF
              END IF                               
            END IF                               
          END DO      
        END DO !I=1,NDAMP
C        
      RETURN 
      END SUBROUTINE DAMPDTNODA
