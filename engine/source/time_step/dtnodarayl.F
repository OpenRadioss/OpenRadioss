Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    dtnodarayl                       ../engine/source/time_step/dtnodarayl.F
!||--- called by ------------------------------------------------------
!||    resol                            ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    damping_vref_compute_dampa       ../engine/source/assembly/damping_vref_compute_dampa.F90
!||--- uses       -----------------------------------------------------
!||    damping_vref_compute_dampa_mod   ../engine/source/assembly/damping_vref_compute_dampa.F90
!||    groupdef_mod                     ../common_source/modules/groupdef_mod.F
!||====================================================================
      SUBROUTINE DTNODARAYL(MS       ,IN    ,STIFN ,STIFR ,DT2T     ,
     1                      IGRNOD   ,DAMPR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      use damping_vref_compute_dampa_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "stati_c.inc"
#include      "statr_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL 
      my_real , INTENT(IN) :: DT2T 
      my_real , DIMENSION(NUMNOD), INTENT(IN) ::MS 
      my_real , DIMENSION(NUMNOD*IRODDL), INTENT(IN) ::IN 
      my_real , DIMENSION(NUMNOD), INTENT(INOUT) ::STIFN 
      my_real , DIMENSION(NUMNOD*IRODDL), INTENT(INOUT) ::STIFR 
      my_real , DIMENSION(NRDAMP,NDAMP), INTENT(INOUT) ::DAMPR
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD), INTENT(IN) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,I,J,ISK,IGR,ND,K,L,LL,ITYPE,FL_VREL,ID_RBY,FL_FREQ_RANGE
      my_real FACTB,DAMPBR,DAMPAR,D_TSTART,D_TSTOP,DAMPT,
     .   DT2N(NUMNOD),BB,COEFF,DTN,DTN2,DAMPAI,DAMPBI,DT0,DAMPA3,
     .   ALPHA05,BETA1,DTMAX,FAC,DTMI1,DTMI2,BBI,FACI,DAMP_A(3)     
C----------------------------------------------------------
        DTMI1 = DTMIN1(11)
        DTMAX = EP04
        DT0 = DTMAX
        FACI = ZERO
        IF (DT1>ZERO) THEN
          DT0=MIN(DT0,DT1)
          DTMI1 = MAX(DTMI1,DT0)
        ELSE          
          IF (DTMI1>ZERO) DT0=MIN(DT0,DTMI1) 
        END IF
        DTMI2 = DTMI1/DTFAC1(11)
c--    
        DO I=1,NDAMP
C--       
          FL_VREL = 0
          FL_FREQ_RANGE = 0
          ITYPE = NINT(DAMPR(21,I))
          SELECT CASE (ITYPE)
             CASE(2) 
               FL_VREL = 1
             CASE(3) 
               FL_FREQ_RANGE = 1
          END SELECT
!          FL_FREQ_RANGE = NINT(DAMPR(31,I))
          IF ((DAMPR(19,I)/=0).OR.(FL_FREQ_RANGE > 0)) CYCLE                                               
          D_TSTART = DAMPR(17,I)
          D_TSTOP  = DAMPR(18,I)         
          IF ((TT<D_TSTART).OR.(TT>D_TSTOP)) CYCLE
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
          FACTB = ONE
          DAMPAI = MAX(DAMPR(3,I),DAMPR(5,I),DAMPR(7,I))
          DAMPAI = MAX(DAMPAI,DAMPR(9,I),DAMPR(11,I),DAMPR(13,I))
          DAMPBI = MAX(DAMPR(4,I),DAMPR(6,I),DAMPR(8,I))
          DAMPBI = MAX(DAMPBI,DAMPR(10,I),DAMPR(12,I),DAMPR(14,I))
          DAMPT = DT0*FACTB
          DAMPBI = MIN(DAMPBI,DAMPT)
C--          
C--       /DAMP/VREL - recompute damping parameters at current time
!          FL_VREL = NINT(DAMPR(21,I))
          IF (FL_VREL==1) THEN
            call damping_vref_compute_dampa(i,ndamp,nrdamp,dampr,dt1,tt,damp_a)
            DAMPAI = MAX(DAMP_A(1),DAMP_A(2),DAMP_A(3))
            DAMPBI = ZERO
          ENDIF  
C--
          IF (DTMI2 > ZERO) THEN
            BBI = DAMPBI+HALF*DAMPAI*DTMI2*DTMI2
            DTN2= SQRT(DTMI2*DTMI2+TWO*BBI*DTMI2)
            BBI = DAMPBI/DTN2+HALF*DAMPAI*DTN2
            FACI = SQRT(BBI*BBI+ONE) -BBI
          END IF                               
          DO J=1,IGRNOD(IGR)%NENTITY
            ND=IGRNOD(IGR)%ENTITY(J)
            IF(STIFN(ND)>EM20 .AND. MS(ND)>ZERO)THEN
              BB=DAMPBI/DT2N(ND)+HALF*DAMPAI*DT2N(ND)
              FAC = SQRT(BB*BB+ONE) -BB
              IF (FAC*DT2N(ND) < DTMI2) THEN
                FAC = MAX(FAC,FACI)
              END IF
              COEFF =ONE/FAC**2
              STIFN(ND) =STIFN(ND)*COEFF
            END IF                               
          END DO
          IF (IRODDL/=0) THEN
            DO J=1,IGRNOD(IGR)%NENTITY
              ND=IGRNOD(IGR)%ENTITY(J)
              IF(STIFR(ND)>EM20 .AND. IN(ND)>ZERO)THEN
                DT2N(ND) = SQRT(TWO*IN(ND)/STIFR(ND))
                DT2N(ND) = MIN(DTMAX,DT2N(ND))
                BB=DAMPBI/DT2N(ND)+HALF*DAMPAI*DT2N(ND)
                FAC = SQRT(BB*BB+ONE) -BB
                IF (FAC*DT2N(ND) < DTMI2) THEN
                  FAC = MAX(FAC,FACI)
                END IF
                COEFF =ONE/FAC**2
                STIFR(ND) =STIFR(ND)*COEFF
              END IF                               
            END DO      
          END IF                                             
        END DO !I=1,NDAMP
C----------global damping
        IF(IDAMP>0.OR.ISTAT==3)THEN
          IF (ISTAT==3 .AND.TT > ZERO) THEN
           DAMPA3 = TWO*BETATE/(ONE + BETATE * DT12)
          ELSE
           DAMPA3 = ZERO
          END IF 
          DTN2 = DTMAX
C------- still factor HALF* in another program          
          ALPHA05 = HALF*(DAMPA+DAMPA3)
          BETA1 = MIN(DAMPB,DT0)
          IF (DTMI2 > ZERO) THEN
            BBI = BETA1+ALPHA05*DTMI2*DTMI2
            DTN2= SQRT(DTMI2*DTMI2+TWO*BBI*DTMI2)
            BBI = BETA1/DTN2+ALPHA05*DTN2
            FACI = SQRT(BBI*BBI+ONE) -BBI
          END IF                               
          DO ND=1,NUMNOD
            COEFF =ONE
            IF(STIFN(ND)>EM20 .AND. MS(ND)>ZERO)THEN
              DT2N(ND) = SQRT(TWO*MS(ND)/STIFN(ND))
              DT2N(ND) = MIN(DTMAX,DT2N(ND))
              BB  = BETA1/DT2N(ND) + ALPHA05*DT2N(ND)
              FAC = SQRT(BB*BB+ONE) -BB
              IF (FAC*DT2N(ND) < DTMI2) THEN
                FAC = MAX(FAC,FACI)
              END IF
              COEFF =ONE/FAC**2
              STIFN(ND) =STIFN(ND)*COEFF
            END IF                               
          END DO      
          IF (IRODDL/=0) THEN
            DO ND=1,NUMNOD
              COEFF =ONE
              IF(STIFR(ND)>EM20 .AND. IN(ND)>ZERO)THEN
                DT2N(ND) = SQRT(TWO*IN(ND)/STIFR(ND))
                DT2N(ND) = MIN(DTMAX,DT2N(ND))
                BB  = BETA1/DT2N(ND) + ALPHA05*DT2N(ND)
                FAC = SQRT(BB*BB+ONE) -BB
                IF (FAC*DT2N(ND) < DTMI2) THEN
                  FAC = MAX(FAC,FACI)
                END IF
                COEFF =ONE/FAC**2
                STIFR(ND) =STIFR(ND)*COEFF
              END IF                               
            END DO      
          END IF                               
        END IF 
C        
      RETURN 
      END SUBROUTINE DTNODARAYL
