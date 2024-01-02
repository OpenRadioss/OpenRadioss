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
      BLOCK DATA INICOM06
#include      "implicit_f.inc"
#include      "com06_c.inc"
      DATA EPOR/0./
      END
Chd|====================================================================
Chd|  PORO                          source/ale/porous/poro.F      
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
       SUBROUTINE PORO(
     1    GEO    ,NODPOR ,MS ,X   ,V     ,
     2    W      ,AF     ,AM ,SKEW,WEIGHT,
     3    NPORGEO)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     --- /PROP/POROUS (TYPE15) ---
C     FORCES / POROUS MEDIA
C     FP 08/94
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODPOR(*), WEIGHT(*), NPORGEO(*)
      my_real GEO(NPROPG,NUMGEO),MS(*),X(3,NUMNOD),V(3,NUMNOD),W(3,NUMNOD),
     .        AF(3,*),AM(3,*),SKEW(LSKEW,*), AFM(20)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NAD,IG,I,K,JP,N,JRB,NG, FR_POR(NSPMD+2)
      my_real G1,G2,G3,R11,R12,R13,R21,R22,R23,R31,R32,R33,R(3,3),
     .        VX,VY,VZ,
     .        RFM(3,NUMPORL),F1(NUMPORL),F2(NUMPORL), 
     .        F3(NUMPORL),F4(NUMPORL),F5(NUMPORL),F6(NUMPORL)
      DOUBLE PRECISION FPOR6(6,6)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
       NAD=0
C
       DO IG=1,NUMGEO
        IF(INT(GEO(12,IG)) /= 15)CYCLE
        NG= INT(GEO(31,IG))  ! num of global nodes not stored
        N = NPORGEO(IG)      ! num of local nodes stored in SPMD porosity array
        IF(NG == 0)CYCLE
        IF(INT(GEO(20,IG)) == 1)GOTO 146
C
        G1=GEO(24,IG)
        IF(INT(GEO(30,IG)) == 0)THEN
          G2=GEO(25,IG)
          G3=GEO(26,IG)
        ELSEIF(DT1 > 0)THEN
C VELOCITY CORRECTION
          G2=ONE/DT1
          G3=ONE/DT1
        ELSE
          G2=ZERO
          G3=ZERO
        ENDIF
        K=INT(GEO(27,IG))
        R11=SKEW(1,K)
        R12=SKEW(2,K)
        R13=SKEW(3,K)
        R21=SKEW(4,K)
        R22=SKEW(5,K)
        R23=SKEW(6,K)
        R31=SKEW(7,K)
        R32=SKEW(8,K)
        R33=SKEW(9,K)
        R(1,1)=R11*R11*G1+R21*R21*G2+R31*R31*G3
        R(1,2)=R11*R12*G1+R21*R22*G2+R31*R32*G3
        R(1,3)=R11*R13*G1+R21*R23*G2+R31*R33*G3
        R(2,1)=R(1,2)
        R(2,2)=R12*R12*G1+R22*R22*G2+R32*R32*G3
        R(2,3)=R12*R13*G1+R22*R23*G2+R32*R33*G3
        R(3,1)=R(1,3)
        R(3,2)=R(2,3)
        R(3,3)=R13*R13*G1+R23*R23*G2+R33*R33*G3
        JRB=INT(GEO(29,IG))
C
        IF(IALE == 1)THEN
          DO I=NAD+1,NAD+N
            JP=NODPOR(I)
            VX=MS(JP)*(V(1,JP)-W(1,JP))+AF(1,JP)*DT1
            VY=MS(JP)*(V(2,JP)-W(2,JP))+AF(2,JP)*DT1
            VZ=MS(JP)*(V(3,JP)-W(3,JP))+AF(3,JP)*DT1
            RFM(1,I)=R(1,1)*VX+R(1,2)*VY+R(1,3)*VZ
            RFM(2,I)=R(2,1)*VX+R(2,2)*VY+R(2,3)*VZ
            RFM(3,I)=R(3,1)*VX+R(3,2)*VY+R(3,3)*VZ
          ENDDO
        ELSE
          DO I=NAD+1,NAD+N
            JP=NODPOR(I)
            VX=MS(JP)*V(1,JP)+AF(1,JP)*DT1
            VY=MS(JP)*V(2,JP)+AF(2,JP)*DT1
            VZ=MS(JP)*V(3,JP)+AF(3,JP)*DT1
            RFM(1,I)=R(1,1)*VX+R(1,2)*VY+R(1,3)*VZ
            RFM(2,I)=R(2,1)*VX+R(2,2)*VY+R(2,3)*VZ
            RFM(3,I)=R(3,1)*VX+R(3,2)*VY+R(3,3)*VZ
          ENDDO
        ENDIF
C
        DO I=NAD+1,NAD+N
          JP=NODPOR(I)
          AF(1,JP)=AF(1,JP)-RFM(1,I)
          AF(2,JP)=AF(2,JP)-RFM(2,I)
          AF(3,JP)=AF(3,JP)-RFM(3,I)
        ENDDO
C
C TRANSMISSION OF REACTION FORCES
C
        IF(JRB /= 0)THEN
          DO I=NAD+1,NAD+N
           JP=NODPOR(I)
           IF(WEIGHT(JP) == 1) THEN
             F1(I-NAD)=RFM(1,I)
             F2(I-NAD)=RFM(2,I)
             F3(I-NAD)=RFM(3,I)
             F4(I-NAD)=RFM(3,I)*(X(2,JP)-X(2,JRB))
     +                -RFM(2,I)*(X(3,JP)-X(3,JRB))
             F5(I-NAD)=RFM(1,I)*(X(3,JP)-X(3,JRB))
     +                -RFM(3,I)*(X(1,JP)-X(1,JRB))
             F6(I-NAD)=RFM(2,I)*(X(1,JP)-X(1,JRB))
     +                -RFM(1,I)*(X(2,JP)-X(2,JRB))
           ELSE
             F1(I-NAD)=ZERO
             F2(I-NAD)=ZERO
             F3(I-NAD)=ZERO
             F4(I-NAD)=ZERO
             F5(I-NAD)=ZERO
             F6(I-NAD)=ZERO
           ENDIF
          ENDDO
C
C Parith/ON treatment before SPMD exchange
C
          DO K = 1, 6
           FPOR6(1,K) = ZERO
           FPOR6(2,K) = ZERO
           FPOR6(3,K) = ZERO
           FPOR6(4,K) = ZERO
           FPOR6(5,K) = ZERO
           FPOR6(6,K) = ZERO
          END DO
          CALL SUM_6_FLOAT(1  ,N  ,F1, FPOR6(1,1), 6)
          CALL SUM_6_FLOAT(1  ,N  ,F2, FPOR6(2,1), 6)
          CALL SUM_6_FLOAT(1  ,N  ,F3, FPOR6(3,1), 6)
          CALL SUM_6_FLOAT(1  ,N  ,F4, FPOR6(4,1), 6)
          CALL SUM_6_FLOAT(1  ,N  ,F5, FPOR6(5,1), 6)
          CALL SUM_6_FLOAT(1  ,N  ,F6, FPOR6(6,1), 6)
  
          IF(NSPMD > 1) THEN
C FR_POR built here
            DO I = 1, NSPMD
              FR_POR(I) = 1
            END DO
            FR_POR(NSPMD+1)=NG   ! nb noeuds total
            FR_POR(NSPMD+2)=1    ! PMAIN
            CALL SPMD_EXCH_FR6(FR_POR,FPOR6,6*6)
          END IF

          AFM(1) = FPOR6(1,1)+FPOR6(1,2)+FPOR6(1,3)+
     +             FPOR6(1,4)+FPOR6(1,5)+FPOR6(1,6)
          AFM(2) = FPOR6(2,1)+FPOR6(2,2)+FPOR6(2,3)+
     +             FPOR6(2,4)+FPOR6(2,5)+FPOR6(2,6)
          AFM(3) = FPOR6(3,1)+FPOR6(3,2)+FPOR6(3,3)+
     +             FPOR6(3,4)+FPOR6(3,5)+FPOR6(3,6)
          AFM(4) = FPOR6(4,1)+FPOR6(4,2)+FPOR6(4,3)+
     +             FPOR6(4,4)+FPOR6(4,5)+FPOR6(4,6)
          AFM(5) = FPOR6(5,1)+FPOR6(5,2)+FPOR6(5,3)+
     +             FPOR6(5,4)+FPOR6(5,5)+FPOR6(5,6)
          AFM(6) = FPOR6(6,1)+FPOR6(6,2)+FPOR6(6,3)+
     +             FPOR6(6,4)+FPOR6(6,5)+FPOR6(6,6)
C
          AF(1,JRB)=AF(1,JRB)+AFM(1)
          AF(2,JRB)=AF(2,JRB)+AFM(2)
          AF(3,JRB)=AF(3,JRB)+AFM(3)
          AM(1,JRB)=AM(1,JRB)+AFM(4)
          AM(2,JRB)=AM(2,JRB)+AFM(5)
          AM(3,JRB)=AM(3,JRB)+AFM(6)
C
C INTERNAL ENERGY
C
          DO I=NAD+1,NAD+N
            JP=NODPOR(I)
            IF(WEIGHT(JP) == 1) THEN
              EPOR=EPOR+DT1*
     +                     (RFM(1,I)*(V(1,JP)-W(1,JP))
     +                     +RFM(2,I)*(V(2,JP)-W(2,JP))
     +                     +RFM(3,I)*(V(3,JP)-W(3,JP)))
            ENDIF
          ENDDO
C
        ELSE
          DO I=NAD+1,NAD+N
            JP=NODPOR(I)
            IF(WEIGHT(JP) == 1) THEN
              EPOR=EPOR+DT1*
     +             (RFM(1,I)*V(1,JP)+RFM(2,I)*V(2,JP)+RFM(3,I)*V(3,JP))
            ENDIF
          ENDDO
        ENDIF
 146   NAD=NAD+N
       ENDDO!next IG

      RETURN
      END
