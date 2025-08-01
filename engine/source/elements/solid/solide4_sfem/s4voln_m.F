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
!||    s4voln_m           ../engine/source/elements/solid/solide4_sfem/s4voln_m.F
!||--- called by ------------------------------------------------------
!||    s4forc3            ../engine/source/elements/solid/solide4/s4forc3.F
!||--- calls      -----------------------------------------------------
!||    jacob_j33          ../engine/source/elements/solid/solide8e/jacob_j33.F
!||--- uses       -----------------------------------------------------
!||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
!||====================================================================
      SUBROUTINE S4VOLN_M(
     1   SFEM_NODVAR,      NC1,           NC2,      NC3,
     2   NC4,              MAT,           OFFG,     RHO,
     3   RHO0,             FXX,           FXY,      FXZ,
     4   FYX,              FYY,           FYZ,      FZX,
     5   FZY,              FZZ,           VOL0,     VOLN,
     6   VOL0DP,           VOLDP,         AMU0,     DXX,
     7   DYY,              DZZ,           MATPARAM, NEL,
     8   ISMSTR,           S_SFEM_NODVAR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER NC1(*),NC2(*),NC3(*),NC4(*),MAT(*),NEL
      INTEGER,INTENT(IN) :: S_SFEM_NODVAR
      my_real :: OFFG(*),VOL0(*),AMU0(*),
     .           DXX(*),DYY(*),DZZ(*),VOLN(*),
     .           FXX(*), FXY(*), FXZ(*),
     .           FYX(*), FYY(*), FYZ(*),
     .           FZX(*), FZY(*), FZZ(*),
     .           RHO(*),RHO0
      my_real,INTENT(INOUT) :: SFEM_NODVAR(S_SFEM_NODVAR)
      TYPE(MATPARAM_STRUCT_), DIMENSION(NUMMAT) :: MATPARAM
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr18_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX
      my_real AMU(MVSIZ), SUM,DTR,DTREP_R,DIVDE(MVSIZ),JAC_M(MVSIZ),JAC(MVSIZ),FAC,BASE,JFAC,DVDP
      DOUBLE PRECISION VOL0DP(*),VOLDP(*),SUMDP
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      MX = MAT(1)
      IF(ISMSTR == 1 .OR. ISMSTR == 11)THEN
        IF (TT==ZERO) THEN
          DO I=1,NEL
           IF(OFFG(I) == ZERO) CYCLE
           AMU0(I) = RHO(I)/RHO0-ONE
          ENDDO
        ELSE
          DO I=1,NEL
           IF(OFFG(I) == ZERO) CYCLE
           SUM=SFEM_NODVAR(NC1(I))+SFEM_NODVAR(NC2(I))+SFEM_NODVAR(NC3(I))+SFEM_NODVAR(NC4(I))
           AMU(I) = FOUR/SUM -ONE
           DIVDE(I) = AMU0(I)-AMU(I)
           DTR=DIVDE(I)/DT1             
           DTREP_R = THIRD*(DTR-DXX(I)-DYY(I)-DZZ(I))
           DXX(I) = DXX(I) + DTREP_R
           DYY(I) = DYY(I) + DTREP_R
           DZZ(I) = DZZ(I) + DTREP_R 
           AMU0(I)= RHO(I)/RHO0-ONE-DIVDE(I)
          ENDDO
        END IF
      ELSE
c-------------------------------------------------------------------------
        IF(IRESP == 1)THEN
          DO I=1,NEL
           IF(OFFG(I) == ZERO .OR. ABS(OFFG(I)) > ONE) CYCLE
           SUMDP=SFEM_NODVAR(NC1(I))+SFEM_NODVAR(NC2(I))+SFEM_NODVAR(NC3(I))+SFEM_NODVAR(NC4(I))
           VOLDP(I) = FOURTH*SUMDP*VOL0DP(I) 
           VOLN(I) = VOLDP(I)
          ENDDO
        ELSE
          DO I=1,NEL
           IF(OFFG(I) == ZERO .OR. ABS(OFFG(I)) > ONE) CYCLE
           SUM=SFEM_NODVAR(NC1(I))+SFEM_NODVAR(NC2(I))+SFEM_NODVAR(NC3(I))+SFEM_NODVAR(NC4(I))
           VOLN(I)=FOURTH*SUM*VOL0(I) 
          ENDDO
        END IF            
        IF (MATPARAM(MX)%STRAIN_FORMULATION == 1) THEN
C------compute AMU(t+dt) for large strain   
          IF(IRESP == 1)THEN
            AMU(1:NEL) = VOL0DP(1:NEL)/VOLDP(1:NEL) - ONE
          ELSE
            AMU(1:NEL) = VOL0(1:NEL)/VOLN(1:NEL) - ONE
          END IF
          IF (TT == ZERO) THEN
            AMU0(1:NEL) = AMU(1:NEL)
          ELSE          
            DO I = 1,NEL
              IF(OFFG(I)==ZERO.OR.ABS(OFFG(I))>ONE) CYCLE
              DTR   = (DXX(I) + DYY(I) + DZZ(I))*DT1
              DTREP_R = THIRD*((AMU(I)-AMU0(I))+DTR)/DT1
              DXX(I) = DXX(I) - DTREP_R
              DYY(I) = DYY(I) - DTREP_R
              DZZ(I) = DZZ(I) - DTREP_R  
              AMU0(I) = AMU(I)
            ENDDO
          END IF            
        ENDIF
        IF(ISMSTR >= 10)THEN
           DO I=1,NEL
            IF(OFFG(I)==ZERO) CYCLE
            JAC_M(I)=VOLN(I)/VOL0(I)             
           ENDDO
        ENDIF
        IF((ISMSTR == 2.OR.ISMSTR == 12).AND.IDTMIN(1) == 3) THEN
          IF (TT==ZERO) THEN
            DO I=1,NEL
             IF(OFFG(I)==ZERO) CYCLE
             AMU0(I) = RHO(I)/RHO0-ONE
            ENDDO
          ELSE
            DO I=1,NEL
             IF(OFFG(I) == ZERO .OR. ABS(OFFG(I)) <= ONE) CYCLE
             SUM=SFEM_NODVAR(NC1(I))+SFEM_NODVAR(NC2(I))+SFEM_NODVAR(NC3(I))+SFEM_NODVAR(NC4(I))
             AMU(I) = FOUR/SUM -ONE
             DIVDE(I) = AMU0(I)-AMU(I)
             DTR=DIVDE(I)/DT1             
             DTREP_R = THIRD*(DTR-DXX(I)-DYY(I)-DZZ(I))
             DXX(I) = DXX(I) + DTREP_R
             DYY(I) = DYY(I) + DTREP_R
             DZZ(I) = DZZ(I) + DTREP_R  
             DVDP = DIVDE(I)*(VOL0(I)/VOLN(I))
             AMU0(I)= RHO(I)/RHO0-ONE-DVDP
            ENDDO
C for total strain modif in the next
            IF(IRESP == 1 .AND. ISMSTR == 12)THEN
              DO I=1,NEL
               IF(OFFG(I) == ZERO .OR. ABS(OFFG(I)) <= ONE) CYCLE
               DVDP = DIVDE(I)*(VOL0(I)/VOLN(I))
               AMU0(I) = VOL0DP(I)/VOLDP(I)-ONE-DVDP
              ENDDO
            END IF
          END IF !(TT==ZERO) THEN
         ENDIF
      ENDIF
C--------total strain modif      
       IF (ISMSTR == 11) THEN
C---- modify Dii w/ rho/rho_0       
        DO I=1,NEL
          DTREP_R = -THIRD*(AMU0(I)+FXX(I)+FYY(I)+FZZ(I))
          FXX(I) = FXX(I) + DTREP_R
          FYY(I) = FYY(I) + DTREP_R
          FZZ(I) = FZZ(I) + DTREP_R 
        ENDDO
       ELSEIF(ISMSTR >= 10) THEN
        DO I=1,NEL
          IF(ABS(OFFG(I))<=ONE) CYCLE
          DTREP_R = -THIRD*(AMU0(I)+FXX(I)+FYY(I)+FZZ(I))
          FXX(I) = FXX(I) + DTREP_R
          FYY(I) = FYY(I) + DTREP_R
          FZZ(I) = FZZ(I) + DTREP_R 
        ENDDO
        CALL JACOB_J33(
     1   JAC,     FXX,     FXY,     FXZ,
     2   FYX,     FYY,     FYZ,     FZX,
     3   FZY,     FZZ,     NEL)
C
        FAC=THIRD
        DO I=1,NEL
         IF(ABS(OFFG(I)) > ONE) CYCLE
         BASE = JAC_M(I)/MAX(EM20,JAC(I))
         JFAC =EXP(FAC*LOG(MAX(EM20,BASE)))
         FXX(I) = JFAC*FXX(I)+JFAC-ONE
         FYY(I) = JFAC*FYY(I)+JFAC-ONE
         FZZ(I) = JFAC*FZZ(I)+JFAC-ONE
         FXY(I) = JFAC*FXY(I)
         FYX(I) = JFAC*FYX(I)
         FZX(I) = JFAC*FZX(I)
         FXZ(I) = JFAC*FXZ(I)
         FYZ(I) = JFAC*FYZ(I)
         FZY(I) = JFAC*FZY(I)
        ENDDO
       END IF
      RETURN
      END
