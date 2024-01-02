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
Chd|  INIPHASE                      source/initial_conditions/inivol/iniphase.F
Chd|-- called by -----------
Chd|        INIFILL                       source/initial_conditions/inivol/inifill.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE INIPHASE(IXS      ,IPART_    ,IPHASE    ,IDP      ,
     .                    KVOL     ,NUPARAM   ,UPARAM    ,NTRACE   ,
     .                    ITAGNSOL ,ISOLNOD   ,PART_FILL ,NBIP,
     .                    NBSUBMAT ,MLW       ,ELBUF_TAB ,NG, MULTI_FVM,
     .                    IXQ      ,IXTG      ,ITYP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IPART_(*),IPHASE(NBSUBMAT+1,*),IDP,NUPARAM,
     . ITAGNSOL(NUMNOD),NBIP(NBSUBMAT,*),NTRACE,
     . ISOLNOD,PART_FILL(*),IXQ(NIXQ,*), IXTG(NIXTG,*),ITYP
      my_real
     .   KVOL(NBSUBMAT,*),UPARAM(NUPARAM)
      INTEGER,INTENT(IN) :: NBSUBMAT, MLW, NG
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP), INTENT(IN) :: ELBUF_TAB
      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,J
      INTEGER IX(4)
      my_real AV(NBSUBMAT)
      TYPE(G_BUFEL_) ,POINTER :: GBUF  
      TYPE(L_BUFEL_) ,POINTER :: LBUF  
C-----------------------------------------------
C---
C FILL ELEMENTS INPUT PHASES
C---
        IF(MLW==51)THEN
          AV(1) = UPARAM(4)
          AV(2) = UPARAM(5)
          AV(3) = UPARAM(6)
          AV(4) = UPARAM(46)
        ELSE
          GBUF => ELBUF_TAB(NG)%GBUF
          DO I=1,MULTI_FVM%NBMAT
            LBUF  => ELBUF_TAB(NG)%BUFLY(I)%LBUF(1,1,1) 
            AV(I) = LBUF%VOL(1) / GBUF%VOL(1)              !same value for idx=1,NEL then idx=1 here.
          ENDDO
        ENDIF
C
        DO I=LFT,LLT
          IF (IPART_(I) /= IDP  .AND. PART_FILL(IPART_(I)) == 0) THEN
            KVOL(1:NBSUBMAT,I)   = AV(1:NBSUBMAT)
            PART_FILL(IPART_(I)) = 1
          ELSEIF (IPART_(I) == IDP) THEN
            IPHASE(1,I)          = 1
            IPHASE(NBSUBMAT+1,I) = 1
            KVOL(1,I)            = ZERO
!!            INPHASE(1:NTRACE,I)  = 1
            IF (NBIP(1,I) == 0) NBIP(1,I) = NTRACE
            IF (ISOLNOD == 8) THEN
              DO K=2,9
                J = IXS(K,I)
                IF(ITAGNSOL(J) == 0)ITAGNSOL(J) = 1
              END DO
            ELSEIF (ISOLNOD == 4) THEN
              IX(1) =IXS(2,I)
              IX(2) =IXS(4,I)
              IX(3) =IXS(7,I)
              IX(4) =IXS(6,I)
              DO K=1,4
                J = IX(K)
                IF(ITAGNSOL(J) == 0)ITAGNSOL(J) = 1
              END DO!next K
            ELSEIF(ITYP == 7 .AND. N2D>0)THEN
                IF(ITAGNSOL(IXTG(2,I)) == 0)ITAGNSOL(IXTG(2,I)) = 1
                IF(ITAGNSOL(IXTG(3,I)) == 0)ITAGNSOL(IXTG(3,I)) = 1
                IF(ITAGNSOL(IXTG(4,I)) == 0)ITAGNSOL(IXTG(4,I)) = 1
            ELSEIF(ITYP == 2)THEN
                IF(ITAGNSOL(IXQ(2,I)) == 0)ITAGNSOL(IXQ(2,I)) = 1
                IF(ITAGNSOL(IXQ(3,I)) == 0)ITAGNSOL(IXQ(3,I)) = 1
                IF(ITAGNSOL(IXQ(4,I)) == 0)ITAGNSOL(IXQ(4,I)) = 1
                IF(ITAGNSOL(IXQ(5,I)) == 0)ITAGNSOL(IXQ(5,I)) = 1
            ENDIF!ISOLNOD
            PART_FILL(IDP) = 1
          END IF!(IPART_(I) /= IDP )
        END DO!next I
C-----
      RETURN
      END
