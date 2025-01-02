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
      !||    iniphase        ../starter/source/initial_conditions/inivol/iniphase.F
      !||--- called by ------------------------------------------------------
      !||    inifill         ../starter/source/initial_conditions/inivol/inifill.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      SUBROUTINE INIPHASE(N2D      ,NUMELS    ,NUMELTG   ,NUMELQ   ,
     .                    NUMNOD   ,NGROUP    ,
     .                    IXS      ,IPART_    ,IPHASE    ,IDP      ,
     .                    KVOL     ,NUPARAM   ,UPARAM    ,NTRACE   ,
     .                    ITAGNSOL ,ISOLNOD   ,PART_FILL ,NBIP     ,
     .                    NBSUBMAT ,MLW       ,ELBUF_TAB ,NG       , MULTI_FVM,
     .                    IXQ      ,IXTG      ,ITYP      ,NEL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MULTI_FVM_MOD
      USE CONSTANT_MOD , ONLY : NINE_OVER_10
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: N2D !< 2d/3d flag
      INTEGER,INTENT(IN) :: NUMELS, NUMELTG, NUMELQ, NUMNOD, NGROUP !array sizes
      INTEGER,INTENT(IN) :: NBSUBMAT
      INTEGER IXS(NIXS,NUMELS),IPART_(*),IPHASE(NBSUBMAT+1,*),IDP,NUPARAM
      INTEGER ITAGNSOL(NUMNOD)
      INTEGER :: NBIP(NBSUBMAT,NEL)   !< number of internal points
      INTEGER :: NTRACE               !<  maximum number of internal points (NBIP <= NTRACE, default 7*7*7)
      INTEGER ISOLNOD,PART_FILL(*)
      INTEGER,INTENT(IN) :: IXQ(NIXQ,NUMELQ) !< quad connectivity buffer
      INTEGER,INTENT(IN) :: IXTG(NIXTG,NUMELTG) !< triangles connectivity buffer
      INTEGER, INTENT(IN) :: ITYP !< elem types of the current group (quad, triangles, hexa,)
      my_real KVOL(NBSUBMAT,NEL)      !< volume fractions
      my_real UPARAM(NUPARAM)
      INTEGER,INTENT(IN) :: MLW !< material law (type)
      INTEGER,INTENT(IN) :: NG  !< current elem group
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP), INTENT(IN) :: ELBUF_TAB
      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      INTEGER, INTENT(IN) :: NEL !< number of elements in the group
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,K,J !< loop and temporary integers
      INTEGER :: IMAT  !< submat identifier (in 1:NBSUBMAT)
      INTEGER :: IX(4) !< working array for shell nodes
      my_real :: AV(NBSUBMAT) !< working array for volume fractions
      TYPE(G_BUFEL_) ,POINTER :: GBUF  !< global elem buffer
      TYPE(L_BUFEL_) ,POINTER :: LBUF  !< local elem buffer (submaterials)
C-----------------------------------------------
C---
C FILL ELEMENTS INPUT PHASES
C---
        AV(1:NBSUBMAT) = ZERO

        IF(MLW==51)THEN
          !when using LAW51 nbsubmat=4
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
        DO I=1,NEL
          IF(IPART_(I) /= 0) THEN
            IF (IPART_(I) /= IDP  .AND. PART_FILL(IPART_(I)) == 0) THEN
              KVOL(1:NBSUBMAT,I)   = AV(1:NBSUBMAT)
              PART_FILL(IPART_(I)) = 1
            ELSEIF (IPART_(I) == IDP) THEN
              IMAT=MAXLOC(AV(1:NBSUBMAT),1) ! The default phase is the one that is most prevalent.
              IPHASE(1,I)       = IMAT
              IPHASE(NBSUBMAT+1,I) = 1 !number of present submaterials
              KVOL(IMAT,I)         = ZERO
              IF (NBIP(IMAT,I) == 0) THEN
                NBIP(IMAT,I) = NTRACE
              ENDIF
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
              ELSEIF(ITYP == 7 .AND. N2D > 0)THEN
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
          ENDIF !IF(IPART(I) /= 0)
        END DO!next I
C-----
      RETURN
      END
