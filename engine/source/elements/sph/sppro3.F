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
Chd|  SPPRO3                        source/elements/sph/sppro3.F
Chd|-- called by -----------
Chd|        SPTRIVOX                      source/elements/sph/sptrivox.F
Chd|-- calls ---------------
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|        SPHBOX                        share/modules/sphbox.F
Chd|====================================================================
      SUBROUTINE SPPRO3(IL    ,KXSP ,IXSP ,NOD2SP,JVOIS,
     .                  JSTOR,JPERM ,DVOIS,IREDUCE,KREDUCE,
     .                  KXSPR,IXSPR,TAB_DK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE SPHBOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER KXSP(NISP,*), IXSP(KVOISPH,*), NOD2SP(*),
     .     JVOIS(*),JSTOR(*), JPERM(*), IREDUCE, KREDUCE(*),
     .     KXSPR(*), IXSPR(KVOISPH,*)
C     REAL
        my_real
     .     DVOIS(*),TAB_DK(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER J, KB, JB, NSBT, IB, IL, MM1, MM2, KM, MM, MG,
     .     JJL, NFT, LLT, JL, JG, JLO, LL1, LL2, LL, LG, N, NN,
     .     NVOIS, KL, K, JK, L, NVOIS1, NVOIS2, IERROR
C     REAL
        my_real
     .     CMS2(MVSIZ),XJJ, YJJ, ZJJ,DK, DL
        my_real, DIMENSION(:), ALLOCATABLE :: DSTOR


        IF(IL <= NUMSPH.AND.IL > 0)THEN
C-----------------
C       TRI DE LA LISTE, GARDE LES KVOISPH PREMIERS
C       (COEF DE SECURITE CROISSANT).

C-----------------
          NVOIS=KXSP(5,IL)
          TAB_DK(IL) = -ONE
          IF(NVOIS>KVOISPH)THEN

            IREDUCE    =1
            KREDUCE(IL)=1

            CALL MYQSORT(NVOIS,DVOIS,JPERM,IERROR)
            DO K=1,NVOIS
              JSTOR(K)=JVOIS(K)
            ENDDO
            DO K=1,KVOISPH
              JVOIS(K)=JSTOR(JPERM(K))
            ENDDO
            DK=DVOIS(KVOISPH)
            TAB_DK(IL) = DK
C-----------------
C Choix des cellules a conserver tq distance < DK pour eviter pb de parith/on
            NVOIS=0
            DO K=1,KXSP(5,IL)
              IF(DVOIS(K)<DK)THEN
                NVOIS=NVOIS+1
              END IF
            END DO

          ENDIF
C-----------------
          NVOIS=MIN(NVOIS,KVOISPH)
          KXSP(5,IL)=NVOIS
          DO K=1,NVOIS
            JK=JVOIS(K)
            DK=DVOIS(K)

            IF(JK<=NUMSPH) THEN
              JG       =KXSP(3,JK)
            ELSE
              JG = -JK+NUMSPH ! si voisin remote alors no noeud = - no cellule remote
              XSPHR(1,-JG) = -ABS(XSPHR(1,-JG)) ! reperage cell remote utile
            END IF
            IXSP(K,IL)=JG
          ENDDO
C-----------------------------------------------------------
        ELSEIF(IL > NUMSPH)THEN        ! traitement pour cell remote inutile
C-----------------
          XSPHR(1,IL-NUMSPH) = -ABS(XSPHR(1,IL-NUMSPH)) ! reperage cell remote utile
C
C       TRI DE LA LISTE, GARDE LES KVOISPH PREMIERS
C       (COEF DE SECURITE CROISSANT).
C-----------------
! Get the DK of the original cell
          DK = DKR(IL - NUMSPH)
          NVOIS=KXSPR(IL-NUMSPH)
          IF(DK>=ZERO) THEN
            IF(NVOIS>KVOISPH) THEN
C-----------------
C Choix des cellules a conserver tq distance < DK pour eviter pb de parith/on
              NVOIS=KXSPR(IL-NUMSPH)
              CALL MYQSORT(NVOIS,DVOIS,JPERM,IERROR)
              DO K=1,NVOIS
                JSTOR(K)=JVOIS(K)
              ENDDO
              DO K=1,MIN(KVOISPH,NVOIS)
                JVOIS(K)=JSTOR(JPERM(K))
              ENDDO
              NVOIS=MIN(NVOIS,KVOISPH)
            ELSE
              ALLOCATE(DSTOR(KXSPR(IL-NUMSPH)))
              DO K=1,KXSPR(IL-NUMSPH)
                JSTOR(K)=JVOIS(K)
                DSTOR(K)=DVOIS(K)
              ENDDO
              NVOIS=0
              DO K=1,KXSPR(IL-NUMSPH)
                IF(DSTOR(K)<DK)THEN
                  NVOIS=NVOIS+1
                  JVOIS(NVOIS)=JSTOR(K)
                  DVOIS(NVOIS)=DSTOR(K)
                END IF
              END DO
              DEALLOCATE(DSTOR)
              NVOIS=MIN(NVOIS,KVOISPH)
            ENDIF
          ELSE
            NVOIS=KXSPR(IL-NUMSPH)
          ENDIF
C-----------------
          KXSPR(IL-NUMSPH)=NVOIS
          DO K=1,NVOIS
            JK=JVOIS(K)
            DK=DVOIS(K)
            IF(JK<=NUMSPH) THEN
              JG=KXSP(3,JK)
            ELSE
              print *,'internal error'
            END IF
            IXSPR(K,IL-NUMSPH)=JG
          ENDDO
        ELSE                           ! IL < 0 <=> Gauge
C
Cf gauges IL=-NUMSPH-...
          IL=ABS(IL)
C-----------------
C       TRI DE LA LISTE, GARDE LES KVOISPH PREMIERS
C       (COEF DE SECURITE CROISSANT).

C-----------------
          NVOIS=KXSP(5,IL)
          IF(NVOIS>KVOISPH)THEN

            CALL MYQSORT(NVOIS,DVOIS,JPERM,IERROR)
            DO K=1,NVOIS
              JSTOR(K)=JVOIS(K)
            ENDDO
            DO K=1,KVOISPH
              JVOIS(K)=JSTOR(JPERM(K))
            ENDDO
            DK=DVOIS(KVOISPH)
C-----------------
C Choix des cellules a conserver tq distance < DK pour eviter pb de parith/on
            NVOIS=0
            DO K=1,KXSP(5,IL)
              IF(DVOIS(K)<DK)THEN
                NVOIS=NVOIS+1
              END IF
            END DO
          ENDIF
C-----------------
          NVOIS=MIN(NVOIS,KVOISPH)
          KXSP(5,IL)=NVOIS
          NVOIS1=0
          NVOIS2=NVOIS
          DO K=1,NVOIS
            JK       =JVOIS(K)
            DK       =DVOIS(K)
            IF(JK<=NUMSPH) THEN
              JG       =KXSP(3,JK)
            ELSE
              JG = -JK+NUMSPH ! si voisin remote alors no noeud = - no cellule remote
              XSPHR(1,-JG) = -ABS(XSPHR(1,-JG)) ! reperage cell remote utile
            END IF

            IF(DK<ONE)THEN
              NVOIS1=NVOIS1+1
              IXSP(NVOIS1,IL)=JG
            ELSE
              IXSP(NVOIS2,IL)=JG
              NVOIS2=NVOIS2-1
            ENDIF
          ENDDO
          KXSP(4,IL)=NVOIS1
C-----------------------------------------------------------
        END IF

        RETURN
      END
