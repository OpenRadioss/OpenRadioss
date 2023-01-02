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
Chd|  SPBUC31                       source/elements/sph/spbuc31.F 
Chd|-- called by -----------
Chd|        SPTRI                         source/elements/sph/sptri.F   
Chd|-- calls ---------------
Chd|        SPTRIVOX                      source/elements/sph/sptrivox.F
Chd|        TRI7BOX                       share/modules1/tri7box.F      
Chd|====================================================================
      SUBROUTINE SPBUC31(X      ,KXSP  ,IXSP  ,NOD2SP,
     2                   SPBUF  ,MA    ,JVOIS ,JSTOR ,JPERM ,
     3                   DVOIS  ,IREDUCE,KREDUCE,BMINMA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER KXSP(NISP,*), IXSP(KVOISPH,*), NOD2SP(*),
     .        JVOIS(*), JSTOR(*), JPERM(*), IREDUCE, MA(NSPHACT),
     .        KREDUCE(*)
C     REAL
      my_real
     .        X(3,*),SPBUF(NSPBUF,*),DVOIS(*), BMINMA(6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NSPHACTF, NSPHACTL
C     REAL
      my_real
     .        AAA
      INTEGER NBX,NBY,NBZ
      INTEGER (KIND=8) :: NBX8,NBY8,NBZ8,RES8,LVOXEL8
C-----------------------------------------------

      AAA = SQRT(NSPHACT /
     .           ((BMINMA(1)-BMINMA(4))*(BMINMA(2)-BMINMA(5))
     .           +(BMINMA(2)-BMINMA(5))*(BMINMA(3)-BMINMA(6))
     .           +(BMINMA(3)-BMINMA(6))*(BMINMA(1)-BMINMA(4))))

      AAA = 0.75*AAA
              
      NBX = NINT(AAA*(BMINMA(1)-BMINMA(4)))
      NBY = NINT(AAA*(BMINMA(2)-BMINMA(5)))
      NBZ = NINT(AAA*(BMINMA(3)-BMINMA(6)))
      NBX = MAX(NBX,1)
      NBY = MAX(NBY,1)
      NBZ = MAX(NBZ,1)

      NBX8=NBX
      NBY8=NBY
      NBZ8=NBZ
      RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
      LVOXEL8 = LVOXEL      

      IF(RES8 > LVOXEL8) THEN
        AAA = LVOXEL
        AAA = AAA/((NBX8+2)*(NBY8+2)*(NBZ8+2))
        AAA = AAA**(THIRD)
        NBX = INT((NBX+2)*AAA)-2
        NBY = INT((NBY+2)*AAA)-2
        NBZ = INT((NBZ+2)*AAA)-2
        NBX = MAX(NBX,1)
        NBY = MAX(NBY,1)
        NBZ = MAX(NBZ,1)
      ENDIF
      
      NBX8=NBX
      NBY8=NBY
      NBZ8=NBZ
      RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
      
      IF(RES8 > LVOXEL8) stop 678  

C     initialisation complete de VOXEL
C (en // SMP il y a possibilite de redondance de traitement mais no pb)
      DO I=INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)
        VOXEL1(I)=0
      ENDDO
      INIVOXEL = MAX(INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)+1)

c      CALL MY_BARRIER fait dans SPTRIVOX
C--------------------------------------------------
C     VOXEL SORT
C--------------------------------------------------
      NSPHACTF=1
      NSPHACTL=NSPHACT
      CALL SPTRIVOX(
     1      NSPHACT ,X       ,BMINMA  ,NOD2SP ,
     2      NBX     ,NBY    ,NBZ     ,
     3      MA      ,SPBUF  ,JVOIS   ,JSTOR   ,JPERM  ,
     4      DVOIS   ,IREDUCE,NSPHACTF,NSPHACTL,VOXEL1 ,
     5      KXSP    ,IXSP   ,KREDUCE )
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPPRO31                       source/elements/sph/spbuc31.F 
Chd|-- called by -----------
Chd|        SPTRIVOX                      source/elements/sph/sptrivox.F
Chd|-- calls ---------------
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|====================================================================
      SUBROUTINE SPPRO31(IL    ,KXSP ,IXSP ,NOD2SP,JVOIS,
     .                  JSTOR,JPERM ,DVOIS,IREDUCE,KREDUCE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*), IXSP(KVOISPH,*), NOD2SP(*),
     .   JVOIS(*),JSTOR(*), JPERM(*), IREDUCE, KREDUCE(*)
C     REAL
      my_real
     .   DVOIS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, KB, JB, NSBT, IB, IL, MM1, MM2, KM, MM, MG,
     .   JJL, NFT, LLT, JL, JG, JLO, LL1, LL2, LL, LG, N, NN,
     .   NVOIS, KL, K, JK, L, NVOIS1, NVOIS2, IERROR , ig
C     REAL
      my_real
     .   CMS2(MVSIZ),XJJ, YJJ, ZJJ,DK, DL   


C-----------------
C     TRI DE LA LISTE, GARDE LES KVOISPH PREMIERS 
C     (COEF DE SECURITE CROISSANT).
C-----------------
      NVOIS=KXSP(5,IL)
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

       JG       =KXSP(3,JK)

       IF(DK<ONE)THEN
        NVOIS1=NVOIS1+1
        IXSP(NVOIS1,IL)=JG
       ELSE
        IXSP(NVOIS2,IL)=JG
        NVOIS2=NVOIS2-1
       ENDIF
      ENDDO
      KXSP(4,IL)=NVOIS1
C
      IF(NVOIS1>LVOISPH)IREDUCE=1
C-----------------------------------------------------------
C
      RETURN
      END
C
