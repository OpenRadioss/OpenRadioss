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
Chd|  H3D_SKIN_SOL_DIM              source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- called by -----------
Chd|        H3D_SKIN_DIM                  source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE H3D_SKIN_SOL_DIM(IPARG   ,TAG_SKINS6, NUMSKIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER IPARG(NPARG,*),NUMSKIN, TAG_SKINS6(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      INTEGER I,NG,N,J,N1,N2,
     .        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     .        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     .        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     .        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     .        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     .        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS   ,
     .        NN,ICS,ISOLNOD,NN1,JJ,II,K1,K2,NF(3),NSKIN2,IDB
      INTEGER FACES(4,6),NS(4),PWR(7),LL
      DATA PWR/1,2,4,8,16,32,64/
      DATA FACES/4,3,2,1,
     .           5,6,7,8,
     .           1,2,6,5,
     .           3,4,8,7,
     .           2,3,7,6,
     .           1,5,8,4/
C----tetra4:-------------------------------------------
c              N8=N4   FACES : 2 2 1 1
c              N7=N3           4 3 3 4
c              N6=N3           1 1 3 4
c              N5=N4           2 2 4 3
c              N4=N2           1 2 3 3
c              N3=N2           1 4 4 2
C-----------------------------------------------
       DO NG=1,NGROUP
        CALL INITBUF(IPARG    ,NG      ,                      
     2        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    ) 
!
        IF(MLW == 13 .OR. MLW == 0.OR.ITY /= 1) CYCLE   
C------         
        IF (IGTYP==6 .OR. IGTYP==14 ) THEN
           ISOLNOD = IPARG(28,NG)
           ICS     = IPARG(17,NG)                                        
           IF(ISOLNOD == 4)THEN
             DO I=1,NEL
               N = I + NFT
               LL=TAG_SKINS6(N)
               DO JJ=3,6
                IF(MOD(LL,PWR(JJ+1))/PWR(JJ) /= 0)CYCLE
                NUMSKIN = NUMSKIN + 1
               END DO
             ENDDO
           ELSEIF(ISOLNOD == 6)THEN
           ELSEIF(ISOLNOD == 10)THEN
             DO I=1,NEL
               N = I + NFT
               LL=TAG_SKINS6(N)
               DO JJ=3,6
                IF(MOD(LL,PWR(JJ+1))/PWR(JJ) /= 0)CYCLE
                NUMSKIN = NUMSKIN + 4
               END DO
             ENDDO
           ELSE
             DO I=1,NEL
               N = I + NFT
               LL=TAG_SKINS6(N)
               DO JJ=1,6
                IF(MOD(LL,PWR(JJ+1))/PWR(JJ) /= 0)CYCLE
                NUMSKIN = NUMSKIN + 1
               END DO
             ENDDO
           ENDIF
        ENDIF !IF (IGTYP== 
       END DO ! NG=1,NGROUP
C-----------
      RETURN
      END
