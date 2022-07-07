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
Chd|  H3D_SKIN_OFF                  source/output/h3d/h3d_results/h3d_skin_off.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE H3D_SKIN_OFF(ELBUF_TAB,IPARG,IXS,IXS10,
     .                        TAG_SKINS6,SKIN_OFF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "spmd_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER, DIMENSION(NPARG,NGROUP),INTENT(IN):: IPARG
      INTEGER, DIMENSION(NIXS,NUMELS),INTENT(IN):: IXS
      INTEGER, DIMENSION(6,NUMELS10),INTENT(IN):: IXS10
      INTEGER, DIMENSION(NUMELS),INTENT(IN):: TAG_SKINS6
      my_real, DIMENSION(NUMSKIN),INTENT(OUT):: SKIN_OFF
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      INTEGER I,NSKIN,ISOLNOD,ICS,NG,N,J
      INTEGER 
     .        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     .        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     .        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     .        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     .        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     .        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    , 
     .        NN,NN1,N1,IDB
      INTEGER NC(10,MVSIZ),NMIN,PWR(7),LL
      INTEGER FACES(4,6),NS(4),JJ,II,K1,K2,NF,N2,T3(3),T6(6),TIA4S(3,4)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      DATA PWR/1,2,4,8,16,32,64/
      DATA FACES/4,3,2,1,
     .           5,6,7,8,
     .           1,2,6,5,
     .           3,4,8,7,
     .           2,3,7,6,
     .           1,5,8,4/
      DATA TIA4S/3,5,6,
     .           2,4,5,
     .           1,6,4,
     .           4,6,5/
C-----------------------------------------------
       NSKIN =0
      IF (NUMSKIN> NUMSKINP) THEN      
       DO NG=1,NGROUP
        ISOLNOD = IPARG(28,NG)
        ICS     = IPARG(17,NG)                                        
        CALL INITBUF (IPARG    ,NG      ,                      
     2        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    ) 
!
        GBUF => ELBUF_TAB(NG)%GBUF
        IF(MLW == 13 .OR. MLW == 0) CYCLE          
C-----------------------------------------------
C       THICK-SHELL 
C-----------------------------------------------
!                8--------------7
!               / |            /|
!              5--------------|6
!              |  |           | |
!              |  4-----------|-3
!              | /            |/     
!              1--------------2
        IF (ITY == 1.AND.(IGTYP==20 .OR. IGTYP==21 .OR. IGTYP==22)) THEN

C-------- grp skin_inf first
            DO I=1,NEL
              SKIN_OFF(NSKIN+I) = NINT(MIN(GBUF%OFF(I),ONE))
            END DO
            NSKIN = NSKIN + NEL
C-------- grp skin_sup 
            DO I=1,NEL
              SKIN_OFF(NSKIN+I) = NINT(MIN(GBUF%OFF(I),ONE))
            END DO
            NSKIN = NSKIN + NEL
C-----------------------------------------------
        ENDIF !IF (ITY == 1.AND.(IGTYP==20 
       END DO ! NG=1,NGROUP
      END IF !(NUMSKIN> NUMSKINP) THEN      
C------SOLID elements 
       NFT = NSKIN
      IF (NUMSKIN> (NSKIN+NUMSKINP)) THEN      
C      
       DO NG=1,NGROUP
        CALL INITBUF (IPARG    ,NG      ,                      
     2        MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,    
     3        NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,    
     4        JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,    
     5        NVAUX   ,JPOR    ,KCVT    ,JCLOSE  ,JPLASOL ,    
     6        IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,    
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    ) 
!
        GBUF => ELBUF_TAB(NG)%GBUF
        IF(MLW == 13 .OR. MLW == 0.OR.ITY /= 1) CYCLE   
C------     
        IF (IGTYP==6 .OR. IGTYP==14 ) THEN
           ISOLNOD = IPARG(28,NG)
           ICS     = IPARG(17,NG)                                        
           IF(ISOLNOD == 4)THEN
C---------each face             
             DO I=1,NEL 
               N = I + NFT
               LL=TAG_SKINS6(N)
               JJ = 5
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
C---------3,2,1
                NSKIN = NSKIN + 1
                SKIN_OFF(NSKIN) = NINT(MIN(GBUF%OFF(I),ONE))
               END IF
C---------2,3 ,4            
               JJ = 4
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
                NSKIN = NSKIN + 1
                SKIN_OFF(NSKIN) = NINT(MIN(GBUF%OFF(I),ONE))
               END IF
C---------1,4,3           
               JJ = 3
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
                NSKIN = NSKIN + 1
                SKIN_OFF(NSKIN) = NINT(MIN(GBUF%OFF(I),ONE))
               END IF
C---------1,2,4            
               JJ = 6
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
                NSKIN = NSKIN + 1
                SKIN_OFF(NSKIN) = NINT(MIN(GBUF%OFF(I),ONE))
               END IF
             ENDDO
           ELSEIF(ISOLNOD == 6)THEN
           ELSEIF(ISOLNOD == 10)THEN
C---------each face    4x4         
             DO I=1,NEL 
               N = I + NFT
               LL=TAG_SKINS6(N)
C---------1,2,3             
               JJ = 5
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
                  DO J=1,4
                     NSKIN = NSKIN + 1
                     SKIN_OFF(NSKIN) = NINT(MIN(GBUF%OFF(I),ONE))
                  END DO 
               END IF
C---------2,3 ,4            
               JJ = 4
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
                  DO J=1,4
                     NSKIN = NSKIN + 1
                     SKIN_OFF(NSKIN) = NINT(MIN(GBUF%OFF(I),ONE))
                  END DO 
               END IF
C---------1,4,3           
               JJ = 3
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
                  DO J=1,4
                     NSKIN = NSKIN + 1
                     SKIN_OFF(NSKIN) = NINT(MIN(GBUF%OFF(I),ONE))
                  END DO 
               END IF
C---------1,2,4            
               JJ = 6
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
                  DO J=1,4
                     NSKIN = NSKIN + 1
                     SKIN_OFF(NSKIN) = NINT(MIN(GBUF%OFF(I),ONE))
                  END DO 
               END IF
             ENDDO
C-----------S8 (&degenerated),S20           
           ELSE
             DO I=1,NEL 
                N = I + NFT
                NC(1:8,I) = IXS(2:9,N)
                LL=TAG_SKINS6(N)
C--------per face               :
               DO JJ=1,6
                IF(MOD(LL,PWR(JJ+1))/PWR(JJ) /= 0)CYCLE
                DO II=1,4
                  NS(II)=NC(FACES(II,JJ),I)
                END DO
C---------for degenerated cases                
                DO K1=1,3
                  DO K2=K1+1,4
                    IF(NS(K2)==NS(K1))NS(K2)=0
                  END DO
                END DO
                NN=0
                DO K1=1,4
                  N1=NS(K1)
                  IF(N1/=0)THEN
                     NN=NN+1
                     NS(NN)= N1
                  END IF
                END DO
                IF (NN>2) THEN
                  NSKIN = NSKIN + 1
                  SKIN_OFF(NSKIN) = NINT(MIN(GBUF%OFF(I),ONE))
                END IF
               ENDDO
             ENDDO
           ENDIF
        ENDIF !IF (IGTYP== 
       END DO ! NG=1,NGROUP
      END IF !(NUMSKIN> (NSKIN+NUMSKINP)) THEN  
C------to show pressure 
       NFT = NSKIN
      IF (NUMSKINP>0) THEN      
       DO I=NFT+1,NSKIN
         SKIN_OFF(I) = ONE
       END DO 
      END IF      
C-----------
      RETURN
      END