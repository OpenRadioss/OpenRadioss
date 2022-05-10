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
Chd|  H3D_SOL_SKIN_IXSKIN           source/output/h3d/h3d_results/h3d_sol_skin_ixskin.F
Chd|-- called by -----------
Chd|        H3D_SKIN_IXSKIN               source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SOL_SKIN_TENSOR           source/output/h3d/h3d_results/h3d_sol_skin_tensor.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE H3D_SOL_SKIN_IXSKIN(ELBUF_TAB,IPARG,IPARTS,IXS,IXS10,
     .                               IXSKIN  ,TAG_SKINS6,NSKIN )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "chara_c.inc"
#include      "nchara_c.inc"
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
C     REAL ou REAL*8
      INTEGER IPARG(NPARG,*),IXS(NIXS,*),IPARTS(*),IXSKIN(NIXQ,*),
     .        IXS10(6,*) ,TAG_SKINS6(*) ,NSKIN  
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      INTEGER I,ISOLNOD,ICS,NG,N,J,K
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
C----tetra4:-------------------------------------------
c              N8=N4   FACES : 2 2 1 1
c              N7=N3           4 3 3 4
c              N6=N3           1 1 3 4
c              N5=N4           2 2 4 3
c              N4=N2           1 2 3 3
c              N3=N2           1 4 4 2
c              N2=N1
c              N1=N1
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
        IF(MLW == 13 .OR. MLW == 0.OR.ITY /= 1) CYCLE   
C------     
        IF (IGTYP==6 .OR. IGTYP==14 ) THEN
           ISOLNOD = IPARG(28,NG)
           ICS     = IPARG(17,NG)                                        
           IF(ISOLNOD == 4)THEN
             DO I=1,NEL
               N = I + NFT
               NC(1,I)=IXS(2,N)
               NC(2,I)=IXS(4,N)
               NC(3,I)=IXS(7,N)
               NC(4,I)=IXS(6,N)
             ENDDO
C---------each face             
             DO I=1,NEL 
               N = I + NFT
               LL=TAG_SKINS6(N)
               JJ = 5
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
C---------3,2,1
                NSKIN = NSKIN + 1
                IXSKIN(1,NSKIN) = IPARTS(N)
                IXSKIN(2,NSKIN) = NC(3,I)
                IXSKIN(3,NSKIN) = NC(2,I)
                IXSKIN(4,NSKIN) = NC(1,I)
                IXSKIN(5,NSKIN) = IXSKIN(4,NSKIN)
                IXSKIN(6,NSKIN) = IXS(NIXS-1,N)
                IXSKIN(7,NSKIN) = NSKIN
               END IF
C---------2,3 ,4            
               JJ = 4
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
                NSKIN = NSKIN + 1
                IXSKIN(1,NSKIN) = IPARTS(N)
                IXSKIN(2,NSKIN) = NC(2,I)
                IXSKIN(3,NSKIN) = NC(3,I)
                IXSKIN(4,NSKIN) = NC(4,I)
                IXSKIN(5,NSKIN) = IXSKIN(4,NSKIN)
                IXSKIN(6,NSKIN) = IXS(NIXS-1,N)
                IXSKIN(7,NSKIN) = NSKIN
               END IF
C---------1,4,3           
               JJ = 3
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
                NSKIN = NSKIN + 1
                IXSKIN(1,NSKIN) = IPARTS(N)
                IXSKIN(2,NSKIN) = NC(1,I)
                IXSKIN(3,NSKIN) = NC(4,I)
                IXSKIN(4,NSKIN) = NC(3,I)
                IXSKIN(5,NSKIN) = IXSKIN(4,NSKIN)
                IXSKIN(6,NSKIN) = IXS(NIXS-1,N)
                IXSKIN(7,NSKIN) = NSKIN
               END IF
C---------1,2,4            
               JJ = 6
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
                NSKIN = NSKIN + 1
                IXSKIN(1,NSKIN) = IPARTS(N)
                IXSKIN(2,NSKIN) = NC(1,I)
                IXSKIN(3,NSKIN) = NC(2,I)
                IXSKIN(4,NSKIN) = NC(4,I)
                IXSKIN(5,NSKIN) = IXSKIN(4,NSKIN)
                IXSKIN(6,NSKIN) = IXS(NIXS-1,N)
                IXSKIN(7,NSKIN) = NSKIN
               END IF
             ENDDO
           ELSEIF(ISOLNOD == 6)THEN
           ELSEIF(ISOLNOD == 10)THEN
             DO I=1,NEL
             N = I + NFT
             NC(1,I)=IXS(2,N)
             NC(2,I)=IXS(4,N)
             NC(3,I)=IXS(7,N)
             NC(4,I)=IXS(6,N)
             NN1 = N - NUMELS8
             NC(5:10,I) = IXS10(1:6,NN1)
             ENDDO
C---------each face    4x4         
             DO I=1,NEL 
               N = I + NFT
               LL=TAG_SKINS6(N)
C---------1,2,3             
               JJ = 5
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
                T6(1:3) = NC(1:3,I)
                T6(4:6) = NC(5:7,I)
                  DO J=1,4
                     NSKIN = NSKIN + 1
                     IXSKIN(1,NSKIN) = IPARTS(N)
                     T3(1:3) = T6(TIA4S(1:3,J))
                     IXSKIN(2:4,NSKIN) = T3(1:3)
                     IXSKIN(5,NSKIN) = IXSKIN(4,NSKIN)
                     IXSKIN(6,NSKIN) = IXS(NIXS-1,N)
                     IXSKIN(7,NSKIN) = NSKIN
                  END DO 
               END IF
C---------2,3 ,4            
               JJ = 4
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
                T6(1:3) = NC(2:4,I)
                T6(4) = NC(6,I)
                T6(5) = NC(10,I)
                T6(6) = NC(9,I)
                  DO J=1,4
                     NSKIN = NSKIN + 1
                     IXSKIN(1,NSKIN) = IPARTS(N)
                     T3(1:3) = T6(TIA4S(1:3,J))
                     IXSKIN(2:4,NSKIN) = T3(1:3)
                     IXSKIN(5,NSKIN) = IXSKIN(4,NSKIN)
                     IXSKIN(6,NSKIN) = IXS(NIXS-1,N)
                     IXSKIN(7,NSKIN) = NSKIN
                  END DO 
               END IF
C---------1,4,3           
               JJ = 3
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
                T6(1) = NC(3,I)
                T6(2) = NC(1,I)
                T6(3) = NC(4,I)
                T6(4) = NC(7,I)
                T6(5) = NC(8,I)
                T6(6) = NC(10,I)
                  DO J=1,4
                     NSKIN = NSKIN + 1
                     IXSKIN(1,NSKIN) = IPARTS(N)
                     T3(1:3) = T6(TIA4S(1:3,J))
                     IXSKIN(2:4,NSKIN) = T3(1:3)
                     IXSKIN(5,NSKIN) = IXSKIN(4,NSKIN)
                     IXSKIN(6,NSKIN) = IXS(NIXS-1,N)
                     IXSKIN(7,NSKIN) = NSKIN
                  END DO 
               END IF
C---------1,2,4            
               JJ = 6
               IF(MOD(LL,PWR(JJ+1))/PWR(JJ) == 0) THEN
                T6(1:2) = NC(1:2,I)
                T6(3) = NC(4,I)
                T6(4) = NC(5,I)
                T6(5) = NC(9,I)
                T6(6) = NC(8,I)
                  DO J=1,4
                     NSKIN = NSKIN + 1
                     IXSKIN(1,NSKIN) = IPARTS(N)
                     T3(1:3) = T6(TIA4S(1:3,J))
                     IXSKIN(2:4,NSKIN) = T3(1:3)
                     IXSKIN(5,NSKIN) = IXSKIN(4,NSKIN)
                     IXSKIN(6,NSKIN) = IXS(NIXS-1,N)
                     IXSKIN(7,NSKIN) = NSKIN
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
                  IXSKIN(1,NSKIN) = IPARTS(N)
                  IXSKIN(2:4,NSKIN) = NS(1:3)
                  IF (NN > 3) THEN
                   IXSKIN(5,NSKIN) = NS(4)
                  ELSE
                   IXSKIN(5,NSKIN) = IXSKIN(4,NSKIN)
                  END IF
                  IXSKIN(6,NSKIN) = IXS(NIXS-1,N)
                  IXSKIN(7,NSKIN) = NSKIN
                END IF
               ENDDO
             ENDDO
           ENDIF
        ENDIF !IF (IGTYP== 
       END DO ! NG=1,NGROUP
C-----------
      RETURN
      END