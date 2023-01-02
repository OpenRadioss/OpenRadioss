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
Chd|  DMASANIS                      source/output/anim/generate/dmasani6.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE DMASANIS(ELBUF_TAB,X       ,D       ,GEO     ,IPARG   ,
     2                   IXS       ,MAS     ,PM      ,EL2FA   ,NBF     ,
     3                   IPART     ,IPARTSP ,ISPH3D  )
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
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   MAS(*) ,PM(NPROPM,*),GEO(NPROPG,*),X(3,*),D(3,*)
      INTEGER IPARG(NPARG,*),IXS(NIXS,*),EL2FA(*),NBF,IPART(LIPART1,*),
     .        IPARTSP(*),ISPH3D
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   EVAR(MVSIZ),
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE,THK0,A0,AL0,
     .   RHO0,XX1,XX2,XX3,YY1,YY2,YY3,ZZ1,ZZ2,ZZ3
      INTEGER I, NG, NEL, ISS, ISC,
     .        IADD, N, J, MLW, 
     .        ISTRAIN,NN, K1, K2,JTURB,MT,IMID, IALEL,IPID,
     .        N1,N2,N3,N4,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,
     .        OFFSET,NEL_OLD,ITY_OLD,NFT_FA,N_FA,
     .        INOD, ISOLNOD, IPRT
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C=======================================================================
      NN1 = 1
      NN2 = 1
      NN3 = NN2 + NUMELS 
      NN4 = NN3 + ISPH3D*(NUMSPH+MAXPJET)
C-----------------------------------------------
C       face ext
C-----------------------------------------------
      NEL_OLD = 0
      ITY_OLD = 0
      DO 490 NG=1,NGROUP
        CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        ISOLNOD = IPARG(28,NG)
        IF (IMACH == 3.AND.ISPMD == 0) THEN
          IF (ITY/=ITY_OLD) THEN
            NEL_OLD = 0
            ITY_OLD= ITY
          ENDIF
          NFT_FA = NEL_OLD
          NEL_OLD = NEL_OLD + NEL
        ENDIF
        DO OFFSET = 0,NEL-1,NVSIZ
          NFT   =IPARG(3,NG) + OFFSET
          IAD   =IPARG(4,NG)
          LFT=1
          LLT=MIN(NVSIZ,NEL-OFFSET)
          IF (IMACH == 3.AND.ISPMD == 0) THEN
            NFT_FA = NEL_OLD - NEL + OFFSET
          ELSE
            NFT_FA = NFT
          ENDIF
C-----------------------------------------------
C       SOLIDE
C-----------------------------------------------
          IF(ITY == 1)THEN
           GBUF => ELBUF_TAB(NG)%GBUF
           IALEL=IPARG(7,NG)+IPARG(11,NG)
           DO 130 I=LFT,LLT
             N = I + NFT
             N_FA = I + NFT_FA
             IF(EL2FA(NN2+N_FA)/=0)THEN
               IF (MLW == 0 .or. MLW == 13 .or. IGTYP == 0) THEN
                 VALUE = ZERO
               ELSEIF(IALEL == 0)THEN
                 MT=IXS(1,N)
                 VALUE=PM(89,MT)*GBUF%VOL(I)
               ELSE
                 OFF = MIN(GBUF%OFF(I),ONE)
                 VALUE=GBUF%RHO(I)*GBUF%VOL(I)*OFF
               ENDIF
               IF( NFILSOL /= 0 .AND. GBUF%G_FILL /= 0 ) 
     .           VALUE = VALUE * GBUF%FILL(I)
               IF (ISOLNOD == 16) THEN
                 VALUE = FOURTH*VALUE
                 MAS(EL2FA(NN2+N_FA))   = VALUE
                 MAS(EL2FA(NN2+N_FA)+1) = VALUE
                 MAS(EL2FA(NN2+N_FA)+2) = VALUE
                 MAS(EL2FA(NN2+N_FA)+3) = VALUE
               ELSE
                 MAS(EL2FA(NN2+N_FA)) = VALUE
               ENDIF
             ENDIF
 130       CONTINUE
C
        ELSEIF(ISPH3D == 1 .AND. ITY == 51)THEN
C-----------------------------------------------
C          TETRAS SPH.
C-----------------------------------------------
           GBUF => ELBUF_TAB(NG)%GBUF
           IALEL=IPARG(7,NG)+IPARG(11,NG)
           DO 140 I=LFT,LLT
             N = I + NFT
             N_FA = I + NFT_FA
             IF(EL2FA(NN3+N_FA)/=0)THEN
               IF(IALEL == 0)THEN
                 IPRT=IPARTSP(N)
                 MT  =IPART(1,IPRT)
                 VALUE=PM(89,MT)*GBUF%VOL(I)
               ELSE
                 OFF = MIN(GBUF%OFF(I),ONE)
                 VALUE=GBUF%RHO(I)*GBUF%VOL(I)*OFF
               ENDIF
               MAS(EL2FA(NN3+N_FA)) = VALUE
             ENDIF
 140       CONTINUE
C
        ELSE
        ENDIF
C-----------------------------------------------
C       FIN DE BOUCLE SUR LES OFFSET
C-----------------------------------------------
       END DO
 490  CONTINUE
C-----------------------------------------------
C
      RETURN
      END
