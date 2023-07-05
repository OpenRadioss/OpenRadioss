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
Chd|  DFUNCC_CRK                    source/output/anim/generate/dfuncc_crk.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SPMD_R4GET_PARTN              source/mpi/anim/spmd_r4get_partn.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE DFUNCC_CRK(
     .             ELBUF_TAB   ,LEN        , IFUNC  ,IPARG  ,GEO     ,
     .             IXC         ,IXTG       , MASS   ,PM     ,EL2FA   ,
     .             NBF         ,IADP       , NBF_L  ,EHOUR  ,ANIM    ,
     .             NBPART      ,IADG       , IPM    ,IGEO   ,THKE    ,
     .             ERR_THK_SH4 ,ERR_THK_SH3,XFEM_TAB,IEL_CRK,INDX_CRK,
     .             NBF_CRKXFEMG,EL2FA0     ,CRKEDGE )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE CRACKXFEM_MOD
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
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFUNC,NBF,LEN,NBF_L, NBPART,NBF_CRKXFEMG
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),IXTG(NIXTG,*),EL2FA(*),
     .        IADP(*),IADG(NSPMD,*),IPM(NPROPMI,*),INDX_CRK(*),
     .        IGEO(NPROPGI,*),EL2FA0(*),IEL_CRK(*)
C     REAL
      my_real
     .   MASS(*),GEO(NPROPG,*),
     .   EHOUR(*),ANIM(*),PM(NPROPM,*),THKE(*),
     .   ERR_THK_SH4(*), ERR_THK_SH3(*)
      REAL WAL(NBF_L)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL), TARGET :: XFEM_TAB
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   EVAR(MVSIZ),FUNC(LEN), 
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE,
     .   A1,B1,B2,B3,YEQ,F1,M1,M2,M3, FAC, DAM1(MVSIZ),DAM2(MVSIZ),
     .   WPLA(MVSIZ), DMAX(MVSIZ),WPMAX(MVSIZ),
     .   FAIL(MVSIZ),THK0,THKE0(MVSIZ)
      INTEGER I,NG,NEL,ISC,N,J,MLW,NUVAR,
     .        ISTRAIN,NN,K1,K2,MT,IMID,IPID,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NF,
     .        OFFSET,K,II,KK,IHBE,I1,MPT,IPT,BUF,NUVARR,
     .        IPMAT,PID(MVSIZ),MAT(MVSIZ),MATLY(MVSIZ*100),
     .        IEXPAN,NEL_CRK,NLEVXF,NI,JTURB,
     .        NLAY,NPTT,IXEL,ILAY,IL,IUS,JJ(5)
      INTEGER IXFEM,IP,JPID,CRKS,ICRK,ILAYCRK,ELCRK,NPT0
      INTEGER NELCRK(NCRKPART),IE(NCRKPART)
      REAL R4
C
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C
      TYPE(G_BUFEL_) ,POINTER :: XGBUF
      TYPE(L_BUFEL_) ,POINTER :: XLBUF
C=======================================================================
      NEL_CRK = 0
      FUNC(1:LEN) = ZERO
c
      DO CRKS = 1,NCRKPART
        ICRK = INDX_CRK(CRKS)
        NELCRK(CRKS) = NEL_CRK
        NEL_CRK = NEL_CRK + CRKSHELL(ICRK)%CRKNUMSHELL
        IE(ICRK) = 0
      ENDDO
C
      NN1 = 1
      NN3 = 1
      NN4 = NN3 + NUMELQ
      NN5 = NN4 + NUMELC
      NN6 = NN5 + NUMELTG
C
      DO NG=1,NGROUP
C---
        IXFEM  = IPARG(54,NG)
        IF (IXFEM /= 1 .AND. IXFEM /= 2) CYCLE
C---
        CALL INITBUF(IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTURB   ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS)
C---
        IF (ITY /= 3   .AND. ITY /= 7) CYCLE
        IF (MLW /= 13) THEN
          DO OFFSET = 0,NEL-1,NVSIZ
            NFT   =IPARG(3,NG) + OFFSET
            LFT=1
            LLT=MIN(NVSIZ,NEL-OFFSET)
            NPT    = IPARG(6,NG)
            IHBE   = IPARG(23,NG)
            IF (IHBE == 11) CYCLE
            NUVAR = 0
!
            DO I=1,5
              JJ(I) = NEL*(I-1)
            ENDDO
!
C-----------------------------------------------
C        SHELLS 3-N, 4-N
C-----------------------------------------------
            MPT = IABS(NPT)
            NPT0 = NPT
C-----------------------------------------
C-----------------------------------------
            IF (IXFEM == 1) NPT = 1  !  multlayer xfem
C-----------------------------------------
C-----------------------------------------
            GBUF => ELBUF_TAB(NG)%GBUF
C
            IF (ITY == 3) THEN
              NI = NFT
            ELSE
              NI = NFT + NUMELC
            ENDIF
C-----------------------------------------
C-----------------------------------------
C          LOOP OVER PHANTOM ELEMENTS
C-----------------------------------------
C-----------------------------------------
            DO IXEL=1,NXEL
              XGBUF => XFEM_TAB(NG,IXEL)%GBUF
              NLAY = XFEM_TAB(NG,IXEL)%NLAY
              DO ILAY=1,NLAY
C---
                ICRK = NXEL*(ILAY-1) + IXEL
C---
                IF (NLAY > 1) THEN                           
                  LBUF  => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)
                  XLBUF => XFEM_TAB(NG,IXEL)%BUFLY(ILAY)%LBUF(1,1,1)
                ELSE                                         
                  LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,ILAY)
                  XLBUF => XFEM_TAB(NG,IXEL)%BUFLY(1)%LBUF(1,1,ILAY)
                ENDIF
                XGBUF => XFEM_TAB(NG,IXEL)%GBUF
cc                BUFLY => XFEM_TAB(NG,IXEL)%BUFLY(ILAY)
cc                BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
C---
                NUVAR = 0
C---------------------    
                DO I=LFT,LLT
                  EVAR(I) = ZERO   ! Init to zero in all cases !
                ENDDO
C---------------------
C
                IF (MLW == 0 .OR. MLW == 13) THEN
                  CONTINUE
c---
                ELSE IF (IFUNC == 1) THEN   ! plastic strain
                  IF (NLAY > 1) THEN  ! multi                                                                           
cc                    IPT = INT((1+NPT)/2)  ! NPT = 1                                                                   
                    IPT = ILAY                                                                                          
                    IF (ELBUF_TAB(NG)%BUFLY(IPT)%L_PLA > 0) THEN                                                        
                      LBUF  => ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(1,1,1)                                                       
                      XLBUF => XFEM_TAB(NG,IXEL)%BUFLY(IPT)%LBUF(1,1,1)                                                   
                      DO I=LFT,LLT                                                                                        
                        N = I + NI                                                                                        
                        ELCRK = IEL_CRK(N)                                                                                
                        IF (ELCRK > 0) THEN                                                                               
                          ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)                                                           
                          IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer                               
                            EVAR(I) = ABS(LBUF%PLA(I)) ! for law25, plastic work < 0 if the layer has reached failure-p   
                          ELSE  !  cracked layer                                                                          
                            EVAR(I) = ABS(XLBUF%PLA(I)) ! for law25, plastic work < 0 if the layer has reached failure-p  
                          ENDIF                                                                                           
                        ENDIF                                                                                             
                      ENDDO                                                                                               
                    ENDIF  !  IF (L_PLA > 0)
                  ELSEIF (GBUF%G_PLA > 0 ) THEN  !  mono                                                                                         
                    IPT = MAX(1,INT((1+NPT)/2))                                                                         
                    LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,IPT)                                                        
                    XLBUF => XFEM_TAB(NG,IXEL)%BUFLY(1)%LBUF(1,1,IPT)                                                   
                    DO I=LFT,LLT                                                                                        
                      N = I + NI                                                                                        
                      ELCRK = IEL_CRK(N)                                                                                
                      IF (ELCRK > 0) THEN                                                                               
                        ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)                                                           
                        IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer                               
                          EVAR(I) = ABS(LBUF%PLA(I)) ! for law25, plastic work < 0 if the layer has reached failure-p   
                        ELSE  !  cracked layer                                                                          
                          EVAR(I) = ABS(XLBUF%PLA(I)) ! for law25, plastic work < 0 if the layer has reached failure-p  
                        ENDIF                                                                                           
                      ENDIF                                                                                             
                    ENDDO                                                                                               
                  ENDIF !  IF (NLAY > 1)                                                                                
                ELSEIF (IFUNC == 3) THEN   ! EINT
                  IF (NLAY > 1) THEN  ! multi
                    DO I=LFT,LLT
                      N = I + NI
                      ELCRK = IEL_CRK(N)
                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                      IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                        EVAR(I) = GBUF%EINT(I) + GBUF%EINT(I+LLT)
                      ELSE  !  cracked layer
                        EVAR(I) = XLBUF%EINT(I) + XLBUF%EINT(I+LLT)
                      ENDIF
                    ENDDO
                  ELSE  !  mono
                    DO I=LFT,LLT
                      N = I + NI
                      ELCRK = IEL_CRK(N)
                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                      IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                        EVAR(I) = GBUF%EINT(I) + GBUF%EINT(I+LLT)
                      ELSE  !  cracked layer
                        EVAR(I) = XGBUF%EINT(I) + XGBUF%EINT(I+LLT)
                      ENDIF
                    ENDDO
                  ENDIF !  IF (NLAY > 1)
                ELSEIF (IFUNC == 5) THEN   ! THK
                  IF (NLAY > 1) THEN  ! multi
                    DO I=LFT,LLT
                      EVAR(I) = XLBUF%THK(I)
                    ENDDO
                  ELSE  !  mono
                    DO I=LFT,LLT
                      EVAR(I) = XGBUF%THK(I)
                    ENDDO
                  ENDIF
                ELSEIF (IFUNC == 7) THEN   ! Von Mises
                  IF (NLAY > 1) THEN  ! multi
                    DO I=LFT,LLT
                      N = I + NI
                      ELCRK = IEL_CRK(N)
                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                      IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                        S1 = GBUF%FOR(JJ(1)+I)
                        S2 = GBUF%FOR(JJ(2)+I)
                        S12= GBUF%FOR(JJ(3)+I)
                      ELSE  !  cracked layer
                        S1 = XLBUF%FOR(JJ(1)+I)
                        S2 = XLBUF%FOR(JJ(2)+I)
                        S12= XLBUF%FOR(JJ(3)+I)
                      ENDIF
                      VONM2= S1*S1 + S2*S2 - S1*S2 + THREE*S12*S12
                      EVAR(I) = SQRT(VONM2)
                    ENDDO
                  ELSE  !  mono
                    DO I=LFT,LLT
                      N = I + NI
                      ELCRK = IEL_CRK(N)
                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                      IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                        S1 = GBUF%FOR(JJ(1)+I)
                        S2 = GBUF%FOR(JJ(2)+I)
                        S12= GBUF%FOR(JJ(3)+I)
                      ELSE  !  cracked layer
                        S1 = XGBUF%FOR(JJ(1)+I)
                        S2 = XGBUF%FOR(JJ(2)+I)
                        S12= XGBUF%FOR(JJ(3)+I)
                      ENDIF
                      VONM2= S1*S1 + S2*S2 - S1*S2 + THREE*S12*S12
                      EVAR(I) = SQRT(VONM2)
                    ENDDO
                  ENDIF !  IF (NLAY > 1)
c---
                ELSEIF (IFUNC >= 14 .AND. IFUNC <= 15) THEN
c---          Sigx, Sigy
                  IUS = IFUNC-13
                  IF (NLAY > 1) THEN  ! multi
                    DO I=LFT,LLT
                      N = I + NI
                      ELCRK = IEL_CRK(N)
                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                      IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                        EVAR(I) = GBUF%FOR(JJ(IUS)+I)
                      ELSE  !  cracked layer
                        EVAR(I) = XLBUF%FOR(JJ(IUS)+I)
                      ENDIF
                    ENDDO
                  ELSE  !  mono
                    DO I=LFT,LLT
                      N = I + NI
                      ELCRK = IEL_CRK(N)
                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                      IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                        EVAR(I) = GBUF%FOR(JJ(IUS)+I)
                      ELSE  !  cracked layer
                        EVAR(I) = XGBUF%FOR(JJ(IUS)+I)
                      ENDIF
                    ENDDO
                  ENDIF !  IF (NLAY > 1)
c---
                ELSEIF (IFUNC >= 17 .AND. IFUNC <= 19) THEN
c---          Sigyx
                  IUS = IFUNC-14
                  IF (NLAY > 1) THEN  ! multi
                    DO I=LFT,LLT
                      N = I + NI
                      ELCRK = IEL_CRK(N)
                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                      IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                        EVAR(I) = GBUF%FOR(JJ(IUS)+I)
                      ELSE  !  cracked layer
                        EVAR(I) = XGBUF%FOR(JJ(IUS)+I)
                      ENDIF
                    ENDDO
                  ELSE  !  mono
                    DO I=LFT,LLT
                      N = I + NI
                      ELCRK = IEL_CRK(N)
                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                      IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                        EVAR(I) = GBUF%FOR(JJ(IUS)+I)
                      ELSE  !  cracked layer
                        EVAR(I) = XGBUF%FOR(JJ(IUS)+I)
                      ENDIF
                    ENDDO
                  ENDIF !  IF (NLAY > 1)
c---
                ELSEIF (IFUNC == 26 .and. GBUF%G_EPSD > 0) THEN
                  IF (NLAY > 1) THEN  ! multi
                    DO I=LFT,LLT
                      N = I + NI
                      ELCRK = IEL_CRK(N)
                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                      IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                        EVAR(I) = GBUF%EPSD(I)
                      ELSE  !  cracked layer
                        EVAR(I) = XLBUF%EPSD(I)
                      ENDIF
                    ENDDO
                  ELSE  !  mono
                    DO I=LFT,LLT
                      N = I + NI
                      ELCRK = IEL_CRK(N)
                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                      IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                        EVAR(I) = GBUF%EPSD(I)
                      ELSE  !  cracked layer
                        EVAR(I) = XGBUF%EPSD(I)
                      ENDIF
                    ENDDO
                  ENDIF !  IF (NLAY > 1)
c---
                ELSEIF (IFUNC == 2155) THEN
C
                  IF (ITY == 3) THEN
                    DO I=LFT,LLT
                      PID(I) = IXC(6,NFT+1)
                    ENDDO
                  ELSEIF (ITY == 7) THEN
                    DO I=LFT,LLT
                      PID(I) = IXTG(5,NFT+1)
                    ENDDO
                  ENDIF
C
                  DO I=LFT,LLT
                    N = I + NI
                    THKE0(I) = THKE(N) * GEO(300+ILAY,PID(I))
                  ENDDO
C
                  IF (NLAY > 1) THEN  ! multi
                    DO I=LFT,LLT
                      N = I + NI
                      ELCRK = IEL_CRK(N)
                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                      THK0 = THKE0(I)
                      IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
C                        EVAR(I) = HUNDRED *(THKE(N) - GBUF%THK(I))/THKE(N)
                        EVAR(I) = HUNDRED *(THK0 - XLBUF%THK(I))/THK0
                      ELSE  !  cracked layer
                        EVAR(I) = HUNDRED *(THK0 - XLBUF%THK(I))/THK0
                      ENDIF
                    ENDDO
                  ELSE  !  mono
                    DO I=LFT,LLT
                      N = I + NI
                      ELCRK = IEL_CRK(N)
                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                      THK0 = THKE(N)
                      IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                        EVAR(I) = HUNDRED *(THK0 - GBUF%THK(I))/THK0
                      ELSE  !  cracked layer
                        EVAR(I) = HUNDRED *(THK0 - XGBUF%THK(I))/THK0
                      ENDIF
                    ENDDO
                  ENDIF !  IF (NLAY > 1)
C---
                ELSEIF (IFUNC == 2040) THEN  ! EPSP/UPPER
                  IF (NLAY > 1) THEN
                    IL  = MAX(1,NPT)
                    IPT = 1
                  ELSE
                    IL  = 1
                    IPT = MAX(1,NPT)
                  ENDIF

                  IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA > 0) THEN
                    IF (NLAY > 1) THEN  ! multi
                      DO I=LFT,LLT
                        N = I + NI
                        ELCRK = IEL_CRK(N)
                        ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                        IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                          EVAR(I) = ABS(
     .                        ELBUF_TAB(NG)%BUFLY(IL)%LBUF(1,1,IPT)%PLA(I))
                        ELSE  !  cracked layer
                          EVAR(I) = ABS(
     .                        XFEM_TAB(NG,IXEL)%BUFLY(ILAY)%LBUF(1,1,IPT)%PLA(I))
                        ENDIF
                      ENDDO
                    ELSE  !  mono
                      DO I=LFT,LLT
                        N = I + NI
                        ELCRK = IEL_CRK(N)
                        ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                        IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                          EVAR(I) = ABS(
     .                        ELBUF_TAB(NG)%BUFLY(IL)%LBUF(1,1,IPT)%PLA(I))
                        ELSE  !  cracked layer
                          EVAR(I) = ABS(
     .                        XFEM_TAB(NG,IXEL)%BUFLY(IL)%LBUF(1,1,IPT)%PLA(I))
                        ENDIF
                      ENDDO
                    ENDIF !  IF (NLAY > 1)
                  ELSE
                    DO I=LFT,LLT
                      EVAR(I) = ZERO
                    ENDDO 
                  ENDIF  ! IF (BUFLY%L_PLA > 0)
c------------------------------------
                ELSEIF (IFUNC == 2041) THEN ! EPSP/LOWER
c------------------------------------
                  IF (NLAY > 1) THEN
                    IL  = MAX(1,NPT)
                    IPT = 1
                  ELSE
                    IL  = 1
                    IPT = MAX(1,NPT)
                  ENDIF
                  IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA > 0) THEN
                    IL = 1
                    IF (NLAY > 1) IL = ILAY
                    DO I=LFT,LLT
                      N = I + NI
                      ELCRK = IEL_CRK(N)
                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                      IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                        EVAR(I) = ABS(
     .                      ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)%PLA(I))
                      ELSE  !  cracked layer
                        EVAR(I) = ABS(
     .                      XFEM_TAB(NG,IXEL)%BUFLY(IL)%LBUF(1,1,1)%PLA(I))
                      ENDIF
                    ENDDO
                  ELSE
                    DO I=LFT,LLT
                      EVAR(I) = ZERO
                    ENDDO 
                  ENDIF
c------------------------------------
                ELSEIF (IFUNC >= 2042 .AND. IFUNC <= 2141) THEN
c------------------------------------
                  IF (NPT == 0) THEN
                    IL  = 1 
                    IPT = 1                            
                  ELSEIF (NLAY > 1) THEN                   
                    IL = MOD ((IFUNC - 2041), 100)     
                    IPT = 1                            
                    IF (IL == 0) IL = 100
                  ELSE                                 
                    IL  = 1                            
                    IPT = MOD ((IFUNC - 2041), 100)     
                    IF (IPT == 0) IPT = 100
                  ENDIF
                  IF (ELBUF_TAB(NG)%BUFLY(IL)%L_PLA > 0) THEN
                    IF (NLAY > 1) THEN  ! multi
                      DO I=LFT,LLT
                        N = I + NI
                        ELCRK = IEL_CRK(N)
                        ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                        IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                          EVAR(I) = ABS(
     .                        ELBUF_TAB(NG)%BUFLY(IL)%LBUF(1,1,IPT)%PLA(I))
                        ELSE  !  cracked layer
                          EVAR(I) = ABS(
     .                        XFEM_TAB(NG,IXEL)%BUFLY(ILAY)%LBUF(1,1,IPT)%PLA(I))
                        ENDIF
                      ENDDO
                    ELSE  !  mono
                      DO I=LFT,LLT
                        N = I + NI
                        ELCRK = IEL_CRK(N)
                        ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                        IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                          EVAR(I) = ABS(
     .                        ELBUF_TAB(NG)%BUFLY(IL)%LBUF(1,1,IPT)%PLA(I))
                        ELSE  !  cracked layer
                          EVAR(I) = ABS(
     .                        XFEM_TAB(NG,IXEL)%BUFLY(IL)%LBUF(1,1,IPT)%PLA(I))
                        ENDIF
                      ENDDO
                    ENDIF !  IF (NLAY > 1)
                  ELSE
                    DO  I=LFT,LLT
                      EVAR(I) = ZERO
                    ENDDO 
                  ENDIF 
                ENDIF ! IFUNC
C----------------------
                IF(MLW == 0 .OR. MLW == 13)THEN
                  DO I=LFT,LLT
                    N = I + NI
                    IF(IEL_CRK(N) > 0) THEN
                      IE(ICRK) = IE(ICRK) + 1
                      FUNC(EL2FA(NELCRK(ICRK) + IE(ICRK))) = ZERO 
                    ENDIF
                  ENDDO
C-------------------
                ELSEIF (IFUNC == 3) THEN
C energie specifique
C-------------------
                  IF (ITY == 3) THEN
                    DO I=LFT,LLT
                      N = I + NI
                      IF (IEL_CRK(N) > 0) THEN
                        IE(ICRK) = IE(ICRK) + 1
                        FUNC(EL2FA(NELCRK(ICRK) + IE(ICRK))) = EVAR(I)/
     .                       MAX(EM30,MASS(EL2FA0(NN4+I+NFT)))
                      ENDIF
                    ENDDO
                  ELSEIF (ITY == 7) THEN
                    DO I=LFT,LLT
                      N = I + NI
                      IF (IEL_CRK(N) > 0) THEN
                        IE(ICRK) = IE(ICRK) + 1
                        FUNC(EL2FA(NELCRK(ICRK) + IE(ICRK))) = EVAR(I)/
     .                       MAX(EM30,MASS(EL2FA0(NN5+I+NFT)))
                      ENDIF
                    ENDDO
                  ENDIF
C-------------------
                ELSEIF (IFUNC == 25 .AND. ITY == 3) THEN
C energie hourglass
C-------------------
                  DO I=LFT,LLT
                    N = I + NFT
                    IF (IEL_CRK(N) > 0) THEN
                      IE(ICRK) = IE(ICRK) + 1
                      FUNC(EL2FA(NELCRK(ICRK) + IE(ICRK))) = EHOUR(N+NUMELS)/
     .                    MAX(EM30,MASS(EL2FA0(NN4+N)))
                    ENDIF
                  ENDDO
C-------------------
                ELSE  ! IFUNC SHELLS
C cas general
C-------------------
                  DO I=LFT,LLT
                    N = I + NI
                    IF (IEL_CRK(N) > 0) THEN
                      IE(ICRK) = IE(ICRK) + 1
                      FUNC(EL2FA(NELCRK(ICRK) + IE(ICRK))) = EVAR(I)
                    ENDIF
                  ENDDO
                ENDIF  ! IFUNC
C-----------------------------------------------
C       FIN DE BOUCLE SUR LES OFFSET
C-----------------------------------------------
              ENDDO  !  DO ILAY=1,NLAY
            ENDDO  !  DO IXEL=1,NXEL
          ENDDO  !  DO OFFSET
        ENDIF  ! MLW /= 13
      ENDDO  !  DO NG=1,NGROUP
C-----------------------------------------------=
      DO CRKS = 1,NCRKPART
        ICRK = INDX_CRK(CRKS)
C
        NEL_CRK = NELCRK(ICRK)
C
         IF (NSPMD == 1) THEN
          DO I=1,IE(ICRK)
             N = EL2FA(NEL_CRK + I)
             R4 = FUNC(N)
             CALL WRITE_R_C(R4,1)
           ENDDO
         ELSE
          DO I=1,IE(ICRK)
             N = EL2FA(NEL_CRK + I)
             WAL(I+NEL_CRK) = FUNC(N)
           ENDDO
         ENDIF
      ENDDO
C
      IF (NSPMD > 1 ) THEN
        IF (ISPMD == 0) THEN
          BUF = NBF_CRKXFEMG
        ELSE
          BUF=1
        ENDIF
          CALL SPMD_R4GET_PARTN(1,NBF_L,NBPART,IADG,WAL,BUF)
      ENDIF
C
      RETURN
      END
