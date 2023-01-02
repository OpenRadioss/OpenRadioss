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
Chd|  TENSORC_CRK                   source/output/anim/generate/tensorc_crk.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SIGROTA_XFE                   source/output/anim/generate/sigrota_xfe.F
Chd|        SPMD_R4GET_PARTN              source/mpi/anim/spmd_r4get_partn.F
Chd|        UROTOV                        source/airbag/uroto.F         
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TENSORC_CRK(ELBUF_TAB,XFEM_TAB,IPARG     ,IPM     ,
     1                       ITENS    ,INVERT  ,EL2FA     ,NBF     ,
     2                       LEN      ,EPSDOT  ,IADP      ,NBF_L   ,
     3                       NBPART   ,IADG    ,X         ,IXC     ,
     4                       IGEO     ,IXTG    ,IEL_CRK   ,IADC_CRK,
     5                       CRKEDGE  ,INDX_CRK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),ITENS,INVERT(*),INDX_CRK(*),
     .   EL2FA(*),IXC(NIXC,*),IGEO(NPROPGI,*), 
     .   NBF,IADP(*),NBF_L,NBPART,IADG(NSPMD,*),
     .   IXTG(NIXTG,*),LEN,IEL_CRK(*),IADC_CRK(*),
     .   IPM(NPROPMI,*)
C     REAL
      my_real
     .   EPSDOT(6,*),X(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL), TARGET :: XFEM_TAB
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   A1,A2,A3,THK,SIGE(MVSIZ,5),TENS(3,LEN)
      REAL R4(18)
      INTEGER I,NI,NG,NEL,NFT,ITY,LFT,NPT,IPT,
     .        N,J,LLT,MLW,ISTRAIN,
     .        IPID,I1,I2,ISTRE,NNI,N0,
     .        KK,IHBE,IREP,BUF,NEL_CRK,
     .        NLAY,NPTT,IXEL,ILAY,NUVARV,IVISC,
     .        IPMAT,IGTYP,MATLY,NLEVXF,NPG,ICRK,JJ(8)
C
      INTEGER IXFEM,K,CRKS,ITG,IA,NN1,NN2,
     .        NN3,NN4,NN5,NN6
      REAL WA(3,NBF_L)
      INTEGER ILAYCRK,ELCRK,NPT0
      INTEGER NELCRK(NCRKPART),IE(NCRKPART)
      INTEGER PID(MVSIZ),MAT(MVSIZ)
C---
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C---
      TYPE(G_BUFEL_) ,POINTER :: XGBUF
      TYPE(L_BUFEL_) ,POINTER :: XLBUF
C
      my_real,
     .   DIMENSION(:), POINTER :: DIR_A
C=======================================================================
!
      NEL_CRK = 0
      ITG = 1 + 4*ECRKXFEC
      TENS = ZERO
c
      DO CRKS = 1,NCRKPART
        ICRK = INDX_CRK(CRKS)
        NELCRK(CRKS) = NEL_CRK
        NEL_CRK = NEL_CRK + CRKSHELL(ICRK)%CRKNUMSHELL
        IE(ICRK) = 0
      ENDDO
C
        DO J=1,18
           R4(J) = ZERO
        ENDDO
C
        NN1 = 1
        NN2 = NN1
        NN3 = NN2
        NN4 = NN3 + NUMELQ
        NN5 = NN4 + NUMELC
        NN6 = NN5 + NUMELTG
C
       
        DO NG=1,NGROUP
          MLW    = IPARG(1,NG)
          NEL    = IPARG(2,NG)
          NFT    = IPARG(3,NG)
          ITY    = IPARG(5,NG)
          NPT    = IABS(IPARG(6,NG))
          ISTRAIN= IPARG(44,NG)
          IHBE   = IPARG(23,NG)
          IGTYP  = IPARG(38,NG)
          IXFEM  = IPARG(54,NG)
          NLEVXF = IPARG(65,NG)
          LFT=1
          LLT=NEL
!
          DO I=1,8  ! length max of GBUF%G_STRA = 8
            JJ(I) = NEL*(I-1)
          ENDDO
!
          DO I=LFT,LLT          
            DO J=1,5            
              SIGE(I,J) = ZERO  
            ENDDO               
          ENDDO
!
          IF (IHBE == 11) CYCLE
          IF (IXFEM /= 1 .AND. IXFEM /= 2) CYCLE
          IF (ITY /= 3   .AND. ITY /= 7) CYCLE
C-----------------------------------------------
C       COQUES 3N, 4N
C-----------------------------------------------
          GBUF => ELBUF_TAB(NG)%GBUF
cc          NLAY = ELBUF_TAB(NG)%NLAY
cc          NPTT = ELBUF_TAB(NG)%NPTT
cc          NXEL = ELBUF_TAB(NG)%NXEL
          IF (ITY == 3) THEN
            N0 = 0
            NNI = NN4
            NI = NFT
          ELSE
            N0 = NUMELC
            NNI = NN5
            NI = NFT + NUMELC
          ENDIF
C
          NPG = 0
          NPT0 = NPT  ! save global NPT
C-----------------------------------------
C-----------------------------------------
          IF (IXFEM == 1) NPT = 1  !  multlayer xfem
C-----------------------------------------
C-----------------------------------------
          A1    = ZERO
          A2    = ZERO
          A3    = ZERO
          ISTRE = 1
C
C------------------------
C        STRESS
C------------------------
          IF (ITENS == 1) THEN
            A1 = ONE
            A2 = ZERO
          ELSEIF (ITENS == 2) THEN
            A1 = ZERO
            A2 = ONE
          ELSEIF (ITENS == 3) THEN
            IF (MLW == 1) THEN
              A1 = ONE
              A2 = SIX
              ELSEIF (MLW == 2  .OR. MLW == 19 .OR.
     .                MLW == 15 .OR.
     .                MLW == 22 .OR. MLW == 25 .OR.
     .                MLW == 27 .OR. MLW == 32 .OR.
     .                MLW >= 28) THEN
              A1 = ONE
              A2 = ZERO
              IPT = NPT
            ELSEIF (MLW == 3 .OR. MLW == 23) THEN
              A1 = ONE
              A2 = ZERO
            ENDIF
          ELSEIF (ITENS == 4) THEN
            IF (MLW == 1) THEN
              A1 = ONE
              A2 = -SIX
              ELSEIF (MLW == 2  .OR. MLW == 19 .OR.
     .                MLW == 15 .OR.
     .                MLW == 22 .OR. MLW == 25.OR.
     .                MLW == 27 .OR. MLW == 32.OR.
     .                MLW >= 28) THEN
              IPT = 1
              A1 = ONE
              A2 = ZERO
            ELSEIF (MLW == 3 .OR. MLW == 23) THEN
              A1 = ONE
              A2 = ZERO
            ENDIF
          ELSEIF (ITENS >= 101 .AND. ITENS <= 200) THEN
            IF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) THEN
              A1 = ONE
              A2 = ZERO
              ELSEIF (MLW == 2  .OR. MLW == 19 .OR.
     .                MLW == 15 .OR.
     .                MLW == 22 .OR. MLW == 25 .OR.
     .                MLW == 27 .OR. MLW == 32 .OR.
     .                MLW >= 28) THEN
              IPT = MIN(NPT,ITENS-100)  
              A1 = ONE
              A2 = ZERO
            ENDIF
C------------------------
C        STRAIN
C------------------------
          ELSEIF (ITENS == 5) THEN
            ISTRE = 0
            A1 = ZERO
            A2 = ZERO
            IF (ISTRAIN == 1) THEN
              A1 = ONE
            ENDIF
          ELSEIF (ITENS == 6) THEN
            ISTRE = 0
            A1 = ZERO
            A2 = ZERO
            IF (ISTRAIN == 1) THEN
              A2 = ONE
            ENDIF
          ELSEIF (ITENS == 7) THEN
            ISTRE = 0
            A1 = ZERO
            A2 = ZERO
            IF (ISTRAIN == 1) THEN
              A1 = ONE
              A2 = HALF
            ENDIF
          ELSEIF (ITENS == 8) THEN
            ISTRE = 0
            A1 = ZERO
            A2 = ZERO
            IF (ISTRAIN == 1) THEN
              A1 = ONE
              A2 = -HALF
            ENDIF
          ELSEIF (ITENS >= 201 .AND. ITENS <= 300) THEN
            ISTRE = 0
            A1 = ZERO
            A2 = ZERO
            IF (ISTRAIN == 1 .AND. NPT /= 0) THEN
              IPT = MIN(NPT,ITENS - 200)  
              A1 = ONE
c              A2 = HALF*(((2*ILAY-ONE)/NPT0)-ONE)
              A2 = HALF*(((2*IPT-ONE)/NPT)-ONE)  
            ENDIF
C------------------------
C        STRAIN RATE
C------------------------
          ELSEIF (ITENS == 91) THEN
            ISTRE = 2
            A1 = ONE
            A2 = ZERO
          ELSEIF (ITENS == 92) THEN
            ISTRE = 2
            A1 = ZERO
            A2 = ONE
          ELSEIF (ITENS == 93) THEN
            ISTRE = 2
            A1 = ONE
            A2 = HALF
          ELSEIF (ITENS == 94) THEN
            ISTRE = 2
            A1 = ONE
            A2 = -HALF
          ELSEIF (ITENS >= 301 .AND. ITENS <= 400) THEN
            IF (NPT /= 0) THEN	
              ISTRE = 2
              IPT = MIN(NPT,ITENS - 300)  
              A1 = ONE
c              A2 = HALF*(((2*ILAY-ONE)/NPT0)-ONE)
              A2 = HALF*(((2*IPT-ONE)/NPT)-ONE)
            ELSE
              ISTRE = 2
              A1 = ONE
              A2 = ZERO
            ENDIF
          ENDIF  !  IF (ITENS == 1)
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
              IF (MLW == 0) THEN
                DO I=LFT,LLT
                  N = I + NI
                  IF (IEL_CRK(N) > 0) THEN
                    IE(ICRK) = IE(ICRK) + 1
                    TENS(1,EL2FA(NELCRK(ICRK) + IE(ICRK))) = ZERO
                    TENS(2,EL2FA(NELCRK(ICRK) + IE(ICRK))) = ZERO
                    TENS(3,EL2FA(NELCRK(ICRK) + IE(ICRK))) = ZERO
                  ENDIF
                ENDDO
                CYCLE
              ENDIF
C---
              IF (ISTRE == 1) THEN
C------------------------
C          STRESS
C------------------------
                IF (ITY == 3) THEN
                  IPID = IXC(6,NFT+1)
                  IGTYP = IGEO(11,IXC(6,NFT+1))
                  DO I=LFT,LLT
                    MAT(I)=IXC(1,NFT+I)
                    PID(I)=IXC(6,NFT+I)
                  ENDDO
                ELSE  ! ITY == 7
                  IPID = IXTG(5,NFT+1)
                  IGTYP = IGEO(11,IXTG(5,NFT+1))
                  DO I=LFT,LLT
                    MAT(I)=IXTG(1,NFT+I)
                    PID(I)=IXTG(5,NFT+I)
                  ENDDO
                ENDIF
c
                IREP = IGEO(6,IPID)
                IVISC = 0
                NUVARV = 0
                IF (MLW == 25) THEN
                  IF (IGTYP == 11) THEN                                
                    IPMAT = 100                                     
                    DO I=1,NEL
                      MATLY = IGEO(IPMAT+ILAY,PID(I))
                      IF (IPM(222,MATLY) > 0 ) IVISC = 1
                    ENDDO
c                  ELSEIF (IGTYP == 9 .OR. IGTYP == 10) THEN
c                  ELSEIF (IGTYP == 17) THEN
                  ENDIF                                             
                ENDIF ! mlw == 25
C----------
              IF (((ITENS >= 101.AND.ITENS <= 200).OR.ITENS==3.OR.ITENS==4)
     .       .AND.(MLW == 25.OR.MLW == 15.OR.(MLW>=28 .AND. 
     .         IGTYP==11)).AND.IREP == 1) THEN
                  CALL SIGROTA_XFE(ELBUF_TAB(NG),XFEM_TAB(NG,IXEL),
     1                    LFT   ,LLT    ,NFT     ,ILAY         ,NEL  ,
     2                    ITY   ,IEL_CRK,IADC_CRK,IADC_CRK(ITG),IXFEM,
     3                    ICRK  ,NLAY   ,SIGE    ,IVISC        ,CRKEDGE )
                  DO I=LFT,LLT
                    N = I + NI
                    IF (IEL_CRK(N) > 0) THEN
                      IE(ICRK) = IE(ICRK) + 1
                      DO J = 1,3
                        R4(J) = SIGE(I,J)
                      ENDDO
cc                   R4(3) = R4(3) * INVERT(EL2FA(NEL_CRK + IE(ICRK)))
                     TENS(1,EL2FA(NELCRK(ICRK) + IE(ICRK))) = R4(1)
                     TENS(2,EL2FA(NELCRK(ICRK) + IE(ICRK))) = R4(2)
                     TENS(3,EL2FA(NELCRK(ICRK) + IE(ICRK))) = R4(3)
                    ENDIF
                  ENDDO
              ELSEIF (((ITENS >= 101.AND.ITENS <= 200).OR.ITENS==3.OR.
     .        ITENS==4).AND.(MLW == 25.OR.MLW == 15.OR.(MLW>=28 .AND. 
     .       IGTYP==11)).AND.IREP == 0) THEN
C stesses
                  IF (NLAY > 1) THEN                           
                    LBUF  => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)
                    XLBUF => XFEM_TAB(NG,IXEL)%BUFLY(ILAY)%LBUF(1,1,1)
                  ELSE                                         
                    LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,ILAY)
                    XLBUF => XFEM_TAB(NG,IXEL)%BUFLY(1)%LBUF(1,1,ILAY)
                  ENDIF
C
                  DO I=LFT,LLT
                    N = I + NI
                    ELCRK = IEL_CRK(N)
                    IF (ELCRK > 0) THEN
                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                      IF (ILAYCRK == 0 .OR.ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                        IF (NLAY > 1) THEN
                          DO J=1,5
                            SIGE(I,J) = GBUF%FOR(JJ(J)+I) ! global from standard elem
                          ENDDO
                        ELSEIF (NLAY == 1) THEN
                          DO J=1,5
                            SIGE(I,J) = GBUF%FOR(JJ(J)+I) ! global from standard elem
                          ENDDO
                        ENDIF
                      ELSE  !  cracked layer
                        IF (NLAY > 1) THEN
                          DO J=1,5
                            SIGE(I,J) = XLBUF%FOR(JJ(J)+I)
                          ENDDO
                        ELSEIF (NLAY == 1) THEN
                          DO J=1,5
                            SIGE(I,J) = XGBUF%FOR(JJ(J)+I)
                          ENDDO
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDDO
C   visc
                  IF (IVISC > 0) THEN
                    DO I=LFT,LLT                                  
                      N = I + NI
                      ELCRK = IEL_CRK(N)
                      IF (ELCRK > 0) THEN
                        ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                        IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                          DO J=1,5
                            SIGE(I,J) = SIGE(I,J) +  LBUF%VISC(JJ(J)+I)
                          ENDDO
                        ELSE  !  cracked layer
                          DO J=1,5
                            SIGE(I,J) = SIGE(I,J) +  XLBUF%VISC(JJ(J)+I)
                          ENDDO
                        ENDIF
                      ENDIF
                    ENDDO
                  ENDIF  !  IF (IVISC > 0)
C directions  - a revoir - if uncracked
c                  DO I=LFT,LLT
c                    I1 = (I-1) * NS1
c                    N = I + NI
c                    ELCRK = IEL_CRK(N)
c                    IF (ELCRK > 0) THEN
c                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
c                      IF (ILAYCRK == 0 .OR.ABS(ILAYCRK) == 1) THEN  !  uncracked layer
c                        IF (NLAY > 1) THEN                              
c                          DIR_A => ELBUF_TAB(NG)%BUFLY(ILAY)%DIRA
c                        ELSE                                            
c                          DIR_A => ELBUF_TAB(NG)%BUFLY(1)%DIRA
c                        ENDIF
c                      ELSE  !  cracked layer
c                        IF (NLAY > 1) THEN                              
c                          DIR_A => XFEM_TAB(NG,IXEL)%BUFLY(ILAY)%DIRA
c                        ELSE                                            
c                          DIR_A => XFEM_TAB(NG,IXEL)%BUFLY(1)%DIRA
c                        ENDIF
c                      ENDIF
c                      CALL UROTO(LFT,LLT,SIGE,DIR_A)
c                    ENDIF
c                  ENDDO
                  IF (NLAY > 1) THEN                              
                    DIR_A => XFEM_TAB(NG,IXEL)%BUFLY(ILAY)%DIRA
                  ELSE                                            
                    DIR_A => XFEM_TAB(NG,IXEL)%BUFLY(1)%DIRA
                  ENDIF                                           
                  CALL UROTOV(LFT,LLT,SIGE,DIR_A,NEL)
!! temporary replaced by (the same) UROTOV() in order to do not affect
!! the other multidimensional buffer ARRAYS which are still not modified
!!                  CALL UROTO(LFT,LLT,SIGE,DIR_A)
C
                  DO I=LFT,LLT
                    N = I + NI
                    IF (IEL_CRK(N) > 0) THEN
                      IE(ICRK) = IE(ICRK) + 1
                      DO J = 1,3
                        R4(J) = SIGE(I,J)
                      ENDDO
                      TENS(1,EL2FA(NELCRK(ICRK) + IE(ICRK))) = R4(1)
                      TENS(2,EL2FA(NELCRK(ICRK) + IE(ICRK))) = R4(2)
                      TENS(3,EL2FA(NELCRK(ICRK) + IE(ICRK))) = R4(3)
                    ENDIF
                  ENDDO                
C----------
                ELSE  ! ITENS
                  IF (NLAY > 1) THEN                           
                    XLBUF => XFEM_TAB(NG,IXEL)%BUFLY(ILAY)%LBUF(1,1,1)
                  ELSE                                         
                    XLBUF => XFEM_TAB(NG,IXEL)%BUFLY(1)%LBUF(1,1,ILAY)
                  ENDIF
                  DO I=LFT,LLT                                        
                    N = I + NI
                    ELCRK = IEL_CRK(N)
                    IE(ICRK) = IE(ICRK) + 1
                    IF (ELCRK > 0) THEN
                      ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)           
                      IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                        DO J=1,3
                          R4(J) = A1 * GBUF%FOR(JJ(J)+I) + A2 * GBUF%MOM(JJ(J)+I) ! global from standard elem
                        ENDDO
                      ELSE  !  cracked layer
                        IF (NLAY > 1) THEN
                          DO J=1,3
                            R4(J) = A1 * XLBUF%FOR(JJ(J)+I) + A2 * XLBUF%MOM(JJ(J)+I)
                          ENDDO
                        ELSEIF (NLAY == 1) THEN
                          DO J=1,3
                            R4(J) = A1 * XGBUF%FOR(JJ(J)+I) + A2 * XGBUF%MOM(JJ(J)+I)
                          ENDDO
                        ENDIF
                      ENDIF
                    ENDIF
                    TENS(1,EL2FA(NELCRK(ICRK) + IE(ICRK))) = R4(1)             
                    TENS(2,EL2FA(NELCRK(ICRK) + IE(ICRK))) = R4(2)             
                    TENS(3,EL2FA(NELCRK(ICRK) + IE(ICRK))) = R4(3)             
                  ENDDO                                               
                ENDIF  !  IF (((ITENS >= 101 ...)
              ELSEIF (ISTRE == 0 .AND. GBUF%G_STRA > 0) THEN
C------------------------
C          STRAIN
C------------------------
                IF (NLAY > 1) THEN                           
                  XLBUF => XFEM_TAB(NG,IXEL)%BUFLY(ILAY)%LBUF(1,1,1)
                ELSE                                         
                  XLBUF => XFEM_TAB(NG,IXEL)%BUFLY(1)%LBUF(1,1,ILAY)
                ENDIF
                DO I=LFT,LLT
                  N = I + NI
                  ELCRK = IEL_CRK(N)
                  IF (ELCRK > 0) THEN
                    ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
                    IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer
                      THK = GBUF%THK(I)
                      IF (ITENS /= 6) THEN
                        DO J=1,3
                          R4(J) = A1 * GBUF%STRA(JJ(J)+I) + 
     .                            A2 * GBUF%STRA(JJ(J)+I) * THK
                        ENDDO
                      ELSE
                        DO J=1,3
                          R4(J) = GBUF%STRA(JJ(J)+I)
                        ENDDO
                      ENDIF
                    ELSE  !  cracked layer
                      IF (ITENS /= 6) THEN
                        IF (NLAY > 1) THEN
                          THK = XLBUF%THK(I)
                          DO J=1,3
                            R4(J) = A1 * XLBUF%STRA(JJ(J)+I) + 
     .                              A2 * XLBUF%STRA(JJ(J)+I) * THK
                          ENDDO
                        ELSEIF (NLAY == 1) THEN
                          THK = XGBUF%THK(I)
                          DO J=1,3
                            R4(J) = A1 * XGBUF%STRA(JJ(J)+I) + 
     .                              A2 * XGBUF%STRA(JJ(J)+I) * THK
                          ENDDO
                        ENDIF
                      ELSE
                        IF (NLAY > 1) THEN
                          DO J=1,3
                            R4(J) = XLBUF%STRA(JJ(J)+I)
                          ENDDO
                        ELSEIF (NLAY == 1) THEN
                          DO J=1,3
                            R4(J) = XGBUF%STRA(JJ(J)+I)
                          ENDDO
                        ENDIF
                      ENDIF
                    ENDIF
C
                    IE(ICRK) = IE(ICRK) + 1
cc                   R4(3) = R4(3) * INVERT(EL2FA(NELCRK(ICRK) + IE(ICRK))) * HALF
                    R4(3) = R4(3) * HALF
                    TENS(1,EL2FA(NELCRK(ICRK) + IE(ICRK))) = R4(1)
                    TENS(2,EL2FA(NELCRK(ICRK) + IE(ICRK))) = R4(2)
                    TENS(3,EL2FA(NELCRK(ICRK) + IE(ICRK))) = R4(3)
                  ENDIF  !  IF (ELCRK > 0)
                ENDDO  !  DO I=LFT,LLT
              ELSEIF (ISTRE == 2) THEN
C------------------------
C          STRAIN RATE
C------------------------
                DO I=LFT,LLT
                  N = I + NI
                  IF (IEL_CRK(N) > 0) THEN
                    THK = GBUF%THK(I)  
                    IF (ITENS /= 92) THEN
                      DO J=1,3
                        R4(J) = A1*EPSDOT(J,N+N0) + A2*EPSDOT(J+3,N+N0)*THK
                      ENDDO
                    ELSE
                      DO J = 1,3
                        R4(J) = EPSDOT(J+3,N+N0)
                      ENDDO
                    ENDIF
cc               R4(3) = R4(3) * INVERT(EL2FA(NEL_CRK + IE)) * HALF
                    R4(3) = R4(3) * HALF
                    IE(ICRK) = IE(ICRK) + 1
                    TENS(1,EL2FA(NELCRK(ICRK) + IE(ICRK))) = R4(1)
                    TENS(2,EL2FA(NELCRK(ICRK) + IE(ICRK))) = R4(2)
                    TENS(3,EL2FA(NELCRK(ICRK) + IE(ICRK))) = R4(3)
                  ENDIF
                ENDDO
              ENDIF  ! IF (ISTRE == 1)
C-----------------------------------------------
            ENDDO  !  DO ILAY=1,NLAY
          ENDDO  !  DO IXEL=1,NXEL
        ENDDO  !  DO NG=1,NGROUP
C----------------------------------------------
      DO CRKS = 1,NCRKPART
        ICRK = INDX_CRK(CRKS)
C
        NEL_CRK = NELCRK(ICRK)
C
        IF (NSPMD == 1)THEN
          DO I=1,IE(ICRK)
            N = EL2FA(NEL_CRK + I)
            R4(1) = TENS(1,N)
            R4(2) = TENS(2,N)
            R4(3) = TENS(3,N)
            CALL WRITE_R_C(R4,3)
          ENDDO
        ELSE
          DO I=1,IE(ICRK)
            N = EL2FA(NEL_CRK + I)
            WA(1,I+NEL_CRK) = TENS(1,N)
            WA(2,I+NEL_CRK) = TENS(2,N)
            WA(3,I+NEL_CRK) = TENS(3,N)
          ENDDO
        ENDIF
      ENDDO
C
      IF (NSPMD > 1) THEN
        IF (ISPMD == 0) THEN
          BUF = NBF*3
        ELSE
          BUF = 1
        ENDIF
        CALL SPMD_R4GET_PARTN(3,3*NBF_L,NCRKPART,IADG,WA,BUF)
      ENDIF
C---
      RETURN
      END
