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
Chd|  TENSORC                       source/output/anim/generate/tensorc.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SIGROTA                       source/output/anim/generate/sigrota.F
Chd|        SPMD_R4GET_PARTN              source/mpi/anim/spmd_r4get_partn.F
Chd|        UROTOV                        source/airbag/uroto.F         
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE TENSORC(ELBUF_TAB,IPARG ,ITENS ,INVERT,NELCUT,
     2                   EL2FA    ,NBF   ,TENS  ,EPSDOT,IADP  ,
     3                   NBF_L    ,NBPART,IADG  ,X     ,IXC   ,
     4                   IGEO     ,IXTG  ,IPM   ,STACK ,MATPARAM_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE MATPARAM_DEF_MOD
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
#include      "param_c.inc"
#include      "scr25_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),ITENS,INVERT(*),
     .   EL2FA(*),IXC(NIXC,*), IGEO(NPROPGI,*), 
     .   NELCUT,NBF,IADP(*),NBF_L,NBPART,IADG(NSPMD,*),
     .   IXTG(NIXTG,*),IPM(NPROPMI,*)
      REAL WA(3*NBF_L)
C     REAL
      my_real
     .   TENS(3,*),EPSDOT(6,*),X(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MATPARAM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   A1,A2,A3,THK
      my_real
     .   SIGE(MVSIZ,5)
      REAL R4(18)
      INTEGER I,NG,NEL,NFT,ITY,LFT,NPT,MPT,IPT,
     .        N,J,LLT,MLW,ISTRAIN,IL,IR,IS,IT,NPTR,NPTS,NLAY,
     .        IPID,I1,I2,NS1,NS2,ISTRE,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,NNI,N0,
     .        IHBE,IREP,BUF,NPG,K,ISROT,NUVARV,IVISC,
     .        IPMAT,IGTYP,MATLY,ISUBSTACK,IIGEO,IADI,IPMAT_IPLY,
     .        NPT_ALL,NPTT,ILAY,IUS,ID_PLY,IPLY,IPANG,IPPOS,IPTHK,JJ(8),
     .        IDX,IDX_MSTRESS,IDX_IDPLY_MSTRESS,IGMAT,IDRAPE,IDIR,IMAT,MAT_ORTH
      INTEGER PID(MVSIZ),MAT(MVSIZ)
C
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF 
      TYPE(L_BUFEL_DIR_) ,POINTER :: LBUF_DIR
C
      my_real,
     .   DIMENSION(:), POINTER :: DIR_A,DIR_B
C-----------------------------------------------
!
      DO J=1,18
        R4(J) = ZERO
      ENDDO
C
      NPG = 1
      NN1 = 1
      NN2 = NN1 
      NN3 = NN2 
      NN4 = NN3 + NUMELQ
      NN5 = NN4 + NUMELC
      NN6 = NN5 + NUMELTG
      NN7 = NN6 
      NN8 = NN7 
      NN9 = NN8 
      NN10= NN9 
C
C
      DO 490 NG=1,NGROUP
C       IF(ANIM_K == 0.AND.IPARG(8,NG) == 1)GOTO 490
        MLW     = IPARG(1,NG)
        NEL     = IPARG(2,NG)
        NFT     = IPARG(3,NG)
        ITY     = IPARG(5,NG)
        IGTYP   = IPARG(38,NG)
        ISROT   = IPARG(41,NG)
        ISTRAIN = IPARG(44,NG)
        ISUBSTACK = IPARG(71,NG)
        IGMAT = IPARG(75,NG)
        IDRAPE = ELBUF_TAB(NG)%IDRAPE
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
        IF (MLW /= 13) THEN
C-----------------------------------------------
C         QUAD
C-----------------------------------------------
          IF(ITY == 2)THEN
            DO I=LFT,LLT
              N = I + NFT
              TENS(1,EL2FA(NN3+N)) = ZERO
              TENS(2,EL2FA(NN3+N)) = ZERO
              TENS(3,EL2FA(NN3+N)) = ZERO 
            ENDDO
C-----------------------------------------------
C         COQUES
C-----------------------------------------------
          ELSEIF (ITY == 3 .OR. ITY == 7) THEN
            GBUF => ELBUF_TAB(NG)%GBUF
            NPTR = ELBUF_TAB(NG)%NPTR
            NPTS = ELBUF_TAB(NG)%NPTS
            NPTT = ELBUF_TAB(NG)%NPTT
            NLAY = ELBUF_TAB(NG)%NLAY
            NPG  = NPTR*NPTS
C
            IHBE = IPARG(23,NG)
            IF (ITY == 3) THEN
              N0 = 0
              NNI = NN4
              IF (IHBE == 11) NPG = 4
            ELSE
              N0 = NUMELC
              NNI = NN5
              IF (IHBE == 11) NPG = 3
            ENDIF
c
            DO I=LFT,LLT                   
              N = I + NFT                  
              TENS(1,EL2FA(NNI+N)) = ZERO  
              TENS(2,EL2FA(NNI+N)) = ZERO  
              TENS(3,EL2FA(NNI+N)) = ZERO  
            ENDDO                          
C
            IF (MLW == 0) GOTO 490
C
            A1    = ZERO
            A2    = ZERO
            A3    = ZERO         
            ISTRE = 1
            IPT   = 1
            NPT   = IABS(IPARG(6,NG))
            MPT   = MAX(1,NPT)
C
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPT_ALL = 0
              DO IPT=1,NLAY
                NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IPT)%NPTT
              ENDDO
              IF (NLAY == 1) MPT  = MAX(1,NPT_ALL)
            ENDIF
            IDX_MSTRESS = 3120 + 3*MX_PLY_ANIM
            IDX_IDPLY_MSTRESS = IDX_MSTRESS + 103
C------------------------
C           STRESS 1:mem, 2:bend, 3:upper, 4: lower
C------------------------
            IF (ITENS == 1) THEN
              NS1 = 5
              NS2 = 3
              A1  = ONE
              A2  = ZERO
            ELSEIF (ITENS == 2) THEN
              NS1 = 5
              NS2 = 3
              A1  = ZERO
              A2  = ONE
            ELSEIF (ITENS == 3) THEN
              NS1 = 5
              NS2 = 3
              IPT = MPT
              IL  = NLAY
              IF (MLW == 1) THEN
                A1 = ONE
c                A2 = 0
                A2 = SIX
              ELSEIF (MLW == 2  .OR. MLW == 19 .OR.
     .                MLW == 15 .OR.
     .                MLW == 22 .OR. MLW == 25 .OR.
     .                MLW == 27 .OR. MLW == 32 .OR.
     .                MLW >= 28) THEN 
                A1  = ONE
                A2  = ZERO
              ELSEIF (MLW == 3 .OR. MLW == 23) THEN
                A1 = ONE
                A2 = ZERO
              ENDIF
            ELSEIF (ITENS == 4) THEN
              NS1 = 5
              NS2 = 3
              IPT = 1
              IL  = 1
              IF (MLW == 1) THEN
                A1 = ONE
                A2 = -SIX
              ELSEIF (MLW == 2  .OR. MLW == 19 .OR.
     .                MLW == 15 .OR.
     .                MLW == 22 .OR. MLW == 25.OR.
     .                MLW == 27 .OR. MLW == 32.OR.
     .                MLW >= 28) THEN 
                A1  = ONE
                A2  = ZERO
              ELSEIF (MLW == 3 .OR. MLW == 23) THEN
                A1 = ONE
                A2 = ZERO
              ENDIF
            ELSEIF (ITENS > 100 .AND. ITENS <  201) THEN
              NS1 = 5
              NS2 = 3
              IPT = MIN(MPT,ITENS-100)
              IF (ITENS - 100 > MPT) THEN
                A1 = ZERO
                A2 = ZERO
              ELSEIF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) THEN
                A1 = ONE
                A2 = ZERO
              ELSEIF (MLW == 2  .OR. MLW == 19 .OR.
     .                MLW == 15 .OR.
     .                MLW == 22 .OR. MLW == 25 .OR.
     .                MLW == 27 .OR. MLW == 32 .OR.
     .                MLW >= 28) THEN 
                A1  = ONE
                A2  = ZERO
              ENDIF
            ELSEIF (ITENS > 400 .AND. ITENS <  501) THEN
C             upper stress within each layer (PID51)
              NS1 = 5
              NS2 = 3
cc              IPT = MIN(MPT,ITENS-100)
C-----
              ILAY = MOD ((ITENS - 400), 100)
              IF (ILAY == 0) ILAY = 100
C-----
              IF (ILAY > MPT) THEN
                A1 = ZERO
                A2 = ZERO
              ELSEIF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) THEN
                A1 = ONE
                A2 = ZERO
              ELSEIF (MLW == 2  .OR. MLW == 19 .OR.
     .                MLW == 15 .OR.
     .                MLW == 22 .OR. MLW == 25 .OR.
     .                MLW == 27 .OR. MLW == 32 .OR.
     .                MLW >= 28) THEN 
                A1  = ONE
                A2  = ZERO
              ENDIF
            ELSEIF (ITENS > 500 .AND. ITENS <  601) THEN
C             lower stress within each layer (PID51)
              NS1 = 5
              NS2 = 3
cc              IPT = MIN(MPT,ITENS-100)
C-----
              ILAY = MOD ((ITENS - 500), 100)
              IF (ILAY == 0) ILAY = 100
C-----
              IF (ILAY > MPT) THEN
                A1 = ZERO
                A2 = ZERO
              ELSEIF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) THEN
                A1 = ONE
                A2 = ZERO
              ELSEIF (MLW == 2  .OR. MLW == 19 .OR.
     .                MLW == 15 .OR.
     .                MLW == 22 .OR. MLW == 25 .OR.
     .                MLW == 27 .OR. MLW == 32 .OR.
     .                MLW >= 28) THEN 
                A1  = ONE
                A2  = ZERO
              ENDIF
            ELSEIF (ITENS > 600 .AND. ITENS <  1611) THEN
C             all stresses within each layer for all NPTT (PID51)
              NS1 = 5
              NS2 = 3
cc              IPT = MIN(MPT,ITENS-100)
C-----
              IUS = ITENS - 600
              ILAY  = INT((IUS - 1)/10)
              IF (ILAY == 0) ILAY = 100
C-----
              IF (ILAY > MPT) THEN
                A1 = ZERO
                A2 = ZERO
              ELSEIF (MLW == 1 .OR. MLW == 3 .OR. MLW == 23) THEN
                A1 = ONE
                A2 = ZERO
              ELSEIF (MLW == 2  .OR. MLW == 19 .OR.
     .                MLW == 15 .OR.
     .                MLW == 22 .OR. MLW == 25 .OR.
     .                MLW == 27 .OR. MLW == 32 .OR.
     .                MLW >= 28) THEN 
                A1  = ONE
                A2  = ZERO
              ENDIF
C------------------------
C        STRAIN
C------------------------
            ELSEIF (ITENS == 5) THEN  ! membrane
              ISTRE = 0
              NS1 = 8
              NS2 = 8
              IF (ISTRAIN == 1) THEN
                A1 = ONE
                A2 = ZERO
              ELSE
                A1 = ZERO
                A2 = ZERO
              ENDIF
            ELSEIF (ITENS == 6) THEN  ! bend
              ISTRE = 0
              NS1 = 8
              NS2 = 8
              A1 = ZERO
              A2 = ONE
            ELSEIF (ITENS == 7) THEN   ! upper
              ISTRE = 0
              NS1 = 8
              NS2 = 8
              IPT = MPT   
              A1 = ONE
              A2 = HALF
            ELSEIF (ITENS == 8) THEN   ! lower
              ISTRE = 0
              NS1 = 8
              NS2 = 8
              IPT = 1  
              A1 = ONE
              A2  = -HALF
            ELSEIF (ITENS >  200 .AND. ITENS < 301) THEN   ! layer
              ISTRE = 0
              NS1 = 8
              NS2 = 8
              IPT = MIN(MPT,ITENS-200)
              IF (ITENS - 200 > MPT) THEN
                A1 = ZERO
                A2 = ZERO
              ELSE
                A1 = ONE
                A2 = HALF*(((2*IPT-ONE)/MPT)-ONE)
              ENDIF
            ELSEIF (ITENS > 1610+ MX_PLY_ANIM  .AND. ITENS <  1611 + 2*MX_PLY_ANIM ) THEN
              IL = ITENS - (1610+ MX_PLY_ANIM)   
              ISTRE = 0
              A1 = ZERO
              A2 = ZERO
              NS1 = 8
              NS2 = 8
              IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
                IF (PLY_ANIM_STRAIN( 3 * (IL - 1) + 2) == 3 )THEN
                  IPANG = 1
                  IPTHK  = IPANG + NLAY
                  IPPOS  = IPTHK + NLAY
                  IPT = PLY_ANIM_STRAIN( 3 * (IL - 1) + 3)
                  DO J=1,NLAY
                    BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    NPTT = BUFLY%NPTT
                    IF (IGTYP == 17 .OR. IGTYP == 51) THEN
		                    ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                    ELSEIF (IGTYP == 52) THEN
                      ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                    ENDIF
                    IF (ID_PLY  == PLY_ANIM_STRAIN( 3 * (IL - 1) + 1) .AND.
     .                  IPT <= NPTT ) THEN
                      A1 = ONE
c                      A2 = STACK%GEO(IPPOS+J,ISUBSTACK)
                      A2 = STACK%GEO(IPPOS+J,ISUBSTACK)+ 
     .                           HALF*(((2*IPT-ONE)/NPTT)-ONE) *
     .                              STACK%GEO(IPTHK+J,ISUBSTACK)
                    ENDIF
                  ENDDO
                ENDIF
              ELSE  
                ISTRE = 0
                A1 = ZERO
                A2 = ZERO
              ENDIF
!
            ELSEIF (ITENS > 1610 + 3*MX_PLY_ANIM  .AND. 
     .              ITENS < 1711 + 3*MX_PLY_ANIM) THEN
!-------------------
              ! STRAI/ILAY/UPPER -> UPPER strain within each layer (PID51,52)
!-------------------
              ISTRE = 0
              A1 = ZERO
              A2 = ZERO
              NS1 = 8
              NS2 = 8
              IF (IGTYP == 51 .OR. IGTYP == 52) THEN
!
                IDX = 1610 + 3*MX_PLY_ANIM
!
                ILAY = MOD((ITENS - IDX),100)
                IF (ILAY == 0) ILAY = 100
                IF (NLAY > 1) THEN
                  IL = MAX(1,ILAY)
                ELSE
                 IL = 1
                ENDIF
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                NPTT = BUFLY%NPTT
                IT  = MAX(1,NPTT)
!
                IPANG = 1
                IPTHK  = IPANG + NLAY
                IPPOS  = IPTHK + NLAY
!
                IF (IL <= NLAY) THEN
                  A1 = ONE
                  A2 = STACK%GEO(IPPOS+IL,ISUBSTACK)+ 
     .                       HALF*(((2*IT-ONE)/NPTT)-ONE) *
     .                       STACK%GEO(IPTHK+IL,ISUBSTACK)
                ENDIF
              ENDIF ! IF (IGTYP == 51 .OR. IGTYP == 52)
!
            ELSEIF (ITENS > 1710 + 3*MX_PLY_ANIM  .AND. 
     .              ITENS < 1811 + 3*MX_PLY_ANIM) THEN
!-------------------
              ! STRAI/ILAY/LOWER -> LOWER strain within each layer (PID51,52)
!-------------------
              ISTRE = 0
              A1 = ZERO
              A2 = ZERO
              NS1 = 8
              NS2 = 8
              IF (IGTYP == 51 .OR. IGTYP == 52) THEN
!
                IDX = 1710 + 3*MX_PLY_ANIM
!
                ILAY = MOD((ITENS - IDX),100)
                IF (ILAY == 0) ILAY = 100
                IF (NLAY > 1) THEN
                  IL = MAX(1,ILAY)
                ELSE
                  IL = 1
                ENDIF
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                NPTT = BUFLY%NPTT
                IT  = 1
!
                IPANG = 1
                IPTHK  = IPANG + NLAY
                IPPOS  = IPTHK + NLAY
!
                IF (IL <= NLAY) THEN
                  A1 = ONE
                  A2 = STACK%GEO(IPPOS+IL,ISUBSTACK)+ 
     .                       HALF*(((2*IT-ONE)/NPTT)-ONE) *
     .                       STACK%GEO(IPTHK+IL,ISUBSTACK)
                ENDIF
              ENDIF ! IF (IGTYP == 51 .OR. IGTYP == 52)
!
            ELSEIF (ITENS > 1810 + 3*MX_PLY_ANIM  .AND. 
     .              ITENS < 2821 + 3*MX_PLY_ANIM) THEN
!-------------------
              ! STRAI/ILAY/IT -> all strain within each layer (PID51,52)
!-------------------
              ISTRE = 0
              A1 = ZERO
              A2 = ZERO
              NS1 = 8
              NS2 = 8
              IF (IGTYP == 51 .OR. IGTYP == 52) THEN
!
                IDX = 1810 + 3*MX_PLY_ANIM
!
                IUS = ITENS - IDX
                ILAY  = INT((IUS - 1)/10)
                IF (ILAY == 0) ILAY = 100
                IL = ILAY
                IT = IUS - 10*IL
!
                IPANG = 1
                IPTHK  = IPANG + NLAY
                IPPOS  = IPTHK + NLAY
!
                IF (IL <= NLAY) THEN
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  NPTT = BUFLY%NPTT
                  IF (IT <= NPTT) THEN
                    A1 = ONE
                    A2 = STACK%GEO(IPPOS+IL,ISUBSTACK)+ 
     .                         HALF*(((2*IT-ONE)/NPTT)-ONE) *
     .                         STACK%GEO(IPTHK+IL,ISUBSTACK)
                  ENDIF
                ENDIF ! IF (IL <= NLAY)
              ENDIF ! IF (IGTYP == 51 .OR. IGTYP == 52)
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
            ELSEIF (ITENS > 300 .AND. ITENS < 401) THEN
              ISTRE = 2
              IPT = MIN(MPT,ITENS - 300)
              IF (ITENS - 300 > MPT) THEN
                A1 = ZERO
                A2 = ZERO            
              ELSEIF (NPT /= 0) THEN	
                A1 = ONE
                A2 = HALF*(((2*IPT-ONE)/MPT)-ONE)
              ELSE
                A1 = ONE
                A2 = ZERO
              ENDIF
            ELSEIF (ITENS > 1610+ 2*MX_PLY_ANIM  .AND. ITENS <  1611 + 3*MX_PLY_ANIM ) THEN
              IL = ITENS - (1610+ 2*MX_PLY_ANIM)   
              ISTRE = 2
              A1 = ZERO
              A2 = ZERO
              IF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
                IF (PLY_ANIM_EPSDOT( 3 * (IL - 1) + 2) == 6 )THEN
                  IPANG = 1
                  IPTHK  = IPANG + NLAY
                  IPPOS  = IPTHK + NLAY
                  IPT = PLY_ANIM_EPSDOT( 3 * (IL - 1) + 3)
                  DO J=1,NLAY
                    BUFLY => ELBUF_TAB(NG)%BUFLY(J)
                    NPTT = BUFLY%NPTT
                    IF (IGTYP == 17 .OR. IGTYP == 51) THEN
		                    ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                    ELSEIF (IGTYP == 52) THEN
                      ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                    ENDIF
                    IF (ID_PLY  == PLY_ANIM_EPSDOT( 3 * (IL - 1) + 1) ) THEN
                      A1 = ONE
c                      A2 = STACK%GEO(IPPOS+J,ISUBSTACK)
                      A2 = STACK%GEO(IPPOS+J,ISUBSTACK)+ 
     .                           HALF*(((2*IPT-ONE)/NPTT)-ONE) *
     .                              STACK%GEO(IPTHK+J,ISUBSTACK)
                    ENDIF
                  ENDDO
                ENDIF
              ELSE  
                ISTRE = 2
                A1 = ZERO
                A2 = ZERO
              ENDIF
!
            ELSEIF (ITENS > 2820 + 3*MX_PLY_ANIM  .AND. 
     .              ITENS < 2921 + 3*MX_PLY_ANIM) THEN
!-------------------
              ! EPSDOT/ILAY/UPPER -> UPPER epsdot within each layer (PID51,52)
!-------------------
              ISTRE = 2
              A1 = ZERO
              A2 = ZERO
              IF (IGTYP == 51 .OR. IGTYP == 52) THEN
!
                IDX = 2820 + 3*MX_PLY_ANIM
!
                ILAY = MOD((ITENS - IDX),100)
                IF (ILAY == 0) ILAY = 100
                IF (NLAY > 1) THEN
                  IL = MAX(1,ILAY)
                ELSE
                  IL = 1
                ENDIF
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                NPTT = BUFLY%NPTT
                IT  = MAX(1,NPTT)
!
                IPANG = 1
                IPTHK  = IPANG + NLAY
                IPPOS  = IPTHK + NLAY
!
                IF (IL <= NLAY) THEN
                  A1 = ONE
                  A2 = STACK%GEO(IPPOS+IL,ISUBSTACK)+ 
     .                       HALF*(((2*IT-ONE)/NPTT)-ONE) *
     .                       STACK%GEO(IPTHK+IL,ISUBSTACK)
                ENDIF
              ENDIF ! IF (IGTYP == 51 .OR. IGTYP == 52)
!
            ELSEIF (ITENS > 2920 + 3*MX_PLY_ANIM  .AND. 
     .              ITENS < 3021 + 3*MX_PLY_ANIM) THEN
!-------------------
              ! EPSDOT/ILAY/LOWER -> LOWER epsdot within each layer (PID51,52)
!-------------------
              ISTRE = 2
              A1 = ZERO
              A2 = ZERO
              IF (IGTYP == 51 .OR. IGTYP == 52) THEN
!
                IDX = 2920 + 3*MX_PLY_ANIM
!
                ILAY = MOD((ITENS - IDX),100)
                IF (ILAY == 0) ILAY = 100
                IF (NLAY > 1) THEN
                  IL = MAX(1,ILAY)
                ELSE
                  IL = 1
                ENDIF
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                NPTT = BUFLY%NPTT
                IT  = 1
!
                IPANG = 1
                IPTHK  = IPANG + NLAY
                IPPOS  = IPTHK + NLAY
!
                IF (IL <= NLAY) THEN
                  A1 = ONE
                  A2 = STACK%GEO(IPPOS+IL,ISUBSTACK)+ 
     .                       HALF*(((2*IT-ONE)/NPTT)-ONE) *
     .                       STACK%GEO(IPTHK+IL,ISUBSTACK)
                ENDIF
              ENDIF ! IF (IGTYP == 51 .OR. IGTYP == 52)
!
            ELSEIF (ITENS > 3020 + 3*MX_PLY_ANIM  .AND. 
     .              ITENS < 4031 + 3*MX_PLY_ANIM) THEN
!-------------------
              ! EPSDOT/ILAY/IT -> all epsdot within each layer (PID51,52)
!-------------------
              ISTRE = 2
              A1 = ZERO
              A2 = ZERO
              IF (IGTYP == 51 .OR. IGTYP == 52) THEN
!
                IDX = 3020 + 3*MX_PLY_ANIM
!
                IUS = ITENS - IDX
                ILAY  = INT((IUS - 1)/10)
                IF (ILAY == 0) ILAY = 100
                IL = ILAY
                IT = IUS - 10*IL
!
                IPANG = 1
                IPTHK  = IPANG + NLAY
                IPPOS  = IPTHK + NLAY
!
                IF (IL <= NLAY) THEN
                  BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                  NPTT = BUFLY%NPTT
                  IF (IT <= NPTT) THEN
                    A1 = ONE
                    A2 = STACK%GEO(IPPOS+IL,ISUBSTACK)+ 
     .                         HALF*(((2*IT-ONE)/NPTT)-ONE) *
     .                         STACK%GEO(IPTHK+IL,ISUBSTACK)
                  ENDIF
                ENDIF ! IF (IL <= NLAY)
              ENDIF ! IF (IGTYP == 51 .OR. IGTYP == 52)
!
            !ELSEIF (ITENS > 4030 + 3*MX_PLY_ANIM) THEN --> see /EPSDOT/ALL/ALL 
            !next available animation file
            ENDIF  !  IF (ITENS == 1)
c-----------------------------------------------------------
            IF (ISTRE == 1) THEN
C------------------------
C             STRESS
C------------------------
              IF (ITY == 3) THEN
                IPID = IXC(6,NFT+1)
                DO I=LFT,LLT
                  MAT(I)=IXC(1,NFT+I)
                  PID(I)=IXC(6,NFT+I)
                ENDDO
              ELSE  ! ITY == 7
                IPID = IXTG(5,NFT+1)
                DO I=LFT,LLT
                  MAT(I)=IXTG(1,NFT+I)
                  PID(I)=IXTG(5,NFT+I)
                ENDDO
              ENDIF
c
              IREP = IGEO(6,IPID)
              IVISC = 0

C----------        
              IF (ITENS == 1) THEN    
                ! /TENS/STRESS/MEMB
                DO I=LFT,LLT
                  N = I + NFT
                  R4(1) = GBUF%FOR(JJ(1)+I)
                  R4(2) = GBUF%FOR(JJ(2)+I)
                  R4(3) = GBUF%FOR(JJ(3)+I)
                  R4(3) = R4(3) * INVERT(EL2FA(NNI+N))
                  TENS(1,EL2FA(NNI+N)) = R4(1)
                  TENS(2,EL2FA(NNI+N)) = R4(2)
                  TENS(3,EL2FA(NNI+N)) = R4(3)
                ENDDO
c
              ELSE IF (ITENS == 2) THEN    
                ! /TENS/STRESS/BEND
c
                DO I=LFT,LLT
                  N = I + NFT
                  R4(1) = GBUF%MOM(JJ(1)+I)
                  R4(2) = GBUF%MOM(JJ(2)+I)
                  R4(3) = GBUF%MOM(JJ(3)+I)
                  R4(3) = R4(3) * INVERT(EL2FA(NNI+N))
                  TENS(1,EL2FA(NNI+N)) = R4(1)
                  TENS(2,EL2FA(NNI+N)) = R4(2)
                  TENS(3,EL2FA(NNI+N)) = R4(3)
                ENDDO
c                                
              ELSE IF (ITENS == 3 .OR. ITENS == 4) THEN    
                ! /TENS/STRESS/UPPER
                ! /TENS/STRESS/LOWER

                IF (ITENS == 3) THEN   ! upper
                  IF (IGTYP == 1 .OR. IGTYP == 9) THEN
                    IPT = MPT
                    IL  = 1
                  ELSE IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16) THEN
                    IPT = 1
                    IL  = ELBUF_TAB(NG)%NLAY
                  END IF
                ELSE IF (ITENS == 4) THEN   ! lower
                  IPT = 1
                  IL  = 1
                END IF
                IMAT  = ELBUF_TAB(NG)%BUFLY(IL)%IMAT                          
                IVISC = IPM(222,IMAT)    
                DO I=1,NEL  
                  DO IR=1,NPTR                
                    DO IS=1,NPTS 
                      LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IPT) 
                      SIGE(I,1) = SIGE(I,1) + LBUF%SIG(JJ(1) + I) / NPG
                      SIGE(I,2) = SIGE(I,2) + LBUF%SIG(JJ(2) + I) / NPG
                      SIGE(I,3) = SIGE(I,3) + LBUF%SIG(JJ(3) + I) / NPG
                    ENDDO                 
                  ENDDO   
                ENDDO
                IF (IVISC > 0) THEN
                  DO I=1,NEL  
                    DO IR=1,NPTR                
                      DO IS=1,NPTS 
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IPT) 
                        SIGE(I,1) = SIGE(I,1) + LBUF%VISC(JJ(1) + I) / NPG
                        SIGE(I,2) = SIGE(I,2) + LBUF%VISC(JJ(2) + I) / NPG
                        SIGE(I,3) = SIGE(I,3) + LBUF%VISC(JJ(3) + I) / NPG
                      ENDDO                 
                    ENDDO
                  ENDDO
                END IF
                MAT_ORTH = MATPARAM_TAB(IMAT)%ORTHOTROPY
                IF (MAT_ORTH == 2) THEN
                  DIR_A => ELBUF_TAB(NG)%BUFLY(IL)%DIRA  
                  CALL UROTO_TENS2D(NEL,SIGE,DIR_A)
                END IF
c
                DO I=LFT,LLT
                  N = I + NFT
                  R4(1) = SIGE(I,1)
                  R4(2) = SIGE(I,2)
                  R4(3) = SIGE(I,3)
                  R4(3) = R4(3) * INVERT(EL2FA(NNI+N))
                  TENS(1,EL2FA(NNI+N)) = R4(1)
                  TENS(2,EL2FA(NNI+N)) = R4(2)
                  TENS(3,EL2FA(NNI+N)) = R4(3)
                ENDDO
c            
              ELSE IF (ITENS > 100 .AND. ITENS < 201) THEN
                ! /TENS/STRESS/NPT     <=> IGTYP = 1,9
                ! /TENS/STRESS/ILAY    <=> IGTYP = 10,11,16
                ! /TENS/STRESS/PLY_ID  <=> IGTYP = 17

                IPT = ITENS-100
                IF (IGTYP == 1 .OR. IGTYP == 9) THEN
                  IL  = 1
                  IPT = MIN(IPT, ELBUF_TAB(NG)%BUFLY(1)%NPTT)
                ELSE IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16) THEN
                  IPT = 1
                  IL  = MIN(IPT, ELBUF_TAB(NG)%NLAY)
                ELSE IF (IGTYP == 17) THEN    ! defined by ply ID
                  IPLY = IPT
                  IPT = 1
                  IL  = 1
                  DO J=1,NLAY
                    ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                    IF (ID_PLY == IPLY) THEN
                      IL = J
                      EXIT
                    END IF
                  END DO
                END IF
                IMAT  = ELBUF_TAB(NG)%BUFLY(IL)%IMAT                          
                IVISC = IPM(222,IMAT)    
                DO I=1,NEL  
                  DO IR=1,NPTR                
                    DO IS=1,NPTS 
                      LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IPT) 
                      SIGE(I,1) = SIGE(I,1) + LBUF%SIG(JJ(1) + I) / NPG
                      SIGE(I,2) = SIGE(I,2) + LBUF%SIG(JJ(2) + I) / NPG
                      SIGE(I,3) = SIGE(I,3) + LBUF%SIG(JJ(3) + I) / NPG
                    ENDDO                 
                  ENDDO   
                ENDDO
                IF (IVISC > 0) THEN
                  DO I=1,NEL  
                    DO IR=1,NPTR                
                      DO IS=1,NPTS 
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IPT) 
                        SIGE(I,1) = SIGE(I,1) + LBUF%VISC(JJ(1) + I) / NPG
                        SIGE(I,2) = SIGE(I,2) + LBUF%VISC(JJ(2) + I) / NPG
                        SIGE(I,3) = SIGE(I,3) + LBUF%VISC(JJ(3) + I) / NPG
                      ENDDO                 
                    ENDDO
                  ENDDO
                END IF
                MAT_ORTH = MATPARAM_TAB(IMAT)%ORTHOTROPY
                IF (MAT_ORTH == 2) THEN
                  DIR_A => ELBUF_TAB(NG)%BUFLY(IL)%DIRA  
                  CALL UROTO_TENS2D(NEL,SIGE,DIR_A)
                END IF
c
                DO I=LFT,LLT
                  N = I + NFT
                  R4(1) = SIGE(I,1)
                  R4(2) = SIGE(I,2)
                  R4(3) = SIGE(I,3)
                  R4(3) = R4(3) * INVERT(EL2FA(NNI+N))
                  TENS(1,EL2FA(NNI+N)) = R4(1)
                  TENS(2,EL2FA(NNI+N)) = R4(2)
                  TENS(3,EL2FA(NNI+N)) = R4(3)
                ENDDO

C----------           
              ELSEIF (ITENS > 400 .AND. ITENS <  501 .AND. 
     .                 (IGTYP == 51 .OR.  IGTYP == 52)) THEN 
                  !  /TENS/STRESS/PLY_ID/UPPER
C-----
                IL   = 1
                IPT  = NPTT
                IPLY = ITENS - 400
                DO J=1,NLAY
                  ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                  IF (ID_PLY == IPLY) THEN
                    IL = J
                    EXIT
                  END IF
                END DO
                IMAT  = ELBUF_TAB(NG)%BUFLY(IL)%IMAT                          
                IVISC = IPM(222,IMAT)    
C
                SIGE(1:NEL,1:3) = ZERO
                DO I=1,NEL                                          
                  DO IR=1,NPTR                                          
                    DO IS=1,NPTS                                        
                      LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IPT)  
                      SIGE(I,1) = SIGE(I,1) + LBUF%SIG(JJ(1) + I) / NPG
                      SIGE(I,2) = SIGE(I,2) + LBUF%SIG(JJ(2) + I) / NPG
                      SIGE(I,3) = SIGE(I,3) + LBUF%SIG(JJ(3) + I) / NPG
                    ENDDO                                               
                  ENDDO    
                ENDDO
c
                IF (IVISC > 0) THEN
                  DO I=1,NEL  
                    DO IR=1,NPTR                
                      DO IS=1,NPTS 
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IPT) 
                        SIGE(I,1) = SIGE(I,1) + LBUF%VISC(JJ(1) + I) / NPG
                        SIGE(I,2) = SIGE(I,2) + LBUF%VISC(JJ(2) + I) / NPG
                        SIGE(I,3) = SIGE(I,3) + LBUF%VISC(JJ(3) + I) / NPG
                      ENDDO                 
                    ENDDO
                  ENDDO
                END IF
c
                MAT_ORTH = MATPARAM_TAB(IMAT)%ORTHOTROPY
                IF (MAT_ORTH == 2) THEN
                  DIR_A => ELBUF_TAB(NG)%BUFLY(IL)%DIRA  
                  CALL UROTO_TENS2D(NEL,SIGE,DIR_A)
                END IF
C
                DO I=LFT,LLT                                            
                  N  = NFT + I 
                  R4(1) = SIGE(I,1)
                  R4(2) = SIGE(I,2)
                  R4(3) = SIGE(I,3)
                  R4(3) = R4(3) * INVERT(EL2FA(NNI+N))
                  TENS(1,EL2FA(NNI+N)) = R4(1)
                  TENS(2,EL2FA(NNI+N)) = R4(2)
                  TENS(3,EL2FA(NNI+N)) = R4(3)
                ENDDO
C-----
              ELSEIF (ITENS > 500 .AND. ITENS <  601 .AND. 
     .               (IGTYP == 51 .OR. IGTYP == 52)) THEN 
                  !  /TENS/STRESS/PLY_ID/LOWER
                IL   = 1
                IPT  = 1
                IPLY = ITENS - 500
                DO J=1,NLAY
                  ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                  IF (ID_PLY == IPLY) THEN
                    IL = J
                    EXIT
                  END IF
                END DO
                IMAT  = ELBUF_TAB(NG)%BUFLY(IL)%IMAT                          
                IVISC = IPM(222,IMAT)    
C
                SIGE(1:NEL,1:3) = ZERO
                DO I=1,NEL                                          
                  DO IR=1,NPTR                                          
                    DO IS=1,NPTS                                        
                      LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IPT)  
                      SIGE(I,1) = SIGE(I,1) + LBUF%SIG(JJ(1) + I) / NPG
                      SIGE(I,2) = SIGE(I,2) + LBUF%SIG(JJ(2) + I) / NPG
                      SIGE(I,3) = SIGE(I,3) + LBUF%SIG(JJ(3) + I) / NPG
                    ENDDO                                               
                  ENDDO    
                ENDDO
c
                IF (IVISC > 0) THEN
                  DO I=1,NEL  
                    DO IR=1,NPTR                
                      DO IS=1,NPTS 
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IPT) 
                        SIGE(I,1) = SIGE(I,1) + LBUF%VISC(JJ(1) + I) / NPG
                        SIGE(I,2) = SIGE(I,2) + LBUF%VISC(JJ(2) + I) / NPG
                        SIGE(I,3) = SIGE(I,3) + LBUF%VISC(JJ(3) + I) / NPG
                      ENDDO                 
                    ENDDO
                  ENDDO
                END IF
c
                MAT_ORTH = MATPARAM_TAB(IMAT)%ORTHOTROPY
                IF (MAT_ORTH == 2) THEN
                  DIR_A => ELBUF_TAB(NG)%BUFLY(IL)%DIRA  
                  CALL UROTO_TENS2D(NEL,SIGE,DIR_A)
                END IF
C
                DO I=LFT,LLT                                            
                  N  = NFT + I 
                  R4(1) = SIGE(I,1)
                  R4(2) = SIGE(I,2)
                  R4(3) = SIGE(I,3)
                  R4(3) = R4(3) * INVERT(EL2FA(NNI+N))
                  TENS(1,EL2FA(NNI+N)) = R4(1)
                  TENS(2,EL2FA(NNI+N)) = R4(2)
                  TENS(3,EL2FA(NNI+N)) = R4(3)
                ENDDO
c
              ELSE IF (ITENS > 600 .AND. ITENS <  1611 .AND. 
     .                (IGTYP == 51 .OR. IGTYP == 52)) THEN
c               /TENS/STRESS/ILAY/IPT

                IUS  = ITENS - 600
                ILAY = INT((IUS - 1)/10)
                IF (ILAY == 0) ILAY = 100
                IL = ILAY
                IT = IUS - 10*IL
                IMAT  = ELBUF_TAB(NG)%BUFLY(IL)%IMAT                          
                IVISC = IPM(222,IMAT)    
C
                SIGE(1:NEL,1:3) = ZERO
                DO I=1,NEL                                          
                  DO IR=1,NPTR                                          
                    DO IS=1,NPTS                                        
                      LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IPT)  
                      SIGE(I,1) = SIGE(I,1) + LBUF%SIG(JJ(1) + I) / NPG
                      SIGE(I,2) = SIGE(I,2) + LBUF%SIG(JJ(2) + I) / NPG
                      SIGE(I,3) = SIGE(I,3) + LBUF%SIG(JJ(3) + I) / NPG
                    ENDDO                                               
                  ENDDO    
                ENDDO
c
                IF (IVISC > 0) THEN
                  DO I=1,NEL  
                    DO IR=1,NPTR                
                      DO IS=1,NPTS 
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IPT) 
                        SIGE(I,1) = SIGE(I,1) + LBUF%VISC(JJ(1) + I) / NPG
                        SIGE(I,2) = SIGE(I,2) + LBUF%VISC(JJ(2) + I) / NPG
                        SIGE(I,3) = SIGE(I,3) + LBUF%VISC(JJ(3) + I) / NPG
                      ENDDO                 
                    ENDDO
                  ENDDO
                END IF
c
                MAT_ORTH = MATPARAM_TAB(IMAT)%ORTHOTROPY
                IF (MAT_ORTH == 2) THEN
                  DIR_A => ELBUF_TAB(NG)%BUFLY(IL)%DIRA  
                  CALL UROTO_TENS2D(NEL,SIGE,DIR_A)
                END IF
C
                DO I=LFT,LLT                                            
                  N  = NFT + I 
                  R4(1) = SIGE(I,1)
                  R4(2) = SIGE(I,2)
                  R4(3) = SIGE(I,3)
                  R4(3) = R4(3) * INVERT(EL2FA(NNI+N))
                  TENS(1,EL2FA(NNI+N)) = R4(1)
                  TENS(2,EL2FA(NNI+N)) = R4(2)
                  TENS(3,EL2FA(NNI+N)) = R4(3)
                ENDDO
c------------------------------------    
              ELSEIF (ITENS > 1610 .AND. ITENS <  1611 + MX_PLY_ANIM ) THEN
                ! /IDPLY/STRESS/PLY_ID/IPT : output in element coordinate system

                SIGE(1:NEL,1:3) = ZERO
                IL  = ITENS - 1610
                IF (PLY_ANIM_STRESS(3*(IL - 1) + 2) == 2) THEN
                  IPT = PLY_ANIM_STRESS(3*(IL - 1) + 3) 
c
                  DO J=1,NLAY
                    IF (IGTYP == 17 .OR. IGTYP == 51) THEN
                      ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                    ELSE IF (IGTYP == 52) THEN
                      ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK) - NUMSTACK)
                    END IF
                    IF (ID_PLY == IPLY) THEN
                      ILAY  = J
                      IMAT  = ELBUF_TAB(NG)%BUFLY(ILAY)%IMAT                          
                      IVISC = IPM(222,IMAT)    
                      IF (IPT <= ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT) THEN 
                        DO I=1,NEL                               
                          DO IR=1,NPTR                                        
                            DO IS=1,NPTS 
                              LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                              SIGE(I,1) = SIGE(I,1) + LBUF%SIG(JJ(1) + I) / NPG
                              SIGE(I,2) = SIGE(I,2) + LBUF%SIG(JJ(2) + I) / NPG
                              SIGE(I,3) = SIGE(I,3) + LBUF%SIG(JJ(3) + I) / NPG
                            ENDDO                                              
                          ENDDO    
                        ENDDO
c
                        MAT_ORTH = MATPARAM_TAB(IMAT)%ORTHOTROPY
                        IF (MAT_ORTH > 0) THEN                 
                          IF (IDRAPE > 0 ) THEN
                            DIR_A => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF_DIR(IPT)%DIRA
                            DIR_B => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF_DIR(IPT)%DIRB
                          ELSE                                            
                            DIR_A => ELBUF_TAB(NG)%BUFLY(ILAY)%DIRA
                            DIR_B => ELBUF_TAB(NG)%BUFLY(ILAY)%DIRB
                          ENDIF                       
                        END IF
                        IF (MAT_ORTH == 2) THEN                 
                          CALL UROTO_TENS2D(NEL,SIGE,DIR_A)
                        ELSE IF (MAT_ORTH == 3) THEN   ! anisotropic (law 58,158 only)
                          CALL UROTO_TENS2D_ANISO(NEL,SIGE,DIR_A,DIR_B)
                        END IF
c
                        DO I=1,NEL                                    
                          N  = NFT + I 
                          R4(1) = SIGE(I,1)
                          R4(2) = SIGE(I,2)
                          R4(3) = SIGE(I,3)
                          R4(3) = R4(3) * INVERT(EL2FA(NNI+N))
                          TENS(1,EL2FA(NNI+N)) = R4(1)
                          TENS(2,EL2FA(NNI+N)) = R4(2)
                          TENS(3,EL2FA(NNI+N)) = R4(3)
                        ENDDO
c
                        EXIT 
                      ENDIF  ! ID_PLY == IPLY
                    ENDIF   ! IPT <== NPTT
                  ENDDO    ! NLAY
                END IF
c            
              ELSE IF (ITENS == IDX_MSTRESS+2 .OR. ITENS == IDX_MSTRESS+3) THEN    
                ! /TENS/MSTRESS/UPPER
                ! /TENS/MSTRESS/LOWER

                IF (ITENS == IDX_MSTRESS+2) THEN   ! upper
                  IF (IGTYP == 1 .OR. IGTYP == 9) THEN
                    IL  = 1
                    IPT = MPT
                  ELSE IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16) THEN
                    IL  = ELBUF_TAB(NG)%NLAY
                    IPT = 1
                  END IF
                ELSE IF (ITENS == IDX_MSTRESS+3) THEN   ! lower
                  IPT = 1
                  IL  = 1
                END IF
                IMAT  = ELBUF_TAB(NG)%BUFLY(IL)%IMAT                          
                IVISC = IPM(222,IMAT)    
                DO I=1,NEL  
                  DO IR=1,NPTR                
                    DO IS=1,NPTS 
                      LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IPT) 
                      SIGE(I,1) = SIGE(I,1) + LBUF%SIG(JJ(1) + I) / NPG
                      SIGE(I,2) = SIGE(I,2) + LBUF%SIG(JJ(2) + I) / NPG
                      SIGE(I,3) = SIGE(I,3) + LBUF%SIG(JJ(3) + I) / NPG
                    ENDDO                 
                  ENDDO   
                ENDDO
                IF (IVISC > 0) THEN
                  DO I=1,NEL  
                    DO IR=1,NPTR                
                      DO IS=1,NPTS 
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IPT) 
                        SIGE(I,1) = SIGE(I,1) + LBUF%VISC(JJ(1) + I) / NPG
                        SIGE(I,2) = SIGE(I,2) + LBUF%VISC(JJ(2) + I) / NPG
                        SIGE(I,3) = SIGE(I,3) + LBUF%VISC(JJ(3) + I) / NPG
                      ENDDO                 
                    ENDDO
                  ENDDO
                END IF
c
                DO I=LFT,LLT
                  N = I + NFT
                  R4(1) = SIGE(I,1)
                  R4(2) = SIGE(I,2)
                  R4(3) = SIGE(I,3)
                  R4(3) = R4(3) * INVERT(EL2FA(NNI+N))
                  TENS(1,EL2FA(NNI+N)) = R4(1)
                  TENS(2,EL2FA(NNI+N)) = R4(2)
                  TENS(3,EL2FA(NNI+N)) = R4(3)
                ENDDO
c
              ELSE IF (ITENS > IDX_MSTRESS+3 .AND. ITENS < IDX_MSTRESS+103) THEN
                ! /TENS/MSTRESS/NPT     <=> IGTYP = 1,9
                ! /TENS/MSTRESS/ILAY    <=> IGTYP = 10,11,16
                ! /TENS/MSTRESS/PLY_ID  <=> IGTYP = 17

                IPT = ITENS-100
                IF (IGTYP == 1 .OR. IGTYP == 9) THEN
                  IL  = 1
                  IPT = MIN(IPT, ELBUF_TAB(NG)%BUFLY(1)%NPTT)
                ELSE IF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16) THEN
                  IPT = 1
                  IL  = MIN(IPT, ELBUF_TAB(NG)%NLAY)
                ELSE IF (IGTYP == 17) THEN    ! defined by ply ID
                  IPLY = IPT
                  IPT = 1
                  IL  = 1
                  DO J=1,NLAY
                    ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                    IF (ID_PLY == IPLY) THEN
                      IL = J
                      EXIT
                    END IF
                  END DO
                END IF
                IMAT  = ELBUF_TAB(NG)%BUFLY(IL)%IMAT                          
                IVISC = IPM(222,IMAT)    
                DO I=1,NEL  
                  DO IR=1,NPTR                
                    DO IS=1,NPTS 
                      LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IPT) 
                      SIGE(I,1) = SIGE(I,1) + LBUF%SIG(JJ(1) + I) / NPG
                      SIGE(I,2) = SIGE(I,2) + LBUF%SIG(JJ(2) + I) / NPG
                      SIGE(I,3) = SIGE(I,3) + LBUF%SIG(JJ(3) + I) / NPG
                    ENDDO                 
                  ENDDO   
                ENDDO
                IF (IVISC > 0) THEN
                  DO I=1,NEL  
                    DO IR=1,NPTR                
                      DO IS=1,NPTS 
                        LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IPT) 
                        SIGE(I,1) = SIGE(I,1) + LBUF%VISC(JJ(1) + I) / NPG
                        SIGE(I,2) = SIGE(I,2) + LBUF%VISC(JJ(2) + I) / NPG
                        SIGE(I,3) = SIGE(I,3) + LBUF%VISC(JJ(3) + I) / NPG
                      ENDDO                 
                    ENDDO
                  ENDDO
                END IF
c
                DO I=LFT,LLT
                  N = I + NFT
                  R4(1) = SIGE(I,1)
                  R4(2) = SIGE(I,2)
                  R4(3) = SIGE(I,3)
                  R4(3) = R4(3) * INVERT(EL2FA(NNI+N))
                  TENS(1,EL2FA(NNI+N)) = R4(1)
                  TENS(2,EL2FA(NNI+N)) = R4(2)
                  TENS(3,EL2FA(NNI+N)) = R4(3)
                ENDDO
c
c------------------------------------    
              ELSEIF (ITENS > IDX_IDPLY_MSTRESS .AND. 
     .                ITENS < IDX_IDPLY_MSTRESS + MX_PLY_ANIM ) THEN
                ! /ANIM/SHELL/IDPLY/MSTRESS/PLY_ID/IPT  : output in material coordinate system

                SIGE(1:NEL,1:3) = ZERO
                IL  = ITENS - IDX_IDPLY_MSTRESS
                IF (PLY_ANIM_STRESS(3*(IL - 1) + 2) == 3) THEN
c
                  IPT = PLY_ANIM_STRESS(3*(IL - 1) + 3)
                  DO J=1,NLAY
                    IF (IGTYP == 17 .OR. IGTYP == 51) THEN
                      ID_PLY = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                    ELSE IF (IGTYP == 52) THEN
                      ID_PLY = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK) - NUMSTACK)
                    END IF
                    IF (ID_PLY == IPLY) THEN
                      ILAY  = J
                      IMAT  = ELBUF_TAB(NG)%BUFLY(ILAY)%IMAT                          
                      IVISC = IPM(222,IMAT)    
                      IF (IPT <= ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT) THEN 
                        DO I=1,NEL                               
                          DO IR=1,NPTR                                        
                            DO IS=1,NPTS 
                              LBUF => ELBUF_TAB(NG)%BUFLY(J)%LBUF(IR,IS,IPT) 
                              SIGE(I,1) = SIGE(I,1) + LBUF%SIG(JJ(1) + I) / NPG
                              SIGE(I,2) = SIGE(I,2) + LBUF%SIG(JJ(2) + I) / NPG
                              SIGE(I,3) = SIGE(I,3) + LBUF%SIG(JJ(3) + I) / NPG
                            ENDDO                                              
                          ENDDO    
                        ENDDO
c
                        DO I=1,NEL                                    
                          N  = NFT + I 
                          R4(1) = SIGE(I,1)
                          R4(2) = SIGE(I,2)
                          R4(3) = SIGE(I,3)
                          R4(3) = R4(3) * INVERT(EL2FA(NNI+N))
                          TENS(1,EL2FA(NNI+N)) = R4(1)
                          TENS(2,EL2FA(NNI+N)) = R4(2)
                          TENS(3,EL2FA(NNI+N)) = R4(3)
                        ENDDO
c
                        EXIT 
                      ENDIF  ! ID_PLY == IPLY
                    ENDIF   ! IPT <== NPTT
                  ENDDO    ! NLAY
                END IF 
C----------         
              ELSE IF (ITENS >= 1 .and. ITENS <= 4) THEN
                ! ITENS=1,2  (membrane, bending) 3,4 ,NPT=0 IGTYP=1
                DO I=LFT,LLT
                  N = I + NFT
                  DO J=1,3
                    R4(J) = A1 * GBUF%FOR(JJ(J)+I) + A2 * GBUF%MOM(JJ(J)+I)
                  ENDDO
                  R4(3) = R4(3) * INVERT(EL2FA(NNI+N))
                  TENS(1,EL2FA(NNI+N)) = R4(1)
                  TENS(2,EL2FA(NNI+N)) = R4(2)
                  TENS(3,EL2FA(NNI+N)) = R4(3)
                ENDDO
              ENDIF  ! STRESS
C------------------------
            ELSEIF (ISTRE == 0 .AND. ISTRAIN > 0) THEN    ! strain 
C--------
C             STRAIN 5:mem,6:bend,7:upper,8:lower
C--------
              IF (ITENS == 5) THEN
                DO I=LFT,LLT
                  N = I + NFT
                  THK = GBUF%THK(I)
                  J  = EL2FA(NNI+N)
                  R4(1) = GBUF%STRA(JJ(1)+I)    
                  R4(2) = GBUF%STRA(JJ(2)+I)    
                  R4(3) = GBUF%STRA(JJ(3)+I) * INVERT(J)*HALF    
                  TENS(1,J) = R4(1) 
                  TENS(2,J) = R4(2) 
                  TENS(3,J) = R4(3) 
                ENDDO
              ELSE
                DO I=LFT,LLT
                  N = I + NFT
                  THK = GBUF%THK(I)
                  J  = EL2FA(NNI+N)
                  R4(1) = A1*GBUF%STRA(JJ(1)+I) + A2*GBUF%STRA(JJ(6)+I) * THK
                  R4(2) = A1*GBUF%STRA(JJ(2)+I) + A2*GBUF%STRA(JJ(7)+I) * THK
                  R4(3) = A1*GBUF%STRA(JJ(3)+I) + A2*GBUF%STRA(JJ(8)+I) * THK
                  R4(3) = R4(3) * INVERT(J) * HALF 
                  TENS(1,J) = R4(1)                  
                  TENS(2,J) = R4(2)                  
                  TENS(3,J) = R4(3)                  
                ENDDO
              ENDIF
C------------------------
            ELSEIF (ISTRE == 2) THEN
C---------
C          STRAIN RATE
C---------
c------------------------------------
              DO I=LFT,LLT
                N = I + NFT
                THK = GBUF%THK(I)	      
                IF (ITENS /= 92) THEN
                  DO J=1,3
                    R4(J) = A1*EPSDOT(J,N+N0) + A2*EPSDOT(J+3,N+N0)*THK
                  ENDDO
                ELSE
                  DO J=1,3
                    R4(J) = EPSDOT(J+3,N+N0)
                  ENDDO
                ENDIF
                R4(3) = R4(3) * INVERT(EL2FA(NNI+N)) * HALF
                TENS(1,EL2FA(NNI+N)) = R4(1)
                TENS(2,EL2FA(NNI+N)) = R4(2)
                TENS(3,EL2FA(NNI+N)) = R4(3)
              ENDDO
            ENDIF  !   (STRESS) ISTRE == 1 
          ENDIF   ! IF (MLW /= 13)
        ENDIF    ! IF(ITY == 2)
c
C-----------------------------------------------
 490  CONTINUE   ! NGROUP
 500  CONTINUE
C-----------------------------------------------
      IF (NSPMD == 1)THEN
        DO N=1,NBF
          R4(1) = TENS(1,N)
          R4(2) = TENS(2,N)
          R4(3) = TENS(3,N)
          CALL WRITE_R_C(R4,3)
        ENDDO
      ELSE
        DO N = 1, NBF_L
          WA(3*N-2) = TENS(1,N)
          WA(3*N-1) = TENS(2,N)
          WA(3*N  ) = TENS(3,N)
        ENDDO
        IF (ISPMD == 0) THEN
           BUF = (NUMELQG+NUMELCG+NUMELTGG)*3
        ELSE
           BUF = 1
        ENDIF
        CALL SPMD_R4GET_PARTN(3,3*NBF_L,NBPART,IADG,WA,BUF)
      ENDIF
C-----------------------------------------------
      IF (NELCUT > 0) THEN  
        IF (NSPMD == 1) THEN
          DO I=1,NELCUT
            CALL WRITE_R_C(R4,3)
          ENDDO
        ELSEIF (ISPMD == 0) THEN
Cel verifier l'interet de ce qui est fait ci-dessus  !!!!!!!!!!!!!!
          DO I=1,NELCUT
            CALL WRITE_R_C(WA(3*NBF_L-2),3)
          ENDDO
        ENDIF
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  TENCGPS1                      source/output/anim/generate/tensorc.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SHLROTG                       source/output/anim/generate/tensor6.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TENCGPS1(ELBUF_TAB ,IPARG,ITENS,TENS1 ,TENS2  ,
     .                    X     ,IXC  ,IGEO ,IXTG  ,ITAGPS )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),ITENS, IGEO(NPROPGI,*), 
     .        IXC(NIXC,*),IXTG(NIXTG,*),ITAGPS(*)
C     REAL
      my_real
     .   TENS1(3,*),TENS2(3,*), X(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   OFF, FAC, A1, A2, A3, THK, EVAR(6,MVSIZ),AREA(MVSIZ)
      INTEGER I, NG, NEL, NFT, ITY, LFT, NPT, IPT,
     .        N, J, LLT, MLW,K,
     .        IPID, I1, I2, IAD2, NS1, NS2  , IALEL, ISTRE,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,NNI,N0,
     .        II, II_L, KK ,INC,IHBE,LEN ,IREP,BUF,NNOD,
     .        NC(20,MVSIZ),ISROT,JJ(5)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
       DO 490 NG=1,NGROUP
        GBUF => ELBUF_TAB(NG)%GBUF
        II = 0
        MLW   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        NFT   =IPARG(3,NG)
        ITY   =IPARG(5,NG)
        LFT=1
        LLT=NEL
        IHBE = IPARG(23,NG)
        NNOD = 0
!
        DO I=1,5
          JJ(I) = NEL*(I-1)
        ENDDO
!
C-----------------------------------------------
C       COQUES
C-----------------------------------------------
        IF(ITY == 3.OR.ITY == 7)THEN
         A1    = ZERO
         A2    = ZERO
         A3    = ZERO         
C------------------------
C        STRESS
C------------------------
C-----upper----
         IF(ITENS == 1)THEN
           NS1 = 5
           NS2 = 3
           IF(MLW == 1)THEN
             A1   = ONE
             A2   = SIX
           ELSEIF(MLW == 2.OR.MLW == 19.OR.
     .            MLW == 15.OR.
     .            MLW == 22.OR.MLW == 25.OR.
     .            MLW == 27.OR.MLW == 32.OR.
     .            MLW>=28)THEN 
             A1   = ONE
             A2   = ZERO
           ELSEIF(MLW == 3.OR.MLW == 23)THEN
             A1   = ONE
             A2   = ZERO
           ENDIF
C-----lower----
         ELSEIF(ITENS == 2)THEN
           NS1 = 5
           NS2 = 3
           IF(MLW == 1)THEN
             A1   = ONE
             A2   = -SIX
           ELSEIF(MLW == 2.OR.MLW == 19.OR.
     .            MLW == 15.OR.
     .            MLW == 22.OR.MLW == 25.OR.
     .            MLW == 27.OR.MLW == 32.OR.
     .            MLW>=28)THEN 
             A1   = ONE
             A2   = ZERO
           ELSEIF(MLW == 3.OR.MLW == 23)THEN
             A1   = ONE
             A2   = ZERO
           ENDIF
         ENDIF
C------------------------
          DO I=LFT,LLT
            DO J = 1,2
              EVAR(J,I) = A1 * GBUF%FOR(JJ(J)+I) + A2 * GBUF%MOM(JJ(J)+I)  
            ENDDO
            EVAR(3,I) = ZERO
            EVAR(4,I) = A1 * GBUF%FOR(JJ(3)+I) + A2 * GBUF%MOM(JJ(3)+I)  
            EVAR(5,I) = A1 * GBUF%FOR(JJ(4)+I) 
            EVAR(6,I) = A1 * GBUF%FOR(JJ(5)+I) 
          ENDDO
          CALL SHLROTG(LFT     ,LLT     ,NFT     ,X       ,EVAR    ,
     1                 ITY     ,IXC     ,IXTG    ,IHBE    ,AREA    )
           IF(ITY == 7)THEN
	    NNOD=3
            DO I=LFT,LLT
                N = I + NFT
	        DO J = 1,NNOD
	          NC(J,I) = IXTG(J+1,N)
                ENDDO
            ENDDO 
           ELSEIF(ITY == 3)THEN
	    NNOD=4
            DO I=LFT,LLT
                N = I + NFT
	        DO J = 1,NNOD
	          NC(J,I) = IXC(J+1,N)
                ENDDO
            ENDDO 
           ENDIF
C-----------------------------------------------
        ELSE
        ENDIF
        DO I=LFT,LLT
	   DO J = 1,NNOD
	      N = NC(J,I)
	      IF (N>0)THEN
	       DO K = 1,3
	        TENS1(K,N) = TENS1(K,N)+EVAR(K,I)
	        TENS2(K,N) = TENS2(K,N)+EVAR(K+3,I)
	       ENDDO 
	       ITAGPS(N) = ITAGPS(N)+1
	      ENDIF
           ENDDO
        ENDDO
 490   CONTINUE
C-----------------------------------------------
C
      RETURN
      END
Chd|====================================================================
Chd|  TENCGPS2                      source/output/anim/generate/tensorc.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SHLROTG                       source/output/anim/generate/tensor6.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE TENCGPS2(ELBUF_TAB ,IPARG,ITENS,TENS1 ,TENS2  ,
     .                    X     ,IXC  ,IGEO ,IXTG  ,GEO    ,
     .                    VGPS   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),ITENS, IGEO(NPROPGI,*), 
     .        IXC(NIXC,*),IXTG(NIXTG,*)
C     REAL
      my_real
     .   TENS1(3,*),TENS2(3,*), X(3,*),GEO(NPROPG,*),VGPS(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   OFF, FAC, A1, A2, A3, THK0, EVAR(6,MVSIZ),AREA(MVSIZ),
     .   VOL(MVSIZ)
      INTEGER I, NG, NEL, NFT, ITY, LFT, NPT, IPT,
     .        N, J, LLT, MLW,K,
     .        IPID, I1, I2, IAD2, NS1, NS2  , IALEL, ISTRE,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,NNI,N0,
     .        II, II_L, KK ,INC,IHBE,LEN ,IREP,BUF,NNOD,
     .        NC(20,MVSIZ),ISROT,JJ(5)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
       DO 490 NG=1,NGROUP
        GBUF => ELBUF_TAB(NG)%GBUF
        II = 0
        MLW   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        NFT   =IPARG(3,NG)
        ITY   =IPARG(5,NG)
        LFT=1
        LLT=NEL
        NNOD = 0
!
        DO I=1,5
          JJ(I) = NEL*(I-1)
        ENDDO
!
C-----------------------------------------------
C       COQUES
C-----------------------------------------------
        IF (ITY == 3.OR.ITY == 7) THEN
         A1    = ZERO
         A2    = ZERO
         A3    = ZERO         
C------------------------
C        STRESS
C------------------------
C-----upper----
         IF(ITENS == 1)THEN
           NS1 = 5
           NS2 = 3
           IF(MLW == 1)THEN
             A1   = ONE
             A2   = SIX
           ELSEIF(MLW == 2.OR.MLW == 19.OR.
     .            MLW == 15.OR.
     .            MLW == 22.OR.MLW == 25.OR.
     .            MLW == 27.OR.MLW == 32.OR.
     .            MLW>=28)THEN 
             A1   = ONE
             A2   = ZERO
           ELSEIF(MLW == 3.OR.MLW == 23)THEN
             A1   = ONE
             A2   = ZERO
           ENDIF
C-----lower----
         ELSEIF(ITENS == 2)THEN
           NS1 = 5
           NS2 = 3
           IF(MLW == 1)THEN
             A1   = ONE
             A2   = -SIX
           ELSEIF(MLW == 2.OR.MLW == 19.OR.
     .            MLW == 15.OR.
     .            MLW == 22.OR.MLW == 25.OR.
     .            MLW == 27.OR.MLW == 32.OR.
     .            MLW>=28)THEN 
             A1   = ONE
             A2   = ZERO
           ELSEIF(MLW == 3.OR.MLW == 23)THEN
             A1   = ONE
             A2   = ZERO
           ENDIF
         ENDIF
C------------------------
          DO I=LFT,LLT
            DO J = 1,2
              EVAR(J,I) = A1 * GBUF%FOR(JJ(J)+I) + A2 * GBUF%MOM(JJ(J)+I)
            ENDDO
            EVAR(3,I) = ZERO
            EVAR(4,I) = A1 * GBUF%FOR(JJ(3)+I) + A2 * GBUF%MOM(JJ(3)+I)  
            EVAR(5,I) = A1 * GBUF%FOR(JJ(4)+I) 
            EVAR(6,I) = A1 * GBUF%FOR(JJ(5)+I) 
          ENDDO
          CALL SHLROTG(LFT     ,LLT     ,NFT     ,X       ,EVAR    ,
     1                 ITY     ,IXC     ,IXTG    ,IHBE    ,AREA    )
           IF(ITY == 7)THEN
	           NNOD=3
            DO I=LFT,LLT
                N = I + NFT
	               DO J = 1,NNOD
	                 NC(J,I) = IXTG(J+1,N)
                ENDDO
                THK0 = GEO(1,IXTG(5,N))
                OFF = MIN(GBUF%OFF(I),ONE)
                VOL(I) = THK0*AREA(I)*OFF
            ENDDO 
           ELSEIF(ITY == 3)THEN
	           NNOD=4
            DO I=LFT,LLT
                N = I + NFT
	               DO J = 1,NNOD
	                 NC(J,I) = IXC(J+1,N)
                ENDDO
                THK0 = GEO(1,IXC(6,N))
                OFF = MIN(GBUF%OFF(I),ONE)
                VOL(I) = THK0*AREA(I)*OFF
            ENDDO 
           ENDIF
C-----------------------------------------------
        ELSE
        ENDIF
        DO I=LFT,LLT
	   DO J = 1,NNOD
	      N = NC(J,I)
	      IF (N>0)THEN
	       DO K = 1,3
	        TENS1(K,N) = TENS1(K,N)+EVAR(K,I)*VOL(I)
	        TENS2(K,N) = TENS2(K,N)+EVAR(K+3,I)*VOL(I)
	       ENDDO 
	       VGPS(N) = VGPS(N)+VOL(I)
	      ENDIF
           ENDDO
        ENDDO
 490   CONTINUE
C-----------------------------------------------
C
      RETURN
      END


