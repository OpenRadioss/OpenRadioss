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
Chd|  DFUNC0                        source/output/anim/generate/dfunc0.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        OUTPUT_SCHLIEREN              source/output/anim/generate/output_schlieren.F
Chd|        SCHLIEREN_BUFFER_GATHERING    source/output/anim/generate/schlieren_buffer_gathering.F
Chd|        SPMD_R4GET_PARTN              source/mpi/anim/spmd_r4get_partn.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F
Chd|====================================================================
      SUBROUTINE DFUNC0(ELBUF_TAB, FUNC             ,IFUNC     ,IPARG   ,
     2                  MASS     , PM               ,EL2FA     ,NBF     ,
     3                  NBPART   , IADG             ,SPBUF     ,IPART   ,
     4                  IPARTSP  , ALE_CONNECTIVITY ,IXQ       ,IPM     ,
     5                  X        , V                ,W         ,
     6                  NERCVOIS , NESDVOIS         ,LERCVOIS  ,LESDVOIS,
     7                  BUFMAT   , MULTI_FVM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INITBUF_MOD
        USE ELBUFDEF_MOD
        USE ALEFVM_MOD
        USE MULTI_FVM_MOD
        USE SCHLIEREN_MOD
        USE ALE_CONNECTIVITY_MOD
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
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real FUNC(*), MASS(*) ,PM(NPROPM,NUMMAT),SPBUF(NSPBUF,*),BUFMAT(*),X(3,NUMNOD),V(3,NUMNOD), W(*)
        INTEGER IPARG(NPARG,NGROUP),EL2FA(*),IFUNC,NBF,
     .          NBPART,IADG(NSPMD,*),IPART(LIPART1,*),IPARTSP(*),
     .          NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),IXQ(NIXQ,NUMELQ),IPM(NPROPMI,NUMMAT)
        TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
        TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
        TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        my_real EVAR(MVSIZ), P, VONM2, VONM, S1, S2, S3, VALUE
        INTEGER I,II(6),N,NN,NN1,NG,NEL,JTURB,MT,MLW,IALEL,IPRT,IUS,BUF,NUVAR,IR,NFAIL,IALEFVM_FLG,IEOS,NVAREOS
        REAL R4,WA(NBF)
        TYPE(G_BUFEL_) ,POINTER :: GBUF
        TYPE(L_BUFEL_) ,POINTER :: LBUF
        TYPE(BUF_MAT_) ,POINTER :: MBUF
        TYPE(BUF_EOS_)  ,POINTER :: EBUF
        my_real, DIMENSION(:),POINTER  ::  DFMAX
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        NN1 = 1

        !-------------------------------------------------------!
        !     INITIALIZATION IF SCHLIEREN DEFINED               !
        !-------------------------------------------------------!
        IF(IFUNC==4892)THEN
          CALL SCHLIEREN_BUFFER_GATHERING(NERCVOIS ,NESDVOIS ,LERCVOIS ,LESDVOIS, IPARG, ELBUF_TAB, MULTI_FVM)
        ENDIF!(IFUNC==4892)

C-----------------------------------------------
        DO 900 NG=1,NGROUP
          MLW = IPARG(1,NG)
          NEL = IPARG(2,NG)
          NFT = IPARG(3,NG)
          ITY = IPARG(5,NG)
          LFT=1
          LLT=NEL
!
          DO I=1,6
            II(I) = NEL*(I-1)
          ENDDO
!
C-----------------------------------------------
          IF (ITY == 51) THEN
C-----------------------------------------------
C       PARTICULES SPH.
C-----------------------------------------------
            IF(IFUNC == 0)THEN
              DO I=LFT,LLT
                N = I + NFT
                FUNC(EL2FA(NN1+N)) = SPBUF(1,N)
              ENDDO
            ELSE
              GBUF => ELBUF_TAB(NG)%GBUF
              LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
              MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
              JTURB  = IPARG(12,NG)*(IPARG(7,NG)+IPARG(11,NG))
              NUVAR = ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT
C-----------
              IF (IFUNC == 1 .AND. GBUF%G_PLA > 0) THEN
                DO I=LFT,LLT
                  N = I + NFT
                  IF (EL2FA(NN1+N)/=0) THEN
                    FUNC(EL2FA(NN1+N)) = GBUF%PLA(I)
                  ENDIF
                ENDDO
c-----------
              ELSEIF (IFUNC == 2) THEN
                DO I=LFT,LLT
                  N = I + NFT
                  FUNC(EL2FA(NN1+N)) = GBUF%RHO(I)
                ENDDO
              ELSEIF (IFUNC == 3) THEN
                DO I=LFT,LLT
                  N = I + NFT
                  IALEL=IPARG(7,NG)+IPARG(11,NG)
                  IF(IALEL == 0)THEN
                    IPRT=IPARTSP(N)
                    MT  =IPART(1,IPRT)
                    VALUE = GBUF%EINT(I)/MAX(EM30,PM(89,MT))
                  ELSE
                    VALUE = GBUF%EINT(I)/MAX(EM30,GBUF%RHO(I))
                  ENDIF
                  FUNC(EL2FA(NN1+N)) = VALUE
                ENDDO
c-----------
              ELSEIF (IFUNC == 4 .AND. GBUF%G_TEMP > 0) THEN
                DO I=LFT,LLT
                  N = I + NFT
                  IF (EL2FA(NN1+N)/=0) FUNC(EL2FA(NN1+N)) = GBUF%TEMP(I)
                ENDDO
c-----------
              ELSEIF(IFUNC == 6 .OR. IFUNC == 7)THEN
                DO I=LFT,LLT
                  N = I + NFT
                  IF(EL2FA(NN1+N)/=0)THEN
                    P = - (GBUF%SIG(II(1) + I)
     .                   + GBUF%SIG(II(2) + I)
     .                   + GBUF%SIG(II(3) + I) ) * THIRD
                    VALUE = P
                    IF (IFUNC == 7) THEN
                      S1=GBUF%SIG(II(1) + I) + P
                      S2=GBUF%SIG(II(2) + I) + P
                      S3=GBUF%SIG(II(3) + I) + P
                      VONM2= THREE*(GBUF%SIG(II(4) + I)**2 +
     .                              GBUF%SIG(II(5) + I)**2 +
     .                              GBUF%SIG(II(6) + I)**2 +
     .                       HALF*(S1*S1+S2*S2+S3*S3) )
                      VONM= SQRT(VONM2)
                      VALUE = VONM
                    ENDIF
                    FUNC(EL2FA(NN1+N)) = VALUE
                  ENDIF
                ENDDO
c-----------
              ELSEIF(IFUNC == 8 .AND. JTURB/=0)THEN
C           ENERGIE TURBULENTE
                DO I=LFT,LLT
                  NN = EL2FA(NN1 + I + NFT)
                  IF(NN/=0)THEN
                    FUNC(NN) = GBUF%RK(I)
                  ENDIF
                ENDDO
c-----------
              ELSEIF(IFUNC == 9)THEN
C           VISCOSITE TURBULENTE
                DO I=LFT,LLT
                  N = I + NFT
                  NN = EL2FA(NN1 + I + NFT)
                  IF(NN/=0)THEN
                    IF(MLW == 6.AND.JTURB/=0)THEN
                      IPRT=IPARTSP(N)
                      MT  =IPART(1,IPRT)
                      VALUE = PM(81,MT)*GBUF%RK(I)**2 / MAX(EM15,GBUF%RE(I))
                    ELSE
                      VALUE = ZERO
                    ENDIF
                    FUNC(NN) = VALUE
                  ENDIF
                ENDDO
c-----------
              ELSEIF(IFUNC == 10)THEN
C           VORTICITE
                DO I=LFT,LLT
                  NN = EL2FA(NN1 + I + NFT)
                  IF(NN/=0)THEN
                    IF(MLW == 6 .OR. MLW == 17)THEN
                      VALUE = LBUF%VK(I)
                    ELSEIF(MLW == 46 .OR. MLW == 47)THEN
                      VALUE = MBUF%VAR(NEL+I)  !UVAR(I,2)
                    ELSE
                      VALUE = ZERO
                    ENDIF
                    FUNC(NN) = VALUE
                  ENDIF
                ENDDO
c-----------
              ELSEIF((IFUNC == 11.OR.IFUNC == 12.OR.IFUNC == 13)
     .            .AND.MLW == 24)THEN
C            dam 1 2 3
                DO I=LFT,LLT
                  N = I + NFT
                  FUNC(EL2FA(NN1+N)) = LBUF%DAM(II(IFUNC-10) + I)
                ENDDO
c-----------
              ELSEIF(IFUNC>=14.AND.IFUNC<=19)THEN
                DO I=LFT,LLT
                  N = I + NFT
                  IF(EL2FA(NN1+N)/=0)THEN
                    FUNC(EL2FA(NN1+N)) = GBUF%SIG(II(IFUNC-13) + I)
                  ENDIF
                ENDDO
c-----------
              ELSEIF(IFUNC>=20.AND.IFUNC<=24)THEN
                IUS = IFUNC - 20
                DO I=LFT,LLT
                  N = I + NFT
                  IF(EL2FA(NN1+N)/=0)THEN
                    IF ( (IUS + 1) < NUVAR) THEN
                      VALUE = MBUF%VAR(I+IUS*NEL)
                    ELSE
                      VALUE = ZERO
                    ENDIF
                    FUNC(EL2FA(NN1+N)) = VALUE
                  ENDIF
                ENDDO
c-----------
              ELSEIF(IFUNC == 25)THEN
                DO I=LFT,LLT
                  N = I + NFT
                  IF(EL2FA(NN1+N)/=0)THEN
C                 FUNC(EL2FA(NN1+N)) = EHOUR(N)
                    VALUE=0.
                    FUNC(EL2FA(NN1+N)) = VALUE
                  ENDIF
                ENDDO
c-----------
              ELSEIF(IFUNC == 3890) THEN

                NFAIL = ELBUF_TAB(NG)%BUFLY(1)%NFAIL
                DO IR=1,NFAIL
                  DFMAX=>
     .              ELBUF_TAB(NG)%BUFLY(1)%FAIL(1,1,1)%FLOC(IR)%DAMMX
                  DO I=LFT,LLT
                    N = I + NFT
                    FUNC(EL2FA(NN1+N)) = DFMAX(I)
                  ENDDO
                ENDDO
c-----------
              ELSEIF(IFUNC == 4892) THEN
                IALEL=IPARG(7,NG)+IPARG(11,NG)
                IF(IALEL == 0)THEN
                  EVAR(LFT:LLT) = ZERO
                ELSE
                  CALL OUTPUT_SCHLIEREN(
     1                           EVAR    ,IXQ    ,X           ,
     2                           IPARG   ,WA_L   ,ELBUF_TAB   ,ALE_CONNECTIVITY   ,GBUF%VOL,
     3                           NG      ,NIXQ   ,ITY)
                ENDIF
                FUNC(EL2FA(NN1+NFT+1:NN1+NFT+LLT)) = EVAR(1:LLT)
c-----------
              ELSEIF (IFUNC == 4893) THEN
                DO I=LFT,LLT
                  N = I + NFT
                  FUNC(EL2FA(NN1+N)) = ISPMD
                ENDDO
c-----------
              ELSEIF (IFUNC == 4895) THEN  !  /ANIM/ELEM/SIGEQ
C  equivalent stress -  (NON VON MISES / VON MISES)
                IF (GBUF%G_SEQ > 0) THEN  !  non VON MISES
                  DO I=LFT,LLT
                    N = I + NFT
                    IF (EL2FA(NN1+N) /= 0) THEN
                      FUNC(EL2FA(NN1+N)) = GBUF%SEQ(I)
                    ENDIF
                  ENDDO
                ELSE                      ! VON MISES
                  DO I=LFT,LLT
                    N = I + NFT
                    IF (EL2FA(NN1+N) /= 0) THEN
                      P = -(GBUF%SIG(II(1) + I)
     .                    + GBUF%SIG(II(2) + I)
     .                    + GBUF%SIG(II(3) + I)) * THIRD
                      S1=GBUF%SIG(II(1) + I) + P
                      S2=GBUF%SIG(II(2) + I) + P
                      S3=GBUF%SIG(II(3) + I) + P
                      VONM2= THREE*(GBUF%SIG(II(4) + I)**2 +
     .                              GBUF%SIG(II(5) + I)**2 +
     .                              GBUF%SIG(II(6) + I)**2 +
     .                        HALF*(S1*S1+S2*S2+S3*S3))
                      VONM= SQRT(VONM2)
                      FUNC(EL2FA(NN1+N)) = VONM
                    ENDIF
                  ENDDO
                ENDIF
c-----------
              ELSEIF(IFUNC == 4930 )THEN   !/ANIM/ELEM/TDET
                DO I=LFT,LLT
                  N = I + NFT
                  IF(EL2FA(NN1+N)/=0)THEN
                    FUNC(EL2FA(NN1+N)) = -GBUF%TB(I)
                  ENDIF
                ENDDO
              ELSEIF(IFUNC == 4937 )THEN   !/ANIM/ELEM/DT
                IF(GBUF%G_DT>0)THEN
                  DO I=LFT,LLT
                    N = I + NFT
                    IF(EL2FA(NN1+N)/=0)THEN
                      FUNC(EL2FA(NN1+N)) = GBUF%DT(I)
                    ENDIF
                  ENDDO
                ENDIF
c-----------
              ELSEIF(IFUNC>=4938 .AND. IFUNC<=4944)THEN
                IF(GBUF%G_MOM>0 )THEN
                  IF(IFUNC>=4938.AND.IFUNC<=4940)THEN
                    DO I=LFT,LLT
                      N = I + NFT
                      IF(EL2FA(NN1+N)/=0)THEN
                        FUNC(EL2FA(NN1+N)) =        GBUF%MOM( II((IFUNC-4937) + I ) )
                      ENDIF
                    ENDDO
                  ELSEIF(IFUNC==4941)THEN
                    DO I=LFT,LLT
                      N     = I + NFT
!                  IAD0  = (I-1)*3
                      IF(EL2FA(NN1+N)/=0)THEN
                        FUNC(EL2FA(NN1+N)) = SQRT( GBUF%MOM(II(1) + I)+GBUF%MOM(II(1) + I)
     +                                            +GBUF%MOM(II(2) + I)+GBUF%MOM(II(2) + I) )
                      ENDIF
                    ENDDO
                  ELSEIF(IFUNC==4942)THEN
                    DO I=LFT,LLT
                      N     = I + NFT
!                  IAD0  = (I-1)*3
                      IF(EL2FA(NN1+N)/=0)THEN
                        FUNC(EL2FA(NN1+N)) = SQRT( GBUF%MOM(II(2) + I)+GBUF%MOM(II(2) + I)
     +                                            +GBUF%MOM(II(3) + I)+GBUF%MOM(II(3) + I) )
                      ENDIF
                    ENDDO
                  ELSEIF(IFUNC==4943)THEN
                    DO I=LFT,LLT
                      N     = I + NFT
!                  IAD0  = (I-1)*3
                      IF(EL2FA(NN1+N)/=0)THEN
                        FUNC(EL2FA(NN1+N)) = SQRT( GBUF%MOM(II(1) + I)+GBUF%MOM(II(1) + I)
     +                                            +GBUF%MOM(II(3) + I)+GBUF%MOM(II(3) + I) )
                      ENDIF
                    ENDDO
                  ELSEIF(IFUNC==4944)THEN
                    DO I=LFT,LLT
                      N     = I + NFT
!                  IAD0  = (I-1)*3
                      IF(EL2FA(NN1+N)/=0)THEN
                        FUNC(EL2FA(NN1+N)) = SQRT( GBUF%MOM(II(1) + I)+GBUF%MOM(II(1) + I)
     +                                            +GBUF%MOM(II(2) + I)+GBUF%MOM(II(2) + I)
     +                                            +GBUF%MOM(II(3) + I)+GBUF%MOM(II(3) + I) )
                      ENDIF
                    ENDDO
                  ENDIF
                ENDIF
c-----------
              ELSEIF(IFUNC>=4945 .AND. IFUNC<=4951)THEN
                IF(GBUF%G_MOM>0 )THEN
                  IF(IFUNC>=4945.AND.IFUNC<=4947)THEN
                    DO I=LFT,LLT
                      N = I + NFT
                      IF(EL2FA(NN1+N)/=0)THEN
                        FUNC(EL2FA(NN1+N)) =        GBUF%MOM( II(IFUNC-4944) + I )           / GBUF%RHO(I)
                      ENDIF
                    ENDDO
                  ELSEIF(IFUNC==4948)THEN
                    DO I=LFT,LLT
                      N     = I + NFT
!                  IAD0  = (I-1)*3
                      IF(EL2FA(NN1+N)/=0)THEN
                        FUNC(EL2FA(NN1+N)) = SQRT( GBUF%MOM(II(1) + I)+GBUF%MOM(II(1) + I)
     +                                            +GBUF%MOM(II(2) + I)+GBUF%MOM(II(2) + I) )  / GBUF%RHO(I)
                      ENDIF
                    ENDDO
                  ELSEIF(IFUNC==4949)THEN
                    DO I=LFT,LLT
                      N     = I + NFT
!                  IAD0  = (I-1)*3
                      IF(EL2FA(NN1+N)/=0)THEN
                        FUNC(EL2FA(NN1+N)) = SQRT( GBUF%MOM(II(2) + I)+GBUF%MOM(II(2) + I)
     +                                            +GBUF%MOM(II(3) + I)+GBUF%MOM(II(3) + I) )  / GBUF%RHO(I)
                      ENDIF
                    ENDDO
                  ELSEIF(IFUNC==4950)THEN
                    DO I=LFT,LLT
                      N     = I + NFT
!                  IAD0  = (I-1)*3
                      IF(EL2FA(NN1+N)/=0)THEN
                        FUNC(EL2FA(NN1+N)) = SQRT( GBUF%MOM(II(1) + I)+GBUF%MOM(II(1) + I)
     +                                            +GBUF%MOM(II(3) + I)+GBUF%MOM(II(3) + I) )  / GBUF%RHO(I)
                      ENDIF
                    ENDDO
                  ELSEIF(IFUNC==4951)THEN
                    DO I=LFT,LLT
                      N     = I + NFT
!                  IAD0  = (I-1)*3
                      IF(EL2FA(NN1+N)/=0)THEN
                        FUNC(EL2FA(NN1+N)) = SQRT( GBUF%MOM(II(1) + I)+GBUF%MOM(II(1) + I)
     +                                            +GBUF%MOM(II(2) + I)+GBUF%MOM(II(2) + I)
     +                                            +GBUF%MOM(II(3) + I)+GBUF%MOM(II(3) + I) )  / GBUF%RHO(I)
                      ENDIF
                    ENDDO
                  ENDIF
                ENDIF
c-----------
                !/ANIM/ELEM/FINT || FINTX || FINTY || FINXY || FINYZ || FINXZ
              ELSEIF (IFUNC>=4952 .AND. IFUNC<=4958)THEN
                IALEFVM_FLG = ALEFVM_Param%IFORM
                IF(IALEFVM_FLG >= 2)THEN
                  IF(IFUNC>=4952 .AND. IFUNC<=4954)THEN
                    DO I=LFT,LLT
                      N      = I+NFT
                      IF(EL2FA(NN1+ N)/=0)THEN
                        FUNC(EL2FA(NN1+ N)) = ALEFVM_Buffer%FINT_CELL(IFUNC-4951, N)
                      ENDIF
                    ENDDO
                  ELSEIF(IFUNC==4955)THEN
                    DO I=LFT,LLT
                      N      = I+NFT
                      IF(EL2FA(NN1+ N)/=0)THEN
                        FUNC(EL2FA(NN1+ N)) = SQRT (  ALEFVM_Buffer%FINT_CELL(1, N)* ALEFVM_Buffer%FINT_CELL(1, N)+
     +                                                ALEFVM_Buffer%FINT_CELL(2, N)* ALEFVM_Buffer%FINT_CELL(2, N) )
                      ENDIF
                    ENDDO
                  ELSEIF(IFUNC==4956)THEN
                    DO I=LFT,LLT
                      N      = I+NFT
                      IF(EL2FA(NN1+ N)/=0)THEN
                        FUNC(EL2FA(NN1+ N)) = SQRT ( ALEFVM_Buffer%FINT_CELL(2, N)* ALEFVM_Buffer%FINT_CELL(2, N)+
     +                                               ALEFVM_Buffer%FINT_CELL(3, N)* ALEFVM_Buffer%FINT_CELL(3, N) )
                      ENDIF
                    ENDDO
                  ELSEIF(IFUNC==4957)THEN
                    DO I=LFT,LLT
                      N      = I+NFT
                      IF(EL2FA(NN1+ N)/=0)THEN
                        FUNC(EL2FA(NN1+ N)) = SQRT ( ALEFVM_Buffer%FINT_CELL(1, N)* ALEFVM_Buffer%FINT_CELL(1,N)+
     +                                               ALEFVM_Buffer%FINT_CELL(3, N)* ALEFVM_Buffer%FINT_CELL(3,N) )
                      ENDIF
                    ENDDO
                  ELSEIF(IFUNC==4958)THEN
                    DO I=LFT,LLT
                      N      = I+NFT
                      IF(EL2FA(NN1+ N)/=0)THEN
                        FUNC(EL2FA(NN1+ N)) = SQRT ( ALEFVM_Buffer%FINT_CELL(1, N)* ALEFVM_Buffer%FINT_CELL(1, N)+
     +                                               ALEFVM_Buffer%FINT_CELL(2, N)* ALEFVM_Buffer%FINT_CELL(2, N)+
     +                                               ALEFVM_Buffer%FINT_CELL(3, N)* ALEFVM_Buffer%FINT_CELL(3, N) )
                      ENDIF
                    ENDDO
                  ENDIF
                ELSE
                  EVAR(LFT:LLT)=ZERO
                ENDIF!IF(IALEFVM_FLG >= 2)
c-----------
              ELSEIF(IFUNC == 4959)THEN   !/ANIM/ELEM/AMS
                IF(GBUF%G_ISMS>0)THEN
                  DO I=LFT,LLT
                    N = I + NFT
                    IF(EL2FA(NN1+N)/=0)THEN
                      FUNC(EL2FA(NN1+N)) = GBUF%ISMS(I)
                    ENDIF
                  ENDDO
                ENDIF
c-----------OFF
              ELSEIF(IFUNC == 4965)THEN
                IF (GBUF%G_OFF > 0) THEN
                  DO I=LFT,LLT
                    N = I + NFT
                    IF(GBUF%OFF(I) > ONE) THEN
                      FUNC(EL2FA(NN1+N)) = GBUF%OFF(I) - ONE
                    ELSEIF((GBUF%OFF(I) >= ZERO .AND. GBUF%OFF(I) <= ONE)) THEN
                      FUNC(EL2FA(NN1+N)) = GBUF%OFF(I)
                    ELSE
                      FUNC(EL2FA(NN1+N)) = -ONE
                    ENDIF
                  ENDDO
                ENDIF
c-----------TILLOTSON
              ELSEIF(IFUNC == 5172)THEN   !/ANIM/ELEM/TILLOTSON
                DO I=LFT,NEL
                  EVAR(I) = ZERO
                ENDDO
                N=1+NFT
                IPRT=IPARTSP(N)
                MT=IPART(1,IPRT)
                IEOS=IPM(4,MT)
                IF(IEOS == 3)THEN
                  EBUF => ELBUF_TAB(NG)%BUFLY(1)%EOS(1,1,1)
                  NVAREOS = ELBUF_TAB(NG)%BUFLY(1)%NVAR_EOS
                  DO  I=1,NEL
                    EVAR(I) = EBUF%VAR(I)
                  ENDDO
                ENDIF
                FUNC(EL2FA(NN1+NFT+1:NN1+NFT+NEL)) = EVAR(1:NEL)
c-----------
              ELSE
                DO I=LFT,LLT
                  N = I + NFT
                  IF(EL2FA(NN1+N)/=0)THEN
                    FUNC(EL2FA(NN1+N)) = ZERO
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
          ELSE
          ENDIF
C-----------------------------------------------
C       FIN DE BOUCLE SUR LES GROUPES
C-----------------------------------------------
 900    CONTINUE
C-----------------------------------------------

        IF (NSPMD == 1) THEN
          DO N=1,NBF
            R4 = FUNC(N)
            CALL WRITE_R_C(R4,1)
          ENDDO
        ELSE
          DO N = 1, NBF
            WA(N) = FUNC(N)
          ENDDO

          IF (ISPMD == 0) THEN
            BUF = NUMSPHG
          ELSE
            BUF=1
          ENDIF

          CALL SPMD_R4GET_PARTN(1,NBF,NBPART,IADG,WA,BUF)
        ENDIF
c-----------
        IF(ALLOCATED(WA_L))DEALLOCATE(WA_L)
        RETURN
      END
