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
Chd|  OUTP_S_S                      source/output/sty/outp_s_s.F
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F
Chd|        SPMD_RGATHER9_1COMM           source/mpi/interfaces/spmd_outp.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F
Chd|====================================================================
      SUBROUTINE OUTP_S_S(NBX     ,KEY     ,TEXT   ,ELBUF_TAB,
     .                    IPARG   ,EANI    ,IXS    ,IPM      ,DD_IAD ,
     .                    SIZLOC  ,SIZP0   ,SIZ_WR)
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
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        CHARACTER*10 KEY
        CHARACTER*40 TEXT
        INTEGER NBX
        INTEGER IPARG(NPARG,*), DD_IAD(NSPMD+1,*),
     .          IXS(NIXS,*),IPM(NPROPMI,*),SIZLOC,SIZP0,SIZ_WR
        my_real
     .     EANI(*)
        TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,II(6),JJ,NBB(20),RESP0,WRTLEN,RES
        INTEGER NG, NEL, IADD,N,MLW,
     .          JJ_OLD, NGF, NGL, NN, LEN,NLAY, NUVAR, NPTT, NPTS,
     .          LIAD, IUS, ISOLNOD, IPT,IL,IR,IS,IT, MLW2, NPTG,
     .          MT, NPTR,K, NPT1,COMPTEUR,L
        INTEGER, DIMENSION(NSPGROUP) :: JJ_LOC
        INTEGER, DIMENSION(NSPGROUP+1,NSPMD) :: ADRESS
        my_real
     .     WA(SIZLOC),WAP0(SIZ_WR),WAP0_LOC(SIZP0)
        my_real
     .     FUNC(6),S1 ,S2 ,S3 ,P ,VONM2, USER(200)
        TYPE(BUF_LAY_) ,POINTER :: BUFLY
        TYPE(L_BUFEL_) ,POINTER :: LBUF
        TYPE(G_BUFEL_) ,POINTER :: GBUF
        TYPE(BUF_MAT_) ,POINTER :: MBUF
C=======================================================================
        IF (ISPMD == 0) THEN
          WRITE(IUGEO,'(2A)')'/SOLID     /SCALAR    /',KEY
          WRITE(IUGEO,'(A)')TEXT
          IF (OUTYY_FMT == 2) THEN
            WRITE(IUGEO,'(A)')'#FORMAT: (1P6E12.5) (VAR(I),I=1,NUMSOL)'
          ELSE
            WRITE(IUGEO,'(A)')'#FORMAT: (1P6E20.13) (VAR(I),I=1,NUMSOL)'
          ENDIF
        ENDIF
C
        JJ_OLD = 0
        RESP0=0
        NGF = 1
        NGL = 0
        JJ = 0
        COMPTEUR = 0
        DO NN=1,NSPGROUP
          NGL = NGL + DD_IAD(ISPMD+1,NN)
          DO NG = NGF, NGL
            ITY   =IPARG(5,NG)
            IF (ITY /= 1 .AND. ITY /= 2) CYCLE
            ISOLNOD = IABS(IPARG(28,NG))
            NUVAR = 0
            CALL INITBUF(IPARG    ,NG      ,
     2           MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,
     3           NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4           JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5           NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6           IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7           ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
c
            BUFLY=> ELBUF_TAB(NG)%BUFLY(1)
            GBUF => ELBUF_TAB(NG)%GBUF
            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
            NLAY = ELBUF_TAB(NG)%NLAY
            NPTR = ELBUF_TAB(NG)%NPTR
            NPTS = ELBUF_TAB(NG)%NPTS
            NPTT = ELBUF_TAB(NG)%NPTT
            NPT  = NPTR * NPTS * NPTT * NLAY
            LFT=1
            LLT=NEL
!
            DO I=1,6
              II(I) = NEL*(I-1)
            ENDDO
!
c-------------------------------
            IF(NBX == 2)THEN
              DO I=LFT,LLT
                JJ = JJ + 1
                N = I + NFT
                WA(JJ) = - (GBUF%SIG(II(1)+I)
     .                   +  GBUF%SIG(II(2)+I)
     .                   +  GBUF%SIG(II(3)+I)) / THREE
              ENDDO
c-------------------------------
            ELSEIF(NBX == -2)THEN
              DO I=LFT,LLT
                JJ = JJ + 1
                N = I + NFT
                P = - (GBUF%SIG(II(1)+I)
     .              +  GBUF%SIG(II(2)+I)
     .              +  GBUF%SIG(II(3)+I)) / THREE
                S1 = GBUF%SIG(II(1)+I) + P
                S2 = GBUF%SIG(II(2)+I) + P
                S3 = GBUF%SIG(II(3)+I) + P
                VONM2 = THREE*(GBUF%SIG(II(4)+I)**2 +
     .                         GBUF%SIG(II(5)+I)**2 +
     .                         GBUF%SIG(II(6)+I)**2 +
     .                 HALF*(S1*S1+S2*S2+S3*S3))
                WA(JJ)= SQRT(VONM2)
              ENDDO
c-------------------------------
            ELSEIF(NBX>=20.AND.NBX<=24) THEN
C            variable user 1:5
              IF(MLW>=28) THEN
                DO I=LFT,LLT
                  NUVAR  = MAX(NUVAR,IPM(8,IXS(1,I+NFT)))
                ENDDO
                IUS = NBX - 20
                DO I=LFT,LLT
                  JJ = JJ + 1
                  N = I + NFT
                  USER(I) = ZERO
                  DO IL=1,NLAY
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        DO IT=1,NPTT
                          MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)
                          IF (NUVAR>IUS) USER(I) = USER(I) +
     .                    MBUF%VAR(IUS*NEL+I)/NPT
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                  WA(JJ) = USER(I)
                ENDDO
              ELSE   ! non user laws
                DO I=LFT,LLT
                  JJ = JJ + 1
                  N = I + NFT
                  WA(JJ)= ZERO
                ENDDO
              ENDIF
c-------------------------------
            ELSEIF (NBX == 26) THEN
              IF (MLW >= 28) THEN
                DO I=LFT,LLT
                  NUVAR  = MAX(NUVAR,IPM(8,IXS(1,I+NFT)))
                ENDDO
c
                DO I=LFT,LLT
                  WA(JJ+ 1 ) = ISOLNOD
                  WA(JJ+ 2 ) = NPT
                  WA(JJ+ 3 ) = NUVAR
                  WA(JJ+ 4 ) = IABS(JHBE)
                  JJ = JJ + 4
                  N = I + NFT
                  DO IL=1,NLAY
                    DO IR=1,NPTR
                      DO IS=1,NPTS
                        DO IT=1,NPTT
                          MBUF => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)
                          DO IUS = 1,NUVAR
                            JJ = JJ +1
                            WA(JJ) = MBUF%VAR(IUS + (I-1)*NUVAR)
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO
c
              ELSE  ! Lois non user
                DO I=LFT,LLT
                  WA(JJ+ 1 ) = ISOLNOD
                  WA(JJ+ 2 ) = NPT
                  WA(JJ+ 3 ) = NUVAR
                  WA(JJ+ 4 ) = IABS(JHBE)
                  JJ = JJ + 4
                ENDDO
              ENDIF
c-------------------------------
            ELSEIF(NBX == 25)THEN
              DO I=LFT,LLT
                JJ = JJ +1
                WA(JJ)=EANI(NFT + I)
              ENDDO
c-------------------------------
            ELSEIF (NBX == 1) THEN
              DO I=LFT,LLT
                JJ = JJ + 1
                WA(JJ)=GBUF%OFF(I)
              ENDDO
c-------------------------------
            ELSEIF (NBX == 3) THEN
              DO I=LFT,LLT
                JJ = JJ + 1
                WA(JJ)=GBUF%EINT(I)
              ENDDO
c-------------------------------
            ELSEIF (NBX == 4) THEN
              DO I=LFT,LLT
                JJ = JJ + 1
                WA(JJ)=GBUF%RHO(I)
              ENDDO
c-------------------------------
            ELSEIF (NBX == 10) THEN
              IF (BUFLY%L_PLA == 0) THEN
                DO I=LFT,LLT
                  JJ = JJ + 1
                  WA(JJ)=ZERO
                ENDDO
              ELSE
                DO I=LFT,LLT
                  JJ = JJ + 1
                  WA(JJ)=LBUF%PLA(I)
                ENDDO
              ENDIF
c-------------------------------
            ELSEIF (NBX == 11) THEN
              IF (BUFLY%L_TEMP == 0) THEN
                DO I=LFT,LLT
                  JJ = JJ + 1
                  WA(JJ)=ZERO
                ENDDO
              ELSE
                DO I=LFT,LLT
                  JJ = JJ + 1
                  WA(JJ)=GBUF%TEMP(I)
                ENDDO
              ENDIF
c-------------------------------
            ELSEIF (NBX == 27) THEN
C  equivalent stress - other then VON MISES
              IF (GBUF%G_SEQ > 0) THEN
                DO I=LFT,LLT
                  JJ = JJ + 1
                  WA(JJ) = GBUF%SEQ(I)
                ENDDO
              ELSE  ! VON MISES
                DO I=LFT,LLT
                  JJ = JJ + 1
                  N = I + NFT
                  P = - (GBUF%SIG(II(1)+I)
     .                +  GBUF%SIG(II(2)+I)
     .                +  GBUF%SIG(II(3)+I)) / THREE
                  S1 = GBUF%SIG(II(1)+I) + P
                  S2 = GBUF%SIG(II(2)+I) + P
                  S3 = GBUF%SIG(II(3)+I) + P
                  VONM2 = THREE*(GBUF%SIG(II(4)+I)**2 +
     .                           GBUF%SIG(II(5)+I)**2 +
     .                           GBUF%SIG(II(6)+I)**2 +
     .                   HALF*(S1*S1+S2*S2+S3*S3))
                  WA(JJ)= SQRT(VONM2)
                ENDDO
              ENDIF
            ENDIF
c-------------------------------
          ENDDO
c-------------------------------
          NGF = NGL + 1
          JJ_LOC(NN) = JJ - COMPTEUR
          COMPTEUR = JJ
        ENDDO ! fin nspgroup
!     ++++++++++
        IF( NSPMD>1 ) THEN
          CALL SPMD_RGATHER9_1COMM(WA,JJ,JJ_LOC,WAP0_LOC,SIZP0,ADRESS)
        ELSE
          WAP0_LOC(1:JJ) = WA(1:JJ)
          ADRESS(1,1) = 1
          DO NN = 2,NSPGROUP+1
            ADRESS(NN,1) = JJ_LOC(NN-1) + ADRESS(NN-1,1)
          ENDDO
        ENDIF
!     ++++++++++
        IF(ISPMD==0) THEN
          RESP0 = 0

          DO NN=1,NSPGROUP
            COMPTEUR = 0
            DO K = 1,NSPMD
              IF((ADRESS(NN+1,K)-ADRESS(NN,K)-1)>=0) THEN
                DO L = ADRESS(NN,K),ADRESS(NN+1,K)-1
                  COMPTEUR = COMPTEUR + 1
                  WAP0(COMPTEUR+RESP0) = WAP0_LOC(L)
                ENDDO  ! l=... , ...
              ENDIF   !if(size_loc>0)
            ENDDO    ! k=1,nspmd

            JJ_OLD = COMPTEUR+RESP0
            IF(JJ_OLD>0) THEN
              IF( NBX == 26) THEN
                J = 0
                DO WHILE(J<JJ_OLD)
                  ISOLNOD= NINT(WAP0(J + 1))
                  NPT    = NINT(WAP0(J + 2))
                  NUVAR  = NINT(WAP0(J + 3))
                  JHBE   = NINT(WAP0(J + 4))
                  J = J + 4
                  IF (OUTYY_FMT == 2) THEN
                    WRITE(IUGEO,'(4I8)') ISOLNOD,NPT,NUVAR,JHBE
                  ELSE
                    WRITE(IUGEO,'(4I10)')ISOLNOD,NPT,NUVAR,JHBE
                  ENDIF
                  IF (NUVAR/=0) THEN
                    DO I = 1,NPT
                      IF(OUTYY_FMT == 2)THEN
                        WRITE(IUGEO,'(1P6E12.5)')(WAP0(J + K),K=1,NUVAR)
                      ELSE
                        WRITE(IUGEO,'(1P6E20.13)')(WAP0(J + K),K=1,NUVAR)
                      ENDIF
                      J = J + NUVAR
                    ENDDO
                  ENDIF
                ENDDO
              ELSE
                RES=MOD(JJ_OLD,6)
                WRTLEN=JJ_OLD-RES
                IF (WRTLEN>0) THEN
                  IF (OUTYY_FMT == 2) THEN
                    WRITE(IUGEO,'(1P6E12.5)')(WAP0(J),J=1,WRTLEN)
                  ELSE
                    WRITE(IUGEO,'(1P6E20.13)')(WAP0(J),J=1,WRTLEN)
                  ENDIF
                ENDIF
                DO I=1,RES
                  WAP0(I)=WAP0(WRTLEN+I)
                ENDDO
                RESP0=RES
              ENDIF    ! nbx= 26
            ENDIF    ! JJ_OLD>0
          ENDDO      ! nn=1,nspgroup
c
          IF (RESP0>0) THEN
            IF (OUTYY_FMT == 2) THEN
              WRITE(IUGEO,'(1P6E12.5)')(WAP0(J),J=1,RESP0)
            ELSE
              WRITE(IUGEO,'(1P6E20.13)')(WAP0(J),J=1,RESP0)
            ENDIF
          ENDIF
        ENDIF       ! ispmd = 0
c-----------
        RETURN
      END
Chd|====================================================================
Chd|  COUNT_ARSZ_SS                 source/output/sty/outp_s_s.F
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F
Chd|        OUTP_ARSZ_SS                  source/mpi/interfaces/spmd_outp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COUNT_ARSZ_SS(IPARG,DD_IAD,IPM,IXS,WASZ,SIZ_WRITE_LOC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "param_c.inc"
#include "com01_c.inc"
#include "scr16_c.inc"
#include "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*),WASZ(2),
     .   IXS(NIXS,*),IPM(NPROPMI,*),SIZ_WRITE_LOC(2*NSPGROUP+2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NN,NG,SZP0(NSPGROUP),RSZP0(NSPGROUP),NGF,NGL,JJ,
     .          WASZ26,P0ARS26,MLW,NEL,NPT,JHBE,ISOLNOD,
     .          NUVAR,I,LFT,LLT,NFT,ITY,WASZ1
C-----------------------------------------------
        WASZ1 = 0

        IF ( OUTP_SS(1)  == 1.OR.OUTP_SS(2)  == 1.OR.OUTP_SS(3)  == 1
     .   .OR.OUTP_SS(4)  == 1.OR.OUTP_SS(5)  == 1.OR.OUTP_SS(6)  == 1
     .   .OR.OUTP_SS(7)  == 1.OR.OUTP_SS(25) == 1.OR.OUTP_SS(20) == 1
     .   .OR.OUTP_SS(21) == 1.OR.OUTP_SS(22) == 1.OR.OUTP_SS(23) == 1
     .   .OR.OUTP_SS(24) == 1 ) THEN

          NGF = 1
          NGL = 0
          DO NN=1,NSPGROUP
            JJ = 0
            NGL = NGL + DD_IAD(ISPMD+1,NN)
            DO NG = NGF,NGL
              NEL = IPARG(2,NG)
              JJ = JJ + NEL
            ENDDO
            WASZ1 = WASZ1+JJ
            NGF = NGL + 1
            SIZ_WRITE_LOC(NN) = JJ
          ENDDO

        ENDIF

        WASZ26 = 0
        IF (OUTP_SS(26) == 1) THEN
          NGF = 1
          NGL = 0
          DO NN=1,NSPGROUP
            JJ = 0
            NGL = NGL + DD_IAD(ISPMD+1,NN)
            DO NG = NGF,NGL
              ITY = IPARG(5,NG)
              IF (ITY /= 1 .and. ITY /= 2) CYCLE
              MLW   =IPARG(1,NG)
              NEL = IPARG(2,NG)
              NFT   =IPARG(3,NG)
              NPT  = IABS(IPARG(6,NG))
              JHBE = IPARG(23, NG)
              ISOLNOD = IABS(IPARG(28,NG))
              LFT=1
              LLT=NEL
              NUVAR = 0
              IF (MLW >= 28) THEN

                DO I=LFT,LLT
                  NUVAR  = MAX(NUVAR,IPM(8,IXS(1,I+NFT)))
                ENDDO

                IF(ISOLNOD == 16.OR.ISOLNOD == 20.OR.
     .            (ISOLNOD == 8.AND.JHBE == 14))THEN
                  JJ = JJ+ (NUVAR*NPT+4)*NEL
                ELSEIF(ISOLNOD == 10.OR.((ISOLNOD == 6.OR.ISOLNOD == 8).
     .               AND.JHBE == 15).OR.JHBE == 12)THEN
                  JJ = JJ + (NUVAR*ABS(NPT)+4)*NEL
                ELSEIF(NPT > 1)THEN
                  JJ = JJ + (NUVAR*NPT+4)*NEL
                ELSE
                  JJ = JJ + (NUVAR+4)*NEL
                ENDIF
              ELSE
cc  pour d'autres type de lois materiaux
                JJ = JJ + 4 * NEL
              ENDIF

            ENDDO
            WASZ26 = WASZ26+JJ
            NGF = NGL + 1
            SIZ_WRITE_LOC(NSPGROUP+NN) = JJ
          ENDDO
        END IF
        WASZ(1) = WASZ1
        WASZ(2) = WASZ26
        DO I=1,2
          SIZ_WRITE_LOC(2*NSPGROUP+I) = WASZ(I)
        ENDDO

        RETURN
      END
