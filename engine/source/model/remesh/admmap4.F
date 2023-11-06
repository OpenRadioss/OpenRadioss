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
Chd|  ADMMAP4                       source/model/remesh/admmap4.F
Chd|-- called by -----------
Chd|        ADMDIV                        source/model/remesh/admdiv.F
Chd|        ADMINI                        source/model/remesh/admini.F
Chd|        ADMREGUL                      source/model/remesh/admregul.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        REMESH_MOD                    share/modules/remesh_mod.F
Chd|====================================================================
      SUBROUTINE ADMMAP4(N      ,IXC    ,X      ,IPARG  ,ELBUF_TAB,
     .                   IGEO   ,IPM    ,SH4TREE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE REMESH_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER N, IXC(NIXC,*), IPARG(NPARG,*),
     .          IGEO(NPROPGI,*), IPM(NPROPMI,*), SH4TREE(KSH4TREE,*)
        my_real
     .          X(3,*)
        TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER IB,M,N1,N2,N3,N4,IR,IS,IT,IL,IPT,NPTR,NPTS,NPTT,NLAY,
     .     I,J,K,II,JJ,I1,IG,NG,NG1,NEL1,NFT1,MLW,NEL,ISTRA,
     .     IEXPAN,IH,LENS,LENM,LENF,NPTM,
     .     PTF,PTM,PTE,PTP,PTS,QTF,QTM,QTE,QTP,QTS,KK(12),KK1(12)
        my_real
     .     NX,NY,NZ,STOT,X13,Y13,Z13,X24,Y24,Z24,ZZ
        my_real
     .     QPG(2,4),S2WAKE(4),SK(2),ST(2),MK(2),MT(2),
     .     SHK(2),SHT(2),Z01(11,11)
        TYPE(G_BUFEL_) ,POINTER :: GBUFS,GBUFT
        TYPE(L_BUFEL_) ,POINTER :: LBUFS,LBUFT
        TYPE(BUF_LAY_) ,POINTER :: BUFLY
C---------------------------
        DATA QPG/-0.5,-0.5,
     .            0.5,-0.5,
     .            0.5, 0.5,
     .           -0.5, 0.5/
        DATA  Z01/
     1   0.       ,0.       ,0.       ,0.       ,0.       ,
     1   0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     2   -.5      ,0.5      ,0.       ,0.       ,0.       ,
     2   0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     3   -.5      ,0.       ,0.5      ,0.       ,0.       ,
     3   0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     4   -.5      ,-.1666667,0.1666667,0.5      ,0.       ,
     4   0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     5   -.5      ,-.25     ,0.       ,0.25     ,0.5      ,
     5   0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     6   -.5      ,-.3      ,-.1      ,0.1      ,0.3      ,
     6   0.5      ,0.       ,0.       ,0.       ,0.       ,0.       ,
     7   -.5      ,-.3333333,-.1666667,0.0      ,0.1666667,
     7   0.3333333,0.5      ,0.       ,0.       ,0.       ,0.       ,
     8   -.5      ,-.3571429,-.2142857,-.0714286,0.0714286,
     8   0.2142857,0.3571429,0.5      ,0.       ,0.       ,0.       ,
     9   -.5      ,-.375    ,-.25     ,-.125    ,0.0      ,
     9   0.125    ,0.25     ,0.375    ,0.5      ,0.       ,0.       ,
     A   -.5      ,-.3888889,-.2777778,-.1666667,0.0555555,
     A   0.0555555,0.1666667,0.2777778,0.3888889,0.5      ,0.       ,
     B   -.5      ,-.4      ,-.3      ,-.2      ,-.1      ,
     B   0.       ,0.1      ,0.2      ,0.3      ,0.4      ,0.5      /
C-----------------------------------------------
        STOT=ZERO
        DO IB=1,4

          M = SH4TREE(2,N)+IB-1
          N1 = IXC(2,M)
          N2 = IXC(3,M)
          N3 = IXC(4,M)
          N4 = IXC(5,M)

          X13 = X(1,N3) - X(1,N1)
          Y13 = X(2,N3) - X(2,N1)
          Z13 = X(3,N3) - X(3,N1)

          X24 = X(1,N4) - X(1,N2)
          Y24 = X(2,N4) - X(2,N2)
          Z24 = X(3,N4) - X(3,N2)

          NX = Y13*Z24 - Z13*Y24
          NY = Z13*X24 - X13*Z24
          NZ = X13*Y24 - Y13*X24

          S2WAKE(IB)=SQRT(NX*NX+NY*NY+NZ*NZ)
          STOT=STOT+S2WAKE(IB)

        END DO
C-----------------------------------------------
        NG  =SH4TREE(4,N)
        MLW = IPARG(1,NG)
C     IF (MLW == 0) GOTO 250
C---
        NEL  = IPARG(2,NG)
        NFT  = IPARG(3,NG)
        NPT  = IPARG(6,NG)
        ISTRA= IPARG(44,NG)
        JHBE = IPARG(23,NG)
        IGTYP= IPARG(38,NG)
        IEXPAN=IPARG(49,NG)
        I = N-NFT

        NPTM = MAX(1,NPT)
        GBUFS => ELBUF_TAB(NG)%GBUF
        NLAY  = ELBUF_TAB(NG)%NLAY
        NPTR  = ELBUF_TAB(NG)%NPTR
        NPTS  = ELBUF_TAB(NG)%NPTS
        NPTT  = ELBUF_TAB(NG)%NPTT
!
        DO K=1,12  ! length max of GBUF%G_HOURG = 12
          KK(K)  = NEL *(K-1)
        ENDDO
!
c----------------------------------------------
        DO IB=1,4

          M  = SH4TREE(2,N)+IB-1
          NG1= SH4TREE(4,M)

          NEL1   = IPARG(2,NG1)
          NFT1   = IPARG(3,NG1)
          I1   = M-NFT1
          GBUFT => ELBUF_TAB(NG1)%GBUF
!
          DO K=1,12  ! length max of GBUF%G_HOURG = 12
            KK1(K) = NEL1*(K-1)
          ENDDO
!
c----
          IF (JHBE == 11) THEN    ! Batoz
c----
            GBUFT%THK(I1) = GBUFS%THK(I) !thk
c         ener totale approximation
            GBUFT%EINT(I1)      = GBUFS%EINT(I)*S2WAKE(IB)/STOT
            GBUFT%EINT(I1+NEL1) = GBUFS%EINT(I+NEL)*S2WAKE(IB)/STOT
c
            GBUFT%OFF(I1) = GBUFS%OFF(I)
c
            IF (GBUFT%G_EPSD > 0) THEN
              GBUFT%EPSD(I1) = GBUFS%EPSD(I) ! eps_dot
            ENDIF
c
            IF (ISTRA > 0) THEN
              DO K=1,8 ! deformations
                GBUFT%STRA(KK1(K)+I1)=GBUFS%STRA(KK(K)+I)
              END DO
            END IF
c
            IF (IEXPAN /= 0) THEN
              GBUFT%TEMP(I1)=GBUFS%TEMP(I)
            END IF
c
c         pla
c
            IF (GBUFT%G_PLA > 0) THEN
              DO IL=1,NLAY
                DO IR=1,NPTR
                  DO IS=1,NPTS
                    DO IT=1,NPTT
                      ELBUF_TAB(NG1)%BUFLY(IL)%LBUF(IR,IS,IT)%PLA(I1) =
     .                ELBUF_TAB(NG) %BUFLY(IL)%LBUF(IR,IS,IT)%PLA(I)
                    END DO
                  END DO
                END DO
              END DO
            ENDIF
c
c         Stress
c
            DO IL=1,NLAY
              DO IR=1,NPTR
                DO IS=1,NPTS
                  DO IT=1,NPTT
                    LBUFT => ELBUF_TAB(NG1)%BUFLY(IL)%LBUF(IR,IS,IT)
                    LBUFS => ELBUF_TAB(NG )%BUFLY(IL)%LBUF(IR,IS,IT)
                    LBUFT%SIG(KK1(1)+I1) = LBUFS%SIG(KK(1)+I)
                    LBUFT%SIG(KK1(2)+I1) = LBUFS%SIG(KK(2)+I)
                    LBUFT%SIG(KK1(3)+I1) = LBUFS%SIG(KK(3)+I)
                    LBUFT%SIG(KK1(4)+I1) = LBUFS%SIG(KK(4)+I)
                    LBUFT%SIG(KK1(5)+I1) = LBUFS%SIG(KK(5)+I)
                  END DO
                END DO
              END DO
            END DO
c
c         Uvar
c
            IF (MLW>=28 .AND. MLW/=32) THEN
              DO IL=1,NLAY
                DO IR=1,NPTR
                  DO IS=1,NPTS
                    DO IT=1,NPTT
                      DO K=1,ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT
                        ELBUF_TAB(NG1)%BUFLY(IL)%MAT(IR,IS,IT)%VAR(NEL1*(K-1)+I1)=
     .                  ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR(NEL*(K-1)+I)
                      END DO
                    END DO
                  END DO
                END DO
              END DO
            END IF
c
            LENF = NEL*5
            LENM = NEL*3
            LENS = NEL*8
            PTF  = 5*NEL*(IB-1)
            PTM  = 3*NEL*(IB-1)
            DO IR=1,NPTR
              DO IS=1,NPTS
                IG  = NPTR*(IS-1) + IR
                QTF = 5*NEL1*(IG-1)
                QTM = 3*NEL1*(IG-1)
                GBUFT%FORPG(QTF+KK1(1)+I1)=GBUFS%FORPG(PTF+KK(1)+I)
                GBUFT%FORPG(QTF+KK1(2)+I1)=GBUFS%FORPG(PTF+KK(2)+I)
                GBUFT%FORPG(QTF+KK1(3)+I1)=GBUFS%FORPG(PTF+KK(3)+I)
                GBUFT%FORPG(QTF+KK1(4)+I1)=GBUFS%FORPG(PTF+KK(4)+I)
                GBUFT%FORPG(QTF+KK1(5)+I1)=GBUFS%FORPG(PTF+KK(5)+I)
!
                GBUFT%MOMPG(QTM+KK1(1)+I1)=GBUFS%MOMPG(PTM+KK(1)+I)
                GBUFT%MOMPG(QTM+KK1(2)+I1)=GBUFS%MOMPG(PTM+KK(2)+I)
                GBUFT%MOMPG(QTM+KK1(3)+I1)=GBUFS%MOMPG(PTM+KK(3)+I)
              ENDDO
            ENDDO
c         end Batoz
c----
          ELSE   !Q4 & QEPH
c----
            GBUFT%FOR(KK1(1)+I1) = GBUFS%FOR(KK(1)+I)
            GBUFT%FOR(KK1(2)+I1) = GBUFS%FOR(KK(2)+I)
            GBUFT%FOR(KK1(3)+I1) = GBUFS%FOR(KK(3)+I)
            GBUFT%FOR(KK1(4)+I1) = GBUFS%FOR(KK(4)+I)
            GBUFT%FOR(KK1(5)+I1) = GBUFS%FOR(KK(5)+I)
!
            GBUFT%MOM(KK1(1)+I1) = GBUFS%MOM(KK(1)+I)
            GBUFT%MOM(KK1(2)+I1) = GBUFS%MOM(KK(2)+I)
            GBUFT%MOM(KK1(3)+I1) = GBUFS%MOM(KK(3)+I)
c
            GBUFT%THK(I1) = GBUFS%THK(I) !thk
c
            IF (JHBE == 22 .OR. JHBE == 23) THEN
              IH = (I-1)*12
              ST(1) =  GBUFS%HOURG(KK(1)+I)
              ST(2) = -GBUFS%HOURG(KK(2)+I)
              MT(1) =  GBUFS%HOURG(KK(3)+I)
              MT(2) = -GBUFS%HOURG(KK(4)+I)
              SK(1) = -GBUFS%HOURG(KK(7)+I)
              SK(2) =  GBUFS%HOURG(KK(8)+I)
              MK(1) = -GBUFS%HOURG(KK(9)+I)
              MK(2) =  GBUFS%HOURG(KK(10)+I)
              SHT(1)=  GBUFS%HOURG(KK(5)+I)
              SHT(2)= -GBUFS%HOURG(KK(6)+I)
              SHK(1)= -GBUFS%HOURG(KK(11)+I)
              SHK(2)=  GBUFS%HOURG(KK(12)+I)

              IF (NPT==0) THEN
                GBUFT%FOR(KK1(1)+I1) = GBUFT%FOR(KK1(1)+I1)
     .          + ST(1)*QPG(2,IB)+SK(1)*QPG(1,IB)
                GBUFT%FOR(KK1(2)+I1) = GBUFT%FOR(KK1(2)+I1)
     .          + ST(2)*QPG(2,IB)+SK(2)*QPG(1,IB)
c             GBUFT%FOR(KK1(3)+I1) = GBUFT%FOR(KK1(3)+I1)
                GBUFT%FOR(KK1(4)+I1) = GBUFT%FOR(KK1(4)+I1)
     .          + SHT(2)*QPG(2,IB)+SHK(2)*QPG(1,IB)
                GBUFT%FOR(KK1(5)+I1) = GBUFT%FOR(KK1(5)+I1)
     .          + SHT(1)*QPG(2,IB)+SHK(1)*QPG(1,IB)
!
                GBUFT%MOM(KK1(1)+I1) = GBUFT%MOM(KK1(1)+I1)
     .          + MT(1)*QPG(2,IB)+MK(1)*QPG(1,IB)
                GBUFT%MOM(KK1(2)+I1) = GBUFT%MOM(KK1(2)+I1)
     .          + MT(2)*QPG(2,IB)+MK(2)*QPG(1,IB)
c             GBUFT%MOM(KK1(3)+I1) = GBUFT%MOM(KK1(3)+I1)
              ELSE
                CONTINUE
              END IF
c---
              DO K=1,12 ! hour
                GBUFT%HOURG(KK1(K)+I1) = ZERO
              END DO
c
            ELSE  !  JHBE
              DO K=1,5 ! hour
                GBUFT%HOURG(KK1(K)+I1) = GBUFS%HOURG(KK(K)+I)
              END DO
            END IF
c         ener totale approximation
            GBUFT%EINT(I1)     = GBUFS%EINT(I)*S2WAKE(IB)/STOT
            GBUFT%EINT(I1+NEL1) = GBUFS%EINT(I+NEL)*S2WAKE(IB)/STOT
c
            GBUFT%OFF(I1) = GBUFS%OFF(I)
            IF (GBUFT%G_EPSD > 0) THEN
              GBUFT%EPSD(I1) = GBUFS%EPSD(I) ! eps_dot
            ENDIF
            IF (IEXPAN/=0) THEN
              GBUFT%TEMP(I1) = GBUFS%TEMP(I)
            END IF
c
            IF (ISTRA > 0) THEN
              DO K=1,8 ! deformations
                GBUFT%STRA(KK1(K)+I1)=GBUFS%STRA(KK(K)+I)
              END DO
            END IF
c
c         Stress
c
            DO IL=1,NLAY
              DO IT=1,NPTT
                LBUFT => ELBUF_TAB(NG1)%BUFLY(IL)%LBUF(1,1,IT)
                LBUFS => ELBUF_TAB(NG )%BUFLY(IL)%LBUF(1,1,IT)
                LBUFT%SIG(KK1(1)+I1) = LBUFS%SIG(KK(1)+I)
                LBUFT%SIG(KK1(2)+I1) = LBUFS%SIG(KK(2)+I)
                LBUFT%SIG(KK1(3)+I1) = LBUFS%SIG(KK(3)+I)
                LBUFT%SIG(KK1(4)+I1) = LBUFS%SIG(KK(4)+I)
                LBUFT%SIG(KK1(5)+I1) = LBUFS%SIG(KK(5)+I)
              END DO
            END DO
c
            IF (JHBE == 22 .OR. JHBE == 23) THEN
              DO IL=1,NLAY
                DO IT=1,NPTT
                  LBUFT => ELBUF_TAB(NG1)%BUFLY(IL)%LBUF(1,1,IT)
                  IPT = IL*IT
                  ZZ = GBUFT%THK(I1)*Z01(IPT,NPT)
                  LBUFT%SIG(KK1(1)+I1) = LBUFT%SIG(KK1(1)+I1)
     .                            + (ST(1)+ZZ*MT(1))*QPG(2,IB)
     .                            + (SK(1)+ZZ*MK(1))*QPG(1,IB)
                  LBUFT%SIG(KK1(2)+I1) = LBUFT%SIG(KK1(2)+I1)
     .                            + (ST(2)+ZZ*MT(2))*QPG(2,IB)
     .            + (SK(2)+ZZ*MK(2))*QPG(1,IB)
C               LBUFT%SIG(KK1(3)+I1) = LBUFT%SIG(KK1(3)+I1)
                  LBUFT%SIG(KK1(4)+I1) = LBUFT%SIG(KK1(4)+I1)
     .                            + SHT(2)*QPG(2,IB) + SHK(2)*QPG(1,IB)
                  LBUFT%SIG(KK1(5)+I1) = LBUFT%SIG(KK1(5)+I1)
     .                            + SHT(1)*QPG(2,IB) + SHK(1)*QPG(1,IB)
                END DO
              END DO
            END IF
c
c         pla
c
            IF (GBUFT%G_PLA > 0) THEN
              DO IL=1,NLAY
                DO IT=1,NPTT
                  ELBUF_TAB(NG1)%BUFLY(IL)%LBUF(1,1,IT)%PLA(I1) =
     .            ELBUF_TAB(NG )%BUFLY(IL)%LBUF(1,1,IT)%PLA(I)
                END DO
              END DO
            ENDIF
c
C         uvar
c
            IF (MLW>=28 .AND. MLW/=32) THEN
              DO IL=1,NLAY
                DO IT=1,NPTT
                  DO K=1,ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT
                    ELBUF_TAB(NG1)%BUFLY(IL)%MAT(1,1,IT)%VAR(NEL1*(K-1)+I1)=
     .              ELBUF_TAB(NG )%BUFLY(IL)%MAT(1,1,IT)%VAR(NEL*(K-1)+I)
                  END DO
                END DO
              END DO
            END IF
c
C----    end Q4 & QEPH
C
          END IF
        END DO   !  IB=1,4
c---------------------------------------------
c     reset source element variables
c---------------------------------------------
        GBUFS%OFF(I)    =-ABS(GBUFS%OFF(I))
!
        GBUFS%FOR(KK(1)+I) = ZERO
        GBUFS%FOR(KK(2)+I) = ZERO
        GBUFS%FOR(KK(3)+I) = ZERO
        GBUFS%FOR(KK(4)+I) = ZERO
        GBUFS%FOR(KK(5)+I) = ZERO
!
        GBUFS%MOM(KK(1)+I) = ZERO
        GBUFS%MOM(KK(2)+I) = ZERO
        GBUFS%MOM(KK(3)+I) = ZERO
        GBUFS%EINT(I) = ZERO
        GBUFS%EINT(I+NEL) = ZERO
        IF (GBUFS%G_EPSD > 0) GBUFS%EPSD(I) = ZERO
        IF (ISTRA > 0) THEN ! deformations
          DO K=1,8
            GBUFS%STRA(KK(K)+I) = ZERO
          END DO
        END IF
c
        DO IR=1,NPTR
          DO IS=1,NPTS
            DO IL=1,NLAY
              DO IT=1,NPTT
                DO K=1,5
                  ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)%SIG(KK(K)+I)=ZERO
                ENDDO
              END DO
            END DO
          END DO
        END DO
c
c     sig moyen
!      IF (NLAY > 1) THEN
!        DO IL=1,NLAY
!          BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
!          DO K=1,5
!            BUFLY%SIGPT(K)=ZERO
!          END DO
!        END DO
!      ELSE
!        BUFLY => ELBUF_TAB(NG)%BUFLY(1)
!        DO IPT=1,NPT
!          II = (IPT-1)*NEL*5
!          DO K=1,5
!            BUFLY%SIGPT(II+K)=ZERO
!          END DO
!        END DO
!      ENDIF
!c
!      IF (JHBE==11) THEN
!        DO IR=1,NPTR
!          DO IS=1,NPTS
!            IG = NPTR*(IS-1) + IR
!            PTF = (IG-1)*NEL*5
!            JJ  = 5*(I-1)
!            DO K=1,5 ! for
!              GBUFS%FORPG(PTF+KK(K)+I) = ZERO
!            END DO
!            PTM = (IG-1)*NEL*3
!            JJ  = 3*(I-1)
!            DO K=1,3 ! mom
!              GBUFS%MOMPG(PTM+KK(K)+I) = ZERO
!            END DO
!
!            IF (ISTRA /= 0) THEN
!              PTE = 8*NEL*(IG-1)
!              JJ  = 8*(I-1)
!              DO K=1,8 ! deformations
!                GBUFS%STRPG(PTE+KK(K)+I) = ZERO
!              END DO
!            END IF
!          END DO
!        END DO
!      END IF
c
C-----------
        RETURN
      END



