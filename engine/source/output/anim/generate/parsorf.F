Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  PARSORF                       source/output/anim/generate/parsorf.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_IGET_PARTN               source/mpi/anim/spmd_iget_partn.F
Chd|        SPMD_IGLOB_PARTN              source/mpi/anim/spmd_iglob_partn.F
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE PARSORF(ELBUF_TAB,
     .                   X      ,D     ,IADD   ,CDG   ,IPARG    ,
     .                   IXT    ,IXP   ,IXR    ,MATER ,EL2FA    ,
     .                   DD_IAD ,IADG  ,IPARTT ,IPARTP,IPARTR   ,
     .                   NFACPTX,IXEDGE,NODGLOB,NB1D  ,NANIM1D_L,
     .                   IPART  ,IGEO  ,IADG_TPR,SIADG)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X(*),D(*),CDG(*)
      INTEGER IADD(*),IPARG(NPARG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .        MATER(*),EL2FA(*),
     .        IADG(NSPMD,*),
     .        DD_IAD(NSPMD+1,*), 
     .        IPARTT(*),IPARTP(*),IPARTR(*),
     .        NFACPTX(3,*),IXEDGE(2,*),IPART(LIPART1,*),
     .        IGEO(NPROPGI,*)
      INTEGER NODGLOB(*),NB1D,NANIM1D_L,SIADG
      INTEGER, DIMENSION(NSPMD,*), INTENT(INOUT) :: IADG_TPR
C
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C     REAL
      my_real
     .   OFF
      INTEGER II(4),IE,NG, ITY, LFT, LLT, KPT, N, I, J, 
     .        IPRT, NEL, IAD, NPAR, NFT, IMID,IALEL,MTN,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,
     .        JJ,K,NAX1D
      INTEGER NP((NB1D+NANIM1D_L)*2),BUF,GBUF
C-----------------------------------------------
      IE = 0
C
      NN1 = 1
      NN2 = 1
      NN3 = 1
      NN4 = NN3 
      NN5 = NN4 
      NN6 = NN5 
      NN7 = NN6 + NUMELT
      NN8 = NN7 + NUMELP
      NN9 = NN8 + NUMELR
      NN10= NN9 
C-----------------------------------------------
      NPAR = 0
      JJ = 0
C-----------------------------------------------
C     PART
C-----------------------------------------------
C en spmd il faut envoyer l'info meme qd on a 0 elem local
      IF (NUMELP + NUMELT + NUMELR /= 0 .OR. NSPMD > 1) THEN
        DO IPRT=1,NPART
          IF (MATER(IPRT) /= 3) CYCLE
C        test scinde car NFACPTX(1,IPRT) peut etre "out of core".
          IF (NUMELXG > 0 . AND. IGEO(11,IPART(2,IPRT)) == 28) CYCLE
C
          NPAR = NPAR + 1
          DO NG=1,NGROUP
            MTN   =IPARG(1,NG)
            NEL   =IPARG(2,NG)
            NFT   =IPARG(3,NG)
            IAD   =IPARG(4,NG)
            ITY   =IPARG(5,NG)
            LFT=1
            LLT=NEL
C-----------------------------------------------
C       TRUSS
C-----------------------------------------------
            IF (ITY == 4) THEN
              DO I=LFT,LLT
                N = I + NFT
                IF (IPARTT(N) /= IPRT) CYCLE
                OFF = ELBUF_TAB(NG)%GBUF%OFF(I)
                IF (NSPMD == 1) THEN
                  II(1) = IXT(2,N)-1
                  II(2) = IXT(3,N)-1
                  CALL WRITE_I_C(II,2)
                ELSE
                  NP(JJ+1) = NODGLOB(IXT(2,N))-1
                  NP(JJ+2) = NODGLOB(IXT(3,N))-1
                ENDIF
                IE = IE + 1
                EL2FA(NN6+N) = IE
                JJ = JJ + 2
              ENDDO ! DO I=LFT,LLT
C-----------------------------------------------
C       POUTRES
C-----------------------------------------------
            ELSEIF (ITY == 5) THEN
              DO I=LFT,LLT
                N = I + NFT
                IF (IPARTP(N) /= IPRT) CYCLE
                  OFF = ELBUF_TAB(NG)%GBUF%OFF(I)
                  IF (NSPMD == 1) THEN
                    II(1) = IXP(2,N)-1
                    II(2) = IXP(3,N)-1
                    CALL WRITE_I_C(II,2)
                  ELSE
                    NP(JJ+1) = NODGLOB(IXP(2,N))-1
                    NP(JJ+2) = NODGLOB(IXP(3,N))-1
                  ENDIF
                  IE = IE + 1
                  EL2FA(NN7+N) = IE
                  JJ = JJ + 2
                ENDDO ! DO I=LFT,LLT
C-----------------------------------------------
C       RESSORTS
C-----------------------------------------------
            ELSEIF (ITY == 6) THEN
              DO I=LFT,LLT
                N = I + NFT
                IF (IPARTR(N) /= IPRT) CYCLE
                OFF = ELBUF_TAB(NG)%GBUF%OFF(I)
                IF (NSPMD == 1) THEN
                  II(1) = IXR(2,N)-1
                  II(2) = IXR(3,N)-1
                  CALL WRITE_I_C(II,2)
                ELSE
                  NP(JJ+1) = NODGLOB(IXR(2,N))-1
                  NP(JJ+2) = NODGLOB(IXR(3,N))-1
                ENDIF
                IE = IE + 1
                EL2FA(NN8+N) = IE
                JJ = JJ + 2
                IF (MTN == 3) THEN
                  IF (NSPMD == 1) THEN
                    II(1) = IXR(3,N)-1
                    II(2) = IXR(4,N)-1
                    CALL WRITE_I_C(II,2)
                  ELSE
                    NP(JJ+1) = NODGLOB(IXR(3,N))-1
                    NP(JJ+2) = NODGLOB(IXR(4,N))-1
                  ENDIF
                  IE = IE + 1
C                  EL2FA(NN8+N) = IE
                  JJ = JJ + 2
                ENDIF ! IF (MTN == 3)
              ENDDO ! DO I=LFT,LLT
            ENDIF ! IF (ITY)
          ENDDO ! DO NG=1,NGROUP
C-----------------------------------------------
C       PART ADRESS
C-----------------------------------------------
          IADD(NPAR) = IE
        ENDDO ! DO IPRT=1,NPART
      ENDIF ! IF (NUMELP + NUMELT + NUMELR /= 0 .OR. NSPMD > 1)
C-----------------------------------------------
C X-ELEMENTS PARTS ARE WRITTEN AFTER ALL (BUT RBODIES) 1D PARTS.
C-----------------------------------------------
      IF (NANIM1D > 0) THEN
        NAX1D=0
        DO IPRT=1,NPART
          IF (MATER(IPRT) /= 3) CYCLE
          IF (IGEO(11,IPART(2,IPRT)) == 28) THEN
            NPAR = NPAR + 1
            DO J=1,NFACPTX(1,IPRT)
              IF (NSPMD == 1) THEN
                II(1)=IXEDGE(1,NAX1D+J)-1
                II(2)=IXEDGE(2,NAX1D+J)-1
                CALL WRITE_I_C(II,2)
              ELSE
                NP(JJ+1)=NODGLOB(IXEDGE(1,NAX1D+J))-1
                NP(JJ+2)=NODGLOB(IXEDGE(2,NAX1D+J))-1
              ENDIF
              JJ = JJ+2
            ENDDO
            NAX1D=NAX1D+NFACPTX(1,IPRT)
            IE=IE+NFACPTX(1,IPRT)
            IADD(NPAR)=-IE
          ENDIF ! IF (IGEO(11,IPART(2,IPRT)) == 28)
        ENDDO ! DO IPRT=1,NPART
      ENDIF ! IF (NANIM1D > 0)
C-----------------------------------------------

      IF (NSPMD > 1) THEN
        IF (ISPMD == 0) THEN
          GBUF = NPART
          BUF = (NB1DG+NANIM1D)*2
        ELSE
          GBUF = 1
          BUF = 1
        ENDIF
        IF(SIADG>0) IADG_TPR(1:NSPMD,1:NPAR) = 0
C
        CALL SPMD_IGLOB_PARTN(IADD,NPAR,IADG,GBUF)
        IF(SIADG>0) THEN
        IF(ISPMD==0) THEN
!       ----------------
        DO K=1,NSPMD
!         ----------------
          N = 1
          IF(IADG(K,N)<=0) THEN
              IADG_TPR(K,N) = 0
          ELSE
             IADG_TPR(K,N) = IADG(K,N)
          ENDIF
          IADG(K,N) = ABS(IADG(K,N))
!         ----------------          
          DO N = 2, NPAR
            IF(IADG(K,N)<=0) THEN
                IADG_TPR(K,N) = IADG_TPR(K,N-1)
            ELSE
                IADG_TPR(K,N) = IADG(K,N)
            ENDIF
            IADG(K,N) = ABS(IADG(K,N))
          ENDDO
!         ----------------
        ENDDO
!       ----------------
        ENDIF
        ENDIF

        CALL SPMD_IGET_PARTN(2,JJ,NP,NPAR,IADG,BUF,1)
      ELSE ! remplissage IADG pour compatibilite mono/multi
        IF(SIADG>0) THEN
        DO I = 1, NPART
          IF(IADG(1,I)<=0) THEN
              IADG_TPR(1,I) = 0
          ELSE
             IADG_TPR(1,I) = IADD(I)
          ENDIF
          IADG(1,I) = ABS(IADD(I))
        END DO
        ENDIF
      ENDIF ! IF (NSPMD > 1)



C-----------------------------------------------
      RETURN
      END
