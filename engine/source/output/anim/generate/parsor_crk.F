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
Chd|  PARSOR_CRK                    source/output/anim/generate/parsor_crk.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_IGET_PARTN               source/mpi/anim/spmd_iget_partn.F
Chd|        SPMD_IGLOB_PARTN              source/mpi/anim/spmd_iglob_partn.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE PARSOR_CRK(IPARG  ,IXC     ,IXTG  ,EL2FA,IDCRK   ,
     .                      IAD_CRK,IAD_CRKG,NBF_L ,NBF  ,IEL_CRK ,
     .                      NODGLOBXFE,INDX_CRK,ITAB     )
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),IPARG(NPARG,NGROUP),IEL_CRK(*),
     .        EL2FA(*),IDCRK,IAD_CRK(*),IAD_CRKG(NSPMD,*),INDX_CRK(*),
     .        NBF_L,NBF,NODGLOBXFE(*),ITAB(*)
C-----------------------------------------------
C     REAL
      my_real OFF
      INTEGER IE,NG,ITY,LFT,LLT,N,I,J,NEL,NFT,JJ,K,BUF,IXFEM,
     .        IPT,ELC,CRKS,ICRK,IE_EL2FA,NEL_CRK,EMPL,NLEVXF
      INTEGER II(4),NP(NBF_L*4)
C=======================================================================
C     CRK ELEMS INIT
C-----------------------------------------------
      NEL_CRK = 0
      JJ = 0
      IE = 0
C-----------------------------------------------
      DO CRKS = 1,NCRKPART
        ICRK = INDX_CRK(CRKS)
        IE_EL2FA = 0
C
C en spmd il faut envoyer l'info meme qd on a 0 elem local
C
        DO NG=1,NGROUP
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG)
          ITY   =IPARG(5,NG)
          IXFEM =IPARG(54,NG)
          LFT=1
          LLT=NEL
          NLEVXF = IPARG(65,NG)
C-----------------------------------------------
C       COQUES
C-----------------------------------------------
          IF (IXFEM > 0) THEN
           IF(IXFEM == 2 .AND. ICRK > NLEVXF) CYCLE
            IF (ITY == 3) THEN
              DO I=LFT,LLT
                N = I + NFT
                IF (IEL_CRK(N) == 0) CYCLE
                IE = IE + 1
                IE_EL2FA = IE_EL2FA + 1
                II(1) = CRKSHELL(ICRK)%XNODEL(1,IE_EL2FA)
                II(2) = CRKSHELL(ICRK)%XNODEL(2,IE_EL2FA)
                II(3) = CRKSHELL(ICRK)%XNODEL(3,IE_EL2FA)
                II(4) = CRKSHELL(ICRK)%XNODEL(4,IE_EL2FA)
c                print*,'ICRK,ID=',ICRK,CRKSHELL(ICRK)%CRKSHELLID(IE_EL2FA)
c                print*,'  NODES=',ITAB(II(1)),ITAB(II(2)),ITAB(II(3)),ITAB(II(4))
                IF (NSPMD == 1) THEN
                  II(1) = II(1)-1+IDCRK
                  II(2) = II(2)-1+IDCRK
                  II(3) = II(3)-1+IDCRK
                  II(4) = II(4)-1+IDCRK
                  CALL WRITE_I_C(II,4)
                ELSE
                  NP(JJ+1) = NODGLOBXFE(II(1))-1+IDCRK
                  NP(JJ+2) = NODGLOBXFE(II(2))-1+IDCRK
                  NP(JJ+3) = NODGLOBXFE(II(3))-1+IDCRK
                  NP(JJ+4) = NODGLOBXFE(II(4))-1+IDCRK
                ENDIF
                EL2FA(NEL_CRK+IE_EL2FA) = IE 
                JJ = JJ + 4
              ENDDO
C-----------------------------------------------
C             COQUES 3 NOEUDS
C-----------------------------------------------
            ELSEIF (ITY == 7) THEN
              DO I=LFT,LLT
                N = I + NFT
                IF (IEL_CRK(N+NUMELC) == 0) CYCLE
                IE = IE + 1
                IE_EL2FA = IE_EL2FA + 1
                II(1) = CRKSHELL(ICRK)%XNODEL(1,IE_EL2FA)
                II(2) = CRKSHELL(ICRK)%XNODEL(2,IE_EL2FA)
                II(3) = CRKSHELL(ICRK)%XNODEL(3,IE_EL2FA)
                II(4) = CRKSHELL(ICRK)%XNODEL(3,IE_EL2FA)
c                II(4) = CRKSHELL(ICRK)%XNODEL(4,IE_EL2FA)
                IF(NSPMD == 1)THEN
                  II(1) = II(1)-1+IDCRK
                  II(2) = II(2)-1+IDCRK
                  II(3) = II(3)-1+IDCRK
                  II(4) = II(4)-1+IDCRK
                  CALL WRITE_I_C(II,4)
                ELSE
                  NP(JJ+1) = NODGLOBXFE(II(1))-1+IDCRK
                  NP(JJ+2) = NODGLOBXFE(II(2))-1+IDCRK
                  NP(JJ+3) = NODGLOBXFE(II(3))-1+IDCRK
                  NP(JJ+4) = NODGLOBXFE(II(3))-1+IDCRK
c                  NP(JJ+4) = NODGLOBXFE(II(4))-1+IDCRK
                ENDIF
                EL2FA(NEL_CRK+IE_EL2FA) = IE
                JJ = JJ + 4
              ENDDO
            ENDIF  !  IF (ITY == 3)
          ENDIF  !  IF (IXFEM == 1 .OR. IXFEM == 2)
        ENDDO  !  DO NG=1,NGROUP
C
        IAD_CRK(ICRK) = IE
        NEL_CRK = NEL_CRK + CRKSHELL(ICRK)%CRKNUMSHELL
      ENDDO  !  DO CRKS = 1,NCRKPART
c----------------------------------------
      IF (NSPMD > 1) THEN
Cel construction tableau global des parts sur p0
        IF(ISPMD == 0)THEN
          CALL SPMD_IGLOB_PARTN(IAD_CRK,NCRKPART,IAD_CRKG,NCRKPART)
          BUF = 4 * NBF
          CALL SPMD_IGET_PARTN(4,JJ,NP,NCRKPART,IAD_CRKG,BUF,1)
        ELSE
          CALL SPMD_IGLOB_PARTN(IAD_CRK,NCRKPART,IAD_CRKG,1)
          BUF=1
          CALL SPMD_IGET_PARTN(4,JJ,NP,NCRKPART,IAD_CRKG,BUF,1)
        ENDIF
C
      ELSE ! remplissage IADG pour compatibilite mono/multi
        DO I = 1, NCRKPART
          IAD_CRKG(1,I) = IAD_CRK(I)
        END DO
      ENDIF
C-----------
      RETURN
      END
