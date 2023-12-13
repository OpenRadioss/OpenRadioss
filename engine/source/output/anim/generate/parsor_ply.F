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
Chd|  PARSOR_PLY                    source/output/anim/generate/parsor_ply.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_IGET_PARTN_PLY           source/mpi/anim/spmd_iget_partn_ply.F
Chd|        SPMD_IGLOB_PARTN              source/mpi/anim/spmd_iglob_partn.F
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        GET_PLY_NOD                   source/output/anim/generate/anin_ply_init.F
Chd|        GET_PLY_NODID                 source/output/anim/generate/anin_ply_init.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE PARSOR_PLY(NEL_PLY, X,       D,       XNORM,  CDG,
     .                      IPARG,   IXC,     IXTG,    INVERT, EL2FA,
     .                      MATER,   IPARTC,  NODGLOB, IDPLY,  IADPLY,
     .                      IADPLYG, PLYNUMC, NBF_PXFEMG)
      USE PLYXFEM_MOD
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X(*),D(*),XNORM(3,*),CDG(*)
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),IPARG(NPARG,*),
     .        INVERT(*), EL2FA(*),MATER(*),
     .        IPARTC(*),NODGLOB(*),
     .        NEL_PLY,IDPLY,IADPLY(*),IADPLYG(*),PLYNUMC,
     .        NBF_PXFEMG
C-----------------------------------------------
      INTEGER GET_PLY_NODID
      EXTERNAL GET_PLY_NODID
      INTEGER GET_PLY_NOD
      EXTERNAL GET_PLY_NOD
C-----------------------------------------------
C     REAL
      my_real
     * OFF
      INTEGER II(4),IE,NG, ITY, LFT, LLT, N, I, J, 
     .        IPRT, NEL, IAD, NPAR, NFT, IMID,IALEL,MTN,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,
     .        JJ, K, SH_IH,BUF,ISHPLYXFEM,
     .        PLYELEMS(NUMELC),IPT,ELC,PLYS,IPLY,IE_EL2FA,
     .        NPCC,IDPLYN
      INTEGER NP(PLYNUMC*4),EMPL
C-----------------------------------------------
C     PLY ELEMS INIT
C-----------------------------------------------
      NPCC = 0
      DO PLYS = 1,NPLYPART
        IPLY = INDX_PLY(PLYS)
        NPCC = NPCC + PLYSHELL(IPLY)%PLYNUMSHELL
      ENDDO

      JJ = 0
      NEL_PLY = 0
      IE=0
      DO PLYS = 1,NPLYPART
        IPLY = INDX_PLY(PLYS)
        PLYELEMS=0
        DO I=1,PLYSHELL(IPLY)%PLYNUMSHELL
           IPT = PLYSHELL(IPLY)%SHELLIPT(I)
           ELC = PLYSHELL(IPLY)%SHID(I)
           PLYELEMS(ELC)=IPT
        ENDDO
C-----------------------------------------------
        IE_EL2FA=0
        NPAR = 0
C
C en spmd il faut envoyer l'info meme qd on a 0 elem local
         DO 490 NG=1,NGROUP
          MTN   =IPARG(1,NG)
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG)
          IAD   =IPARG(4,NG)
          ITY   =IPARG(5,NG)
          ISHPLYXFEM = IPARG(50,NG)
          LFT=1
          LLT=NEL
cccc        KPT    =IPARG(6,NG)    
C-----------------------------------------------
C       COQUES
C-----------------------------------------------
          IF(ISHPLYXFEM > 0) THEN
             IF(ITY==3)THEN
c               KPT   =IPARG(6,NG)
c               IHBE = IPARG(23,NG)
c               ISTRAIN=IPARG(44,NG)
c               IEXPAN=IPARG(49,NG)
c               CALL CBUFAD (IAD-1,NEL,ISTRAIN,KPT,IHBE,0,0,0,IEXPAN,
c     .                      IPARG(1,NG))
                DO 130 I=LFT,LLT
                N = I + NFT
                IF(PLYELEMS(N) == 0)GOTO 130
cc                OFF = BUFEL(IAD+I+SH_IH*NEL-1)
                   II(1) = IXC(2,N)
                   II(2) = IXC(3,N)
                   II(3) = IXC(4,N)
                   II(4) = IXC(5,N)
                   IE = IE + 1
                   IE_EL2FA = IE_EL2FA + 1
c                  CALL FACNOR(X,D,II,XNORM,CDG,INVERT(IE))
                   IF (NSPMD == 1) THEN
                     II(1) = GET_PLY_NODID(IPLY,II(1),IDPLY)- 1
                     II(2) = GET_PLY_NODID(IPLY,II(2),IDPLY)- 1
                     II(3) = GET_PLY_NODID(IPLY,II(3),IDPLY)- 1
                     II(4) = GET_PLY_NODID(IPLY,II(4),IDPLY)- 1
                     CALL WRITE_I_C(II,4)
                   ELSE
                     EMPL = GET_PLY_NOD(IPLY,II(1))
                     NP(JJ+1) = PLYNOD(IPLY)%PLYNODID(EMPL)

                     EMPL = GET_PLY_NOD(IPLY,II(2))
                     NP(JJ+2) = PLYNOD(IPLY)%PLYNODID(EMPL)

                     EMPL = GET_PLY_NOD(IPLY,II(3))
                     NP(JJ+3) = PLYNOD(IPLY)%PLYNODID(EMPL)

                     EMPL = GET_PLY_NOD(IPLY,II(4))
                     NP(JJ+4) = PLYNOD(IPLY)%PLYNODID(EMPL)

                   END IF
                   EL2FA(NEL_PLY+IE_EL2FA) = IE 
                   JJ = JJ + 4
 130           CONTINUE
C-----------------------------------------------
C             COQUES 3 NOEUDS
C-----------------------------------------------
              ELSEIF(ITY==7)THEN
              ENDIF
          ENDIF
 490   CONTINUE
C

        IADPLY(PLYS) = IE
        NEL_PLY = NEL_PLY + PLYSHELL(IPLY)%PLYNUMSHELL
      ENDDO

      IF (NSPMD > 1) THEN
C   construction tableau global des parts sur p0

        IF (ISPMD==0) THEN
         CALL SPMD_IGLOB_PARTN(IADPLY,NPLYPART,IADPLYG,NPLYPART)
         BUF = NBF_PXFEMG*4
         IDPLYN = IDPLY - 1
         CALL SPMD_IGET_PARTN_PLY(4,JJ,NP,NPLYPART,IADPLYG,BUF,1,IDPLYN)
        ELSE
         CALL SPMD_IGLOB_PARTN(IADPLY,NPLYPART,IADPLYG,NPLYPART)
         BUF=1
         IDPLYN = IDPLY - 1
         CALL SPMD_IGET_PARTN_PLY(4,JJ,NP,NPLYPART,IADPLYG,BUF,1,IDPLYN)
        ENDIF

      ENDIF
C-----------------------------------------------
      RETURN
      END
