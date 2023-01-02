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
Chd|  DELNUMBC_PLY                  source/output/anim/generate/delnumbc_ply.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_IGET_PARTN_PLY           source/mpi/anim/spmd_iget_partn_ply.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        GET_PLY_ELEM                  source/output/anim/generate/anin_ply_init.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE DELNUMBC_PLY(IPLY,NEL_PLY,IPARG,
     2                  IXC ,IXTG ,INVERT,
     3                  EL2FA,NBF  ,INUM ,NELCUT,
     4                  IXUR ,DD_IAD  ,IADD ,NBF_L,
     5                  NBPART,IADG,NODGLOB,IDCMAX,
     6                  NBF_PXFEMG)
      USE PLYXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),IPARG(NPARG,*),
     .         EL2FA(*),INUM(*),IXUR(NIXUR,*),
     .        INVERT(*), NBF,NELCUT,NBPART,IADG(NSPMD,*)
      INTEGER DD_IAD(NSPMD+1,*), IADD(*), NBF_L,IPLY,
     .        NEL_PLY,NBF_PXFEMG
C-----------------------------------------------
C     REAL
      my_real
     .   OFF
      INTEGER II,IE,NG, ITY, LFT, LLT, N, I, J,
     .        IPID, NEL, IAD, NFT, IMID,IALEL,
     .        NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,BUF,NODGLOB(*),
     .        IDCMAX, IDCMAXL,ISHPLYXFEM,ID_IE,IDPLY,NPT,
     .        IPT,ELC,PLYELEMS(NUMELC),PLYS,RBUF,NELCOUNT,ELEM

      INTEGER, DIMENSION(:), ALLOCATABLE :: INUMG

      INTEGER GET_PLY_ELEM
      EXTERNAL GET_PLY_ELEM
C-----------------------------------------------
      NELCOUNT = 0
      DO PLYS = 1,NPLYPART
         IPLY = INDX_PLY(PLYS)

         PLYELEMS=0
         DO I=1,PLYSHELL(IPLY)%PLYNUMSHELL
           IPT = PLYSHELL(IPLY)%SHELLIPT(I)
           ELC = PLYSHELL(IPLY)%SHID(I)
           PLYELEMS(ELC)=IPT
         ENDDO
C
        IDPLY  = IDCMAX + NEL_PLY
        II = 0
        DO 490 NG=1,NGROUP
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG)
          IAD   =IPARG(4,NG)
          ITY   =IPARG(5,NG)
          ISHPLYXFEM  = IPARG(50,NG)
          LFT=1
          LLT=NEL
C-----------------------------------------------
C       QUAD
C-----------------------------------------------
          IF(ISHPLYXFEM > 0) THEN
C-----------------------------------------------
C       COQUES
C-----------------------------------------------
            IF(ITY==3)THEN
              DO I=LFT,LLT
                N = I + NFT
                IF(PLYELEMS(N) > 0) THEN
                  II = II + 1
                    ELEM = GET_PLY_ELEM(IPLY,N)
                    INUM(EL2FA(NEL_PLY + II)) =
     *                             PLYSHELL(IPLY)%SHELLID(ELEM)
                ENDIF
              ENDDO
C-----------------------------------------------
C       COQUES 3 NOEUDS
C-----------------------------------------------
            ELSEIF(ITY==7)THEN
              DO I=LFT,LLT
cc             N = I + NFT
cc             INUM(EL2FA(NN5+N)) = IXTG(NIXTG,N)
              ENDDO
C-----------------------------------------------
C       RNUR
C-----------------------------------------------
            ENDIF
          ENDIF
 490    CONTINUE
C-----------------------------------------------
        NEL_PLY = NEL_PLY + PLYSHELL(IPLY)%PLYNUMSHELL
      ENDDO

      IF (NSPMD == 1) THEN
        DO I=1,NBF
          INUM(I) = INUM(I) + IDCMAX
        ENDDO
        CALL WRITE_I_C(INUM,NBF)

        IDCMAX=0
        DO I=1,NBF
           IDCMAX=MAX(IDCMAX,INUM(I))
        ENDDO

      ELSE
C
        IF (ISPMD==0) THEN
          RBUF = NBF_PXFEMG
          CALL SPMD_IGET_PARTN_PLY(1,NBF_L,INUM,NPLYPART,IADG,RBUF,1,
     *                             IDCMAX)
        ELSE
          RBUF = 1
          CALL SPMD_IGET_PARTN_PLY(1,NBF_L,INUM,NPLYPART,IADG,RBUF,1,
     *                             IDCMAX)
        END IF
      ENDIF

C-----------------------------------------------
C
      RETURN
      END
