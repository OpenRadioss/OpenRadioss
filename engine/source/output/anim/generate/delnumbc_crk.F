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
Chd|  DELNUMBC_CRK                  source/output/anim/generate/delnumbc_crk.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_IGET_PARTN               source/mpi/anim/spmd_iget_partn.F
Chd|        SPMD_MAX_XFE_I                source/mpi/elements/spmd_xfem.F
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE DELNUMBC_CRK(IPARG  ,IEL_CRK ,INUM    ,IDCMAX ,
     .                        EL2FA  ,IAD_CRKG,NBF_L   ,NBF    ,INDX_CRK )
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),EL2FA(*),IAD_CRKG(NSPMD,*),IEL_CRK(*),
     .   INDX_CRK(*),INUM(*)
      INTEGER NBF_L,NBF
C-----------------------------------------------
      INTEGER I,N,II,IE,NG,ITY,NEL,NFT,IXFEM,IDCMAX,CRKS,RBUF,ELEM,ICRK,
     .   NEL_CRK,NLEVXF
      my_real OFF
C=======================================================================
c
      IF (NSPMD > 1) CALL SPMD_MAX_XFE_I(IDCMAX)
C-----------------------------------------------
C
      NEL_CRK = 0
      DO CRKS= 1,NCRKPART
        ICRK = INDX_CRK(CRKS)
        II   = 0
        DO NG=1,NGROUP
          IXFEM  = IPARG(54,NG)
          NLEVXF = IPARG(65,NG)
          IF (IXFEM > 0) THEN
            IF (IXFEM == 2 .AND. ICRK > NLEVXF) CYCLE
            NEL = IPARG(2,NG)   
            NFT = IPARG(3,NG)   
            ITY = IPARG(5,NG)   
C-----------------------------------------------
C           COQUES 4 N
C-----------------------------------------------
            IF (ITY == 3) THEN
              DO I=1,NEL
                N = I + NFT
                ELEM = IEL_CRK(N)
                IF (IEL_CRK(N) > 0) THEN
                  II = II + 1
                  INUM(EL2FA(NEL_CRK + II)) =
     *                       CRKSHELL(ICRK)%CRKSHELLID(ELEM)+IDCMAX
                ENDIF
              ENDDO
C-----------------------------------------------
C            COQUES 3 NOEUDS
C-----------------------------------------------
            ELSEIF (ITY == 7) THEN
              DO I=1,NEL
                N = I + NFT
                ELEM = IEL_CRK(N+NUMELC)
                IF(IEL_CRK(N+NUMELC) > 0) THEN
                  II = II + 1
                  INUM(EL2FA(NEL_CRK + II)) =
     *                   CRKSHELL(ICRK)%CRKSHELLID(ELEM+ECRKXFEC)+IDCMAX
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO ! NG=1,NGROUP
C-----
        NEL_CRK = NEL_CRK + CRKSHELL(ICRK)%CRKNUMSHELL
      ENDDO ! DO CRKS = 1,NCRKPART
C-----------------------------------------------
      IF (NSPMD == 1) THEN
        CALL WRITE_I_C(INUM,NBF)
        IDCMAX = 0
        DO I=1,NBF
          IDCMAX = MAX(IDCMAX,INUM(I))
        ENDDO
      ELSE
        IF (ISPMD == 0) THEN
          RBUF = NBF
        ELSE
          RBUF = 1
        END IF
        CALL SPMD_IGET_PARTN(1,NBF_L,INUM,NCRKPART,IAD_CRKG,RBUF,1)
      ENDIF
C-----------
      RETURN
      END
