Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  DELNUMB0                      source/output/anim/generate/delnumb0.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_IGET_PARTN               source/mpi/anim/spmd_iget_partn.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE DELNUMB0(IPARG   ,EL2FA   ,NBF     ,INUM  ,DD_IAD,
     .                    IADD    ,NBPART  ,IADG    ,KXSP  )
C=======================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),EL2FA(*),INUM(*),
     .        NBF,IADG(NSPMD,*),KXSP(NISP,*)
      INTEGER DD_IAD(NSPMD+1,*), IADD(*), NBPART
C-----------------------------------------------
      INTEGER NG, ITY, LFT, LLT, N, I, NEL, IAD, NFT,
     .        NN1, NN2,BUF
C-----------------------------------------------
C
      NN1 = 1
      NN2 = NN1 + NUMSPH+MAXPJET
C
      DO 490 NG=1,NGROUP
        NEL   =IPARG(2,NG)
        NFT   =IPARG(3,NG)
        IAD   =IPARG(4,NG)
        ITY   =IPARG(5,NG)
        LFT=1
        LLT=NEL
C-----------------------------------------------
        IF(ITY==51)THEN
C-----------------------------------------------
C         PARTICULES SPH.
C-----------------------------------------------
          DO I=LFT,LLT
           N = I + NFT
           IF(EL2FA(NN1+N)/=0)THEN
               INUM(EL2FA(NN1+N)) = KXSP(NISP,N)
           ENDIF
          ENDDO
        ELSE
        ENDIF
 490  CONTINUE
C-----------------------------------------------
      IF (NSPMD == 1) THEN
        CALL WRITE_I_C(INUM,NBF)
      ELSE
        IF (ISPMD==0) THEN
          BUF = NUMSPHG
        ELSE 
          BUF = 1
        ENDIF

        CALL SPMD_IGET_PARTN(1,NBF,INUM,NBPART,IADG,BUF,1)

      ENDIF
C-----------------------------------------------
C
      RETURN
      END
