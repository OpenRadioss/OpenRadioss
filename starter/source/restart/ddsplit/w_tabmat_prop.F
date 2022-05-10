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
C
Chd|====================================================================
Chd|  W_TABMAT_PROP                 source/restart/ddsplit/w_tabmat_prop.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        MID_PID_MOD                   share/modules1/mid_pid_mod.F  
Chd|====================================================================
      SUBROUTINE W_TABMAT_PROP(IPARG,IXC,IXTG,IXS,PROC,NGROUP_L,
     .                         POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,
     .                         MID_PID_SHELL,MID_PID_TRI,MID_PID_SOL,
     .                         IPARTC,IPARTG,IPARTS)
        USE MID_PID_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(*), INTENT(IN) :: IPARTC,IPARTG,IPARTS
      INTEGER, DIMENSION(2,NPART), INTENT(IN) :: POIN_PART_SHELL,POIN_PART_TRI
      INTEGER, DIMENSION(2,NPART,7), INTENT(IN) :: POIN_PART_SOL
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT), INTENT(IN) :: MID_PID_SHELL,MID_PID_TRI
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT,7), INTENT(IN) :: MID_PID_SOL
      INTEGER IPARG(NPARG,*)
      INTEGER IXC(NIXC,NUMELC),IXTG(NIXTG,NUMELTG),IXS(NIXS,NUMELS)
      INTEGER NGROUP_L,PROC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real TAB_MAT(NGROUP_L)
      INTEGER NG_L,NG,P,MID,PID,NFT,ISOL,INDI,ITY,ILAW
      INTEGER :: POIN_ELM_TYP,POIN_MID,POIN_PID,POIN_PART
C-----------------------------------------------
!     write the element cost
!     for LAW = 0, write ZERO (because the array MID_PID_[X] does not exist)
      NG_L=0
      TAB_MAT(1:NGROUP_L)=ZERO
      DO NG=1,NGROUP
        P   = IPARG(32,NG)
        
        IF(P==PROC)THEN
           NG_L = NG_L+1
           NFT = IPARG(3,NG)+1
           ITY = IPARG(5,NG)
           ILAW = IPARG(1,NG)

           IF(ITY==1) THEN
             MID = IXS(1,NFT)
             PID = IXS(10,NFT)
             ISOL = IPARG(28,NG)

             IF(ISOL==4) THEN
               INDI = 6
               POIN_ELM_TYP = 6
             ELSEIF(ISOL==6) THEN
               INDI = 5
               POIN_ELM_TYP = 5
             ELSEIF(ISOL==8) THEN
               INDI = 1
               POIN_ELM_TYP = 7
             ELSEIF(ISOL==10) THEN
               INDI = 2
               POIN_ELM_TYP = 2
             ELSEIF(ISOL==16) THEN
               INDI = 3
               POIN_ELM_TYP = 3
             ELSEIF(ISOL==20) THEN
               INDI = 4
               POIN_ELM_TYP = 4
             ELSE
               INDI = 7
               POIN_ELM_TYP = 1
             ENDIF

             INDI = INDI+2
             POIN_PART = IPARTS(NFT)
             POIN_MID = POIN_PART_SOL(1,POIN_PART,POIN_ELM_TYP)
             POIN_PID = POIN_PART_SOL(2,POIN_PART,POIN_ELM_TYP)
             IF(ILAW/=0) THEN
                TAB_MAT(NG_L) = MID_PID_SOL(POIN_MID,POIN_ELM_TYP)%COST1D(POIN_PID)
             ELSE
                TAB_MAT(NG_L) = ZERO
             ENDIF
             
           ELSEIF(ITY==3) THEN
             MID = IXC(1,NFT)
             PID = IXC(6,NFT)
             POIN_PART = IPARTC(NFT)
             POIN_MID = POIN_PART_SHELL(1,POIN_PART)
             POIN_PID = POIN_PART_SHELL(2,POIN_PART)
             IF(ILAW/=0) THEN
                TAB_MAT(NG_L)= MID_PID_SHELL(POIN_MID)%COST1D(POIN_PID)
             ELSE
                TAB_MAT(NG_L) = ZERO
             ENDIF

           ELSEIF(ITY==7) THEN
             MID = IXTG(1,NFT)
             PID = IXTG(5,NFT)
             POIN_PART = IPARTG(NFT)
             POIN_MID = POIN_PART_TRI(1,POIN_PART)
             POIN_PID = POIN_PART_TRI(2,POIN_PART)
             IF(ILAW/=0) THEN
                TAB_MAT(NG_L)= MID_PID_TRI(POIN_MID)%COST1D(POIN_PID)
             ELSE
                TAB_MAT(NG_L) = ZERO
             ENDIF
           ENDIF
        ENDIF
      ENDDO

      CALL WRITE_DB(TAB_MAT,NGROUP_L)
C-----------------------------------------------
      END
