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
Chd|  FVWRESTP                      source/restart/ddsplit/fvwrestp.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/fvmbag_meshcontrol_mod.F
Chd|====================================================================
	  SUBROUTINE FVWRESTP(PROC)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
      USE FVMBAG_MESHCONTROL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, I, NNS, NNTR, LENP, NPOLY, LENH, NPOLH, TABI(11),
     .        LENP_ANIM, NPOLH_ANIM, LENH_ANIM, NNS_ANIM
C--------------------------------------
C     ECRITURE DES DIMENSIONS
C--------------------------------------
      LEN=1
        CALL WRITE_I_C(NFVBAG,LEN)
C
      DO I=1,NFVBAG
	 CALL WRITE_DB(FVDATA(I)%CFL_COEF,1)
	 CALL WRITE_DB(FVDATA(I)%DTMIN,1)
         TABI(1)=FVSPMD(I)%NN_L
         TABI(2)=FVSPMD(I)%NNA_L
         TABI(3)=FVSPMD(I)%NNSA_L
         TABI(4)=FVSPMD(I)%NSA
         TABI(5)=FVSPMD(I)%NNSA
         TABI(6)=FVSPMD(I)%NELSA
         TABI(7)=FVSPMD(I)%PMAIN
         TABI(8)=FVSPMD(I)%NNI_L
         LEN=8
	 CALL WRITE_I_C(TABI,LEN)
	 
         LEN=2*(FVSPMD(I)%NN_L+FVSPMD(I)%NNI_L)
         IF (LEN.GT.0) THEN
              CALL WRITE_I_C(FVSPMD(I)%IBUF_L,LEN)
              DEALLOCATE(FVSPMD(I)%IBUF_L)
         ENDIF
         LEN=2*FVSPMD(I)%NNA_L
         IF (LEN.GT.0) THEN
              CALL WRITE_I_C(FVSPMD(I)%IBUFA_L,LEN)
              DEALLOCATE(FVSPMD(I)%IBUFA_L)
         ENDIF
         LEN=2*FVSPMD(I)%NNSA_L
         IF (LEN.GT.0) THEN
              CALL WRITE_I_C(FVSPMD(I)%IBUFSA_L,LEN)
              DEALLOCATE(FVSPMD(I)%IBUFSA_L)
         ENDIF
	 LEN = 1
	 CALL WRITE_I_C(KMESH(FVID(I)), LEN)
         IF (PROC.NE.FVSPMD(I)%PMAIN) CYCLE
C
         LEN=8*FVSPMD(I)%NSA
         IF (LEN.GT.0) THEN
              CALL WRITE_I_C(FVSPMD(I)%IXSA,LEN)
              DEALLOCATE(FVSPMD(I)%IXSA)
         ENDIF
         LEN=3*FVSPMD(I)%NELSA
         IF (LEN.GT.0) THEN
              CALL WRITE_I_C(FVSPMD(I)%ELEMSA,LEN)
              DEALLOCATE(FVSPMD(I)%ELEMSA)
         ENDIF
C
         NNS=FVDATA(I)%NNS
         NNTR=FVDATA(I)%NNTR
         LENP=FVDATA(I)%LENP
         NPOLY=FVDATA(I)%NPOLY
         LENH=FVDATA(I)%LENH
         NPOLH=FVDATA(I)%NPOLH
         NPOLH_ANIM=FVDATA(I)%NPOLH_ANIM
         IF (NPOLH_ANIM.GT.0) THEN
            LENP_ANIM=FVDATA(I)%IFVTADR_ANIM(NPOLY+1)
            LENH_ANIM=FVDATA(I)%IFVPADR_ANIM(NPOLH_ANIM+1)
            NNS_ANIM=FVDATA(I)%NNS_ANIM
         ELSE
            LENP_ANIM=0
            LENH_ANIM=0
            NNS_ANIM=0
         ENDIF
C
         TABI(1)=NNS
         TABI(2)=NNTR
         TABI(3)=LENP
         TABI(4)=NPOLY
         TABI(5)=LENH
         TABI(6)=NPOLH     
         TABI(7)=LENP_ANIM     
         TABI(8)=LENH_ANIM 
         TABI(9)=NPOLH_ANIM 
         TABI(10)=NNS_ANIM
         TABI(11)=FVDATA(I)%ID
         LEN=11
	 CALL WRITE_I_C(TABI,LEN)
       
C--------------------------------------
C     ECRITURE DES ENTIERS
C--------------------------------------
         LEN=3*NNS
           CALL WRITE_I_C(FVDATA(I)%IFVNOD,LEN)
         LEN=6*NNTR
           CALL WRITE_I_C(FVDATA(I)%IFVTRI,LEN)
           CALL WRITE_I_C(FVDATA(I)%IFVPOLY,LENP)
           CALL WRITE_I_C(FVDATA(I)%IFVTADR,NPOLY+1)
           CALL WRITE_I_C(FVDATA(I)%IFVPOLH,LENH)
           CALL WRITE_I_C(FVDATA(I)%IFVPADR,NPOLH+1)
           CALL WRITE_I_C(FVDATA(I)%IDPOLH,NPOLH)
           CALL WRITE_I_C(FVDATA(I)%IBPOLH,NPOLH)
C
         IF (NPOLH_ANIM.GT.0) THEN
              CALL WRITE_I_C(FVDATA(I)%IFVPOLY_ANIM,LENP_ANIM)
              CALL WRITE_I_C(FVDATA(I)%IFVTADR_ANIM,NPOLY+1)
              CALL WRITE_I_C(FVDATA(I)%IFVPOLH_ANIM,LENH_ANIM)
              CALL WRITE_I_C(FVDATA(I)%IFVPADR_ANIM,NPOLH_ANIM+1)
            LEN=6*NNTR
              CALL WRITE_I_C(FVDATA(I)%IFVTRI_ANIM,LEN)
              CALL WRITE_I_C(FVDATA(I)%REDIR_ANIM,NNS_ANIM)
         ENDIF
C--------------------------------------
C     ECRITURE DES REELS
C--------------------------------------
	 LEN = 1
	 CALL WRITE_DB(FVDATA(I)%PDISP_OLD,LEN)
	 CALL WRITE_DB(FVDATA(I)%PDISP,LEN)
         LEN=2*NNS
           CALL WRITE_DB(FVDATA(I)%RFVNOD,LEN)
           CALL WRITE_DB(FVDATA(I)%MPOLH,NPOLH)
         LEN=3*NPOLH
           CALL WRITE_DB(FVDATA(I)%QPOLH,LEN)
           CALL WRITE_DB(FVDATA(I)%EPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%PPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%RPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%GPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%TPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%CPAPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%CPBPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%CPCPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%CPDPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%CPEPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%CPFPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%RMWPOLH,NPOLH)
           CALL WRITE_DB(FVDATA(I)%VPOLH_INI,NPOLH)
           CALL WRITE_DB(FVDATA(I)%DLH,1)
           CALL WRITE_DB(FVDATA(I)%DTPOLH,NPOLH)
C
         IF (NPOLH_ANIM.GT.0) THEN
            LEN=3*NNS_ANIM
              CALL WRITE_DB(FVDATA(I)%NOD_ANIM,LEN)
         ENDIF
C
      ENDDO
      
      FVMBAG_DTOLD = EP20
      CALL WRITE_DB(FVMBAG_DTOLD,1)
      
      IF (ALLOCATED(FVSPMD)) DEALLOCATE(FVSPMD)
C
      RETURN
      END
