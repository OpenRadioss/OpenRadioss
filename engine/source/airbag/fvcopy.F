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
Chd|  FVCOPY                        source/airbag/fvcopy.F        
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FVCOPY(MONVOL)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE MESSAGE_MOD
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MONVOL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K1, N, ITYP, IFV, NNS, NNTR, NPOLY, NPOLH, 
     .        LENP, LENH, I, J, IMESH, LENP_ANIM, NPOLH_ANIM,
     .        LENH_ANIM, NNS_ANIM,NFVBAG_MAX


C ========= 
C     The current number of active FVMBAGS (NFVBAG) can be lower
C     than the size of FVDATA, since FVDATA is build before switches to
C     UP
      K1 = 1
      NFVBAG_MAX=0
      DO N=1,NVOLU
         ITYP=MONVOL(K1-1+2)
         IF (ITYP.EQ.6.OR.ITYP.EQ.8) THEN
            IFV = MONVOL(K1-1+45)                   
            NFVBAG_MAX = MAX(NFVBAG_MAX,IFV)
         ENDIF
         K1 = K1 + NIMV
      ENDDO
      NFVBAG_MAX = MAX(NFVBAG_MAX,NFVBAG)
C
      IF (.NOT.ALLOCATED(FVDATA_OLD)) THEN
         ALLOCATE(FVDATA_OLD(NFVBAG_MAX))
         DO I=1,NFVBAG_MAX
            NULLIFY(FVDATA_OLD(I)%IFVPOLY_ANIM,
     .              FVDATA_OLD(I)%IFVTADR_ANIM,
     .              FVDATA_OLD(I)%IFVPOLH_ANIM,
     .              FVDATA_OLD(I)%IFVPADR_ANIM,
     .              FVDATA_OLD(I)%REDIR_ANIM,
     .              FVDATA_OLD(I)%IFVTRI_ANIM,
     .              FVDATA_OLD(I)%NOD_ANIM,
     .              FVDATA_OLD(I)%IFVPOLY,
     .              FVDATA_OLD(I)%IFVTADR,
     .              FVDATA_OLD(I)%IFVPOLH,
     .              FVDATA_OLD(I)%IFVPADR,
     .              FVDATA_OLD(I)%IDPOLH,
     .              FVDATA_OLD(I)%IBPOLH,
     .              FVDATA_OLD(I)%IFVTRI,
     .              FVDATA_OLD(I)%IFVNOD,
     .              FVDATA_OLD(I)%RFVNOD,
     .              FVDATA_OLD(I)%MPOLH,
     .              FVDATA_OLD(I)%EPOLH,
     .              FVDATA_OLD(I)%PPOLH,
     .              FVDATA_OLD(I)%RPOLH,
     .              FVDATA_OLD(I)%GPOLH,
     .              FVDATA_OLD(I)%CPAPOLH,
     .              FVDATA_OLD(I)%CPBPOLH,
     .              FVDATA_OLD(I)%CPCPOLH,
     .              FVDATA_OLD(I)%RMWPOLH,
     .              FVDATA_OLD(I)%VPOLH_INI,
     .              FVDATA_OLD(I)%QPOLH,
     .              FVDATA_OLD(I)%DTPOLH,
     .              FVDATA_OLD(I)%TPOLH,
     .              FVDATA_OLD(I)%CPDPOLH,
     .              FVDATA_OLD(I)%CPEPOLH,
     .              FVDATA_OLD(I)%CPFPOLH)
        ENDDO

        DO I=1,NFVBAG_MAX
            FVDATA_OLD(I)%CFL_COEF = ZERO
            FVDATA_OLD(I)%DTMIN = ZERO
            FVDATA_OLD(I)%PDISP_OLD = ZERO
            FVDATA_OLD(I)%PDISP = ZERO
            FVDATA_OLD(I)%DLH = ZERO

            FVDATA_OLD(I)%NNS = 0
            FVDATA_OLD(I)%NNTR = 0
            FVDATA_OLD(I)%LENP = 0
            FVDATA_OLD(I)%NPOLY = 0
            FVDATA_OLD(I)%LENH = 0
            FVDATA_OLD(I)%NPOLH = 0

            FVDATA_OLD(I)%NPOLH_ANIM = 0
            FVDATA_OLD(I)%NNS_ANIM = 0
            FVDATA_OLD(I)%ID = 0
        ENDDO
      ENDIF
      IF (.NOT.ALLOCATED(FVDATA)) THEN
         ALLOCATE(FVDATA(NFVBAG_MAX))
         DO I=1,NFVBAG_MAX
            NULLIFY(FVDATA(I)%IFVPOLY_ANIM,
     .              FVDATA(I)%IFVTADR_ANIM,
     .              FVDATA(I)%IFVPOLH_ANIM,
     .              FVDATA(I)%IFVPADR_ANIM,
     .              FVDATA(I)%REDIR_ANIM,
     .              FVDATA(I)%IFVTRI_ANIM,
     .              FVDATA(I)%NOD_ANIM,
     .              FVDATA(I)%IFVPOLY,
     .              FVDATA(I)%IFVTADR,
     .              FVDATA(I)%IFVPOLH,
     .              FVDATA(I)%IFVPADR,
     .              FVDATA(I)%IDPOLH,
     .              FVDATA(I)%IBPOLH,
     .              FVDATA(I)%IFVTRI,
     .              FVDATA(I)%IFVNOD,
     .              FVDATA(I)%RFVNOD,
     .              FVDATA(I)%MPOLH,
     .              FVDATA(I)%EPOLH,
     .              FVDATA(I)%PPOLH,
     .              FVDATA(I)%RPOLH,
     .              FVDATA(I)%GPOLH,
     .              FVDATA(I)%CPAPOLH,
     .              FVDATA(I)%CPBPOLH,
     .              FVDATA(I)%CPCPOLH,
     .              FVDATA(I)%RMWPOLH,
     .              FVDATA(I)%VPOLH_INI,
     .              FVDATA(I)%QPOLH,
     .              FVDATA(I)%DTPOLH,
     .              FVDATA(I)%TPOLH,
     .              FVDATA(I)%CPDPOLH,
     .              FVDATA(I)%CPEPOLH,
     .              FVDATA(I)%CPFPOLH)
         ENDDO
        DO I=1,NFVBAG_MAX
            FVDATA(I)%CFL_COEF = ZERO
            FVDATA(I)%DTMIN = ZERO
            FVDATA(I)%PDISP_OLD = ZERO
            FVDATA(I)%PDISP = ZERO
            FVDATA(I)%DLH = ZERO

            FVDATA(I)%NNS = 0
            FVDATA(I)%NNTR = 0
            FVDATA(I)%LENP = 0
            FVDATA(I)%NPOLY = 0
            FVDATA(I)%LENH = 0
            FVDATA(I)%NPOLH = 0

            FVDATA(I)%NPOLH_ANIM = 0
            FVDATA(I)%NNS_ANIM = 0
            FVDATA(I)%ID = 0
        ENDDO
      ENDIF
C
      K1=1
      IFV=0
      DO N=1,NVOLU
         ITYP=MONVOL(K1-1+2)
         IF (ITYP.EQ.6.OR.ITYP.EQ.8) THEN
            IFV = MONVOL(K1-1+45)                   
            IMESH=MONVOL(K1-1+56)
C
            NNS=MONVOL(K1-1+46)
            NNTR=MONVOL(K1-1+47)
            NPOLY=MONVOL(K1-1+48)
            NPOLH=MONVOL(K1-1+49)
C
            IF (IMESH.EQ.1.AND.DT1.GT.ZERO.AND.NCYCLE.NE.0) THEN
               IF (ISPMD.NE.FVSPMD(I)%PMAIN-1) THEN
                  ALLOCATE(FVDATA_OLD(I)%IFVNOD(3,0),
     .                     FVDATA_OLD(I)%RFVNOD(2,0),
     .                     FVDATA_OLD(I)%IFVTRI(6,0),
     .                     FVDATA_OLD(I)%IFVPOLY(0),
     .                     FVDATA_OLD(I)%IFVTADR(0),
     .                     FVDATA_OLD(I)%IFVPOLH(0),
     .                     FVDATA_OLD(I)%IFVPADR(0),
     .                     FVDATA_OLD(I)%MPOLH(0),
     .                     FVDATA_OLD(I)%QPOLH(3,0),
     .                     FVDATA_OLD(I)%EPOLH(0),
     .                     FVDATA_OLD(I)%PPOLH(0),
     .                     FVDATA_OLD(I)%RPOLH(0),
     .                     FVDATA_OLD(I)%GPOLH(0),
     .                     FVDATA_OLD(I)%CPAPOLH(0),
     .                     FVDATA_OLD(I)%CPBPOLH(0),
     .                     FVDATA_OLD(I)%CPCPOLH(0),
     .                     FVDATA_OLD(I)%RMWPOLH(0),
     .                     FVDATA_OLD(I)%TPOLH(0),
     .                     FVDATA_OLD(I)%CPDPOLH(0),
     .                     FVDATA_OLD(I)%CPEPOLH(0),
     .                     FVDATA_OLD(I)%CPFPOLH(0))
                  GOTO 1999
               ENDIF
               IF (ASSOCIATED(FVDATA_OLD(IFV)%IFVNOD)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%IFVNOD)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%RFVNOD)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%RFVNOD)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%IFVTRI)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%IFVTRI)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%IFVPOLY)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%IFVPOLY)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%IFVTADR)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%IFVTADR)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%IFVPOLH)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%IFVPOLH)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%IFVPADR)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%IFVPADR)
C
               IF (ASSOCIATED(FVDATA_OLD(IFV)%MPOLH)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%MPOLH)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%QPOLH)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%QPOLH)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%EPOLH)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%EPOLH)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%PPOLH)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%PPOLH)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%RPOLH)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%RPOLH)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%GPOLH)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%GPOLH)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%CPAPOLH)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%CPAPOLH)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%CPBPOLH)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%CPBPOLH)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%CPCPOLH)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%CPCPOLH)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%RMWPOLH)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%RMWPOLH)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%TPOLH)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%TPOLH)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%CPDPOLH)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%CPDPOLH)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%CPEPOLH)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%CPEPOLH)
               IF (ASSOCIATED(FVDATA_OLD(IFV)%CPFPOLH)) 
     .                   DEALLOCATE(FVDATA_OLD(IFV)%CPFPOLH)
C
               LENP=FVDATA(IFV)%LENP
               LENH=FVDATA(IFV)%LENH
C
               FVDATA_OLD(IFV)%NNS=NNS
               FVDATA_OLD(IFV)%NNTR=NNTR
               FVDATA_OLD(IFV)%LENP=LENP
               FVDATA_OLD(IFV)%NPOLY=NPOLY
               FVDATA_OLD(IFV)%LENH=LENH
               FVDATA_OLD(IFV)%NPOLH=NPOLH
C
               ALLOCATE(FVDATA_OLD(IFV)%IFVNOD(3,NNS),
     .                  FVDATA_OLD(IFV)%RFVNOD(2,NNS),
     .                  FVDATA_OLD(IFV)%IFVTRI(6,NNTR),
     .                  FVDATA_OLD(IFV)%IFVPOLY(LENP),
     .                  FVDATA_OLD(IFV)%IFVTADR(NPOLY+1),
     .                  FVDATA_OLD(IFV)%IFVPOLH(LENH),
     .                  FVDATA_OLD(IFV)%IFVPADR(NPOLH+1),
     .                  FVDATA_OLD(IFV)%MPOLH(NPOLH),
     .                  FVDATA_OLD(IFV)%QPOLH(3,NPOLH),
     .                  FVDATA_OLD(IFV)%EPOLH(NPOLH),
     .                  FVDATA_OLD(IFV)%PPOLH(NPOLH),
     .                  FVDATA_OLD(IFV)%RPOLH(NPOLH),
     .                  FVDATA_OLD(IFV)%GPOLH(NPOLH),
     .                  FVDATA_OLD(IFV)%CPAPOLH(NPOLH),
     .                  FVDATA_OLD(IFV)%CPBPOLH(NPOLH),
     .                  FVDATA_OLD(IFV)%CPCPOLH(NPOLH),
     .                  FVDATA_OLD(IFV)%RMWPOLH(NPOLH),
     .                  FVDATA_OLD(IFV)%TPOLH(NPOLH),
     .                  FVDATA_OLD(IFV)%CPDPOLH(NPOLH),
     .                  FVDATA_OLD(IFV)%CPEPOLH(NPOLH),
     .                  FVDATA_OLD(IFV)%CPFPOLH(NPOLH))
C
               DO I=1,NNS
                  FVDATA_OLD(IFV)%IFVNOD(1,I)=FVDATA(IFV)%IFVNOD(1,I)
                  FVDATA_OLD(IFV)%IFVNOD(2,I)=FVDATA(IFV)%IFVNOD(2,I)
                  FVDATA_OLD(IFV)%IFVNOD(3,I)=FVDATA(IFV)%IFVNOD(3,I)
                  FVDATA_OLD(IFV)%RFVNOD(1,I)=FVDATA(IFV)%RFVNOD(1,I)
                  FVDATA_OLD(IFV)%RFVNOD(2,I)=FVDATA(IFV)%RFVNOD(2,I)
               ENDDO
               DO I=1,NNTR
                  DO J=1,6
                     FVDATA_OLD(IFV)%IFVTRI(J,I)=
     .                             FVDATA(IFV)%IFVTRI(J,I)
                  ENDDO
               ENDDO
               DO I=1,LENP
                  FVDATA_OLD(IFV)%IFVPOLY(I)=FVDATA(IFV)%IFVPOLY(I)
               ENDDO
               DO I=1,NPOLY+1
                  FVDATA_OLD(IFV)%IFVTADR(I)=FVDATA(IFV)%IFVTADR(I)
               ENDDO
               DO I=1,LENH
                  FVDATA_OLD(IFV)%IFVPOLH(I)=FVDATA(IFV)%IFVPOLH(I)
               ENDDO
               DO I=1,NPOLH+1
                  FVDATA_OLD(IFV)%IFVPADR(I)=FVDATA(IFV)%IFVPADR(I)
               ENDDO
               DO I=1,NPOLH
                  FVDATA_OLD(IFV)%MPOLH(I)=FVDATA(IFV)%MPOLH(I)
                  FVDATA_OLD(IFV)%QPOLH(1,I)=FVDATA(IFV)%QPOLH(1,I)
                  FVDATA_OLD(IFV)%QPOLH(2,I)=FVDATA(IFV)%QPOLH(2,I)
                  FVDATA_OLD(IFV)%QPOLH(3,I)=FVDATA(IFV)%QPOLH(3,I)
                  FVDATA_OLD(IFV)%EPOLH(I)=FVDATA(IFV)%EPOLH(I)
                  FVDATA_OLD(IFV)%PPOLH(I)=FVDATA(IFV)%PPOLH(I)
                  FVDATA_OLD(IFV)%RPOLH(I)=FVDATA(IFV)%RPOLH(I)
                  FVDATA_OLD(IFV)%GPOLH(I)=FVDATA(IFV)%GPOLH(I)
                  FVDATA_OLD(IFV)%CPAPOLH(I)=FVDATA(IFV)%CPAPOLH(I)
                  FVDATA_OLD(IFV)%CPBPOLH(I)=FVDATA(IFV)%CPBPOLH(I)
                  FVDATA_OLD(IFV)%CPCPOLH(I)=FVDATA(IFV)%CPCPOLH(I)
                  FVDATA_OLD(IFV)%RMWPOLH(I)=FVDATA(IFV)%RMWPOLH(I)
                  FVDATA_OLD(IFV)%TPOLH(I)=FVDATA(IFV)%TPOLH(I)
                  FVDATA_OLD(IFV)%CPDPOLH(I)=FVDATA(IFV)%CPDPOLH(I)
                  FVDATA_OLD(IFV)%CPEPOLH(I)=FVDATA(IFV)%CPEPOLH(I)
                  FVDATA_OLD(IFV)%CPFPOLH(I)=FVDATA(IFV)%CPFPOLH(I)
               ENDDO
C
            ELSEIF ((IMESH.EQ.0.AND.NCYCLE.EQ.0).OR. 
     .              (IMESH==0.AND.MCHECK > 0)) THEN
               IF (ISPMD.NE.FVSPMD(IFV)%PMAIN-1) THEN
                  ALLOCATE(FVDATA(IFV)%IFVNOD(3,0),
     .                     FVDATA(IFV)%RFVNOD(2,0),
     .                     FVDATA(IFV)%IFVTRI(6,0),
     .                     FVDATA(IFV)%IFVPOLY(0),
     .                     FVDATA(IFV)%IFVTADR(0),
     .                     FVDATA(IFV)%IFVPOLH(0),
     .                     FVDATA(IFV)%IFVPADR(0),
     .                     FVDATA(IFV)%IDPOLH(0),
     .                     FVDATA(IFV)%IBPOLH(0),
     .                     FVDATA(IFV)%MPOLH(0),
     .                     FVDATA(IFV)%QPOLH(3,0),
     .                     FVDATA(IFV)%EPOLH(0),
     .                     FVDATA(IFV)%PPOLH(0),
     .                     FVDATA(IFV)%RPOLH(0),
     .                     FVDATA(IFV)%GPOLH(0),
     .                     FVDATA(IFV)%CPAPOLH(0),
     .                     FVDATA(IFV)%CPBPOLH(0),
     .                     FVDATA(IFV)%CPCPOLH(0),
     .                     FVDATA(IFV)%RMWPOLH(0),
     .                     FVDATA(IFV)%VPOLH_INI(0),
     .                     FVDATA(IFV)%DTPOLH(0),
     .                     FVDATA(IFV)%TPOLH(0),
     .                     FVDATA(IFV)%CPDPOLH(0),
     .                     FVDATA(IFV)%CPEPOLH(0),
     .                     FVDATA(IFV)%CPFPOLH(0),
     .                     FVDATA(IFV)%IFVPOLY_ANIM(0),
     .                     FVDATA(IFV)%IFVTADR_ANIM(0),
     .                     FVDATA(IFV)%IFVPOLH_ANIM(0),
     .                     FVDATA(IFV)%IFVPADR_ANIM(0),
     .                     FVDATA(IFV)%IFVTRI_ANIM(6,0),
     .                     FVDATA(IFV)%REDIR_ANIM(0),
     .                     FVDATA(IFV)%NOD_ANIM(3,0))
                  GOTO 1999
               ENDIF 
               LENP=FVDATA_OLD(IFV)%LENP
               LENH=FVDATA_OLD(IFV)%LENH
               NPOLH_ANIM=FVDATA_OLD(IFV)%NPOLH_ANIM
               NNS_ANIM=FVDATA_OLD(IFV)%NNS_ANIM
               LENP_ANIM=FVDATA_OLD(IFV)%IFVTADR_ANIM(NPOLY+1)
               LENH_ANIM=FVDATA_OLD(IFV)%IFVPADR_ANIM(NPOLH_ANIM+1)
C
               FVDATA(IFV)%NNS=NNS
               FVDATA(IFV)%NNTR=NNTR
               FVDATA(IFV)%LENP=LENP
               FVDATA(IFV)%NPOLY=NPOLY
               FVDATA(IFV)%LENH=LENH
               FVDATA(IFV)%NPOLH=NPOLH
               FVDATA(IFV)%NPOLH_ANIM=NPOLH_ANIM
               FVDATA(IFV)%NNS_ANIM=NNS_ANIM
               FVDATA(IFV)%ID=FVDATA_OLD(IFV)%ID
C
               ALLOCATE(FVDATA(IFV)%IFVNOD(3,NNS),
     .                  FVDATA(IFV)%RFVNOD(2,NNS),
     .                  FVDATA(IFV)%IFVTRI(6,NNTR),
     .                  FVDATA(IFV)%IFVPOLY(LENP),
     .                  FVDATA(IFV)%IFVTADR(NPOLY+1),
     .                  FVDATA(IFV)%IFVPOLH(LENH),
     .                  FVDATA(IFV)%IFVPADR(NPOLH+1),
     .                  FVDATA(IFV)%IDPOLH(NPOLH),
     .                  FVDATA(IFV)%IBPOLH(NPOLH),
     .                  FVDATA(IFV)%MPOLH(NPOLH),
     .                  FVDATA(IFV)%QPOLH(3,NPOLH),
     .                  FVDATA(IFV)%EPOLH(NPOLH),
     .                  FVDATA(IFV)%PPOLH(NPOLH),
     .                  FVDATA(IFV)%RPOLH(NPOLH),
     .                  FVDATA(IFV)%GPOLH(NPOLH),
     .                  FVDATA(IFV)%CPAPOLH(NPOLH),
     .                  FVDATA(IFV)%CPBPOLH(NPOLH),
     .                  FVDATA(IFV)%CPCPOLH(NPOLH),
     .                  FVDATA(IFV)%RMWPOLH(NPOLH),
     .                  FVDATA(IFV)%VPOLH_INI(NPOLH),
     .                  FVDATA(IFV)%DTPOLH(NPOLH),
     .                  FVDATA(IFV)%TPOLH(NPOLH),
     .                  FVDATA(IFV)%CPDPOLH(NPOLH),
     .                  FVDATA(IFV)%CPEPOLH(NPOLH),
     .                  FVDATA(IFV)%CPFPOLH(NPOLH),
     .                  FVDATA(IFV)%IFVPOLY_ANIM(LENP_ANIM),
     .                  FVDATA(IFV)%IFVTADR_ANIM(NPOLY+1),
     .                  FVDATA(IFV)%IFVPOLH_ANIM(LENH_ANIM),
     .                  FVDATA(IFV)%IFVPADR_ANIM(NPOLH_ANIM+1),
     .                  FVDATA(IFV)%IFVTRI_ANIM(6,NNTR),
     .                  FVDATA(IFV)%REDIR_ANIM(NNS_ANIM),
     .                  FVDATA(IFV)%NOD_ANIM(3,NNS_ANIM))
C
               FVDATA(IFV)%CFL_COEF = FVDATA_OLD(IFV)%CFL_COEF
               FVDATA(IFV)%PDISP_OLD = FVDATA_OLD(IFV)%PDISP_OLD
               FVDATA(IFV)%PDISP = FVDATA_OLD(IFV)%PDISP
               FVDATA(IFV)%DTMIN = FVDATA_OLD(IFV)%DTMIN
C
               DO I=1,NNS
                  FVDATA(IFV)%IFVNOD(1,I)=FVDATA_OLD(IFV)%IFVNOD(1,I)
                  FVDATA(IFV)%IFVNOD(2,I)=FVDATA_OLD(IFV)%IFVNOD(2,I)
                  FVDATA(IFV)%IFVNOD(3,I)=FVDATA_OLD(IFV)%IFVNOD(3,I)
                  FVDATA(IFV)%RFVNOD(1,I)=FVDATA_OLD(IFV)%RFVNOD(1,I)
                  FVDATA(IFV)%RFVNOD(2,I)=FVDATA_OLD(IFV)%RFVNOD(2,I)
               ENDDO
               DO I=1,NNTR
                  DO J=1,6
                     FVDATA(IFV)%IFVTRI(J,I)=
     .                             FVDATA_OLD(IFV)%IFVTRI(J,I)
                  ENDDO
               ENDDO
               DO I=1,LENP
                  FVDATA(IFV)%IFVPOLY(I)=FVDATA_OLD(IFV)%IFVPOLY(I)
               ENDDO
               DO I=1,NPOLY+1
                  FVDATA(IFV)%IFVTADR(I)=FVDATA_OLD(IFV)%IFVTADR(I)
               ENDDO
               DO I=1,LENH
                  FVDATA(IFV)%IFVPOLH(I)=FVDATA_OLD(IFV)%IFVPOLH(I)
               ENDDO
               DO I=1,NPOLH+1
                  FVDATA(IFV)%IFVPADR(I)=FVDATA_OLD(IFV)%IFVPADR(I)
               ENDDO
               DO I=1,NPOLH
                  FVDATA(IFV)%IDPOLH(I)=FVDATA_OLD(IFV)%IDPOLH(I)
                  FVDATA(IFV)%IBPOLH(I)=FVDATA_OLD(IFV)%IBPOLH(I)
               ENDDO
               DO I=1,LENP_ANIM
                  FVDATA(IFV)%IFVPOLY_ANIM(I)=
     .                        FVDATA_OLD(IFV)%IFVPOLY_ANIM(I)
               ENDDO
               DO I=1,NPOLY+1
                  FVDATA(IFV)%IFVTADR_ANIM(I)=
     .                        FVDATA_OLD(IFV)%IFVTADR_ANIM(I)
               ENDDO
               DO I=1,LENH_ANIM
                  FVDATA(IFV)%IFVPOLH_ANIM(I)=
     .                        FVDATA_OLD(IFV)%IFVPOLH_ANIM(I)
               ENDDO
               DO I=1,NPOLH_ANIM+1
                  FVDATA(IFV)%IFVPADR_ANIM(I)=
     .                        FVDATA_OLD(IFV)%IFVPADR_ANIM(I)
               ENDDO
               DO I=1,NNTR
                  DO J=1,6
                     FVDATA(IFV)%IFVTRI_ANIM(J,I)=
     .                          FVDATA_OLD(IFV)%IFVTRI_ANIM(J,I)
                  ENDDO
               ENDDO
               DO I=1,NNS_ANIM
                  FVDATA(IFV)%REDIR_ANIM(I)=
     .                       FVDATA_OLD(IFV)%REDIR_ANIM(I)
               ENDDO
               DO I=1,NNS_ANIM
                  DO J=1,3
                     FVDATA(IFV)%NOD_ANIM(J,I)=
     .                          FVDATA_OLD(IFV)%NOD_ANIM(J,I)
                  ENDDO
               ENDDO   
               DO I=1,NPOLH
                  FVDATA(IFV)%MPOLH(I)=FVDATA_OLD(IFV)%MPOLH(I)
                  FVDATA(IFV)%QPOLH(1,I)=FVDATA_OLD(IFV)%QPOLH(1,I)
                  FVDATA(IFV)%QPOLH(2,I)=FVDATA_OLD(IFV)%QPOLH(2,I)
                  FVDATA(IFV)%QPOLH(3,I)=FVDATA_OLD(IFV)%QPOLH(3,I)
                  FVDATA(IFV)%EPOLH(I)=FVDATA_OLD(IFV)%EPOLH(I)
                  FVDATA(IFV)%PPOLH(I)=FVDATA_OLD(IFV)%PPOLH(I)
                  FVDATA(IFV)%RPOLH(I)=FVDATA_OLD(IFV)%RPOLH(I)
                  FVDATA(IFV)%GPOLH(I)=FVDATA_OLD(IFV)%GPOLH(I)
                  FVDATA(IFV)%CPAPOLH(I)=FVDATA_OLD(IFV)%CPAPOLH(I)
                  FVDATA(IFV)%CPBPOLH(I)=FVDATA_OLD(IFV)%CPBPOLH(I)
                  FVDATA(IFV)%CPCPOLH(I)=FVDATA_OLD(IFV)%CPCPOLH(I)
                  FVDATA(IFV)%RMWPOLH(I)=FVDATA_OLD(IFV)%RMWPOLH(I)
                  FVDATA(IFV)%VPOLH_INI(I)=FVDATA_OLD(IFV)%VPOLH_INI(I)
                  FVDATA(IFV)%DTPOLH(I)=FVDATA_OLD(IFV)%DTPOLH(I)
                  FVDATA(IFV)%TPOLH(I)=FVDATA_OLD(IFV)%TPOLH(I)
                  FVDATA(IFV)%CPDPOLH(I)=FVDATA_OLD(IFV)%CPDPOLH(I)
                  FVDATA(IFV)%CPEPOLH(I)=FVDATA_OLD(IFV)%CPEPOLH(I)
                  FVDATA(IFV)%CPFPOLH(I)=FVDATA_OLD(IFV)%CPFPOLH(I)
               ENDDO
               FVDATA(IFV)%DLH=FVDATA_OLD(IFV)%DLH
C
               DEALLOCATE(FVDATA_OLD(IFV)%IFVNOD,
     .                    FVDATA_OLD(IFV)%RFVNOD,
     .                    FVDATA_OLD(IFV)%IFVTRI,
     .                    FVDATA_OLD(IFV)%IFVPOLY,
     .                    FVDATA_OLD(IFV)%IFVTADR,
     .                    FVDATA_OLD(IFV)%IFVPOLH,
     .                    FVDATA_OLD(IFV)%IFVPADR,
     .                    FVDATA_OLD(IFV)%IDPOLH,
     .                    FVDATA_OLD(IFV)%IBPOLH,
     .                    FVDATA_OLD(IFV)%MPOLH,
     .                    FVDATA_OLD(IFV)%QPOLH,
     .                    FVDATA_OLD(IFV)%EPOLH,
     .                    FVDATA_OLD(IFV)%PPOLH,
     .                    FVDATA_OLD(IFV)%RPOLH,
     .                    FVDATA_OLD(IFV)%GPOLH,
     .                    FVDATA_OLD(IFV)%CPAPOLH,
     .                    FVDATA_OLD(IFV)%CPBPOLH,
     .                    FVDATA_OLD(IFV)%CPCPOLH,
     .                    FVDATA_OLD(IFV)%RMWPOLH,
     .                    FVDATA_OLD(IFV)%VPOLH_INI,
     .                    FVDATA_OLD(IFV)%DTPOLH,
     .                    FVDATA_OLD(IFV)%TPOLH,
     .                    FVDATA_OLD(IFV)%CPDPOLH,
     .                    FVDATA_OLD(IFV)%CPEPOLH,
     .                    FVDATA_OLD(IFV)%CPFPOLH,
     .                    FVDATA_OLD(IFV)%IFVPOLY_ANIM,
     .                    FVDATA_OLD(IFV)%IFVTADR_ANIM,
     .                    FVDATA_OLD(IFV)%IFVPOLH_ANIM,
     .                    FVDATA_OLD(IFV)%IFVPADR_ANIM,
     .                    FVDATA_OLD(IFV)%IFVTRI_ANIM,
     .                    FVDATA_OLD(IFV)%REDIR_ANIM,
     .                    FVDATA_OLD(IFV)%NOD_ANIM)
            ENDIF
         ENDIF
 1999    CONTINUE
         K1=K1+NIMV
      ENDDO
C
        MCHECK = 0
      RETURN
      END
