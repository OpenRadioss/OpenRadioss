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
Chd|  S_USER                        source/output/sty/s_user.F    
Chd|-- called by -----------
Chd|        OUTP_C_S                      source/output/sty/outp_c_s.F  
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S_USER(NBX,IMX,IHBE,NEL,NPT,MLW,IPM,IGEO, IXC,
     .                  ITY,JJ,ELBUF_TAB,WA,NFT, FUNC,
     .                  NLAY,NPTR,NPTS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IHBE,NEL,NPT,JJ,MLW,ITY,NLAY,NPTR,NPTS,
     .IXC(NIXC,*),IPM(NPROPMI,*),IGEO(NPROPGI,*)
      my_real
     .   WA(*)
      TYPE (ELBUF_STRUCT_) , TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,II,I1,IPT,IL,IR,IS,IT,
     .        NUVAR,IGTYP,NFT, NBX, IMX,NPTT
      my_real
     .   FAC,AA,VAR(200),FUNC(6)
      TYPE(BUF_MAT_) ,POINTER :: MBUF
C=======================================================================
C     QBAT----
      IF (ITY == 3 .AND. IHBE == 11) THEN
       FAC = FOURTH
      ENDIF
C     DKT18----
      IF (ITY == 7 .AND. IHBE == 11) THEN
       FAC = THIRD
      ENDIF
C------------------------
C---QEPH:------
        IF (IHBE == 23) THEN
C---Transfert to QBAT------
        ELSEIF (IHBE == 11) THEN ! QBAT,DKT18
          IF (MLW == 29.OR.MLW == 30.OR.MLW == 31.OR.MLW>=33) THEN
          NUVAR  = 0
          DO I=1,NEL
            NUVAR  = MAX(NUVAR,IPM(8,IXC(1,NFT+1)))
          ENDDO
          IGTYP = IGEO(11,IXC(6,NFT+1))
          ENDIF
c
           II = NBX - 19
           I1 = (II -1)*NEL
           DO I=1,NEL
            AA = ZERO
            IF (MLW == 29.OR.MLW == 30.OR.MLW == 31.OR.MLW>=33) THEN
c
              IF (NLAY > 1) THEN
                IT = 1
                DO IPT=1,NLAY
                  DO IR=1,NPTR
                   DO IS=1,NPTS
                     MBUF => ELBUF_TAB%BUFLY(IPT)%MAT(1,1,IT)
                     VAR(IPT) = VAR(IPT) + MBUF%VAR(I1 + I )*FAC
                     IF (VAR(IPT) >= AA) AA = VAR(IPT)
            ENDDO
           ENDDO
                ENDDO
              ELSE  ! NLAY = 1
                IL = 1
                NPTT = ELBUF_TAB%NPTT
                DO IPT=1,NPTT
                  VAR(IPT) = ZERO
                  DO IR=1,NPTR
                   DO IS=1,NPTS
                     MBUF => ELBUF_TAB%BUFLY(IL)%MAT(1,1,IPT)
                     VAR(IPT) = VAR(IPT) + MBUF%VAR(I1 + I )*FAC
                     IF (VAR(IPT) >= AA) AA = VAR(IPT)
                   ENDDO
                  ENDDO
                ENDDO
              ENDIF  ! NLAY
c
             IF(IMX == 0)THEN
             WA(JJ +1) = VAR(IABS(NPT)/2 + 1)
             ELSE
             WA(JJ + 1) = AA
             ENDIF
             JJ = JJ + 1
           ENDIF
          ENDDO
        ELSE   ! IHBE == 11
c         error message------
         ENDIF
C-----------
      RETURN
      END
