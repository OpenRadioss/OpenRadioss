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
C
Chd|====================================================================
Chd|  INCOQ3                        source/interfaces/inter3d1/incoq3.F
Chd|-- called by -----------
Chd|        I1CHK3                        source/interfaces/inter3d1/i1chk3.F
Chd|        I20STI3                       source/interfaces/inter3d1/i20sti3.F
Chd|        I21ELS3                       source/interfaces/inter3d1/i21els3.F
Chd|        I23GAP3                       source/interfaces/inter3d1/i23gap3.F
Chd|        I24GAPM                       source/interfaces/inter3d1/i24sti3.F
Chd|        I25GAPM                       source/interfaces/inter3d1/i25sti3.F
Chd|        I2BUC1                        source/interfaces/inter3d1/i2buc1.F
Chd|        I2CHK3                        source/interfaces/inter3d1/i2chk3.F
Chd|        I2COR3                        source/interfaces/inter3d1/i2cor3.F
Chd|        I3STI3                        source/interfaces/inter3d1/i3sti3.F
Chd|        I7STI3                        source/interfaces/inter3d1/i7sti3.F
Chd|        ININTR_ORTHDIRFRIC            source/interfaces/interf1/inintr_orthdirfric.F
Chd|        R2R_COUNT                     source/coupling/rad2rad/r2r_count.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INCOQ3(IRECT ,IXC ,IXTG ,NINT ,NEL      ,
     .                  NELTG ,IS  ,GEO  ,PM   ,KNOD2ELC ,
     .              KNOD2ELTG ,NOD2ELC ,NOD2ELTG,THK,NTY,
     .              IGEO ,PM_STACK , IWORKSH )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINT, NEL, IS, NELTG,NTY
      INTEGER IRECT(4,*), IXC(NIXC,*), IXTG(NIXTG,*),
     .        KNOD2ELC(*) ,KNOD2ELTG(*) ,NOD2ELC(*) ,NOD2ELTG(*),
     .        IGEO(NPROPGI,*),IWORKSH(3,*)
C     REAL
      my_real
     .   GEO(NPROPG,*), PM(NPROPM,*),THK(*),PM_STACK(20,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, J, II, K, IAD,IGTYP, IPGMAT,IGMAT,ISUBSTACK
C     REAL
      my_real
     .   DXM, STM, DX, ST
C-----------------------------------------------
      NEL=0
      NELTG=0
      DXM = ZERO
      STM = ZERO
      IPGMAT = 700
      IF(IRECT(3,IS)==IRECT(4,IS).AND.NUMELTG/=0)THEN
       IF(IRECT(1,IS)>NUMNOD) RETURN
       DO 230 IAD=KNOD2ELTG(IRECT(1,IS))+1,KNOD2ELTG(IRECT(1,IS)+1)
        N = NOD2ELTG(IAD)
        DO 220 J=1,3
          II=IRECT(J,IS)
          DO 210 K=1,3
            IF(IXTG(K+1,N)==II) GOTO 220
  210     CONTINUE
          GOTO 230
  220   CONTINUE
          IGTYP = IGEO(11,IXTG(5,N)) 
          IF ( THK(NUMELC+N) /= ZERO .AND. IINTTHICK == 0 .AND.
     .       (NTY == 7 .OR. NTY == 20.OR. NTY == 22)) THEN
            DX=THK(NUMELC+N)
          ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR.IGTYP ==52) THEN
           DX=THK(NUMELC+N) 
        ELSE
          DX = GEO(1,IXTG(5,N))
        ENDIF
        IGMAT = IGEO(98,IXTG(5,N))
        IF (IXTG(1,N)>0) THEN
          IF(IGTYP == 11 .AND. IGMAT > 0) THEN
            ST = GEO(IPGMAT + 2 ,IXTG(5,N)) 
          ELSEIF(IGTYP ==52 .OR.
     .          ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
            ISUBSTACK = IWORKSH(3,NUMELC + N)
             ST = PM_STACK(2 ,ISUBSTACK) 
          ELSE
            ST = PM(20,IXTG(1,N))
          ENDIF  
        ELSE
          ST = 0.
        ENDIF
        IF (DX>DXM) THEN
          DXM = DX
          NELTG = N
          STM = ST
        ELSEIF(DX==DXM) THEN
          IF ((ST>=STM).OR.(STM==0.)) THEN
            NELTG = N
            STM = ST
          ENDIF
        ENDIF       
  230  CONTINUE
      ENDIF
C
      IF(NUMELC/=0) THEN
       DO 430 IAD=KNOD2ELC(IRECT(1,IS))+1,KNOD2ELC(IRECT(1,IS)+1)
        N = NOD2ELC(IAD)
        DO 420 J=1,4
          II=IRECT(J,IS)
          DO 410 K=1,4
            IF(IXC(K+1,N)==II) GOTO 420
  410     CONTINUE
          GOTO 430
  420   CONTINUE
        IGTYP = IGEO(11,IXC(6,N))
          IF ( THK(N) /= ZERO .AND. IINTTHICK == 0 .AND.
     .       (NTY == 7 .OR. NTY == 20.OR. NTY == 22)) THEN
            DX=THK(N)
          ELSEIF(IGTYP == 17 .OR. IGTYP ==51 .OR. IGTYP ==52) THEN
            DX=THK(N)
        ELSE 
          DX = GEO(1,IXC(6,N))
          ENDIF
        IF (IXC(1,N)>0) THEN
          ST = PM(20,IXC(1,N))
        ELSE
          ST = ZERO
        ENDIF
        IF (DX>DXM) THEN
          DXM = DX
          NEL = N
          STM = ST
        ELSEIF(DX==DXM) THEN
          IF ((ST>STM).OR.(STM==ZERO)) THEN
            NEL = N
            STM = ST
          ENDIF
        ENDIF       
  430  CONTINUE
      ENDIF
      RETURN
      END
