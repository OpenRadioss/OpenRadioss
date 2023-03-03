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
C
Chd|====================================================================
Chd|  I11COQ                        source/interfaces/inter3d1/i11coq.F
Chd|-- called by -----------
Chd|        I11STI3                       source/interfaces/inter3d1/i11sti3.F
Chd|        I20STI3E                      source/interfaces/inter3d1/i20sti3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I11COQ(IRECT,IXC,IXTG,NINT,NEL,
     .                  NELTG,IS ,GEO,PM,THK,IGEO,
     .                  KNOD2ELC,KNOD2ELTG,NOD2ELC,NOD2ELTG,
     .                  PM_STACK, IWORKSH)
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
      INTEGER NINT, NEL, IS, NELTG
      INTEGER IRECT(2,*), IXC(NIXC,*), IXTG(NIXTG,*),IGEO(NPROPGI,*),
     .        KNOD2ELC(*),KNOD2ELTG(*),NOD2ELC(*),NOD2ELTG(*),IWORKSH(3,*)
C     REAL
      my_real
     .   GEO(NPROPG,*), PM(NPROPM,*),THK(*),PM_STACK(20,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, J, II, K, IAD,IGTYP,IPGMAT,IGMAT,ISUBSTACK
C     REAL
      my_real
     .   DXM, STM, DX, ST
C
      NEL=0
      NELTG=0
      DXM = ZERO
      STM = ZERO
      IPGMAT = 700
      IGMAT = 0
      IF(NUMELTG/=0)THEN
        DO 230 IAD=KNOD2ELTG(IRECT(1,IS))+1,KNOD2ELTG(IRECT(1,IS)+1)
          N = NOD2ELTG(IAD)
          DO 220 J=1,2
            II=IRECT(J,IS)
            DO 210 K=1,3
              IF(IXTG(K+1,N)==II) GOTO 220
  210       CONTINUE
            GOTO 230
  220     CONTINUE
          IGTYP = GEO(11,IXTG(5,N))
          IF ( THK(NUMELC+N) /= ZERO .AND. IINTTHICK == 0) THEN
            DX=THK(NUMELC+N)
          ELSEIF(IGTYP ==17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
            DX=THK(NUMELC+N)
          ELSE
            DX = GEO(1,IXTG(5,N))
          ENDIF
          IGMAT = IGEO(98,IXTG(5,N))
          IF (IXTG(1,N)>0) THEN
            ST = PM(20,IXTG(1,N))
            IF(IGTYP == 11 .AND. IGMAT > 0) THEN
               ST=GEO(IPGMAT+2,IXTG(5,N))
            ELSEIF(IGTYP == 52 .OR. 
     .           ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
               ISUBSTACK = IWORKSH(3,NUMELC+N)
               ST=PM_STACK(2,ISUBSTACK)
            ENDIF
          ELSE
            ST = ZERO
          ENDIF
          IF (DX>DXM) THEN
            DXM = DX
            NELTG = N
            STM = ST
          ELSEIF(DX==DXM) THEN
            IF ((ST>=STM).OR.(STM==ZERO)) THEN
              NELTG = N
              STM = ST
            ENDIF
          ENDIF        
  230   CONTINUE
      ENDIF
      IF(NUMELC/=0) THEN
        DO 430 IAD=KNOD2ELC(IRECT(1,IS))+1,KNOD2ELC(IRECT(1,IS)+1)
          N = NOD2ELC(IAD)
          DO 420 J=1,2
            II=IRECT(J,IS)
            DO 410 K=1,4
              IF(IXC(K+1,N)==II) GOTO 420
  410       CONTINUE
            GOTO 430
  420     CONTINUE
          IGTYP = IGEO(11,IXC(6,N))
          IF ( THK(N) /= ZERO .AND. IINTTHICK == 0) THEN
            DX=THK(N)
          ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP  == 52) THEN
            DX=THK(N)
          ELSE
            DX = GEO(1,IXC(6,N))
          ENDIF
          IGMAT = IGEO(98,IXC(6,N))
          IF (IXC(1,N)>0) THEN
            ST = PM(20,IXC(1,N))
            IF(IGTYP == 11 .AND. IGMAT > 0)THEN
              ST=GEO(IPGMAT + 2 ,IXC(6,N))
            ELSEIF(IGTYP == 52 .OR. 
     .        ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0)) THEN
               ISUBSTACK = IWORKSH(3,N)
               ST=PM_STACK(2,ISUBSTACK)
            ENDIF   
          ELSE
            ST = 0.
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
  430   CONTINUE
      ENDIF
      RETURN
      END
C
Chd|====================================================================
Chd|  I11FIL                        source/interfaces/inter3d1/i11coq.F
Chd|-- called by -----------
Chd|        I11STI3                       source/interfaces/inter3d1/i11sti3.F
Chd|        I20STI3E                      source/interfaces/inter3d1/i20sti3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I11FIL(IRECT,IXT,IXP,IXR,NINT,NELT,
     .                  NELP,NELR,NELX,IS,NOD2EL1D,
     .                  KNOD2EL1D,KXX,IXX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINT, NELT, IS, NELP, NELR, NELX
      INTEGER IRECT(2,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .        NOD2EL1D(*),KNOD2EL1D(*),KXX(NIXX,*),IXX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, J, II, K, IAD
C
      NELP=0
      NELT=0
      NELR=0
      NELX=0
      IF(NUMELT+NUMELP+NUMELR+NUMELX/=0)THEN
       DO IAD=KNOD2EL1D(IRECT(1,IS))+1,KNOD2EL1D(IRECT(1,IS)+1)
        N = NOD2EL1D(IAD)
        IF(N>0.AND.N<=NUMELT)THEN
          IF(IRECT(1,IS)==IXT(2,N).AND.IRECT(2,IS)==IXT(3,N).OR.
     .       IRECT(2,IS)==IXT(2,N).AND.IRECT(1,IS)==IXT(3,N))THEN
            NELT=N
            RETURN
          ENDIF
        ELSEIF(N>0.AND.N<=NUMELT+NUMELP)THEN
          N=N-NUMELT
          IF(IRECT(1,IS)==IXP(2,N).AND.IRECT(2,IS)==IXP(3,N).OR.
     .       IRECT(2,IS)==IXP(2,N).AND.IRECT(1,IS)==IXP(3,N))THEN
            NELP=N
            RETURN
          ENDIF
        ELSEIF(N>0.AND.N<=NUMELT+NUMELP+NUMELR)THEN
          N=N-NUMELT-NUMELP
          IF(IRECT(1,IS)==IXR(2,N).AND.IRECT(2,IS)==IXR(3,N).OR.
     .       IRECT(2,IS)==IXR(2,N).AND.IRECT(1,IS)==IXR(3,N).OR.
     .       IRECT(1,IS)==IXR(3,N).AND.IRECT(2,IS)==IXR(4,N).OR.
     .       IRECT(2,IS)==IXR(3,N).AND.IRECT(1,IS)==IXR(4,N))THEN
            NELR=N
            RETURN
          ENDIF
        ELSEIF(N>0.AND.N<=NUMELT+NUMELP+NUMELR+NUMELX)THEN
          N=N-NUMELT-NUMELP-NUMELR
          DO J=KXX(4,N),KXX(4,N)+KXX(3,N)-1
            IF(IRECT(1,IS)==IXX(J).AND.IRECT(2,IS)==IXX(J+1).OR.
     .         IRECT(2,IS)==IXX(J).AND.IRECT(1,IS)==IXX(J+1))THEN
                NELX=N
                RETURN
            ENDIF
          ENDDO

        ENDIF
       ENDDO
      ENDIF
C
      RETURN
      END
