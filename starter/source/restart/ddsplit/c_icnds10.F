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
Chd|====================================================================
Chd|  C_ICNDS10                     source/restart/ddsplit/c_icnds10.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE C_ICNDS10(ICNDS10,ITAGND,PROC,NS10E_L,NBDDCNDM,MS,MS_ND)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  ICNDS10(3,*),PROC,NS10E_L,ITAGND(*),NBDDCNDM
      my_real
     .   MS(*),MS_ND
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN,N1,N2,P,NF
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGI
C-----------------------------------------------
C------ COMPUTE MS_ND per PROC
        MS_ND = ZERO     
      DO N = 1, NS10E
       NN = ICNDS10(1,N)
        DO P = 1, PROC
          IF(NLOCAL(NN,P)==1)GOTO 10
        ENDDO
       IF(NLOCAL(NN,PROC+1)==1.AND.ITAGND(NN)<=NS10E)THEN
        MS_ND = MS_ND + MS(NN)
       ENDIF
 10     CONTINUE
      ENDDO
C
      DO N = 1, NS10E
       NN = ICNDS10(1,N)
       IF(NLOCAL(NN,PROC+1)==1.AND.ITAGND(NN)<=NS10E)THEN
c        DO P = 1, PROC
c         IF(NLOCAL(NN,P)==1) GO TO 100           
c        ENDDO
        N1 = ICNDS10(2,N)
        N2 = ICNDS10(3,N)
        NS10E_L = NS10E_L + 1
 100    CONTINUE         
       ENDIF
      ENDDO
      IF (NS10E_L==0) RETURN
!     allocate 1d array
      ALLOCATE( TAGI(NUMNOD) )
      TAGI(1:NUMNOD) = 0 
C-------------pour main nodes---------
      DO N = 1, NS10E
        NN = ICNDS10(1,N)
        IF(NLOCAL(NN,PROC+1)/=1.OR.ITAGND(NN)>NS10E) CYCLE
        N1 = ICNDS10(2,N)
        N2 = ICNDS10(3,N)
C----- normally N1,N2 are local as  NN-----     
         NF = 0
         DO P = 1, NSPMD
          NF = NF +NLOCAL(NN,P)           
         ENDDO
            IF(NLOCAL(N1,PROC+1)==1) THEN
              IF(TAGI(N1)==0.AND.NF > 1) THEN
C decompte des frontieres de main nodes
                DO P = 1, NSPMD
                  IF(NLOCAL(N1,P)==1)THEN
                    NBDDCNDM = NBDDCNDM + 1
                  END IF
                END DO
C on ne se compte pas soi-meme
                NBDDCNDM = NBDDCNDM - 1
C pour ne pas prendre en compte 2 fois des noeuds main ds les frontieres
c                IF (NF > 1) NBDDCNDM = NBDDCNDM + 1
                TAGI(N1) = 1
              END IF
            ENDIF
C----- N2 -------       
            IF(NLOCAL(N2,PROC+1)==1) THEN
              IF(TAGI(N2)==0.AND.NF > 1) THEN
C decompte des frontieres de main nodes
                DO P = 1, NSPMD
                  IF(NLOCAL(N2,P)==1)THEN
                    NBDDCNDM = NBDDCNDM + 1
                  END IF
                END DO
C on ne se compte pas soi-meme
                NBDDCNDM = NBDDCNDM - 1
C pour ne pas prendre en compte 2 fois des noeuds main ds les frontieres
c                IF (NF > 1) NBDDCNDM = NBDDCNDM + 1
                TAGI(N2) = 1
              END IF
            ENDIF
      END DO
C ----------------------------
      DEALLOCATE( TAGI )
C ----------------------------
C
      RETURN
      END
