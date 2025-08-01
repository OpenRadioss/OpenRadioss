Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
CCgw|============================================================
!||====================================================================
!||    w_iconv     ../starter/source/restart/ddsplit/w_iconv.F
!||--- called by ------------------------------------------------------
!||    ddsplit     ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||====================================================================
      SUBROUTINE W_ICONV(IBCV,CEP,PROC,NODLOCAL,NCONV_L,LEN_IA,
     .                   CEL,IPARG,NUMLOCGROUP,NUMCONV,NICONV)
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
      INTEGER ,INTENT(IN) :: NUMCONV
      INTEGER ,INTENT(IN) :: NICONV
      INTEGER LEN_IA, PROC, NCONV_L, IBCV(NICONV,*),CEP(*),
     .        NODLOCAL(*)
      INTEGER IPARG(NPARG,*),NUMLOCGROUP(NGROUP),CEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IBTMP(NICONV,NCONV_L), N1, N2, N3, N4, OFF, NB_L
      INTEGER K,ID,ITY,NG,NELG,NFT
C-----------------------------------------------
      OFF = NUMELC+NUMELTG+NUMELS+NUMELQ+NUMELT+NUMELP+NUMELR
     +    + NUMELX + NCONLD
      NB_L = 0
      K = 0
C
      DO I = 1, NUMCONV
        IF(CEP(I+OFF)==PROC) THEN
           NB_L = NB_L + 1
           N1 = IBCV(1,I)
           N2 = IBCV(2,I)
           N3 = IBCV(3,I)
           N4 = IBCV(4,I)
           IBTMP(1,NB_L) = NODLOCAL(N1)
           IBTMP(2,NB_L) = NODLOCAL(N2)
           IBTMP(3,NB_L) = NODLOCAL(N3)
           IBTMP(4,NB_L) = 0
           IF(N4 /= 0) IBTMP(4,NB_L) = NODLOCAL(N4)
           DO J = 5, NICONV
             IBTMP(J,NB_L) = IBCV(J,I)
           END DO
C ============== Local numbering
           IF(IBTMP(7,NB_L)  == 1) THEN  
            ID = IBTMP(8,NB_L) 
            DO NG = 1,NGROUP
              NELG = IPARG(2,NG)
              NFT  = IPARG(3,NG)
              ITY  = IPARG(5,NG)
              IF (ITY == 1) THEN
              ! Solid
                K = ID - NFT
                IF (K <= NELG) GOTO 100
              ENDIF 
            ENDDO  ! NG = 1,NGROUP
 100        CONTINUE
            IBTMP(NICONV-1,NB_L) = NUMLOCGROUP(NG) !element local group number
            IBTMP(NICONV  ,NB_L) = K  ! element index in the group
            IBTMP(8       ,NB_L) = CEL(ID)
           ENDIF
        END IF
      END DO
C
       CALL WRITE_I_C(IBTMP,NICONV*NCONV_L)
       LEN_IA = LEN_IA + NICONV*NCONV_L
C
      RETURN
      END
