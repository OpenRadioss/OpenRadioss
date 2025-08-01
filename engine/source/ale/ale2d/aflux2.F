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
!||====================================================================
!||    aflux2                 ../engine/source/ale/ale2d/aflux2.F
!||--- called by ------------------------------------------------------
!||    aflux0                 ../engine/source/ale/aflux0.F
!||--- calls      -----------------------------------------------------
!||    amulf2                 ../engine/source/ale/bimat/amulf2.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||====================================================================
      SUBROUTINE AFLUX2(PM         ,IXQ  ,V    ,W     ,X   ,
     .                  FLUX       ,FLU1 ,FILL ,DFILL ,VOL ,
     .                  ALE_CONNECT,ALPH)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,NUMELQ)
      my_real PM(NPROPM,NUMMAT),V(3,NUMNOD),W(3,NUMNOD),X(3,NUMNOD),FLUX(4,*),FLU1(*),FILL(*),DFILL(*),VOL(*),ALPH(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MAT(mvsiz), NC1(mvsiz), NC2(mvsiz), NC3(mvsiz), NC4(mvsiz), I,II,J,IAD2
      my_real
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ), 
     .   N1Y(MVSIZ), N2Y(MVSIZ), N3Y(MVSIZ), N4Y(MVSIZ), N1Z(MVSIZ), N2Z(MVSIZ), N3Z(MVSIZ), N4Z(MVSIZ),
     .   FLUX1(MVSIZ), FLUX2(MVSIZ), FLUX3(MVSIZ), FLUX4(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ), VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ),
     .   VDY1(MVSIZ), VDY2(MVSIZ),VDY3(MVSIZ), VDY4(MVSIZ), 
     .   VDZ1(MVSIZ), VDZ2(MVSIZ),VDZ3(MVSIZ), VDZ4(MVSIZ), UPW(mvsiz),
     .   REDUC,UPWL(4,MVSIZ)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      MAT(1) = 1
      DO I=LFT,LLT
        II=I+NFT
        MAT(I)=IXQ(1,II)
        NC1(I)=IXQ(2,II)
        NC2(I)=IXQ(3,II)
        NC3(I)=IXQ(4,II)
        NC4(I)=IXQ(5,II)

        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))

        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))

        Y3(I)=X(2,NC3(I))
        Z3(I)=X(3,NC3(I))

        Y4(I)=X(2,NC4(I))
        Z4(I)=X(3,NC4(I))

        VDY1(I)=V(2,NC1(I)) - W(2,NC1(I))
        VDZ1(I)=V(3,NC1(I)) - W(3,NC1(I))

        VDY2(I)=V(2,NC2(I)) - W(2,NC2(I))
        VDZ2(I)=V(3,NC2(I)) - W(3,NC2(I))

        VDY3(I)=V(2,NC3(I)) - W(2,NC3(I))
        VDZ3(I)=V(3,NC3(I)) - W(3,NC3(I))

        VDY4(I)=V(2,NC4(I)) - W(2,NC4(I))
        VDZ4(I)=V(3,NC4(I)) - W(3,NC4(I))
      ENDDO
C-----------------------------------------------
C     RELATIVE VELOCITY ON FACE
C-----------------------------------------------
      DO I=LFT,LLT
        VY1(I)=HALF*(VDY1(I)+VDY2(I))
        VY2(I)=HALF*(VDY2(I)+VDY3(I))
        VY3(I)=HALF*(VDY3(I)+VDY4(I))
        VY4(I)=HALF*(VDY4(I)+VDY1(I))

        VZ1(I)=HALF*(VDZ1(I)+VDZ2(I))
        VZ2(I)=HALF*(VDZ2(I)+VDZ3(I))
        VZ3(I)=HALF*(VDZ3(I)+VDZ4(I))
        VZ4(I)=HALF*(VDZ4(I)+VDZ1(I))
      ENDDO
C------------------------------------------
C     NORMAL VECTOR
C------------------------------------------
      DO I=LFT,LLT
        N1Y(I)= (Z2(I)-Z1(I))
        N1Z(I)=-(Y2(I)-Y1(I))

        N2Y(I)= (Z3(I)-Z2(I))
        N2Z(I)=-(Y3(I)-Y2(I))

        N3Y(I)= (Z4(I)-Z3(I))
        N3Z(I)=-(Y4(I)-Y3(I))

        N4Y(I)= (Z1(I)-Z4(I))
        N4Z(I)=-(Y1(I)-Y4(I))
      ENDDO

      IF(N2D == 1)THEN
        DO I=LFT,LLT
         N1Y(I)= N1Y(I)*(Y1(I)+Y2(I))*HALF
         N1Z(I)= N1Z(I)*(Y1(I)+Y2(I))*HALF
         N2Y(I)= N2Y(I)*(Y2(I)+Y3(I))*HALF
         N2Z(I)= N2Z(I)*(Y2(I)+Y3(I))*HALF
         N3Y(I)= N3Y(I)*(Y3(I)+Y4(I))*HALF
         N3Z(I)= N3Z(I)*(Y3(I)+Y4(I))*HALF
         N4Y(I)= N4Y(I)*(Y1(I)+Y4(I))*HALF
         N4Z(I)= N4Z(I)*(Y1(I)+Y4(I))*HALF   
       ENDDO
      ENDIF
C--------------------
C     VOLUME FLUXES
C--------------------
      DO I=LFT,LLT
        FLUX1(I)=(VY1(I)*N1Y(I)+VZ1(I)*N1Z(I))
        FLUX2(I)=(VY2(I)*N2Y(I)+VZ2(I)*N2Z(I))
        FLUX3(I)=(VY3(I)*N3Y(I)+VZ3(I)*N3Z(I))
        FLUX4(I)=(VY4(I)*N4Y(I)+VZ4(I)*N4Z(I))
      ENDDO
C-----------------------------------------------
C     MULTIMAT LAW51 (SUBMATERIALS)
C-----------------------------------------------
      IF(NINT(PM(19,MAT(1))) == 51)THEN
        DO I=LFT,LLT
          FLUX(1,I)=FLUX1(I)
          FLUX(2,I)=FLUX2(I)
          FLUX(3,I)=FLUX3(I)
          FLUX(4,I)=FLUX4(I)
        ENDDO
        RETURN
      ENDIF
C-----------------------------------------------
C     UPWIND
C-----------------------------------------------
      DO J=1,4
        DO I=LFT,LLT
          UPWL(J,I)=PM(16,MAT(I))
        ENDDO
      ENDDO
      !======================================================!
      !  BOUNDARY FACE : no volume flux by default           !
      !    slip wall bc                                      !
      !======================================================!
      DO I=LFT,LLT
       IAD2 = ALE_CONNECT%ee_connect%iad_connect(I + NFT)
       REDUC=PM(92,MAT(I))
       
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
       IF(II == 0)THEN
        FLUX1(I)=FLUX1(I)*REDUC
       ENDIF
C
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
       IF(II == 0)THEN
        FLUX2(I)=FLUX2(I)*REDUC
       ENDIF
C
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
       IF(II == 0)THEN
        FLUX3(I)=FLUX3(I)*REDUC
       ENDIF
C
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
       IF(II == 0)THEN
        FLUX4(I)=FLUX4(I)*REDUC
       ENDIF
C
      ENDDO !next I
C-----------------------------------------------
C     MULTIMAT LAW 20 (OBSOLETE)
C-----------------------------------------------
      IF(JMULT /= 0)THEN
        DO I=LFT,LLT
          UPW(I) =PM(16,MAT(I))
        ENDDO
        CALL AMULF2(FILL,DFILL,FLUX,FLU1,VOL,ALE_CONNECT,ALPH,
     .              FLUX1, FLUX2, FLUX3, FLUX4, UPW, 
     .              NC1, NC2, NC3, NC4)
      ELSE
C
        DO I=LFT,LLT
          FLUX(1,I)=FLUX1(I)-UPWL(1,I)*ABS(FLUX1(I))
          FLUX(2,I)=FLUX2(I)-UPWL(2,I)*ABS(FLUX2(I))
          FLUX(3,I)=FLUX3(I)-UPWL(3,I)*ABS(FLUX3(I))
          FLUX(4,I)=FLUX4(I)-UPWL(4,I)*ABS(FLUX4(I))

          FLU1(I)  =FLUX1(I)+UPWL(1,I)*ABS(FLUX1(I))
     .             +FLUX2(I)+UPWL(2,I)*ABS(FLUX2(I))
     .             +FLUX3(I)+UPWL(3,I)*ABS(FLUX3(I))
     .             +FLUX4(I)+UPWL(4,I)*ABS(FLUX4(I))
        ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
      END
C
