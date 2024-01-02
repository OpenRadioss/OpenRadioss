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
Chd|  EFLUX3                        source/ale/euler3d/eflux3.F   
Chd|-- called by -----------
Chd|        AFLUX0                        source/ale/aflux0.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE EFLUX3(PM,IXS,V,FLUX,FLU1,VEUL,ALE_CONNECT,TAG22)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22TRI_MOD
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
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "inter22.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
! SPMD CASE : SIXS >= NIXS*NUMELS    (SIXS = NIXS*NUMELS_L+NIXS*NSVOIS_L)
! IXQ(1:NIXS, 1:NUMELS) local elems
!    (1:NIXS, NUMELS+1:) additional elems (also on adjacent domains but connected to the boundary of the current domain)
!
      INTEGER IXS(NIXS,SIXS/NIXS)
      my_real PM(NPROPM,NUMMAT), V(3,NUMNOD), FLUX(6,*), FLU1(*), VEUL(LVEUL,*), TAG22(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MAT(MVSIZ), NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ), NC5(MVSIZ), NC6(MVSIZ),
     .   NC7(MVSIZ), NC8(MVSIZ), I, II,J, IAD2
      my_real N1X(MVSIZ),
     .         N2X(MVSIZ), N3X(MVSIZ), N4X(MVSIZ), N5X(MVSIZ), N6X(MVSIZ), N1Y(MVSIZ), N2Y(MVSIZ), N3Y(MVSIZ),
     .         N4Y(MVSIZ), N5Y(MVSIZ), N6Y(MVSIZ), N1Z(MVSIZ), N2Z(MVSIZ), N3Z(MVSIZ), N4Z(MVSIZ), N5Z(MVSIZ),
     .         N6Z(MVSIZ), FLUX1(MVSIZ), FLUX2(MVSIZ), FLUX3(MVSIZ), FLUX4(MVSIZ), FLUX5(MVSIZ),
     .         FLUX6(MVSIZ), VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ), VX4(MVSIZ), VX5(MVSIZ), VX6(MVSIZ),
     .         VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ), VY5(MVSIZ), VY6(MVSIZ), VZ1(MVSIZ), VZ2(MVSIZ),
     .         VZ3(MVSIZ), VZ4(MVSIZ), VZ5(MVSIZ), VZ6(MVSIZ), VDX1(MVSIZ), VDX2(MVSIZ), VDX3(MVSIZ),
     .         VDX4(MVSIZ), VDX5(MVSIZ), VDX6(MVSIZ), VDX7(MVSIZ), VDX8(MVSIZ), VDY1(MVSIZ), VDY2(MVSIZ),
     .         VDY3(MVSIZ), VDY4(MVSIZ), VDY5(MVSIZ), VDY6(MVSIZ), VDY7(MVSIZ), VDY8(MVSIZ), VDZ1(MVSIZ),
     .         VDZ2(MVSIZ), VDZ3(MVSIZ), VDZ4(MVSIZ), VDZ5(MVSIZ), VDZ6(MVSIZ), VDZ7(MVSIZ), VDZ8(MVSIZ),
     .         REDUC,UPWL(6,MVSIZ)
      LOGICAL debug_outp
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      !======================================================!
      ! INITIALIZATION : COORDINATES & RELATIVE VELOCITIES   !
      !======================================================!
      DO I=LFT,LLT
        II=I+NFT
        MAT(I)=IXS(1,II)
        NC1(I)=IXS(2,II)
        NC2(I)=IXS(3,II)
        NC3(I)=IXS(4,II)
        NC4(I)=IXS(5,II)
        NC5(I)=IXS(6,II)
        NC6(I)=IXS(7,II)
        NC7(I)=IXS(8,II)
        NC8(I)=IXS(9,II)

        VDX1(I)=V(1,NC1(I))
        VDY1(I)=V(2,NC1(I))
        VDZ1(I)=V(3,NC1(I))

        VDX2(I)=V(1,NC2(I))
        VDY2(I)=V(2,NC2(I))
        VDZ2(I)=V(3,NC2(I))

        VDX3(I)=V(1,NC3(I))
        VDY3(I)=V(2,NC3(I))
        VDZ3(I)=V(3,NC3(I))

        VDX4(I)=V(1,NC4(I))
        VDY4(I)=V(2,NC4(I))
        VDZ4(I)=V(3,NC4(I))

        VDX5(I)=V(1,NC5(I))
        VDY5(I)=V(2,NC5(I))
        VDZ5(I)=V(3,NC5(I))

        VDX6(I)=V(1,NC6(I))
        VDY6(I)=V(2,NC6(I))
        VDZ6(I)=V(3,NC6(I))

        VDX7(I)=V(1,NC7(I))
        VDY7(I)=V(2,NC7(I))
        VDZ7(I)=V(3,NC7(I))

        VDX8(I)=V(1,NC8(I))
        VDY8(I)=V(2,NC8(I))
        VDZ8(I)=V(3,NC8(I))
      ENDDO
      
      !======================================================!
      ! RELATIVE VELOCITIES ON EACH FACE                     !
      !    [V_face] = 1/4 Sum([V_node])                      !
      !    Results are divided by 2 at this step :           !
      !    [0.5*V_face] = 1/8 Sum([V_node])                  !       
      !======================================================!
      DO I=LFT,LLT
        VX1(I)=ONE_OVER_8*(VDX1(I)+VDX2(I)+VDX3(I)+VDX4(I))
        VX2(I)=ONE_OVER_8*(VDX3(I)+VDX4(I)+VDX8(I)+VDX7(I))
        VX3(I)=ONE_OVER_8*(VDX5(I)+VDX6(I)+VDX7(I)+VDX8(I))
        VX4(I)=ONE_OVER_8*(VDX1(I)+VDX2(I)+VDX6(I)+VDX5(I))
        VX5(I)=ONE_OVER_8*(VDX2(I)+VDX3(I)+VDX7(I)+VDX6(I))
        VX6(I)=ONE_OVER_8*(VDX1(I)+VDX4(I)+VDX8(I)+VDX5(I))

        VY1(I)=ONE_OVER_8*(VDY1(I)+VDY2(I)+VDY3(I)+VDY4(I))
        VY2(I)=ONE_OVER_8*(VDY3(I)+VDY4(I)+VDY8(I)+VDY7(I))
        VY3(I)=ONE_OVER_8*(VDY5(I)+VDY6(I)+VDY7(I)+VDY8(I))
        VY4(I)=ONE_OVER_8*(VDY1(I)+VDY2(I)+VDY6(I)+VDY5(I))
        VY5(I)=ONE_OVER_8*(VDY2(I)+VDY3(I)+VDY7(I)+VDY6(I))
        VY6(I)=ONE_OVER_8*(VDY1(I)+VDY4(I)+VDY8(I)+VDY5(I))

        VZ1(I)=ONE_OVER_8*(VDZ1(I)+VDZ2(I)+VDZ3(I)+VDZ4(I))
        VZ2(I)=ONE_OVER_8*(VDZ3(I)+VDZ4(I)+VDZ8(I)+VDZ7(I))
        VZ3(I)=ONE_OVER_8*(VDZ5(I)+VDZ6(I)+VDZ7(I)+VDZ8(I))
        VZ4(I)=ONE_OVER_8*(VDZ1(I)+VDZ2(I)+VDZ6(I)+VDZ5(I))
        VZ5(I)=ONE_OVER_8*(VDZ2(I)+VDZ3(I)+VDZ7(I)+VDZ6(I))
        VZ6(I)=ONE_OVER_8*(VDZ1(I)+VDZ4(I)+VDZ8(I)+VDZ5(I))  
      ENDDO
      
      !======================================================!
      ! NORMAL VECTORS ON EACH DACE                          !
      !    2S[n] = [diag1] x [diag2]                         ! 
      !    where                                             !
      !      [n] : unitary normal vector on face             !
      !======================================================!
      DO I=LFT,LLT
        II=I+NFT
        N1X(I)=VEUL(14,II)
        N2X(I)=VEUL(15,II)
        N3X(I)=VEUL(16,II)
        N4X(I)=VEUL(17,II)
        N5X(I)=VEUL(18,II)
        N6X(I)=VEUL(19,II)

        N1Y(I)=VEUL(20,II)
        N2Y(I)=VEUL(21,II)
        N3Y(I)=VEUL(22,II)
        N4Y(I)=VEUL(23,II)
        N5Y(I)=VEUL(24,II)
        N6Y(I)=VEUL(25,II)

        N1Z(I)=VEUL(26,II)
        N2Z(I)=VEUL(27,II)
        N3Z(I)=VEUL(28,II)
        N4Z(I)=VEUL(29,II)
        N5Z(I)=VEUL(30,II)
        N6Z(I)=VEUL(31,II)
      ENDDO
      
      !======================================================!
      ! FLUXES CALCULATION ON EACH FACE                      !
      !    FLUX_face = [V_face].[n]                          ! 
      !              = [0.5*V_face] . [2S*n]                 !       
      !======================================================!
      DO I=LFT,LLT
        FLUX1(I)=(VX1(I)*N1X(I)+VY1(I)*N1Y(I)+VZ1(I)*N1Z(I))
        FLUX2(I)=(VX2(I)*N2X(I)+VY2(I)*N2Y(I)+VZ2(I)*N2Z(I))
        FLUX3(I)=(VX3(I)*N3X(I)+VY3(I)*N3Y(I)+VZ3(I)*N3Z(I))
        FLUX4(I)=(VX4(I)*N4X(I)+VY4(I)*N4Y(I)+VZ4(I)*N4Z(I))
        FLUX5(I)=(VX5(I)*N5X(I)+VY5(I)*N5Y(I)+VZ5(I)*N5Z(I))
        FLUX6(I)=(VX6(I)*N6X(I)+VY6(I)*N6Y(I)+VZ6(I)*N6Z(I))
      ENDDO


      !INTERFACE 22 ONLY - OUTPUT---------------! OBSOLETE
      IF(INT22>0)THEN
        debug_outp = .false.
        if(ibug22_flux/=0)then
          debug_outp = .false.
          if(ibug22_flux>0)then
            do i=lft,llt
              if(ixs(11,I+nft) == ibug22_flux)then
                if(tag22(i)==zero)then
                  debug_outp=.true.
                  exit
                endif
              endif
            enddo
          elseif(ibug22_flux==-1)then
            debug_outp = .true.
          endif
        endif      
        !INTERFACE 22 ONLY - OUTPUT---------------!
        if(debug_outp)then
!#!include "lockon.inc"       
          print *, "    |--------eflux3.F--------|"
          print *, "    |   THREAD INFORMATION   |"
          print *, "    |------------------------|" 
          print *, "     NCYCLE =", NCYCLE
          do i=lft,llt
            if(ibug22_flux/=ixs(11,I+NFT).and.ibug22_flux/=-1)cycle        
            if (tag22(i)==zero)print *,"       UNCUT"!cycle
            print *,                    "      brique=", ixs(11,nft+i)
            write (*,FMT='(A,6E26.14)') "       Flux(1:6)=", FLUX(1:6,I)
            write (*,FMT='(A,1E26.14)') "       Flu1=", FLU1(I)
            print *, "      ------------------------"          
          enddo
!#!include "lockoff.inc"       
        endif
      ENDIF    
      !-----------------------------------------!


      !======================================================!
      ! TRIMATERIAL CASE INITIALIZATION (LAW51)              !
      ! -->RETURN                                            !
      !======================================================!
      IF(NINT(PM(19,MAT(1))) == 51)THEN
        DO I=LFT,LLT
          FLUX(1,I)=FLUX1(I)
          FLUX(2,I)=FLUX2(I)
          FLUX(3,I)=FLUX3(I)
          FLUX(4,I)=FLUX4(I)
          FLUX(5,I)=FLUX5(I)
          FLUX(6,I)=FLUX6(I)
        ENDDO !next I
        RETURN
      ENDIF

      !======================================================!
      !  BOUNDARY FACE : no volume flux by default           !
      !    slip wall bc                                      !
      !======================================================!
      DO J=1,6
        DO I=LFT,LLT
          UPWL(J,I)=PM(16,MAT(I))
        ENDDO !next I
      ENDDO !next J

      DO I=LFT,LLT
       IAD2 = ALE_CONNECT%ee_connect%iad_connect(I + NFT)
       REDUC=PM(92,MAT(I))
       !---face1---!
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
       IF(II == 0)THEN
        FLUX1(I)=FLUX1(I)*REDUC
       ENDIF
       !---face2---!
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
       IF(II == 0)THEN
        FLUX2(I)=FLUX2(I)*REDUC
       ENDIF
       !---face3---!
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
       IF(II == 0)THEN
        FLUX3(I)=FLUX3(I)*REDUC
       ENDIF
       !---face4---!
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
       IF(II == 0)THEN
        FLUX4(I)=FLUX4(I)*REDUC
       ENDIF
       !---face5---!
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
       IF(II == 0)THEN
        FLUX5(I)=FLUX5(I)*REDUC
       ENDIF
       !---face6---!
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1)
       IF(II == 0)THEN
        FLUX6(I)=FLUX6(I)*REDUC
       ENDIF
      ENDDO !next I

      DO I=LFT,LLT
        FLUX(1,I)=FLUX1(I)-UPWL(1,I)*ABS(FLUX1(I))
        FLUX(2,I)=FLUX2(I)-UPWL(2,I)*ABS(FLUX2(I))
        FLUX(3,I)=FLUX3(I)-UPWL(3,I)*ABS(FLUX3(I))
        FLUX(4,I)=FLUX4(I)-UPWL(4,I)*ABS(FLUX4(I))
        FLUX(5,I)=FLUX5(I)-UPWL(5,I)*ABS(FLUX5(I))
        FLUX(6,I)=FLUX6(I)-UPWL(6,I)*ABS(FLUX6(I))

        FLU1(I)  =FLUX1(I)+UPWL(1,I)*ABS(FLUX1(I))
     .           +FLUX2(I)+UPWL(2,I)*ABS(FLUX2(I))
     .           +FLUX3(I)+UPWL(3,I)*ABS(FLUX3(I))
     .           +FLUX4(I)+UPWL(4,I)*ABS(FLUX4(I))
     .           +FLUX5(I)+UPWL(5,I)*ABS(FLUX5(I))
     .           +FLUX6(I)+UPWL(6,I)*ABS(FLUX6(I))
      ENDDO !next I
C-----------------------------------------------
      RETURN
      END
