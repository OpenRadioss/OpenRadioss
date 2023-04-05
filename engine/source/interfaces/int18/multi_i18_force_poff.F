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
Chd|====================================================================
Chd|  MULTI_I18_FORCE_POFF          source/interfaces/int18/multi_i18_force_poff.F
Chd|-- called by -----------
Chd|        I18FOR3                       source/interfaces/int18/i18for3.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE MULTI_I18_FORCE_POFF(DT, JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     .     NSVG  ,H1   ,H2   ,H3   ,H4     ,STIF ,
     .     FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     .     FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     .     FXI   ,FYI  ,FZI  ,A    ,STIFN  ,NIN,
     .     JTASK, MULTI_FVM  ,X    ,IXS    ,V  ,
     .     ELBUF_TAB,IGROUPS,IPARG ,MSI)
!$COMMENT
!       MULTI_I18_FORCE_POFF description
!       accumulation of force for local and remote nodes
!       
!       MULTI_I18_FORCE_POFF organization :
!       - secondary nodes:
!            * if NSV > 0 --> local node (phantom node id = NSV - NUMNOD)
!                             accumulation in FORC_INT array
!            * if NSV < 0 --> remote node
!                             accumulation in AFI array
!$ENDCOMMENT

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MULTI_FVM_MOD
      USE TRI7BOX
      USE ELBUFDEF_MOD       
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
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER JLT, NIN,INTTH,JTASK,IGROUPS(NUMELS),IPARG(NPARG,*),
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ),IXS(NIXS,*)
      my_real, DIMENSION(2*MVSIZ), INTENT(in) :: MSI
      my_real
     .    DT, H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .    FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .    FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .    FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .    FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .    FXI(MVSIZ),FYI(MVSIZ),FZI(MVSIZ),
     .    A(3,*), X(3,*), STIFN(*), V(3,*)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, ISHIFT, NODFI, NG, ILOC, NEL, NFT
      INTEGER :: SHIFT_FORCE_INT
      my_real MASS
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
c     Nodal forces on the interface
      DO I=1,JLT
         J1=IX1(I)
         A(1,J1)=A(1,J1)+FX1(I)
         A(2,J1)=A(2,J1)+FY1(I)
         A(3,J1)=A(3,J1)+FZ1(I)
         STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H1(I))
C     
         J1=IX2(I)
         A(1,J1)=A(1,J1)+FX2(I)
         A(2,J1)=A(2,J1)+FY2(I)
         A(3,J1)=A(3,J1)+FZ2(I)
         STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H2(I))
C     
         J1=IX3(I)
         A(1,J1)=A(1,J1)+FX3(I)
         A(2,J1)=A(2,J1)+FY3(I)
         A(3,J1)=A(3,J1)+FZ3(I)
         STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H3(I))
C     
         J1=IX4(I)
         A(1,J1)=A(1,J1)+FX4(I)
         A(2,J1)=A(2,J1)+FY4(I)
         A(3,J1)=A(3,J1)+FZ4(I)
         STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H4(I))
      ENDDO
C
      NODFI = NLSKYFI(NIN)
      ISHIFT = NODFI*(JTASK-1)
      DO I=1,JLT
         IG=NSVG(I)
         ! --------------------
         ! local node
         IF (IG > 0) THEN
            IG = IG - NUMNOD
            NG = IGROUPS(IG)
            NFT=IPARG(3,NG)
            NEL=IPARG(2,NG)
            ILOC=IG-NFT     

            SHIFT_FORCE_INT = (JTASK-1)*NUMELS
            MULTI_FVM%FORCE_INT(1,IG+SHIFT_FORCE_INT) = MULTI_FVM%FORCE_INT(1,IG+SHIFT_FORCE_INT) - DT*FXI(I)
            MULTI_FVM%FORCE_INT(2,IG+SHIFT_FORCE_INT) = MULTI_FVM%FORCE_INT(2,IG+SHIFT_FORCE_INT) - DT*FYI(I)
            MULTI_FVM%FORCE_INT(3,IG+SHIFT_FORCE_INT) = MULTI_FVM%FORCE_INT(3,IG+SHIFT_FORCE_INT) - DT*FZI(I)
         ! --------------------
         ! remote node
         ELSE
            IG = - IG
            AFI(NIN)%P(1,IG+ISHIFT) = AFI(NIN)%P(1,IG+ISHIFT) - DT * FXI(I) 
            AFI(NIN)%P(2,IG+ISHIFT) = AFI(NIN)%P(2,IG+ISHIFT) - DT * FYI(I) 
            AFI(NIN)%P(3,IG+ISHIFT) = AFI(NIN)%P(3,IG+ISHIFT) - DT * FZI(I) 
         ENDIF
         ! --------------------
      ENDDO
      END SUBROUTINE MULTI_I18_FORCE_POFF

