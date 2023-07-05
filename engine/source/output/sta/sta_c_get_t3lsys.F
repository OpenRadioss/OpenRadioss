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
Chd|  GET_T3LSYS                    source/output/sta/sta_c_get_t3lsys.F
Chd|-- called by -----------
Chd|        STAT_C_STRSFG                 source/output/sta/stat_c_strsfg.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        CORTDIR3                      source/elements/shell/coque/cortdir3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
        SUBROUTINE GET_T3LSYS(JFT    ,JLT    ,IXTG    ,X    ,OFFG   ,
     .                        IREL   ,VQ     ,NLAY   ,IREP  ,NEL    ,
     .                        DIR_A  ,DIR_B,ELBUF_STR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
c-----------------------------------------------
c   g l o b a l   p a r a m e t e r s
c-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER IXTG(NIXTG,*),JFT,JLT,IREL,NLAY,IREP,NEL
      my_real 
     .   X(3,*), OFFG(*),VQ(3,3,MVSIZ),DIR_A(*),DIR_B(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,K,L
      INTEGER I2,I3,I1
      my_real 
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),R21(MVSIZ),R22(MVSIZ),
     .   R23(MVSIZ),R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   SZ(MVSIZ),DETA1(MVSIZ)
C----------------------------------------------
        DO I=JFT,JLT
          I1=IXTG(2,I)
          I2=IXTG(3,I)
          I3=IXTG(4,I)

          RX(I)=X(1,I2)-X(1,I1)
          RY(I)=X(2,I2)-X(2,I1)
          RZ(I)=X(3,I2)-X(3,I1)
          SX(I)=X(1,I3)-X(1,I1)
          SY(I)=X(2,I3)-X(2,I1)
          SZ(I)=X(3,I3)-X(3,I1)
        ENDDO 
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      CALL CLSKEW3(JFT,JLT,IREL,
     .   RX, RY, RZ, 
     .   SX, SY, SZ, 
     .   R11,R12,R13,R21,R22,R23,R31,R32,R33,DETA1,OFFG )
      DO I=JFT,JLT
        VQ(1,1,I)=R11(I)
        VQ(2,1,I)=R21(I)
        VQ(3,1,I)=R31(I)
        VQ(1,2,I)=R12(I)
        VQ(2,2,I)=R22(I)
        VQ(3,2,I)=R32(I)
        VQ(1,3,I)=R13(I)
        VQ(2,3,I)=R23(I)
        VQ(3,3,I)=R33(I)
      ENDDO 
         CALL CORTDIR3(ELBUF_STR,DIR_A  ,DIR_B ,JFT    ,JLT   ,
     .                NLAY     ,IREP   ,RX    ,RY     ,RZ     , 
     .                SX       ,SY     ,SZ    ,R11    ,R21    ,
     .                R31      ,R12    ,R22   ,R32    ,NEL    )
C
      RETURN
      END
