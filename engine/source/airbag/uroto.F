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
Chd|  UROTO                         source/airbag/uroto.F         
Chd|-- called by -----------
Chd|        MULAWGLC                      source/materials/mat_share/mulawglc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UROTO(JFT,JLT,SIG,DIR,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NEL
      my_real
     .   SIG(5,*), DIR(NEL,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real S1, S2, S3, S4, S5
C-----------------------------------------------
      DO I=JFT,JLT
       S1 = DIR(I,1)*DIR(I,1)*SIG(1,I)
     .    + DIR(I,2)*DIR(I,2)*SIG(2,I)
     .-TWO*DIR(I,1)*DIR(I,2)*SIG(3,I)
       S2 = DIR(I,2)*DIR(I,2)*SIG(1,I)
     .    + DIR(I,1)*DIR(I,1)*SIG(2,I)
     .+TWO*DIR(I,2)*DIR(I,1)*SIG(3,I)
       S3 = DIR(I,1)*DIR(I,2)*SIG(1,I)
     .    - DIR(I,2)*DIR(I,1)*SIG(2,I)
     .    +(DIR(I,1)*DIR(I,1)-DIR(I,2)*DIR(I,2))*SIG(3,I)
       S4 = DIR(I,2)*SIG(5,I)+DIR(I,1)*SIG(4,I)
       S5 = DIR(I,1)*SIG(5,I)-DIR(I,2)*SIG(4,I)
       SIG(1,I)=S1
       SIG(2,I)=S2
       SIG(3,I)=S3
       SIG(4,I)=S4
       SIG(5,I)=S5
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  UROTOV                        source/airbag/uroto.F         
Chd|-- called by -----------
Chd|        H3D_SHELL_TENSOR              source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|        M25CPLRC                      source/materials/mat/mat025/m25cplrc.F
Chd|        M32PLAS                       source/materials/mat/mat032/m32plas.F
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        SIGROTA                       source/output/anim/generate/sigrota.F
Chd|        SIGROTA_XFE                   source/output/anim/generate/sigrota_xfe.F
Chd|        TENSORC                       source/output/anim/generate/tensorc.F
Chd|        TENSORC_CRK                   source/output/anim/generate/tensorc_crk.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UROTOV(JFT,JLT,SIG,DIR,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NEL
      my_real SIG(MVSIZ,5), DIR(NEL,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real S1, S2, S3, S4, S5
C-----------------------------------------------
      DO I=JFT,JLT
       S1 = DIR(I,1)*DIR(I,1)*SIG(I,1)
     .    + DIR(I,2)*DIR(I,2)*SIG(I,2)
     .-TWO*DIR(I,1)*DIR(I,2)*SIG(I,3)
       S2 = DIR(I,2)*DIR(I,2)*SIG(I,1)
     .    + DIR(I,1)*DIR(I,1)*SIG(I,2)
     .+TWO*DIR(I,2)*DIR(I,1)*SIG(I,3)
       S3 = DIR(I,1)*DIR(I,2)*SIG(I,1)
     .    - DIR(I,2)*DIR(I,1)*SIG(I,2)
     .    +(DIR(I,1)*DIR(I,1)-DIR(I,2)*DIR(I,2))*SIG(I,3)
       S4 = DIR(I,2)*SIG(I,5)+DIR(I,1)*SIG(I,4)
       S5 = DIR(I,1)*SIG(I,5)-DIR(I,2)*SIG(I,4)
       SIG(I,1)=S1
       SIG(I,2)=S2
       SIG(I,3)=S3
       SIG(I,4)=S4
       SIG(I,5)=S5
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  UROTO_SIG                     source/airbag/uroto.F         
Chd|-- called by -----------
Chd|        SIGEPS27C                     source/materials/mat/mat027/sigeps27c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UROTO_SIG(JFT,JLT,SIG,DIR,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NEL
      my_real SIG(NEL,5),DIR(NEL,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real S1, S2, S3, S4, S5
C-----------------------------------------------
      DO I=JFT,JLT
        S1 = DIR(I,1)*DIR(I,1)*SIG(I,1)
     .     + DIR(I,2)*DIR(I,2)*SIG(I,2)
     . -TWO*DIR(I,1)*DIR(I,2)*SIG(I,3)
        S2 = DIR(I,2)*DIR(I,2)*SIG(I,1)
     .     + DIR(I,1)*DIR(I,1)*SIG(I,2)
     . +TWO*DIR(I,2)*DIR(I,1)*SIG(I,3)
        S3 = DIR(I,1)*DIR(I,2)*SIG(I,1)
     .     - DIR(I,2)*DIR(I,1)*SIG(I,2)
     .     +(DIR(I,1)*DIR(I,1)-DIR(I,2)*DIR(I,2))*SIG(I,3)
        S4 = DIR(I,2)*SIG(I,5)+DIR(I,1)*SIG(I,4)
        S5 = DIR(I,1)*SIG(I,5)-DIR(I,2)*SIG(I,4)
        SIG(I,1)=S1
        SIG(I,2)=S2
        SIG(I,3)=S3
        SIG(I,4)=S4
        SIG(I,5)=S5
      ENDDO
C
      RETURN
      END
