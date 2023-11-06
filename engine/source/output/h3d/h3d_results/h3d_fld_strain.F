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
Chd|  H3D_FLD_STRAIN                source/output/h3d/h3d_results/h3d_fld_strain.F
Chd|-- called by -----------
Chd|        H3D_SKIN_SCALAR               source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|-- calls ---------------
Chd|        ROTO_SIG2D                    source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|        TSH_DIR2                      source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE H3D_FLD_STRAIN(ELBUF_TAB,X  ,IXS   ,
     .                          JHBE,MLWI,ILAY,KCVT,IOR_TSH,
     .                          ICSTR,NPTR,NPTS,NEL,F_EXP,EVAR )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER , INTENT(IN) :: JHBE,ILAY,MLWI,KCVT,IOR_TSH,
     .                          NPTR,NPTS,ICSTR,NEL
        INTEGER ,DIMENSION(NIXS,NUMELS), INTENT(IN) ::  IXS
        my_real , INTENT(IN) ::  F_EXP
        my_real ,DIMENSION(3,MVSIZ), INTENT(OUT) ::  EVAR
        my_real ,DIMENSION(3,NUMNOD), INTENT(IN) ::  X
        TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        my_real
     .     DIR(MVSIZ,2),DIRB(MVSIZ,2)
        INTEGER I,I1,II,J,IR,IS,IT,IL,JJ(4)

        TYPE(G_BUFEL_)  ,POINTER :: GBUF
        TYPE(L_BUFEL_)  ,POINTER :: LBUF
C-----------------------------------------------
        EVAR(1:3,1:NEL) = ZERO
        IT = 1
        DO I=1,4
          JJ(I) = NEL*(I-1)
        ENDDO
        GBUF => ELBUF_TAB%GBUF
        IF (JHBE==15) THEN
          IR = 1
          IS = 1
          LBUF => ELBUF_TAB%BUFLY(ILAY)%LBUF(IR,IS,IT)
          IF (MLWI == 12 .OR. MLWI == 14) THEN
            DO I=1,NEL
              EVAR(1:2,I) = LBUF%EPE(JJ(1:2) + I)
              EVAR(3,I)   = HALF*LBUF%EPE(JJ(4) + I)
            ENDDO
          ELSEIF (MLWI /= 49 ) THEN
            DO I=1,NEL
              EVAR(1:2,I) = LBUF%STRA(JJ(1:2) + I)
              EVAR(3,I)   = HALF*LBUF%STRA(JJ(4) + I)
            ENDDO
          END IF
C------
        ELSE ! 14,16
          IF (MLWI == 12 .OR. MLWI == 14) THEN
            DO IR=1,NPTR
              DO IS=1,NPTS
                LBUF => ELBUF_TAB%BUFLY(ILAY)%LBUF(IR,IS,IT)
                DO I=1,NEL
                  EVAR(1:2,I) = EVAR(1:2,I)+LBUF%EPE(JJ(1:2) + I)
                  EVAR(3,I)   = EVAR(3,I)+HALF*LBUF%EPE(JJ(4) + I)
                ENDDO
              END DO
            END DO
          ELSEIF (MLWI /= 49 ) THEN
            DO IR=1,NPTR
              DO IS=1,NPTS
                LBUF => ELBUF_TAB%BUFLY(ILAY)%LBUF(IR,IS,IT)
                DO I=1,NEL
                  EVAR(1:2,I) = EVAR(1:2,I)+LBUF%STRA(JJ(1:2) + I)
                  EVAR(3,I)   = EVAR(3,I)+ HALF*LBUF%STRA(JJ(4) + I)
                ENDDO
              ENDDO
            END DO
          END IF
        END IF
        EVAR(1:3,1:NEL) = F_EXP*EVAR(1:3,1:NEL)
C------
        IF (KCVT==2) THEN
          IF(IOR_TSH==1)THEN
            DO I=1,NEL
              DIR(I,1:2)= GBUF%GAMA(JJ(1:2) + I)
            ENDDO
          ELSEIF(IOR_TSH==2)THEN
            IF(JHBE==14)THEN
              IR = 1
              IS = 1
            END IF
            LBUF => ELBUF_TAB%BUFLY(ILAY)%LBUF(IR,IS,IT)
            DO I=1,NEL
              DIR(I,1:2)= LBUF%GAMA(JJ(1:2) + I)
            ENDDO
          END IF
          CALL TSH_DIR2(X,IXS,DIR,DIRB,ICSTR,NEL)
          CALL ROTO_SIG2D(1,NEL,EVAR,DIRB)
        END IF !(KCVT==2) THEN
C
        RETURN
      END SUBROUTINE H3D_FLD_STRAIN
