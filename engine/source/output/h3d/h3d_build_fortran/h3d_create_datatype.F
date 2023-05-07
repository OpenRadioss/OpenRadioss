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
Chd|  H3D_CREATE_DATATYPE           source/output/h3d/h3d_build_fortran/h3d_create_datatype.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
       SUBROUTINE H3D_CREATE_DATATYPE(H3D_DATA,IPARI)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (H3D_DATABASE) :: H3D_DATA
      INTEGER, INTENT(IN) ::  IPARI(NPARI,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
       INTEGER I,J,II,IUS,IPT,ILAY,CPT_CS_DATA,INTER_ID
       CHARACTER MES*80,CHINFO*10
C-----------------------------------------------
        DO I=1,H3D_DATA%N_OUTP_H3D
          IF(H3D_DATA%OUTPUT_LIST(I)%OK == 1 )  THEN
            IF(H3D_DATA%OUTPUT_LIST(I)%ETYPE ==  1) THEN
c-------noda
              IF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  1) THEN
c------------scalar
                  IF(H3D_DATA%OUTPUT_LIST(I)%INTER /= 0 )THEN
                    INTER_ID = IPARI(15,H3D_DATA%OUTPUT_LIST(I)%INTER)
                  ELSE
                    INTER_ID = 0
                  ENDIF
                  CALL C_H3D_CREATE_NODAL_SCALAR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      INTER_ID,H3D_DATA%OUTPUT_LIST(I)%COMMENT,
     .                                    H3D_DATA%OUTPUT_LIST(I)%S_COMMENT)
              ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  2) THEN
c------------vector
                  CALL C_H3D_CREATE_NODAL_VECTOR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%INFO2, H3D_DATA%OUTPUT_LIST(I)%STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%S_STRING2,H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT)
              ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  3) THEN
c------------tensor
                  CALL C_H3D_CREATE_NODAL_TENSOR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%INFO2, H3D_DATA%OUTPUT_LIST(I)%STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%S_STRING2,H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT)
              ENDIF
            ELSEIF(H3D_DATA%OUTPUT_LIST(I)%ETYPE ==  4) THEN
c-------1D
              IF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  1) THEN
c------------scalar
                  CALL C_H3D_CREATE_1D_SCALAR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT,H3D_DATA%OUTPUT_LIST(I)%IPT)
              ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  2) THEN
c------------vector
                  CALL C_H3D_CREATE_1D_VECTOR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT)
              ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  3) THEN
c------------tensor
                  CALL C_H3D_CREATE_1D_TENSOR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT,H3D_DATA%OUTPUT_LIST(I)%IPT)
              ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  4) THEN
c------------torsor
                  CALL C_H3D_CREATE_1D_TORSOR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT)
              ENDIF
            ELSEIF(H3D_DATA%OUTPUT_LIST(I)%ETYPE ==  2) THEN
c-------shells
              IF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  1) THEN
c------------scalar
                  CALL C_H3D_CREATE_SHELL_SCALAR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%INFO2, H3D_DATA%OUTPUT_LIST(I)%STRING2, 
     .                      H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%LAYER, H3D_DATA%OUTPUT_LIST(I)%IPT, H3D_DATA%OUTPUT_LIST(I)%PLY,
     .                      H3D_DATA%OUTPUT_LIST(I)%IUVAR,H3D_DATA%OUTPUT_LIST(I)%GAUSS,H3D_DATA%OUTPUT_LIST(I)%IDMDS,
     .                      H3D_DATA%OUTPUT_LIST(I)%IMDSVAR             , H3D_DATA%OUTPUT_LIST(I)%IDMATMDS ,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT             , H3D_DATA%OUTPUT_LIST(I)%S_COMMENT,
     .                      H3D_DATA%OUTPUT_LIST(I)%MDSVAR_NAME         , H3D_DATA%OUTPUT_LIST(I)%SMDSVAR_NAME,
     .                      H3D_DATA%OUTPUT_LIST(I)%OBJECT_ID)
              ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  2) THEN
c------------vector
                  CALL C_H3D_CREATE_SHELL_VECTOR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%INFO2, H3D_DATA%OUTPUT_LIST(I)%STRING2, 
     .                      H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%LAYER, H3D_DATA%OUTPUT_LIST(I)%IPT, H3D_DATA%OUTPUT_LIST(I)%PLY,
     .                      H3D_DATA%OUTPUT_LIST(I)%IUVAR,H3D_DATA%OUTPUT_LIST(I)%GAUSS,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT)
              ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  3) THEN
c------------tensor
                  CALL C_H3D_CREATE_SHELL_TENSOR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%INFO2, H3D_DATA%OUTPUT_LIST(I)%STRING2, 
     .                      H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%LAYER, H3D_DATA%OUTPUT_LIST(I)%IPT, H3D_DATA%OUTPUT_LIST(I)%PLY,
     .                      H3D_DATA%OUTPUT_LIST(I)%IUVAR,H3D_DATA%OUTPUT_LIST(I)%GAUSS,H3D_DATA%OUTPUT_LIST(I)%OBJECT_ID,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT)
              ENDIF
            ELSEIF(H3D_DATA%OUTPUT_LIST(I)%ETYPE ==  3) THEN
c-------solids
              IF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  1) THEN
c------------scalar
                  CALL C_H3D_CREATE_SOLID_SCALAR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%LAYER, H3D_DATA%OUTPUT_LIST(I)%IUVAR  , H3D_DATA%OUTPUT_LIST(I)%IR       ,
     .                      H3D_DATA%OUTPUT_LIST(I)%IS, H3D_DATA%OUTPUT_LIST(I)%IT        , H3D_DATA%OUTPUT_LIST(I)%IDMDS    ,
     .                      H3D_DATA%OUTPUT_LIST(I)%IMDSVAR                               , H3D_DATA%OUTPUT_LIST(I)%IDMATMDS ,  
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT                               , H3D_DATA%OUTPUT_LIST(I)%S_COMMENT ,
     .                      H3D_DATA%OUTPUT_LIST(I)%MDSVAR_NAME                           , H3D_DATA%OUTPUT_LIST(I)%SMDSVAR_NAME,
     .                      H3D_DATA%OUTPUT_LIST(I)%OBJECT_ID)
              ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  2) THEN
c------------vector
                  CALL C_H3D_CREATE_SOLID_VECTOR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%LAYER, H3D_DATA%OUTPUT_LIST(I)%IUVAR  , H3D_DATA%OUTPUT_LIST(I)%IR       ,
     .                      H3D_DATA%OUTPUT_LIST(I)%IS, H3D_DATA%OUTPUT_LIST(I)%IT        ,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT)
              ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  3) THEN
c------------tensor
                  CALL C_H3D_CREATE_SOLID_TENSOR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%LAYER, H3D_DATA%OUTPUT_LIST(I)%IUVAR  , H3D_DATA%OUTPUT_LIST(I)%IR       ,
     .                      H3D_DATA%OUTPUT_LIST(I)%IS   , H3D_DATA%OUTPUT_LIST(I)%IT     ,H3D_DATA%OUTPUT_LIST(I)%OBJECT_ID ,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT,
     .                      H3D_DATA%OUTPUT_LIST(I)%IS_CORNER_DATA)
              ENDIF
            ELSEIF(H3D_DATA%OUTPUT_LIST(I)%ETYPE ==  5) THEN
c-------sph
              IF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  1) THEN
c------------scalar
                  CALL C_H3D_CREATE_SPH_SCALAR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%IUVAR,
     .                     H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT)
              ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  3) THEN
c------------tensor
                  CALL C_H3D_CREATE_SPH_TENSOR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%IUVAR,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT)
              ENDIF
            ELSEIF(H3D_DATA%OUTPUT_LIST(I)%ETYPE ==  6) THEN
c-------quad
              IF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  1) THEN
c------------scalar
                  CALL C_H3D_CREATE_QUAD_SCALAR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%IUVAR,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT,H3D_DATA%OUTPUT_LIST(I)%OBJECT_ID)
              ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  2) THEN
c------------vector
                  CALL C_H3D_CREATE_QUAD_VECTOR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%IUVAR,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT)
              ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  3) THEN
c------------tensor
                  CALL C_H3D_CREATE_QUAD_TENSOR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%LAYER, H3D_DATA%OUTPUT_LIST(I)%IUVAR  , H3D_DATA%OUTPUT_LIST(I)%IR       ,
     .                      H3D_DATA%OUTPUT_LIST(I)%IS,    H3D_DATA%OUTPUT_LIST(I)%IT     ,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT)
              ENDIF
            ELSEIF(H3D_DATA%OUTPUT_LIST(I)%ETYPE ==  7) THEN
c-------skin
              IF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  1) THEN
c------------scalar
                  CALL C_H3D_CREATE_SKIN_SCALAR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%IUVAR,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT)
              ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  2) THEN
c------------vector
                  CALL C_H3D_CREATE_SKIN_VECTOR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%IUVAR,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT)
              ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE ==  3) THEN
c------------tensor
                  CALL C_H3D_CREATE_SKIN_TENSOR_DATATYPE(
     .                      H3D_DATA%OUTPUT_LIST(I)%N_OUTP, H3D_DATA%OUTPUT_LIST(I)%STRING1, H3D_DATA%OUTPUT_LIST(I)%S_STRING1,
     .                      H3D_DATA%OUTPUT_LIST(I)%INFO1, H3D_DATA%OUTPUT_LIST(I)%STRING2, H3D_DATA%OUTPUT_LIST(I)%S_STRING2,
     .                      H3D_DATA%OUTPUT_LIST(I)%LAYER, H3D_DATA%OUTPUT_LIST(I)%IUVAR  , H3D_DATA%OUTPUT_LIST(I)%IR       ,
     .                      H3D_DATA%OUTPUT_LIST(I)%IS,    H3D_DATA%OUTPUT_LIST(I)%IT     ,
     .                      H3D_DATA%OUTPUT_LIST(I)%COMMENT,H3D_DATA%OUTPUT_LIST(I)%S_COMMENT)
              ENDIF
            ENDIF
          ENDIF
        ENDDO


        CALL C_H3D_CREATE_SHELL_ERODED(H3D_DATA%N_OUTP_H3D+3, 'Erosion Status', 14,0,'',0)
        CALL C_H3D_CREATE_SOLID_ERODED(H3D_DATA%N_OUTP_H3D+4, 'Erosion Status', 14,0,'',0)
        CALL C_H3D_CREATE_ONED_ERODED(H3D_DATA%N_OUTP_H3D+5, 'Erosion Status', 14,0,'',0)
        CALL C_H3D_CREATE_SPH_ERODED(H3D_DATA%N_OUTP_H3D+6, 'Erosion Status', 14,0,'',0)
        CALL C_H3D_CREATE_QUAD_ERODED(H3D_DATA%N_OUTP_H3D+7, 'Erosion Status', 14,0,'',0)
        CALL C_H3D_CREATE_SKIN_ERODED(H3D_DATA%N_OUTP_H3D+8, 'Erosion Status', 14,0,'',0)


        RETURN

       END
