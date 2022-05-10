Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  C_FORTRAN_DGSSV               stub/superlu_stub.F           
Chd|-- called by -----------
Chd|        EIGSOL                        com/eig/eigsol.F              
Chd|        MSTATIC                       com/eig/mstatic.F             
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|====================================================================
        SUBROUTINE C_FORTRAN_DGSSV(IOPT,   N,      NNZ, NRHS, VALUE_OP,
     .                     ROWIND, COLPTR, B,   LDB,  FACTORS,
     .                     PRSP,   INFLU )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------

            INTEGER :: IOPT,   N,      NNZ, NRHS, VALUE_OP(*),
     .                     ROWIND(*), COLPTR(*) , B,   LDB,
     .                     PRSP,   INFLU 
            INTEGER(KIND=8) FACTORS(8)
C-----------------------------------------------
            WRITE(6,*) "ERROR: SuperLU is no longer supported"
            CALL FLUSH(6)
            CALL ARRET(5)
        END SUBROUTINE
Chd|====================================================================
Chd|  C_FREEMEM                     stub/superlu_stub.F           
Chd|-- called by -----------
Chd|        MSTATIC                       com/eig/mstatic.F             
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|====================================================================
        SUBROUTINE C_FREEMEM(FACTORS)
            INTEGER(KIND=8) FACTORS(8)
C-----------------------------------------------
            WRITE(6,*) "ERROR: SuperLU is no longer supported"
            CALL FLUSH(6)
            CALL ARRET(5)
        END SUBROUTINE
