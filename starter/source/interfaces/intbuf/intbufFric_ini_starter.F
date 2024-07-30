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
      !||====================================================================
      !||    intbuf_fric_ini_starter   ../starter/source/interfaces/intbuf/intbufFric_ini_starter.F
      !||--- called by ------------------------------------------------------
      !||    lectur                    ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    message_mod               ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE INTBUF_FRIC_INI_STARTER(INTBUF_FRIC_TAB  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE INTBUF_FRIC_MOD                     
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   M y  A l l o c a t e                         
C----------------------------------------------- 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
c      my_real
c     .   
      TYPE(INTBUF_FRIC_STRUCT_) :: INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NINF , NSET ,S_TABPARTS_FRIC ,IORTH ,MFROT ,LENC
C-----------------------------------------------

      DO NINF = 1,NINTERFRIC 

        NSET = INTBUF_FRIC_TAB(NINF)%NSETPRTS       
        S_TABPARTS_FRIC = INTBUF_FRIC_TAB(NINF)%S_TABPARTS_FRIC 
        IORTH = INTBUF_FRIC_TAB(NINF)%IORTHFRIC
        MFROT = INTBUF_FRIC_TAB(NINF)%FRICMOD
        IF(MFROT ==0 ) THEN   
           LENC =2  
        ELSE
           LENC = 8
        ENDIF 
C=======================================================================
c       ALLOCATIONS
C=======================================================================

          CALL MY_ALLOC(INTBUF_FRIC_TAB(NINF)%TABCOUPLEPARTS_FRIC,NSET) 
          INTBUF_FRIC_TAB(NINF)%TABCOUPLEPARTS_FRIC(1:NSET) = 0 

          CALL MY_ALLOC(INTBUF_FRIC_TAB(NINF)%TABPARTS_FRIC,S_TABPARTS_FRIC) 
          INTBUF_FRIC_TAB(NINF)%TABPARTS_FRIC(1:S_TABPARTS_FRIC) = 0 

          CALL MY_ALLOC(INTBUF_FRIC_TAB(NINF)%ADPARTS_FRIC,S_TABPARTS_FRIC+1) 
          INTBUF_FRIC_TAB(NINF)%ADPARTS_FRIC(1:S_TABPARTS_FRIC+1) = 0 

          CALL MY_ALLOC(INTBUF_FRIC_TAB(NINF)%IFRICORTH,NSET) 
          INTBUF_FRIC_TAB(NINF)%IFRICORTH(1:NSET) = 0 

          IF (IORTH == 0 ) THEN
             CALL MY_ALLOC(INTBUF_FRIC_TAB(NINF)%TABCOEF_FRIC,LENC*(NSET+1)) 
              INTBUF_FRIC_TAB(NINF)%TABCOEF_FRIC(1:LENC*(NSET+1)) = ZERO 
          ELSE
             CALL MY_ALLOC(INTBUF_FRIC_TAB(NINF)%TABCOEF_FRIC,LENC+2*LENC*NSET) 
              INTBUF_FRIC_TAB(NINF)%TABCOEF_FRIC(1:2*LENC*NSET+LENC) = ZERO 
          ENDIF

C=======================================================================
c       END ALLOCATIONS
C=======================================================================
      ENDDO     ! NINF  = 1,NINTERFRIC
C-----
      RETURN

      END SUBROUTINE INTBUF_FRIC_INI_STARTER
