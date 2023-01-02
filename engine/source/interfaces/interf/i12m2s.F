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
Chd|  I12M2S                        source/interfaces/interf/i12m2s.F
Chd|-- called by -----------
Chd|        INTTI12A                      source/interfaces/interf/intti12.F
Chd|-- calls ---------------
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE I12M2S(NSN,IRECTS,IRTL,NODVAR,NRTM,NRTS,
     .                  ISEGM,ISEGS,SEGVAR,JCODV)
      USE SEGVAR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,IRECTS(4,*),IRTL(*),NRTM,NRTS,ISEGM(*),ISEGS(*),
     .        JCODV(*)
      my_real
     .   NODVAR(*)
      TYPE(t_segvar),TARGET :: SEGVAR
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "conv_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR,KVAR, II,I, ADS,J,NN
      my_real, DIMENSION(:), POINTER :: ptr      
C-----------------------------------------------
      NIR=2
      IF(N2D==0)NIR=4
      !-------------------------------------!
      !   main SEGMENT -> SECND NODES     !
      !-------------------------------------!
      DO KVAR=1,NVCONV
      
       SELECT CASE(KVAR)
         CASE(1)
           ptr(1:) => SEGVAR%RHO(1:)
         CASE(2)
           ptr(1:) => SEGVAR%EINT(1:)
         CASE(3)
           ptr(1:) => SEGVAR%RK(1:)
         CASE(4)
           ptr(1:) => SEGVAR%RE(1:)
         CASE(5)
           ptr(1:) => SEGVAR%UVAR(1:)
       END SELECT
        
        DO II=1,NSN
          I=IRTL(II)
          ADS=NVCONV*(II-1)+KVAR
          NODVAR(ADS)=ptr(ISEGM(I))
        ENDDO
        
        DO I=1,NRTM
          ptr(ISEGM(I))=ZERO
        ENDDO        
        
      ENDDO
      
      !-------------------------------------!      
      !   SECND NODE -> SECND SEGMENT       !
      !-------------------------------------!
      DO KVAR=1,NVCONV

       SELECT CASE(KVAR)
         CASE(1)
           ptr(1:) => SEGVAR%RHO(1:)
         CASE(2)
           ptr(1:) => SEGVAR%EINT(1:)
         CASE(3)
           ptr(1:) => SEGVAR%RK(1:)
         CASE(4)
           ptr(1:) => SEGVAR%RE(1:)
         CASE(5)
           ptr(1:) => SEGVAR%UVAR(1:)
       END SELECT
             
        DO I=1,NRTS
          ptr(ISEGS(I))=ZERO
          DO J=1,NIR
             II=IRECTS(J,I)
             ADS=NVCONV*(II-1)+KVAR
             ptr(ISEGS(I))=ptr(ISEGS(I))+NODVAR(ADS)
          ENDDO
          ptr(ISEGS(I))=ptr(ISEGS(I))/FLOAT(NIR)
        ENDDO
      ENDDO

      RETURN
      END


