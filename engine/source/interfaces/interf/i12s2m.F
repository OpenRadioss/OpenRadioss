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
Chd|  I12S2M                        source/interfaces/interf/i12s2m.F
Chd|-- called by -----------
Chd|        INTTI12A                      source/interfaces/interf/intti12.F
Chd|-- calls ---------------
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE I12S2M(NSN,IRTL,NRTM,JCODV,NODVARS,MCOUNT,
     +                  NMN,NODVARM,IRECTM,NCOUNT,MSR,
     +                  SEGVAR,ISEGM,NOINT)
      USE SEGVAR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NRTM,IRTL(*),JCODV(*),ISEGM(*),IRECTM(4,*),NMN,MSR(*),NOINT
      my_real MCOUNT(*),NODVARS(*),NODVARM(*),NCOUNT(*)
      TYPE(t_segvar),TARGET :: SEGVAR
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "conv_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, II, L, JJ,KVAR,SEGAD,ADS,ADM,PB,TEST
      my_real,DIMENSION(:),POINTER :: ptr
C-----------------------------------------------
      TEST=0
      NIR=2
      IF(N2D==0)NIR=4


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

        DO I=1,NRTM
          ptr(ISEGM(I))=ZERO
        ENDDO

      ENDDO

      DO I=1,NRTM
        MCOUNT(I)=ZERO
      ENDDO

      DO II=1,NSN
        L=IRTL(II)
        MCOUNT(L)=MCOUNT(L)+ONE
      ENDDO

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
          L=IRTL(II)
          ADS=NVCONV*(II-1)+KVAR
          ptr(ISEGM(L))=ptr(ISEGM(L))+NODVARS(ADS)
         ENDDO

      ENDDO

      !CALCUL PARTICULIER SI IL EXISTE ONE SEGMENT SANS NOEUD SECOND. DETECTE
      PB=0
      DO I=1,NRTM
        IF(MCOUNT(I)==ZERO)PB=1
      ENDDO
      IF(PB==1)THEN
        DO I=1,NMN

          NCOUNT(I)=ZERO
        ENDDO
        DO II=1,NSN
            L=IRTL(II)
            DO JJ=1,NIR
              NCOUNT(IRECTM(JJ,L))= NCOUNT(IRECTM(JJ,L))+1
            ENDDO
        ENDDO
        DO KVAR=1,NVCONV
          DO I=1,NMN
            ADM=NVCONV*(I-1)+KVAR
            NODVARM(ADM)=ZERO
          ENDDO
          DO II=1,NSN
            L=IRTL(II)
            ADS=NVCONV*(II-1)+KVAR
            DO JJ=1,NIR
              ADM=NVCONV*(IRECTM(JJ,L)-1)+KVAR
              NODVARM(ADM)=NODVARM(ADM)+NODVARS(ADS)
            ENDDO
          ENDDO
        ENDDO
        DO KVAR=1,NVCONV
          DO I=1,NMN
            ADM=NVCONV*(I-1)+KVAR
            IF(NCOUNT(I)>ZERO)THEN
              NODVARM(ADM)=NODVARM(ADM)/NCOUNT(I)
            ENDIF
          ENDDO
        ENDDO
      ENDIF

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

        DO I=1,NRTM
          SEGAD=NVCONV*(ISEGM(I)-1)+KVAR
          IF(MCOUNT(I)>ZERO)THEN
            ptr(ISEGM(I))=ptr(ISEGM(I))/MCOUNT(I)
          ELSE
            ptr(ISEGM(I))=ZERO
            L=0
            DO JJ=1,NIR
              II=IRECTM(JJ,I)
              ADM=NVCONV*(II-1)+KVAR
              IF(NCOUNT(II)>ZERO)THEN
                ptr(ISEGM(I))=ptr(ISEGM(I))+NODVARM(ADM)
                L=L+1
              ENDIF
            ENDDO
            IF(L>0)THEN
              ptr(ISEGM(I))=ptr(ISEGM(I))/FLOAT(L)
            ELSE

              TEST=TEST+1
      !        WRITE(IOUT,'(A,I8,A,I8,A)')
      !+      '*** WARNING INTERF #',NOINT,'MAIN SEGMENT #',I,
      !+      ' WITHOUT SECONDARY NODE'
            ENDIF
           ENDIF

        ENDDO
      ENDDO
      ! IF(TEST >0)WRITE(ISTDO,'(A,I8,I8,A)')
      !+      '*** WARNING INTERF #',NOINT,
      !+      TEST,' MAIN SEGMENTS WITHOUT SECONDARY NODE'

      RETURN
      END

