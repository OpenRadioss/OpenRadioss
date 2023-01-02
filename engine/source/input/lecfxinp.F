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
Chd|  LECFXINP                      source/input/lecfxinp.F       
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        EIG_MOD                       share/modules/eig_mod.F       
Chd|====================================================================
      SUBROUTINE LECFXINP(NFXINP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE EIG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "eigcom.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFXINP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, ID, IDD, CIEIG(NEIG), ITYP
      my_real
     .        KSI, CFEIG(NEIG)
C
      DO I=1,NEIG
         CIEIG(I)=0
         CFEIG(I)=ZERO
      ENDDO
C
      KSI=ZERO
      DO I=1,NFXINP
         READ(IIN,'(I10,F20.0,I10)') ID, KSI, ITYP
         J=0
         II=0
         DO WHILE (J<=NEIG.AND.II==0) 
            J=J+1
            IDD=EIGIPM(1,J)
            IF (IDD==ID) THEN
               II=J
               EIGIPM(1,J)=-IDD
               EIGRPM(2,J)=KSI
               EIGIPM(16,J)=ITYP
            ENDIF
         ENDDO
         IF (II==0) THEN
            WRITE(ISTDO,*)' ** ERROR:  INPUT ERROR IN OPTION FXINPUT'
            WRITE(IOUT,* )' ** ERROR:  INPUT ERROR IN OPTION FXINPUT'
            WRITE(IOUT,*) '    UNKNOWN EIGENPROBLEM ID'
            CALL ARRET(2)
         ELSE
            CIEIG(II)=ID
            CFEIG(II)=KSI
         ENDIF
      ENDDO
C
      IF (IMACH/=3.OR.ISPMD==0) THEN
         WRITE(IOUT,'(A)') ' INPUT FILES FOR FLEXIBLE BODIES'
         DO I=1,NEIG
            IF (CIEIG(I)>0) 
     .         WRITE(IOUT,'(A14,I10,A17,1PG20.13)') 
     .   ' EIGENPROBLEM ',CIEIG(I),' CRITICAL DAMPING ',CFEIG(I)
         ENDDO
         WRITE(IOUT,*)
      ENDIF
C   
      RETURN
      END     
