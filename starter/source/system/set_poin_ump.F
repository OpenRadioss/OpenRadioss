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
Chd|  SET_POIN_UMP                  source/system/set_poin_ump.F  
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SET_POIN_UMP(IPART,IPM,TAB_UMP,POIN_UMP,TAILLE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "fluid_scr_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
      LOGICAL LOI_FLUID
      EXTERNAL LOI_FLUID
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(LIPART1,*),IPM(NPROPMI,*),
     .        POIN_UMP(NUMMAT),TAB_UMP(5,NPART),TAILLE
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER RES,MARQUEUR,MARQUEUR2,TAB_LOCAL(NPART),TAB_LOCAL2(NUMMAT),
     .   K1,K2,I,J,ILAW, IMID

C=======================================================================

!     ---------------------------------
!     Timer Mat/Prop
      IF(NPART>0) THEN
       DO I=1,NPART
         ILAW=0
         IMID=IPART(1,I)
         IF(IMID > 0) ILAW = IPM(2,IMID)
         TAB_UMP(1,I) = IPART(5,I)
         TAB_UMP(2,I) = IPART(6,I)
         TAB_UMP(3,I) = IPART(1,I)
         TAB_UMP(4,I) = IPART(2,I)
         TAB_UMP(5,I) = ILAW
         TAB_LOCAL(I) = 0
       ENDDO

       TAILLE = NPART
       IF(NPART>1) THEN
        DO I=2,NPART
          DO J=1,I-1
           IF( (TAB_LOCAL(J) == 0 )       .AND.
     .        (TAB_UMP(3,I)==TAB_UMP(3,J)).AND.
     .        (TAB_UMP(4,I)==TAB_UMP(4,J)))    THEN
            TAB_LOCAL(J) = -1
            TAILLE = TAILLE - 1
           ENDIF
         ENDDO
        ENDDO
       ENDIF
       IF(TAILLE<NPART) THEN
        MARQUEUR2 = 0
        DO I=1,NPART
          IF(TAB_LOCAL(I)==0) THEN
           MARQUEUR2 = MARQUEUR2 + 1
           TAB_UMP(1,MARQUEUR2) = TAB_UMP(1,I)
           TAB_UMP(2,MARQUEUR2) = TAB_UMP(2,I)
           TAB_UMP(3,MARQUEUR2) = TAB_UMP(3,I)
           TAB_UMP(4,MARQUEUR2) = TAB_UMP(4,I)
           TAB_UMP(5,MARQUEUR2) = TAB_UMP(5,I)
          ENDIF
        ENDDO
        DO I= TAILLE+1,NPART
           TAB_UMP(1,I) = 0
           TAB_UMP(2,I) = 0
           TAB_UMP(3,I) = 0
           TAB_UMP(4,I) = 0
           TAB_UMP(5,I) = 0
        ENDDO
       ENDIF


       I = TAILLE
       IF(TAILLE>1) THEN
        MARQUEUR = 0
        IF(NUMMAT>1) THEN
         DO WHILE ((MARQUEUR==0).AND.(I>0)) 
           MARQUEUR=1
           DO J=1,I-1
             IF(TAB_UMP(1,J) > TAB_UMP(1,J+1)) THEN
              MARQUEUR = TAB_UMP(1,J)
              TAB_UMP(1,J) = TAB_UMP(1,J+1)
              TAB_UMP(1,J+1) = MARQUEUR
              MARQUEUR = TAB_UMP(2,J)
              TAB_UMP(2,J) = TAB_UMP(2,J+1)
              TAB_UMP(2,J+1) = MARQUEUR
              MARQUEUR = TAB_UMP(3,J)
              TAB_UMP(3,J) = TAB_UMP(3,J+1)
              TAB_UMP(3,J+1) = MARQUEUR
              MARQUEUR = TAB_UMP(4,J)
              TAB_UMP(4,J) = TAB_UMP(4,J+1)
              TAB_UMP(4,J+1) = MARQUEUR
              MARQUEUR = TAB_UMP(5,J)
              TAB_UMP(5,J) = TAB_UMP(5,J+1)
              TAB_UMP(5,J+1) = MARQUEUR
              MARQUEUR=0
             ENDIF
           ENDDO
           I=I-1
         ENDDO    
         J = 1
         MARQUEUR = 1
         POIN_UMP = 0
         POIN_UMP(TAB_UMP(3,1)) = 1
         TAB_LOCAL2 = 0
         TAB_LOCAL2(1) = 1
         DO I=2,TAILLE
           IF(TAB_UMP(3,I-1)/=TAB_UMP(3,I)) THEN
            MARQUEUR = MARQUEUR + 1
            POIN_UMP(TAB_UMP(3,I)) = I
            TAB_LOCAL2(MARQUEUR) = I
           ENDIF
         ENDDO
        ELSE      
         POIN_UMP(1) = 1    
        ENDIF 

        IF(MARQUEUR>1) THEN
         K1=TAB_LOCAL2(1)
         DO I=2,MARQUEUR    
          MARQUEUR2 = 0
          K2 = TAB_LOCAL2(I)-1

          DO WHILE ((MARQUEUR2==0).AND.(K2>K1).AND.
     .              (K2*K1>0)) 
           MARQUEUR2=1
           DO J=K1,K2-1
             IF(TAB_UMP(2,J) > TAB_UMP(2,J+1)) THEN
              MARQUEUR2 = TAB_UMP(2,J)
              TAB_UMP(2,J) = TAB_UMP(2,J+1)
              TAB_UMP(2,J+1) = MARQUEUR2
              MARQUEUR2 = TAB_UMP(4,J)
              TAB_UMP(4,J) = TAB_UMP(4,J+1)
              TAB_UMP(4,J+1) = MARQUEUR2
              MARQUEUR2=0
             ENDIF
           ENDDO
           K2=K2-1
          ENDDO
          K1=TAB_LOCAL2(I)
         ENDDO
        ELSEIF(MARQUEUR==1) THEN
         MARQUEUR2 = 0
         I=TAILLE
         DO WHILE ((MARQUEUR2==0).AND.(I>0)) 
           MARQUEUR2=1
           DO J=1,I-1
             IF(TAB_UMP(2,J) > TAB_UMP(2,J+1)) THEN
              MARQUEUR2 = TAB_UMP(2,J)
              TAB_UMP(2,J) = TAB_UMP(2,J+1)
              TAB_UMP(2,J+1) = MARQUEUR2
              MARQUEUR2 = TAB_UMP(4,J)
              TAB_UMP(4,J) = TAB_UMP(4,J+1)
              TAB_UMP(4,J+1) = MARQUEUR2
              MARQUEUR2=0
             ENDIF
           ENDDO
           I=I-1
         ENDDO
        ENDIF
       ELSE
        POIN_UMP(1:NUMMAT) = 0
        IF(TAB_UMP(3,1) > 0) POIN_UMP(TAB_UMP(3,1)) = 1
       ENDIF    
      ENDIF
C
      END
