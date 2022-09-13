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
Chd|  SORTIE_ERROR                  source/output/sortie_error.F  
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_VGATH_ERR                source/mpi/anim/spmd_vgath_err.F
Chd|====================================================================
      SUBROUTINE SORTIE_ERROR(V,NODGLOB,WEIGHT,ITAB,MS,
     1                        MS0,PARAM,PARTSAV,IPART,PM,
     2                        IGEO)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "spmd_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real 
     .   V(3,*),MS(*),MS0(*),PM(NPROPM,*),PARTSAV(NPSAV,*)
      INTEGER 
     .   NODGLOB(*),WEIGHT(*),ITAB(*),PARAM,
     .   IPART(LIPART1,*),IGEO(NPROPGI,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   I,J,K,LENG,FLAG,ISMASSCHANGE,PROPID,MATID
      my_real 
     .   MAXVEL(PARAM),VEL,
     .   MAXENCIN(PARAM),ENCIN,VEL2,
     .   MAXMASS(PARAM),MASS,
     .   MAXDMASS(PARAM),DMASS,
     .   MAXENINT(PARAM),ENINT
      INTEGER 
     .   IDNOD(PARAM),IDPART(PARAM)
C=======================================================================
      IF(ISPMD == 0) WRITE(IOUT,3000)
      IF(ISPMD == 0) WRITE(IOUT,3100)
C
      IF(NSPMD>1) CALL SPMD_GLOB_DSUM9(PARTSAV,NPSAV*(NPART+NTHPART))
C
      IDPART = 0
      MAXENCIN = ZERO
      DO I=1,NPART
        FLAG = 0
        ENCIN= PARTSAV(2,I)
        DO J=1,PARAM
          IF(ENCIN > MAXENCIN(J) .AND. FLAG == 0) THEN
            DO K=1,PARAM-J
              IDPART(PARAM-K+1) = IDPART(PARAM-K)
              MAXENCIN(PARAM-K+1) = MAXENCIN(PARAM-K) 
            ENDDO
            IDPART(J) = I
            MAXENCIN(J) = ENCIN
            FLAG = 1
          ENDIF
        ENDDO
      ENDDO
      IF(ISPMD == 0) WRITE(IOUT,3000)
      IF(ISPMD == 0) WRITE(IOUT,1800)
      IF(ISPMD == 0) WRITE(IOUT,3000)
      IF(ISPMD == 0) WRITE(IOUT,1801)

      DO I=1,PARAM
        IF(MAXENCIN(I) /= ZERO) THEN
          MATID = NINT(PM(19,IPART(1,IDPART(I))))
          PROPID = IGEO(11,IPART(2,IDPART(I)))
          WRITE(IOUT,1900)IPART(4,IDPART(I)),MAXENCIN(I),
     .        IPART(6,IDPART(I)),PROPID,
     .        IPART(5,IDPART(I)),MATID
        ENDIF
      ENDDO
      
 1800 FORMAT('***  PARTS WITH HIGHEST KINETIC ENERGY')
 1801 FORMAT('   PART_ID   K-ENERGY   PROP_ID PROP_TYPE    MAT_ID
     .  MAT_TYPE')
 1900 FORMAT(I10,G11.4,4I10)
C
      IDPART = 0
      MAXENINT = ZERO
      DO I=1,NPART
        FLAG = 0
        ENINT= PARTSAV(1,I)+PARTSAV(24,I)+PARTSAV(26,I)
        DO J=1,PARAM
          IF(ENINT > MAXENINT(J) .AND. FLAG == 0) THEN
            DO K=1,PARAM-J
              IDPART(PARAM-K+1) = IDPART(PARAM-K)
              MAXENINT(PARAM-K+1) = MAXENINT(PARAM-K) 
            ENDDO
            IDPART(J) = I
            MAXENINT(J) = ENINT
            FLAG = 1
          ENDIF
        ENDDO
      ENDDO
      IF(ISPMD == 0) WRITE(IOUT,3000)
      IF(ISPMD == 0) WRITE(IOUT,2000)
      IF(ISPMD == 0) WRITE(IOUT,3000)
      IF(ISPMD == 0) WRITE(IOUT,2001)
      DO I=1,PARAM
        IF(MAXENINT(I) /= ZERO) THEN
          MATID = NINT(PM(19,IPART(1,IDPART(I))))
          PROPID = IGEO(11,IPART(2,IDPART(I)))
          WRITE(IOUT,2100)IPART(4,IDPART(I)),MAXENINT(I),
     .        IPART(6,IDPART(I)),PROPID,
     .        IPART(5,IDPART(I)),MATID
        ENDIF
      ENDDO
      
 2000 FORMAT('***  PARTS WITH HIGHEST INTERNAL ENERGY')
 2001 FORMAT('   PART_ID   I-ENERGY   PROP_ID PROP_TYPE    MAT_ID
     .  MAT_TYPE')
 2100 FORMAT(I10,G11.4,4I10)
C
      IDNOD = 0
      MAXVEL = ZERO
      IF (NSPMD == 1) THEN
       DO I=1,NUMNOD
         FLAG = 0
         VEL=SQRT(V(1,I)**2+V(2,I)**2+V(3,I)**2)
         DO J=1,PARAM
           IF(VEL > MAXVEL(J) .AND. FLAG == 0) THEN
             DO K=1,PARAM-J
               IDNOD(PARAM-K+1) = IDNOD(PARAM-K)
               MAXVEL(PARAM-K+1) = MAXVEL(PARAM-K) 
             ENDDO
             IDNOD(J) = I
             MAXVEL(J) = VEL
             FLAG = 1
           ENDIF
         ENDDO
       ENDDO
       WRITE(IOUT,3000)
       WRITE(IOUT,1000)
       WRITE(IOUT,3000)
       WRITE(IOUT,1001)
       DO I=1,PARAM
         IF(MAXVEL(I) /= ZERO)THEN
           ENCIN = HALF * MS(IDNOD(I))*MAXVEL(I)**2
           WRITE(IOUT,1100) ITAB(IDNOD(I)),MAXVEL(I),ENCIN,
     .                    MS(IDNOD(I)),MS0(IDNOD(I)),
     .             (MS(IDNOD(I))-MS0(IDNOD(I)))/MAX(EM20,MS0(IDNOD(I)))
         ENDIF
       ENDDO
      ELSE
        IF (ISPMD==0) THEN
           LENG = NUMNODG
        ELSE
           LENG = 0
        ENDIF
        CALL SPMD_VGATH_ERR(V,MS,MS0,NODGLOB,WEIGHT,PARAM,1,ITAB,LENG) 
      ENDIF
 1000 FORMAT('***  NODES WITH HIGHEST VELOCITY')
 1001 FORMAT('      NODE   VELOCITY     K-ENER       MASS      MASS0
     .   DM/MASS0')
 1100 FORMAT(I10,5G11.4)
C
c      IDNOD = 0
c      MAXENCIN = ZERO
c      IF (NSPMD == 1) THEN
c       DO I=1,NUMNOD
c         FLAG = 0
c         ENCIN=HALF*MS(I)*(V(1,I)**2+V(2,I)**2+V(3,I)**2)
c         DO J=1,PARAM
c           IF(ENCIN > MAXENCIN(J) .AND. FLAG == 0) THEN
c             DO K=1,PARAM-J
c               IDNOD(PARAM-K+1) = IDNOD(PARAM-K)
c               MAXENCIN(PARAM-K+1) = MAXENCIN(PARAM-K) 
c             ENDDO
c             IDNOD(J) = I
c             MAXENCIN(J) = ENCIN
c             FLAG = 1
c           ENDIF
c         ENDDO
c       ENDDO
c       WRITE(IOUT,3000)
c       WRITE(IOUT,1200)
c       WRITE(IOUT,3000)
c       WRITE(IOUT,1201)
c       DO I=1,PARAM
c         WRITE(IOUT,1300) ITAB(IDNOD(I)),MAXENCIN(I),MS(IDNOD(I)),
c     .                     MS0(IDNOD(I)),MS(IDNOD(I))/MS0(IDNOD(I))
c       ENDDO
c      ELSE
c        IF (ISPMD==0) THEN
c           LENG = NUMNODG
c        ELSE
c           LENG = 0
c        ENDIF
c        CALL SPMD_VGATH_ERR(V,MS,MS0,NODGLOB,WEIGHT,PARAM,2,ITAB,LENG) 
c      ENDIF
c      
c 1200 FORMAT('***  NODES WITH HIGHEST KINEMATIC ENERGY')
c 1201 FORMAT('  NODE   K-ENER    MASS  MASS0   MASS/MASS0')
c 1300 FORMAT(I10,4G11.4)
C
c      IDNOD = 0
c      MAXMASS = ZERO
c      IF (NSPMD == 1) THEN
c       DO I=1,NUMNOD
c         FLAG = 0
c         MASS=MS(I)
c         DO J=1,PARAM
c           IF(MASS > MAXMASS(J) .AND. FLAG == 0) THEN
c             DO K=1,PARAM-J
c               IDNOD(PARAM-K+1) = IDNOD(PARAM-K)
c               MAXMASS(PARAM-K+1) = MAXMASS(PARAM-K) 
c             ENDDO
c             IDNOD(J) = I
c             MAXMASS(J) = MASS
c             FLAG = 1
c           ENDIF
c         ENDDO
c       ENDDO
c       WRITE(IOUT,3000)
c       WRITE(IOUT,1400)
c       WRITE(IOUT,3000)
c       WRITE(IOUT,1401)
c       DO I=1,PARAM
c         WRITE(IOUT,1500) ITAB(IDNOD(I)),MAXMASS(I),
c     .                     MS0(IDNOD(I)),MS(IDNOD(I))/MS0(IDNOD(I))
c       ENDDO
c      ELSE
c        IF (ISPMD==0) THEN
c           LENG = NUMNODG
c        ELSE
c           LENG = 0
c        ENDIF
c        CALL SPMD_VGATH_ERR(V,MS,MS0,NODGLOB,WEIGHT,PARAM,3,ITAB,LENG) 
c      ENDIF
c      
c 1400 FORMAT('***  NODES WITH HIGHEST MASS')
c 1401 FORMAT('  NODE     MASS  MASS0   MASS/MASS0')
c 1500 FORMAT(I10,3G11.4)
C
      IDNOD = 0
      MAXDMASS = ZERO
      IF(IDTMIN(11) == 3 .OR. IDTMIN(11) == 8) THEN
        IF (NSPMD == 1) THEN
         DO I=1,NUMNOD
           FLAG = 0
           DMASS=(MS(I)-MS0(I))/MAX(MS0(I),EM20)
           DO J=1,PARAM
             IF(DMASS > MAXDMASS(J) .AND. FLAG == 0) THEN
               DO K=1,PARAM-J
                 IDNOD(PARAM-K+1) = IDNOD(PARAM-K)
                 MAXDMASS(PARAM-K+1) = MAXDMASS(PARAM-K) 
               ENDDO
               IDNOD(J) = I
               MAXDMASS(J) = DMASS
               FLAG = 1
             ENDIF
           ENDDO
         ENDDO
         ISMASSCHANGE = 0
         DO I=1,PARAM
           IF(MAXDMASS(I) /=ZERO) ISMASSCHANGE = 1
         ENDDO
         IF(ISMASSCHANGE /= 0)THEN
           WRITE(IOUT,3000)
           WRITE(IOUT,1600)
           WRITE(IOUT,3000)
           WRITE(IOUT,1601)
           DO I=1,PARAM
             IF(MAXDMASS(I) /= ZERO)
     .       WRITE(IOUT,1700) ITAB(IDNOD(I)),MS(IDNOD(I)),MS0(IDNOD(I)),
     .                        MAXDMASS(I)
           ENDDO
         ENDIF
        ELSE
          IF (ISPMD==0) THEN
             LENG = NUMNODG
          ELSE
             LENG = 0
          ENDIF
          CALL SPMD_VGATH_ERR(V,MS,MS0,NODGLOB,WEIGHT,PARAM,4,ITAB,LENG) 
        ENDIF
      ENDIF
      
 1600 FORMAT('***  NODES WITH HIGHEST MASS CHANGE')
 1601 FORMAT('      NODE       MASS      MASS0   DM/MASS0')
 1700 FORMAT(I10,3G11.4)
C
 3000 FORMAT('      ')
 3100 FORMAT('                          ** STATISTICS   **')
c-----------
      RETURN
      END
