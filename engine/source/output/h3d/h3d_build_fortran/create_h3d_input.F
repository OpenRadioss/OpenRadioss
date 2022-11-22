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
Chd|  CREATE_H3D_INPUT              source/output/h3d/h3d_build_fortran/create_h3d_input.F
Chd|-- called by -----------
Chd|        H3D_READ                      source/output/h3d/h3d_build_fortran/h3d_read.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        NVAR                          source/input/nvar.F           
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE CREATE_H3D_INPUT(H3D_DATA,IKAD,IKEY,IREC,NBC,KEY0,KEY2,KEY3,KEY4,KEY5,KEY6,KEY7,KEY8)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "nchara_c.inc"
#include      "units_c.inc"
#include      "scr14_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (H3D_DATABASE) :: H3D_DATA
      INTEGER IKAD(0:*),IKEY,IREC
      INTEGER NBC
      CHARACTER KEY0(*)*5 
      CHARACTER(LEN=ncharkey) :: KEY2, KEY3, KEY4, KEY5,KEY6,KEY7,KEY8
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER(LEN=ncharline):: CARTE,CARTE1,KEY3_GLOB
      INTEGER I,J,L,N_H3D_PART,CPT,
     .      IS_CHAR_KEY3,IS_CHAR_KEY4,IS_CHAR_KEY5,IS_CHAR_KEY6,IS_CHAR_KEY7,IS_CHAR_KEY8,
     .      IS_EMPTY_KEY3,IS_EMPTY_KEY4,IS_EMPTY_KEY5,IS_EMPTY_KEY6,IS_EMPTY_KEY7,IS_EMPTY_KEY8
C=========================================================================
      H3D_DATA%N_INPUT_H3D = H3D_DATA%N_INPUT_H3D + 1
      H3D_DATA%INPUT_LIST(H3D_DATA%N_INPUT_H3D)%KEY2 = KEY2
      H3D_DATA%INPUT_LIST(H3D_DATA%N_INPUT_H3D)%KEY3 = KEY3
      H3D_DATA%INPUT_LIST(H3D_DATA%N_INPUT_H3D)%KEY4 = KEY4
      H3D_DATA%INPUT_LIST(H3D_DATA%N_INPUT_H3D)%KEY5 = KEY5
      H3D_DATA%INPUT_LIST(H3D_DATA%N_INPUT_H3D)%KEY6 = KEY6
      H3D_DATA%INPUT_LIST(H3D_DATA%N_INPUT_H3D)%KEY7 = KEY7
      H3D_DATA%INPUT_LIST(H3D_DATA%N_INPUT_H3D)%KEY8 = KEY8


      N_H3D_PART = 0
      DO J=1,NBC
         READ(IUSC1,REC=IREC+J-1,FMT='(A)',ERR=999)CARTE
         N_H3D_PART=N_H3D_PART + NVAR(CARTE)
      ENDDO
c
      H3D_DATA%INPUT_LIST(H3D_DATA%N_INPUT_H3D)%NB_PART = N_H3D_PART
      IF (N_H3D_PART /= 0 )
     .       ALLOCATE(H3D_DATA%INPUT_LIST(H3D_DATA%N_INPUT_H3D)%PART_LIST(N_H3D_PART))
c
      N_H3D_PART = 0
      DO J=1,NBC
         READ(IUSC1,REC=IREC+J-1,FMT='(A)',ERR=999)CARTE
         CALL WRIUSC2(IREC+J-1,1,KEY0(IKEY))
         READ(IUSC2,*,ERR=999,END=999)
     .        (H3D_DATA%INPUT_LIST(H3D_DATA%N_INPUT_H3D)%PART_LIST(N_H3D_PART + L),L=1,NVAR(CARTE))
           N_H3D_PART=N_H3D_PART + NVAR(CARTE)
      ENDDO
c      DO J=1,N_H3D_PART
c        print *,'ds la construct du read_input',H3D_DATA%INPUT_LIST(H3D_DATA%N_INPUT_H3D)%PART_LIST(J)
c      ENDDO
c
C-------------------------------------------------- 
C SEARCH Keywords for Actication of some output computation
C-------------------------------------------------- 
      IS_CHAR_KEY3 = 1
      IS_EMPTY_KEY3 = 1
      IS_CHAR_KEY4 = 1
      IS_EMPTY_KEY4 = 1
      IS_CHAR_KEY5 = 1
      IS_EMPTY_KEY5 = 1
      IS_CHAR_KEY6 = 1
      IS_EMPTY_KEY6 = 1
      IS_CHAR_KEY7 = 1
      IS_EMPTY_KEY7 = 1
      IS_CHAR_KEY8 = 1
      IS_EMPTY_KEY8 = 1
      KEY3_GLOB = ''
      DO I=1,ncharkey
        IF ( KEY3(I:I) == '=' ) IS_CHAR_KEY3 = 0
        IF ( KEY3(I:I) /= ' ' ) IS_EMPTY_KEY3 = 0
        IF ( KEY4(I:I) == '=' ) IS_CHAR_KEY4 = 0
        IF ( KEY4(I:I) /= ' ' ) IS_EMPTY_KEY4 = 0
        IF ( KEY5(I:I) == '=' ) IS_CHAR_KEY5 = 0
        IF ( KEY5(I:I) /= ' ' ) IS_EMPTY_KEY5 = 0
        IF ( KEY6(I:I) == '=' ) IS_CHAR_KEY6 = 0
        IF ( KEY6(I:I) /= ' ' ) IS_EMPTY_KEY6 = 0
        IF ( KEY7(I:I) == '=' ) IS_CHAR_KEY7 = 0
        IF ( KEY7(I:I) /= ' ' ) IS_EMPTY_KEY7 = 0
        IF ( KEY8(I:I) == '=' ) IS_CHAR_KEY8 = 0
        IF ( KEY8(I:I) /= ' ' ) IS_EMPTY_KEY8 = 0
      ENDDO
C-------------------------------------------------- 
      CPT = 0
      IF ( IS_CHAR_KEY3 == 1 .AND. IS_EMPTY_KEY3 == 0) THEN
        DO I=1,ncharkey
          IF ( KEY3(I:I) /= ' ' ) THEN 
            CPT = CPT + 1
            KEY3_GLOB(CPT:CPT) = KEY3(I:I)
          ENDIF
        ENDDO
      ENDIF
      IF ( IS_CHAR_KEY4 == 1 .AND. IS_EMPTY_KEY4 == 0 ) THEN
        CPT = CPT + 1
        KEY3_GLOB(CPT:CPT) = '/'
        DO I=1,ncharkey
          IF ( KEY4(I:I) /= ' ' ) THEN 
            CPT = CPT + 1
            KEY3_GLOB(CPT:CPT) = KEY4(I:I)
          ENDIF
        ENDDO
      ENDIF
      IF ( IS_CHAR_KEY5 == 1 .AND. IS_EMPTY_KEY5 == 0 ) THEN
        CPT = CPT + 1
        KEY3_GLOB(CPT:CPT) = '/'
        DO I=1,ncharkey
          IF ( KEY5(I:I) /= ' ' ) THEN 
            CPT = CPT + 1
            KEY3_GLOB(CPT:CPT) = KEY5(I:I)
          ENDIF
        ENDDO
      ENDIF
      IF ( IS_CHAR_KEY6 == 1 .AND. IS_EMPTY_KEY6 == 0 ) THEN
        CPT = CPT + 1
        KEY3_GLOB(CPT:CPT) = '/'
        DO I=1,ncharkey
          IF ( KEY6(I:I) /= ' ' ) THEN 
            CPT = CPT + 1
            KEY3_GLOB(CPT:CPT) = KEY6(I:I)
          ENDIF
        ENDDO
      ENDIF
      IF ( IS_CHAR_KEY7 == 1 .AND. IS_EMPTY_KEY7 == 0 ) THEN
        CPT = CPT + 1
        KEY3_GLOB(CPT:CPT) = '/'
        DO I=1,ncharkey
          IF ( KEY7(I:I) /= ' ' ) THEN 
            CPT = CPT + 1
            KEY3_GLOB(CPT:CPT) = KEY7(I:I)
          ENDIF
        ENDDO
      ENDIF
      IF ( IS_CHAR_KEY8 == 1 .AND. IS_EMPTY_KEY8 == 0 ) THEN
        CPT = CPT + 1
        KEY3_GLOB(CPT:CPT) = '/'
        DO I=1,ncharkey
          IF ( KEY8(I:I) /= ' ' ) THEN 
            CPT = CPT + 1
            KEY3_GLOB(CPT:CPT) = KEY8(I:I)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------------------- 
      IF(KEY2 == 'NODA') THEN   
c scalar
        IF (KEY3_GLOB == 'DT') H3D_DATA%N_SCAL_DT  = 1
        IF (KEY3_GLOB == 'DMASS') H3D_DATA%N_SCAL_DMAS  = 1
        IF (KEY3_GLOB == 'DINER') H3D_DATA%N_SCAL_DINER  = 1
        IF (KEY3_GLOB == 'DAMA2') H3D_DATA%N_SCAL_DAMA2   = 1
        IF (KEY3_GLOB == 'SKID_LINE') H3D_DATA%N_SCAL_SKID  = 1
        IF (KEY3_GLOB == 'STIFR') H3D_DATA%N_SCAL_STIFR  = 1
        IF (KEY3_GLOB == 'STIF') H3D_DATA%N_SCAL_STIFN  = 1
c vector
        IF (KEY3_GLOB == 'CONT'.OR.KEY3_GLOB == 'CONT/MAX') H3D_DATA%N_VECT_CONT  = 1
        IF (KEY3_GLOB == 'CONT/MAX') H3D_DATA%N_VECT_CONT_MAX  = 1
        IF (KEY3_GLOB == 'FINT') H3D_DATA%N_VECT_FINT  = 1
        IF (KEY3_GLOB == 'FEXT') H3D_DATA%N_VECT_FEXT   = 1
        IF (KEY3_GLOB == 'PCONT'.OR.KEY3_GLOB == 'PCONT/TMAX') H3D_DATA%N_VECT_PCONT   = 1
        IF (KEY3_GLOB == 'PCONT/TMAX') H3D_DATA%N_VECT_PCONT_MAX  = 1
        IF (KEY3_GLOB == 'CONT2') H3D_DATA%N_VECT_CONT2   = 1
        IF (KEY3_GLOB == 'PCONT2') H3D_DATA%N_VECT_PCONT2   = 1
        IF (KEY3_GLOB == 'DROT') H3D_DATA%N_VECT_DROT   = 1 
        IF (KEY3_GLOB == 'DXANC') H3D_DATA%N_VECT_DXANC  = 1
        IF (KEY3_GLOB == 'FREAC') H3D_DATA%N_VECT_FREAC  = 1
        IF (KEY3_GLOB == 'MREAC') H3D_DATA%N_VECT_MREAC  = 1
        IF (KEY3_GLOB == 'CLUST/FORCE') H3D_DATA%N_VECT_CLUST_FORCE  = 1 
        IF (KEY3_GLOB == 'CLUST/MOM') H3D_DATA%N_VECT_CLUST_MOM   = 1    
      ENDIF
      
      IF(KEY2 == "ELEM" .OR. KEY2 == 'SOLID' .OR. KEY2 == 'QUAD')THEN
        IF (KEY3_GLOB == 'VECT/CONT') H3D_DATA%N_VECT_CONT  = 1      
      ENDIF
c
      ! Just for EPSDOT
      IF(KEY2 == 'SHELL') THEN
c tensor
        IF (KEY3_GLOB == 'TENS/EPSDOT') IEPSDOT = 1
      ENDIF
c
      IF(KEY2 == 'SPRING' .OR. KEY2 == 'BEAM' .OR. KEY2 == 'TRUSS' ) THEN
        IF (KEY3_GLOB == 'FORC') H3D_DATA%UND_FORC = 1
      ENDIF
c
      ! Just for STRAIN
      IF (KEY2 == 'QUAD') THEN
c tensor
        IF (KEY3_GLOB == 'TENS/STRAIN') H3D_DATA%STRAIN = 1
      ENDIF
C-------------------------------------------------- 

c
      RETURN
 999  print *,'error lecture'
      CALL ARRET(0)
      END
