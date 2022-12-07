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
Chd|  H3D_GENE_KEYWORD              source/output/h3d/input_list/h3d_gene_keyword.F
Chd|-- called by -----------
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE H3D_GENE_KEYWORD(KEY2_READ,KEY2,KEY3_READ,KEY3_GLOB,NB_KEY,CPT_KEY,MULTI_FVM,
     .                            IS_MODEL_NPT,IS_MODEL_LAYER,IS_MODEL_PLY,IS_MDSVAR,IS_MDSVAR_DEF,
     .                            IS_PLY_ALL,IS_LAYER_ALL,IS_IPT,IS_LAYER,IS_PLY,IS_ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE H3D_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "chara_c.inc"
#include      "nchara_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER NB_KEY,CPT_KEY
       CHARACTER(LEN=ncharkey) ::  KEY2,KEY2_READ
       CHARACTER(LEN=ncharline) ::  KEY3_READ,KEY3_GLOB
       TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NIPMAX,I1,I2,II,IS_MODEL_NPT,IS_MODEL_LAYER,IS_MODEL_PLY,
     .        IS_MDSVAR,IS_MDSVAR_DEF,IS_PLY_ALL,IS_LAYER_ALL,IS_IPT,
     .        IS_LAYER,IS_PLY,IS_ID
      CHARACTER(LEN=2) :: CH_OPTION
C=========================================================================
c-----------------------------------------------
      IF (KEY2_READ == 'MASS') THEN
c-----------------------------------------------
        NB_KEY = 8
        IF (CPT_KEY == 1) KEY2 = 'NODA'
        IF (CPT_KEY == 1) KEY3_GLOB = 'MASS'
        IF (CPT_KEY == 2) KEY2 = 'SHELL'
        IF (CPT_KEY == 2) KEY3_GLOB = 'MASS'
        IF (CPT_KEY == 3) KEY2 = 'SOLID'
        IF (CPT_KEY == 3) KEY3_GLOB = 'MASS'
        IF (CPT_KEY == 4) KEY2 = 'SPH'
        IF (CPT_KEY == 4) KEY3_GLOB = 'MASS'
        IF (CPT_KEY == 5) KEY2 = 'BEAM'
        IF (CPT_KEY == 5) KEY3_GLOB = 'MASS'
        IF (CPT_KEY == 6) KEY2 = 'SPRING'
        IF (CPT_KEY == 6) KEY3_GLOB = 'MASS'
        IF (CPT_KEY == 7) KEY2 = 'TRUSS'
        IF (CPT_KEY == 7) KEY3_GLOB = 'MASS'
        IF (CPT_KEY == 8) KEY2 = 'QUAD'
        IF (CPT_KEY == 8) KEY3_GLOB = 'MASS'
c-----------------------------------------------
      ELSEIF (KEY2_READ == 'SOLID') THEN
c-----------------------------------------------
        IF (KEY3_READ == 'ORTHD') THEN
          NB_KEY = 3
          IF (CPT_KEY == 1) KEY3_GLOB = 'ORTHD/PSI'
          IF (CPT_KEY == 2) KEY3_GLOB = 'ORTHD/THETA'
          IF (CPT_KEY == 3) KEY3_GLOB = 'ORTHD/PHI'
        ELSEIF (KEY3_READ == 'MOM') THEN
          NB_KEY = 7
          IF (CPT_KEY == 1) KEY3_GLOB = 'MOMX'
          IF (CPT_KEY == 2) KEY3_GLOB = 'MOMY'
          IF (CPT_KEY == 3) KEY3_GLOB = 'MOMZ'
          IF (CPT_KEY == 4) KEY3_GLOB = 'MOMXY'
          IF (CPT_KEY == 5) KEY3_GLOB = 'MOMYZ'
          IF (CPT_KEY == 6) KEY3_GLOB = 'MOMXZ'
          IF (CPT_KEY == 7) KEY3_GLOB = '|MOM|'
        ELSEIF (KEY3_READ == 'VEL') THEN
          NB_KEY = 7
          IF (CPT_KEY == 1) KEY3_GLOB = 'VELX'
          IF (CPT_KEY == 2) KEY3_GLOB = 'VELY'
          IF (CPT_KEY == 3) KEY3_GLOB = 'VELZ'
          IF (CPT_KEY == 4) KEY3_GLOB = 'VELXY'
          IF (CPT_KEY == 5) KEY3_GLOB = 'VELYZ'
          IF (CPT_KEY == 6) KEY3_GLOB = 'VELXZ'
          IF (CPT_KEY == 7) KEY3_GLOB = '|VEL|'
        ELSEIF (KEY3_READ == 'MDS' .AND. IS_MDSVAR == 0 .AND.
     .          IS_IPT ==0 .AND. IS_LAYER == 0 .AND. IS_PLY == 0) THEN
          NB_KEY = IS_PLY_ALL+IS_LAYER_ALL+1
          IF (CPT_KEY == 1) THEN
            KEY3_GLOB = 'MDS'
            IS_MDSVAR = 1
            IS_MDSVAR_DEF = 1
            IF(IS_MODEL_PLY == 1 .OR. IS_MODEL_LAYER == 1 )IS_LAYER_ALL = 1
          ENDIF
        ELSE
          NB_KEY = 1
          KEY3_GLOB = KEY3_READ
        ENDIF
c-----------------------------------------------
      ELSEIF (KEY2_READ == 'NODA') THEN
c-----------------------------------------------
        IF (KEY3_READ == 'FOPT') THEN
          NB_KEY = 2
          IF (CPT_KEY == 1) KEY3_GLOB = 'FOPT/FORCE'
          IF (CPT_KEY == 2) KEY3_GLOB = 'FOPT/MOMENT'
        ELSEIF (KEY3_READ == 'PCONT') THEN
          NB_KEY = 2
          IF (CPT_KEY == 1) KEY3_GLOB = 'PCONT/NORMAL'
          IF (CPT_KEY == 2) KEY3_GLOB = 'PCONT/TANGENT'
        ELSEIF (KEY3_READ == 'PCONT/TMAX') THEN
          NB_KEY = 2
          IF (CPT_KEY == 1) KEY3_GLOB = 'MAXPCONT/NORMAL'
          IF (CPT_KEY == 2) KEY3_GLOB = 'MAXPCONT/TANGENT'
        ELSEIF (KEY3_READ == 'PCONT2') THEN
          NB_KEY = 2
          IF (CPT_KEY == 1) KEY3_GLOB = 'PCONT2/NORMAL'
          IF (CPT_KEY == 2) KEY3_GLOB = 'PCONT2/TANGENT'
        ELSEIF (KEY3_READ == 'PCONT2/TMAX') THEN
          NB_KEY = 2
          IF (CPT_KEY == 1) KEY3_GLOB = 'MAXPCONT2/NORMAL'
          IF (CPT_KEY == 2) KEY3_GLOB = 'MAXPCONT2/TANGENT'
        ELSEIF (KEY3_READ == 'PCONT2/TMIN') THEN
          NB_KEY = 2
          IF (CPT_KEY == 1) KEY3_GLOB = 'MINPCONT2/NORMAL'
          IF (CPT_KEY == 2) KEY3_GLOB = 'MINPCONT2/TANGENT'
        ELSEIF (KEY3_READ == 'DAMA2') THEN
          NB_KEY = 2
          IF (CPT_KEY == 1) KEY3_GLOB = 'DAMA2/NORMAL'
          IF (CPT_KEY == 2) KEY3_GLOB = 'DAMA2/TANGENT'
        ELSE
          NB_KEY = 1
          KEY3_GLOB = KEY3_READ
        ENDIF
c-----------------------------------------------
      ELSEIF (KEY2_READ == 'QUAD') THEN
         IF (KEY3_READ == 'VFRAC') THEN
c     
c     create specific keywords for LAW151, number of present materials is not
c     known a priori
c     
           IF (MULTI_FVM%IS_USED) THEN
              NB_KEY = MULTI_FVM%NBMAT
              DO II = 1, NB_KEY
                 IF (CPT_KEY == II) THEN
                    WRITE(KEY3_GLOB, '(A,I0)') 'M151VFRAC', II 
                 ENDIF
              ENDDO
           ELSE
              NB_KEY = 4
              IF (CPT_KEY == 1) KEY3_GLOB = 'VFRAC1'
              IF (CPT_KEY == 2) KEY3_GLOB = 'VFRAC2'
              IF (CPT_KEY == 3) KEY3_GLOB = 'VFRAC3'
              IF (CPT_KEY == 4) KEY3_GLOB = 'VFRAC4'
           ENDIF
        ENDIF
c-----------------------------------------------
      ELSEIF (KEY2_READ == 'ELEM') THEN
        IF (KEY3_READ == 'VFRAC') THEN
c
c     create specific keywords for LAW151, number of present materials is not
c     known a priori
c           
           IF (MULTI_FVM%IS_USED) THEN
              NB_KEY = MULTI_FVM%NBMAT
              DO II = 1, NB_KEY
                 IF (CPT_KEY == II) THEN
                    WRITE(KEY3_GLOB, '(A,I0)') 'M151VFRAC', II 
                 ENDIF
              ENDDO
           ELSE
              NB_KEY = 4
              IF (CPT_KEY == 1) KEY3_GLOB = 'VFRAC1'
              IF (CPT_KEY == 2) KEY3_GLOB = 'VFRAC2'
              IF (CPT_KEY == 3) KEY3_GLOB = 'VFRAC3'
              IF (CPT_KEY == 4) KEY3_GLOB = 'VFRAC4'
           ENDIF
        ELSEIF (KEY3_READ == 'PHASE_DENS') THEN
           IF (MULTI_FVM%IS_USED) THEN
              NB_KEY = MULTI_FVM%NBMAT
              DO II = 1, NB_KEY
                 IF (CPT_KEY == II) THEN
                    WRITE(KEY3_GLOB, '(A,I0)') 'M151DENS', II 
                 ENDIF
              ENDDO
           ENDIF
        ELSEIF (KEY3_READ == 'PHASE_ENER') THEN
           IF (MULTI_FVM%IS_USED) THEN
              NB_KEY = MULTI_FVM%NBMAT
              DO II = 1, NB_KEY
                 IF (CPT_KEY == II) THEN
                    WRITE(KEY3_GLOB, '(A,I0)') 'M151ENER', II 
                 ENDIF
              ENDDO
           ENDIF 
        ELSEIF (KEY3_READ == 'PHASE_PRES') THEN
           IF (MULTI_FVM%IS_USED) THEN
              NB_KEY = MULTI_FVM%NBMAT
              DO II = 1, NB_KEY
                 IF (CPT_KEY == II) THEN
                    WRITE(KEY3_GLOB, '(A,I0)') 'M151PRES', II 
                 ENDIF
              ENDDO
           ENDIF 
        ELSEIF (KEY3_READ == 'ORTHD') THEN
          NB_KEY = 3
          IF (CPT_KEY == 1) KEY3_GLOB = 'ORTHD/PSI'
          IF (CPT_KEY == 2) KEY3_GLOB = 'ORTHD/THETA'
          IF (CPT_KEY == 3) KEY3_GLOB = 'ORTHD/PHI'
        ELSEIF (KEY3_READ == 'MOM') THEN
          NB_KEY = 7
          IF (CPT_KEY == 1) KEY3_GLOB = 'MOMX'
          IF (CPT_KEY == 2) KEY3_GLOB = 'MOMY'
          IF (CPT_KEY == 3) KEY3_GLOB = 'MOMZ'
          IF (CPT_KEY == 4) KEY3_GLOB = 'MOMXY'
          IF (CPT_KEY == 5) KEY3_GLOB = 'MOMYZ'
          IF (CPT_KEY == 6) KEY3_GLOB = 'MOMXZ'
          IF (CPT_KEY == 7) KEY3_GLOB = '|MOM|'
        ELSEIF (KEY3_READ == 'VEL') THEN
          NB_KEY = 7
          IF (CPT_KEY == 1) KEY3_GLOB = 'VELX'
          IF (CPT_KEY == 2) KEY3_GLOB = 'VELY'
          IF (CPT_KEY == 3) KEY3_GLOB = 'VELZ'
          IF (CPT_KEY == 4) KEY3_GLOB = 'VELXY'
          IF (CPT_KEY == 5) KEY3_GLOB = 'VELYZ'
          IF (CPT_KEY == 6) KEY3_GLOB = 'VELXZ'
          IF (CPT_KEY == 7) KEY3_GLOB = '|VEL|'
        ELSEIF (KEY3_READ == 'FORC') THEN
          NB_KEY = 9
          IF (CPT_KEY == 1) KEY3_GLOB = 'F1'
          IF (CPT_KEY == 2) KEY3_GLOB = 'F2'
          IF (CPT_KEY == 3) KEY3_GLOB = 'F3'
          IF (CPT_KEY == 4) KEY3_GLOB = 'M11'
          IF (CPT_KEY == 5) KEY3_GLOB = 'M21'
          IF (CPT_KEY == 6) KEY3_GLOB = 'M31'
          IF (CPT_KEY == 7) KEY3_GLOB = 'M12'
          IF (CPT_KEY == 8) KEY3_GLOB = 'M22'
          IF (CPT_KEY == 9) KEY3_GLOB = 'M32'
        ELSEIF (KEY3_READ == 'MDS' .AND. IS_MDSVAR == 0 .AND.
     .          IS_IPT ==0 .AND. IS_LAYER == 0 .AND. IS_PLY == 0) THEN
          NB_KEY = IS_PLY_ALL+IS_LAYER_ALL+1
          IF (CPT_KEY == 1) THEN
            KEY3_GLOB = 'MDS'
            IS_MDSVAR = 1
            IS_MDSVAR_DEF = 1
            IF(IS_MODEL_PLY == 1 .OR. IS_MODEL_LAYER == 1 )IS_LAYER_ALL = 1
          ENDIF
        ENDIF
c-----------------------------------------------

c-----------------------------------------------
c        IF (KEY3_READ == 'KEYWORD') THEN
c          NB_KEY = 3
c          IF (CPT_KEY == 1) KEY3_GLOB = 'KEYWORD1'
c          IF (CPT_KEY == 2) KEY3_GLOB = 'KEYWORD2'
c          IF (CPT_KEY == 3) KEY3_GLOB = 'KEYWORD3'
c-----------------------------------------------
c        ELSEIF (KEY3_READ(1:6) == 'VFRAC_') THEN
c          I1 = 0
c          I2 = 0
c          IF (KEY3_READ(8:9) == '->') THEN
c            READ (KEY3_READ(7:7),FMT='(I)'),I1
c            READ (KEY3_READ(10:11),FMT='(I2)'),I2
c          ELSEIF (KEY3_READ(9:10) == '->') THEN
c            READ (KEY3_READ(7:8),FMT='(I2)'),I1
c            READ (KEY3_READ(11:12),FMT='(I2)'),I2
c          ENDIF
c          NB_KEY = I2 - I1 +1
c          DO J=1,NB_KEY
c            IF (J+I1-1 <= 9) WRITE(CH_OPTION,'(I1)')J+I1-1
c            IF (J+I1-1 > 9) WRITE(CH_OPTION,'(I2)')J+I1-1
c            IF (CPT_KEY == J) KEY3_GLOB = 'VFRAC'//CH_OPTION
c          ENDDO
c-----------------------------------------------
      ELSEIF (KEY2_READ == 'SHELL') THEN
c-----------------------------------------------
         IF (KEY3_READ == 'VFRAC') THEN
c     
c     create specific keywords for LAW151, number of present materials is not
c     known a priori
c           
            IF (MULTI_FVM%IS_USED) THEN
               NB_KEY = MULTI_FVM%NBMAT
               DO II = 1, NB_KEY
                  IF (CPT_KEY == II) THEN
                     WRITE(KEY3_GLOB, '(A,I0)') 'M151VFRAC', II 
                  ENDIF
               ENDDO
            ELSE
               NB_KEY = 4
               IF (CPT_KEY == 1) KEY3_GLOB = 'VFRAC1'
               IF (CPT_KEY == 2) KEY3_GLOB = 'VFRAC2'
               IF (CPT_KEY == 3) KEY3_GLOB = 'VFRAC3'
               IF (CPT_KEY == 4) KEY3_GLOB = 'VFRAC4'
            ENDIF
        ELSEIF (KEY3_READ == 'MDS' .AND. IS_MDSVAR == 0 .AND.
     .          IS_IPT ==0 .AND. IS_LAYER == 0 .AND. IS_PLY == 0) THEN
          NB_KEY = IS_PLY_ALL+IS_LAYER_ALL+1
          IF (CPT_KEY == 1) THEN
            KEY3_GLOB = 'MDS'
            IS_MDSVAR = 1
            IS_MDSVAR_DEF = 1
            IF(IS_MODEL_PLY == 1 .OR. IS_MODEL_LAYER == 1 )IS_LAYER_ALL = 1
          ENDIF
        ENDIF
c-----------------------------------------------
      ELSEIF (KEY3_READ == 'PHASE_DENS') THEN
c-----------------------------------------------
         IF (MULTI_FVM%IS_USED) THEN
            NB_KEY = MULTI_FVM%NBMAT
            DO II = 1, NB_KEY
               IF (CPT_KEY == II) THEN
                  WRITE(KEY3_GLOB, '(A,I0)') 'M151DENS', II 
               ENDIF
            ENDDO
         ENDIF
c-----------------------------------------------
      ELSEIF (KEY3_READ == 'PHASE_ENER') THEN
c-----------------------------------------------
         IF (MULTI_FVM%IS_USED) THEN
            NB_KEY = MULTI_FVM%NBMAT
            DO II = 1, NB_KEY
               IF (CPT_KEY == II) THEN
                  WRITE(KEY3_GLOB, '(A,I0)') 'M151ENER', II 
               ENDIF
            ENDDO
         ENDIF 
c-----------------------------------------------
      ELSEIF (KEY3_READ == 'PHASE_PRES') THEN
c-----------------------------------------------
         IF (MULTI_FVM%IS_USED) THEN
            NB_KEY = MULTI_FVM%NBMAT
            DO II = 1, NB_KEY
               IF (CPT_KEY == II) THEN
                  WRITE(KEY3_GLOB, '(A,I0)') 'M151PRES', II 
               ENDIF
            ENDDO
         ENDIF 
c-----------------------------------------------
      ELSEIF (KEY2_READ == 'BEAM' .OR. KEY2_READ == 'SPRING' .OR. KEY2_READ == 'TRUSS') THEN
c-----------------------------------------------
        IF (KEY3_READ == 'FORC') THEN
          NB_KEY = 9
          IF (CPT_KEY == 1) KEY3_GLOB = 'F1'
          IF (CPT_KEY == 2) KEY3_GLOB = 'F2'
          IF (CPT_KEY == 3) KEY3_GLOB = 'F3'
          IF (CPT_KEY == 4) KEY3_GLOB = 'M11'
          IF (CPT_KEY == 5) KEY3_GLOB = 'M21'
          IF (CPT_KEY == 6) KEY3_GLOB = 'M31'
          IF (CPT_KEY == 7) KEY3_GLOB = 'M12'
          IF (CPT_KEY == 8) KEY3_GLOB = 'M22'
          IF (CPT_KEY == 9) KEY3_GLOB = 'M32'
        ELSE
          NB_KEY = 1
          KEY3_GLOB = KEY3_READ
        ENDIF
      ELSE
        NB_KEY = 1
        KEY3_GLOB = KEY3_READ
      ENDIF
c-----------------------------------------------
      CPT_KEY = CPT_KEY + 1
c-----------------------------------------------
      END
