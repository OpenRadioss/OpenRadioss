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
Chd|====================================================================
Chd|  ST_QAPRINT_ADMESH             source/output/qaprint/st_qaprint_admesh.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_ADMESH(IPART ,IPADMESH,PADMESH,SH4TREE,SH3TREE,
     2                                 IXC   ,IXTG  ,SH4TRIM,SH3TRIM    )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IPART(LIPART1,*), IPADMESH(KIPADMESH,*)
      INTEGER, INTENT(IN) :: SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*),
     .                       SH4TRIM(*),SH3TRIM(*) ,IXC(NIXC,*), IXTG(NIXTG,*)
      my_real, INTENT(IN) :: PADMESH(KPADMESH,*)

C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MY_ID, N , IP
      CHARACTER *nchartitle TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
C-----------------------------------------------
C     /ADMESH
C-----------------------------------------------
      IF (MYQAKEY('ADMESH')) THEN

        IF (NADMESH > 0) THEN    

          WRITE(VARNAME,'(A)') 'NADMESH'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NADMESH,0.0_8)
          
          IF(LEVELMAX /= 0) THEN
             WRITE(VARNAME,'(A)') 'LEVELMAX'
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),LEVELMAX,0.0_8)
          ENDIF

          IF(IADMRULE /= 0) THEN
             WRITE(VARNAME,'(A)') 'IADMRULE'
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IADMRULE,0.0_8)
          ENDIF

          IF(IADMSTAT /= 0) THEN
             WRITE(VARNAME,'(A)') 'IADMSTAT'
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IADMSTAT,0.0_8)
          ENDIF

          IF(ISTATCND /= 0) THEN
             WRITE(VARNAME,'(A)') 'ISTATCND'
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISTATCND,0.0_8)
          ENDIF

          IF(IADMERRT /= 0) THEN
             WRITE(VARNAME,'(A)') 'IADMERRT'
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IADMERRT,0.0_8)
          ENDIF

          IF(DTADMESH /= ZERO) THEN
            WRITE(VARNAME,'(A)') 'DTADMESH'
            TEMP_DOUBLE = DTADMESH
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDIF

          IF(TADMESH /= ZERO) THEN
            WRITE(VARNAME,'(A)') 'TADMESH'
            TEMP_DOUBLE = TADMESH
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDIF


          DO IP=1,NPART
C
             MY_ID = IPART(4,IP)
             IF(IPART(10,IP) /= 0) THEN
C
                CALL QAPRINT('PART_ID_ADMESH_', MY_ID,0.0_8)
C
                CALL QAPRINT('LEVELMAX',IPART(10,IP),0.0_8)
C
                DO I=1,KIPADMESH
                   WRITE(VARNAME,'(A,I0)') 'IPADMESH_',I
                   IF(IPADMESH(I,IP)/= 0) CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPADMESH(I,IP),0.0_8)
                ENDDO
C
                DO I=1,KPADMESH
                    WRITE(VARNAME,'(A,I0)') 'PADMESH_',I
                    TEMP_DOUBLE = PADMESH(I,IP)
                    IF(TEMP_DOUBLE/= ZERO) CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
                ENDDO
             ENDIF
C
          ENDDO
C
          DO N=1,NUMELC
             CALL QAPRINT('ELEM_ADMESH_4N_ID_', IXC(NIXC,N),0.0_8)
             DO I=1,KSH4TREE
                WRITE(VARNAME,'(A,I0)') 'SH4TREE_',I
                 IF(SH4TREE(I,N)/= 0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),SH4TREE(I,N),0.0_8)
             ENDDO
           ENDDO
C
          DO N=1,NUMELTG
             CALL QAPRINT('ELEM_ADMESH_3N_ID_', IXTG(NIXTG,N),0.0_8)
             DO I=1,KSH3TREE
                WRITE(VARNAME,'(A,I0)') 'SH3TREE_',I
                 IF(SH3TREE(I,N)/= 0)CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),SH3TREE(I,N),0.0_8)
             ENDDO
           ENDDO

          IF(IADMSTAT/=0)THEN

            DO N=1,LSH4TRIM 
               IF(SH4TRIM(N)/= 0) THEN
                 CALL QAPRINT('ELEM_ADMESH_STAT_4N_ID_', IXC(NIXC,N),0.0_8)
                 WRITE(VARNAME,'(A,I0)') 'SH4TRIM_',N
                 CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),SH4TRIM(N),0.0_8)
               ENDIF
            ENDDO

            DO N=1,LSH3TRIM
               IF(SH3TRIM(N)/= 0) THEN
                  CALL QAPRINT('ELEM_ADMESH_STAT_3N_ID_', IXTG(NIXTG,N),0.0_8)
                  WRITE(VARNAME,'(A,I0)') 'SH3TRIM_',N
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),SH3TRIM(N),0.0_8)
               ENDIF
            ENDDO

          ENDIF

        ENDIF

       ENDIF
C-----------------------------------------------
      RETURN
      END
