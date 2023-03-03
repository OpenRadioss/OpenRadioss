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
Chd|  ST_QAPRINT_REFSTA             source/output/qaprint/st_qaprint_refsta.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        REFSTA_MOD                    share/modules1/refsta_mod.F   
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_REFSTA(XREFC     ,XREFTG    ,XREFS     ,TAGXREF,   IXS       ,IXC       ,IXTG  ,ITAB, XYZREF    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE QA_OUT_MOD
        !USE GROUPDEF_MOD
        USE REFSTA_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: TAGXREF(NUMNOD),IXC(NIXC,*),IXTG(NIXTG,*),IXS(NIXS,*)
      my_real, INTENT(IN) :: XREFC(4,3,NUMELC),XREFTG(3,3,NUMELTG),XREFS(8,3,NUMELS8),XYZREF(3,NUMNOD)
      INTEGER,INTENT(IN), DIMENSION(NUMNOD) :: ITAB
      
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: OK_QA
      CHARACTER (LEN=255) :: VARNAME
  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(.NOT. IS_REFSTA)RETURN

      OK_QA = MYQAKEY('/REFSTA')  
                                                         
      IF (OK_QA) THEN   
                                                                                  
            WRITE(VARNAME,'(A)') 'REFSTA%NITRS                  ='              
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NITRS,0.0_8)             

            WRITE(VARNAME,'(A)') 'REFSTA%RS0_FMT                ='              
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),RS0_FMT,0.0_8)

            WRITE(VARNAME,'(A,A)') 'REFSTA%XRFILE                 =',REFSTA_FILENAME(1:LEN_TRIM(REFSTA_FILENAME))            
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,0.0_8)
                                   
            !WRITE(VARNAME,'(A,I0,A,I0,A)') 'USER_INIVOL(',KK ,')%CONTY(',JJ,')%RATIO_FILL ='              
            !CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,VFRAC)                             
                
      ENDIF                                                                             
C-----------------------------------------------
      RETURN
      END

