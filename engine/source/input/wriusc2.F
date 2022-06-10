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
Chd|  WRIUSC2                       source/input/wriusc2.F        
Chd|-- called by -----------
Chd|        CREATE_H3D_INPUT              source/output/h3d/h3d_build_fortran/create_h3d_input.F
Chd|        FRALEONOFF                    source/input/fraleonoff.F     
Chd|        FRALNK                        source/input/fralnk.F         
Chd|        FRALNK2                       source/input/fralnk.F         
Chd|        FREABF                        source/input/freabf.F         
Chd|        FREANIM                       source/output/anim/reader/freanim.F
Chd|        FRECPL                        source/input/frecpl.F         
Chd|        FREDAMP                       source/input/fredamp.F        
Chd|        FREDEBUG                      source/input/fredebug.F       
Chd|        FREDLI                        source/input/fredli.F         
Chd|        FREDYNAIN                     source/input/fredynain.F      
Chd|        FREFLW                        source/input/freflw.F         
Chd|        FREFORM                       source/input/freform.F        
Chd|        FREFUNC                       source/input/frefunc.F        
Chd|        FREFVBAG                      source/input/frefvbag.F       
Chd|        FREFXINP                      source/input/frefxinp.F       
Chd|        FREIMPL                       source/input/freimpl.F        
Chd|        FREINIV                       source/input/freiniv.F        
Chd|        FREINT                        source/input/freint.F         
Chd|        FRENOIS                       source/input/frenois.F        
Chd|        FREOUTP                       source/input/freoutp.F        
Chd|        FRESTAT                       source/input/frestat.F        
Chd|        FRETHERMAL                    source/constraints/thermic/frethermal.F
Chd|        FREUPWIND                     source/input/freupwind.F      
Chd|        FREUPWM                       source/input/freupwm.F        
Chd|        H3D_READ                      source/output/h3d/h3d_build_fortran/h3d_read.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE WRIUSC2(IREC,NBC,KEY0)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IREC, NBC
      CHARACTER KEY0*(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "chara_c.inc"
#include      "nchara_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   REC, ERR
      CHARACTER CARTE*ncharline
C-----------------------------------------------
      REWIND(IUSC2)
      DO 100 I=1,NBC
       READ(IUSC1,'(A)',REC=IREC,ERR=999)CARTE
       WRITE(IUSC2,'(A)')CARTE
 100  CONTINUE
      REWIND(IUSC2)
      RETURN
 999  CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0)
      CALL ARRET(0)
      END
