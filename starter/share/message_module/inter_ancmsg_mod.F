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
Chd|  INTER_ANCMSG_MOD              share/message_module/inter_ancmsg_mod.F
Chd|-- called by -----------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INTER_ANCMSG_MOD
      INTERFACE
      SUBROUTINE ANCMSG(MSGID ,MSGTYPE,ANMODE,
     .                  I1    ,I2    ,I3    ,I4    ,I5    ,
     .                  I6    ,I7    ,I8    ,I9    ,I10   ,
     .                  I11   ,I12   ,I13   ,I14   ,I15   ,
     .                  I16   ,I17   ,I18   ,I19   ,I20   ,
     .                  R1    ,R2    ,R3    ,R4    ,
     .                  R5    ,R6    ,R7    ,R8    ,R9    ,
     .                  C1    ,C2    ,C3    ,C4    ,
     .                  C5    ,C6    ,C7    ,C8    ,C9    ,   
     .                  PRMOD )   
c     .                  PRMOD ,IOPTION,IOPTION_TYPE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MSGID,ANMODE,MSGTYPE,PRMOD
c      INTEGER MSGID,ANMODE,MSGTYPE,PRMOD,IOPTION,IOPTION_TYPE
      INTEGER
     .                 I1,I2,I3,I4,I5,
     .                 I6,I7,I8,I9,I10,
     .                 I11,I12,I13,I14,I15,
     .                 I16,I17,I18,I19,I20
      my_real
     .                 R1,R2,R3,R4,
     .                 R5,R6,R7,R8,R9
      CHARACTER(*)
     .                 C1,C2,C3,C4,
     .                 C5,C6,C7,C8,C9
      OPTIONAL ::
     .                 I1,I2,I3,I4,I5,
     .                 I6,I7,I8,I9,I10,
     .                 I11,I12,I13,I14,I15,
     .                 I16,I17,I18,I19,I20,
     .                 R1,R2,R3,R4,
     .                 R5,R6,R7,R8,R9,
     .                 C1,C2,C3,C4,
     .                 C5,C6,C7,C8,C9,
     .                 PRMOD
c     .                 PRMOD,IOPTION,IOPTION_TYPE
      END SUBROUTINE
      END INTERFACE
C=======================================================================
      END MODULE 
