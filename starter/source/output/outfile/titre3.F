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
Chd|  TITRE3                        source/output/outfile/titre3.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TITRE3
C------------------------------------
C     NOEUDS => LECTURE DES ELEMENTS
C     CAS IFORM8=2
C------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "titr_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TITRE(12)=' .. NODES'
      TITRE(13)=' .. BOUNDARY CONDITIONS'
      TITRE(14)=' .. SKEW SYSTEMS '
      TITRE(15)=' .. 3D SOLID ELEMENTS'
      TITRE(16)=' .. 2D SOLID ELEMENTS'
      TITRE(17)=' .. 3D SHELL ELEMENTS'
      TITRE(18)=' .. 3D TRUSS ELEMENTS'
      TITRE(19)=' .. 3D BEAM ELEMENTS'
      TITRE(20)=' .. 3D SPRING ELEMENTS'
      TITRE(21)=' .. 3D TRIANGULAR SHELL ELEMENTS'
      TITRE(22)=' .. 3D ISOGEOMETRIC SOLID ELEMENTS'
      TITRE(23)=' .. 2D TRIANGULAR SOLID ELEMENTS'
C
      TITRE(29)=' .. ALE LINKS '
      TITRE(30)=' .. ALE/THE TABLES '
      TITRE(31)=' .. PROPERTIES '
      TITRE(32)=' .. TIME FUNCTIONS '
      TITRE(33)=' .. CONCENTRATED LOADS'
      TITRE(34)=' .. PRESSURE LOADS'
      TITRE(35)=' .. INITIAL VELOCITIES'
      TITRE(36)=' .. DETONATORS'
      TITRE(37)=' .. ELEMENT GROUPS'
      TITRE(38)=' .. INTERFACES '
      TITRE(39)=' .. RIGID WALLS'
      TITRE(40)=' .. RIGID WALLS ALE'
      TITRE(41)=' .. RIGID BODIES'
      TITRE(42)=' .. RIGID LINKS TO MADYMO'
      TITRE(43)=' .. ADDED MASSES'
      TITRE(44)=' .. IMPOSED VELOCITIES '
      TITRE(45)=' .. ELEMENT BUFFER INITIALIZATION'
      TITRE(46)=' .. GEOMETRY PLOT FILE '
      TITRE(47)=' .. RESTART FILE '
      TITRE(48)=' .. ERROR ==> NO RESTART FILE '
      TITRE(49)=' .. LASER IMPACT'
C
      TITRE(50)=' .. PARALLEL RESTART FILES GENERATION'
C
      TITRE(51)=' .. FLEXIBLE BODIES'
C
      TITRE(52)=' .. EIGENVECTORS CALCULATION'
      TITRE(53)='    ----> INITIAL MAPPING'
      TITRE(54)=' .. MAPPING DATA'
      TITRE(68)=' .. INTERFACE BUFFER INITIALIZATION'
      TITRE(69)=' .. ELEMENTARY BOUNDARY CONDITIONS'
      TITRE(70)='        NODAL POINT COORDINATES '
      TITRE(71)='        ----------------------- '
      TITRE(72)(1:37)='        NODE                 X-ORD   '
      TITRE(72)(38:77)='            Y-ORD               Z-ORD   '
      TITRE(72)(78:89)='            '
C
C
      TITRE(73)='         NODAL POINT SELECTION '
      TITRE(74)='         --------------------- '
      TITRE(75)(1:7) =' NODE  '
      TITRE(75)(8:50)=' IS GREATER THAN NUMNOD  '
C
      TITRE(80)='     BOUNDARY CONDITIONS'
      TITRE(81)='     -------------------'
      TITRE(82)=
     .'     NODE  TRANS. ROTAT. GRID   LAGRA.       SKEW '
      TITRE(83)='     -WARNING BOUNDARY CONDITION NODE NUMBER'
      TITRE(84)(1:7)=' NODE  '
      TITRE(84)(8:25)=' SKEW SYSTEM      '
      TITRE(85)='     SKEW SYSTEM SETS '
      TITRE(87)='         NODAL POINT SKEW FRAME '
      TITRE(206)(1:36)='  ELEMENTS WITH NEGATIVE MAL. NUMBER'
      TITRE(206)(37:66)=' ARE WALL NEIGHBOURS ELEMENTS '
C
      TITRE(206)(67:89)='                        '
      TITRE(90)='            EIGHT NODE BRICK ELEMENTS'
      TITRE(91)='            -------------------------'
C
      TITRE(92)(1:33)='   ELEMENT   INTERNAL      MATER      PRSET'
      TITRE(92)(34:66)='      NODE1      NODE2      NODE3      NODE4'
      TITRE(93)='     NODE5     NODE6     NODE7     NODE8'
C
      TITRE(94)='       8-NODE ELEMENT SELECTION'
      TITRE(95)='       ------------------------'
C
      TITRE(100)='            TWO DIMENSIONAL ELEMENTS '
      TITRE(101)='            ------------------------'
C     '----+----1----+----2----+----3----+----4----+----5----+----6----+-'
      TITRE(102)='   ELEMENT   INTERNAL      MATER      PRSET'//
     .           '      NODE1      NODE2      NODE3      NODE4 '
C
      TITRE(103)='       4-NODE ELEMENT SELECTION'
      TITRE(104)='       ------------------------'
      TITRE(105)='          ANGLE            THICKNESS'
C
      TITRE(110)='            SHELL ELEMENTS '
      TITRE(111)='            -------------- '
      TITRE(112)='       SHELL ELEMENT SELECTION'
      TITRE(113)='       -----------------------'
C
      TITRE(114)='               MERGE NODES               '
      TITRE(115)='   --------------------------------------'
      TITRE(116)='    CNODES              MERGED TO NODES  '
      TITRE(117)=' .. CNODES'

      TITRE(118)='     SUBMODEL SKEWS(INTERNAL)              '
      TITRE(119)='     --------------------------------------'
      TITRE(207)='               MERGE CNODES              '
      TITRE(208)='    CNODES              MERGED TO CNODES '
      RETURN
      END
