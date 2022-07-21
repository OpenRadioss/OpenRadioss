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
Chd|  LEC_INISTATE_D00_BRICK_CHECK  source/elements/initia/lec_inistate_d00_brick-check.F
Chd|-- called by -----------
Chd|        HM_READ_INISTATE_D00          source/elements/initia/hm_read_inistate_d00.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LEC_INISTATE_D00_BRICK_CHECK (
     1                  IXS            ,IGEO          ,ITRIS         ,ISOLNODD00    ,IE            ,
     2                  NPT            ,NLAY          ,ISOLNOD       ,JJHBE         ,IGTYP         ,
     3                  ISROT          ,KEYWORD )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----A----+----B----+----C----+----D--
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------                     
      USE MESSAGE_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IGEO(NPROPGI,*) ,ITRIS(*), ISOLNODD00(*)
      INTEGER IE, NPT, ISOLNOD, JJHBE, IGTYP, ISROT, NLAY
      CHARACTER*15,KEYWORD 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IPIDD00, JJHBED00, NPTD00, NPTF, NPTR, NPTS, NPTT, ICSTR, IINT, NLY,NNOD
C-----------------------------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----A----+----B----+----C----+----D--
C-----------------------------------------
C     INITIALE STRESS FILE D00
C     CHECK INPUT CONSISTENCY BETWEEN /INIBRI AND D00
C     Erreur if difference in data ( D00 & INIBRI)
C-----------------------------------------		   
      IPIDD00  = IXS(10,IE)
      JJHBED00 = IGEO(10,IPIDD00)
      IGTYP    = IGEO(11,IPIDD00)
      ISROT    = IGEO(20,IPIDD00)
      ICSTR    = IGEO(14,IPIDD00)
      IINT     = IGEO(15,IPIDD00)
C
C  Solid property ISOLID
C
      IF (JJHBED00 == 0) JJHBED00 = 1
      IF (ISOLNOD==4.OR.ISOLNOD==10) JJHBED00=1
      IF (ISOLNOD==16.OR.ISOLNOD==20) JJHBED00=16
      IF ((ISOLNOD==4.OR.ISOLNOD==10).AND.JJHBE /= 1) JJHBE = 1
      IF (JJHBE == 0) JJHBE = 1

      IF ((JJHBED00 .NE.JJHBE) .AND.((JJHBED00 .NE. 2).AND.
     .         (JJHBE .NE. 0)) .AND. ISOLNOD .NE. 4)  THEN
          CALL ANCMSG(MSGID=695,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ITRIS(IE),
     .                C1='FORMULATION',
     .                C2='SOLID PROPERTY',
     .                I2=IGEO(1,IPIDD00),
     .                C3=KEYWORD)
      ENDIF
C
C  Number of nodes
C
      NNOD = ISOLNOD
      IF (ISOLNOD==10 .AND. ISROT==1) NNOD=4
      IF (ISOLNODD00(IE) .NE.NNOD) THEN
          CALL ANCMSG(MSGID=695,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ITRIS(IE),
     .                C1='NUMBER OF NODES',
     .                C2='BRICK',
     .                I2=ITRIS(IE),
     .                C3=KEYWORD)
      ENDIF
C
      NPTD00 = 0
      IF (JJHBED00 == 14 .OR.ISOLNOD==16 .OR.ISOLNOD==20 ) THEN
       NPTF = IGEO(4,IPIDD00)
       NPTR=NPTF/100
       NPTS=MOD(NPTF/10,10)
       NPTT=MOD(NPTF,10)
      END IF
C      
      IF (JJHBED00 == 14 .AND.IGTYP==22 ) THEN
       SELECT CASE (ICSTR)
        CASE(100)                                             
         NLY = NPTR
         IF (NLY ==0) NLY =IINT 
         NPTD00 = NLY*NPTS*NPTT
        CASE(10)                                          
         NLY = NPTS
         IF (NLY ==0) NLY =IINT 
         NPTD00 = NLY*NPTR*NPTT
        CASE(1)                                          
         NLY = NPTT
         IF (NLY ==0) NLY =IINT 
         NPTD00 = NLY*NPTR*NPTS
       END SELECT
C
C  Number of layers
C
       IF (KEYWORD == '/INIBRI/ORTHO  ' .AND. NLY /= NLAY) THEN
          CALL ANCMSG(MSGID=695,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ITRIS(IE),
     .                C1='NUMBER OF LAYERS',
     .                C2='SOLID PROPERTY',
     .                I2=IGEO(1,IPIDD00),
     .                C3=KEYWORD)
       ENDIF
C
C	   
      ELSEIF (JJHBED00 == 14 .OR.ISOLNOD==16 .OR.ISOLNOD==20 ) THEN
       NPTD00 = NPTR*NPTS*NPTT
      ELSEIF (JJHBED00 == 12 .OR.JJHBED00==17.OR.JJHBED00==18) THEN
       NPTD00 = 8
      ENDIF

      IF (NPTD00 == 0 .and. IGTYP == 43)   NPTD00 = 4
			 
      IF (NPTD00 == 0)   NPTD00 = 1
      IF (ISOLNOD == 10) NPTD00 = 4
      IF (ISOLNOD == 4 .AND. (ISROT == 0 .OR. ISROT == 3)) THEN
          NPTD00 = 1
      ELSEIF(ISOLNOD == 4 )THEN
          NPTD00 = 4
      ELSEIF(JJHBED00 == 15)THEN
          NPTD00 = IGEO(4,IPIDD00)
      ENDIF

      IF(KEYWORD =='/INIBRI/ORTHO  '.OR.KEYWORD=='/INIBRI/EREF ')RETURN
C
C  Number of integration points
C
      IF (NPTD00 .NE.NPT) THEN
          CALL ANCMSG(MSGID=695,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ITRIS(IE),
     .                C1='NUMBER OF INTEGRATION POINTS',
     .                C2='SOLID PROPERTY',
     .                I2=IGEO(1,IPIDD00),
     .                C3=KEYWORD)
      ENDIF
C
      RETURN
      END

