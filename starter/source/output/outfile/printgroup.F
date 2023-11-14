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
Chd|  PRINTGROUP                    source/output/outfile/printgroup.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|====================================================================
      SUBROUTINE PRINTGROUP(
     1           ITAB     ,ITABM1   ,IGRNOD   ,NINICRACK,BUFSF   ,
     2           IGRSURF  ,IGRSLIN  ,INICRACK ,IXS      ,IXQ     ,
     3           IXC      ,IXT      ,IXP      ,IXR      ,IXTG    ,
     4           IXS10    ,IXS20    ,IXS16    ,IPART    ,
     5           KXX      ,IXIG3D   ,KXIG3D   ,
     6           IGRQUAD  ,IGRBRIC  ,IGRSH4N  ,IGRSH3N  ,IGRTRUSS,
     7           IGRBEAM  ,IGRSPRING,IGRPART  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
      USE R2R_MOD
      USE OPTIONDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),ITABM1(*),NINICRACK,IXS(NIXS,*),
     .        IXC(NIXC,*),IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .        IXTG(NIXTG,*),IXS10(6,*),IXS20(12,*),IXS16(8,*),
     .        IPART(LIPART1,*),IXQ(NIXQ,*),
     .        KXX(NIXX,*),KXIG3D(NIXIG3D,*),IXIG3D(*)
      my_real
     .        BUFSF(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE (GROUP_)  , DIMENSION(NGRPART) :: IGRPART
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   , DIMENSION(NSLIN)   :: IGRSLIN
      TYPE (INICRACK_), DIMENSION(NINICRACK) :: INICRACK
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "scr23_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,II,KK,NEL,NNOD,NSEG,IAD0,DGR,ITYP,SRFTYP,NE,
     .        KR,IADR,NSEGIGE,ID
      CHARACTER TITR*nchartitle,MESS*40,FMTT*15
C-----------------------------------------------
      IF (IPRI >= 0) THEN
C-----------------------------------------------
        IF (NGRBRIC > 0) THEN
c          MESS='BRIC. ELEMENT GROUP'
c          WRITE(ISTDO,'(2A)')' .. ',MESS
          DO I = 1,NGRBRIC
            ID = IGRBRIC(I)%ID
            TITR = IGRBRIC(I)%TITLE
            NEL = IGRBRIC(I)%NENTITY
              IF (NSUBDOM>0) THEN
C-----------> Multidomaines -> on prend le nb d'elts avant split----      
                 IF ((NEL==0).AND.(IGRBRIC(I)%R2R_ALL/=0)) GOTO 150
              ENDIF   
C----------------------------------------------------------------------      
            IF (NEL == 0 .AND. IGRBRIC(I)%SET_GROUP == 0) THEN
              CALL ANCMSG(MSGID=534,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1='BRICK',C2='BRICK',C3=TRIM(TITR),
     .                    I1=ID)
            ENDIF 
            IF (IPRI > 0) THEN
              WRITE(IOUT,'(//)')
              WRITE(IOUT,'(A,I10,3A,I6,A)')' BRIC. GROUP:',ID,',',TRIM(TITR),',',NEL,' ELEMENTS: '
              WRITE(IOUT,'(A)')' --------------------'
              WRITE(IOUT,FMT=FMW_10I)(IXS(NIXS,IGRBRIC(I)%ENTITY(J)),J=1,NEL)
            END IF
150      CONTINUE
          ENDDO
        ENDIF
C-----------------------------------------------
        IF (NGRQUAD > 0) THEN
c          MESS='QUAD ELEMENT GROUP'
c          WRITE(ISTDO,'(2A)')' .. ',MESS
          DO I = 1,NGRQUAD
            ID = IGRQUAD(I)%ID
            TITR = IGRQUAD(I)%TITLE
            NEL =IGRQUAD(I)%NENTITY
              IF (NSUBDOM>0) THEN
C-----------> Multidomaines -> on prend le nb d'elts avant split----      
                 IF ((NEL==0).AND.(IGRQUAD(I)%R2R_ALL/=0)) GOTO 160
              ENDIF 
C----------------------------------------------------------------------      
            IF (NEL == 0 .AND. IGRQUAD(I)%SET_GROUP == 0) THEN
              CALL ANCMSG(MSGID=534,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1='QUAD',C2='QUAD',C3=TRIM(TITR),
     .                    I1=ID)
            ENDIF 
            IF (IPRI > 0) THEN
              WRITE(IOUT,'(//)')
              WRITE(IOUT,'(A,I10,3A,I6,A)')' QUAD GROUP:',ID,',',TRIM(TITR),',',NEL,' ELEMENTS: '
              WRITE(IOUT,'(A)')' --------------------'
              WRITE(IOUT,FMT=FMW_10I)(IXQ(NIXQ,IGRQUAD(I)%ENTITY(J)),J=1,NEL)
            END IF
160      CONTINUE
          ENDDO
        ENDIF
C-----------------------------------------------
        IF (NGRSHEL > 0) THEN
c          MESS ='SHEL. ELEMENT GROUP'
c          WRITE(ISTDO,'(2A)')' .. ',MESS
          DO I = 1,NGRSHEL
            ID = IGRSH4N(I)%ID
            TITR = IGRSH4N(I)%TITLE
            NEL =IGRSH4N(I)%NENTITY
              IF (NSUBDOM>0) THEN
C-----------> Multidomaines -> on prend le nb d'elts avant split----      
                 IF ((NEL==0).AND.(IGRSH4N(I)%R2R_ALL/=0)) GOTO 170
              ENDIF 
C----------------------------------------------------------------------      
            IF (NEL == 0 .AND. IGRSH4N(I)%SET_GROUP == 0) THEN
              CALL ANCMSG(MSGID=534,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1='SHELL',C2='SHELL',C3=TRIM(TITR),
     .                    I1=ID)
            ENDIF  
            IF (IPRI > 0) THEN
              WRITE(IOUT,'(A)') ' '
              WRITE(IOUT,'(A)') ' '
              WRITE(IOUT,'(A,I10,3A,I6,A)')' SHEL. GROUP:',ID,',',TRIM(TITR),',',NEL,' ELEMENTS: '
              WRITE(IOUT,'(A)')' --------------------'
              WRITE(IOUT,FMT=FMW_10I) (IXC(NIXC,IGRSH4N(I)%ENTITY(J)),J=1,NEL)
            END IF
170      CONTINUE
          ENDDO
        ENDIF
C-----------------------------------------------
        IF (NGRTRUS > 0) THEN
c          MESS ='TRUS ELEMENT GROUP'
c          WRITE(ISTDO,'(2A)')' .. ',MESS
          DO I = 1,NGRTRUS
            ID = IGRTRUSS(I)%ID
            TITR = IGRTRUSS(I)%TITLE
            NEL =IGRTRUSS(I)%NENTITY
              IF (NSUBDOM>0) THEN
C-----------> Multidomaines -> on prend le nb d'elts avant split----      
                 IF ((NEL==0).AND.(IGRTRUSS(I)%R2R_ALL/=0)) GOTO 180
              ENDIF 
C----------------------------------------------------------------------      
            IF (NEL == 0.AND. IGRTRUSS(I)%SET_GROUP == 0) THEN
              CALL ANCMSG(MSGID=534,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1='TRUS',C2='TRUS',C3=TRIM(TITR),
     .                    I1=ID)
            ENDIF  
            IF (IPRI > 0) THEN
              WRITE(IOUT,'(//)')
              WRITE(IOUT,'(A,I10,3A,I6,A)')' TRUSS GROUP:',ID,',',TRIM(TITR),',',NEL,' ELEMENTS: '
              WRITE(IOUT,'(A)')' --------------------'
              WRITE(IOUT,FMT=FMW_10I)(IXT(NIXT,IGRTRUSS(I)%ENTITY(J)),J=1,NEL)
            END IF
180      CONTINUE
          ENDDO
        ENDIF
C-----------------------------------------------
        IF (NGRBEAM > 0) THEN
c          MESS ='BEAM ELEMENT GROUP'
c          WRITE(ISTDO,'(2A)')' .. ',MESS
          DO I = 1,NGRBEAM
            ID = IGRBEAM(I)%ID
            NEL =IGRBEAM(I)%NENTITY
            TITR = IGRBEAM(I)%TITLE
              IF (NSUBDOM>0) THEN
C-----------> Multidomaines -> on prend le nb d'elts avant split----      
                 IF ((NEL==0).AND.(IGRBEAM(I)%R2R_ALL/=0)) GOTO 190
              ENDIF 
C----------------------------------------------------------------------      
            IF (NEL == 0 .AND. IGRBEAM(I)%SET_GROUP == 0) THEN
              CALL ANCMSG(MSGID=534,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1='BEAM',C2='BEAM',C3=TRIM(TITR),
     .                    I1=ID)
            ENDIF
            IF (IPRI > 0) THEN
              WRITE(IOUT,'(//)')
              WRITE(IOUT,'(A,I10,3A,I6,A)')' BEAM GROUP:',ID,',',TRIM(TITR),',',NEL,' ELEMENTS: '
              WRITE(IOUT,'(A)')' --------------------'
              WRITE(IOUT,FMT=FMW_10I)(IXP(NIXP,IGRBEAM(I)%ENTITY(J)),J=1,NEL)
            END IF
190      CONTINUE
          ENDDO
        ENDIF
C-----------------------------------------------
        IF (NGRSPRI > 0) THEN
c          MESS ='SPRI ELEMENT GROUP'
c          WRITE(ISTDO,'(2A)')' .. ',MESS
          DO I = 1,NGRSPRI
            ID = IGRSPRING(I)%ID
            TITR = IGRSPRING(I)%TITLE
            NEL =IGRSPRING(I)%NENTITY
              IF (NSUBDOM>0) THEN
C-----------> Multidomaines -> on prend le nb d'elts avant split----      
                 IF ((NEL==0).AND.(IGRSPRING(I)%R2R_ALL/=0)) GOTO 200
              ENDIF 
C----------------------------------------------------------------------      
            IF (NEL == 0 .AND. IGRSPRING(I)%SET_GROUP == 0) THEN
              CALL ANCMSG(MSGID=534,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1='SPRI',C2='SPRI',C3=TRIM(TITR),
     .                    I1=ID)
            ENDIF 
            IF (IPRI > 0) THEN
              WRITE(IOUT,'(//)')
              WRITE(IOUT,'(A,I10,3A,I6,A)')' SPRING GROUP:',ID,',',TRIM(TITR),',',NEL,' ELEMENTS: '
              WRITE(IOUT,'(A)')' --------------------'
              WRITE(IOUT,FMT=FMW_10I)(IXR(NIXR,IGRSPRING(I)%ENTITY(J)),J=1,NEL)
            END IF
200      CONTINUE
          ENDDO
        ENDIF
C-----------------------------------------------
        IF (NGRSH3N > 0) THEN
c          MESS ='SHELL3N ELEMENT GROUP'
c          WRITE(ISTDO,'(2A)')' .. ',MESS
          DO I = 1,NGRSH3N
            ID = IGRSH3N(I)%ID
            TITR = IGRSH3N(I)%TITLE
            NEL =IGRSH3N(I)%NENTITY
              IF (NSUBDOM>0) THEN
C-----------> Multidomaines -> on prend le nb d'elts avant split----      
                 IF ((NEL==0).AND.(IGRSH3N(I)%R2R_ALL/=0)) GOTO 210
              ENDIF 
C----------------------------------------------------------------------         
            IF (NEL == 0 .AND. IGRSH3N(I)%SET_GROUP == 0) THEN
              IF(N2D==0)
     .        CALL ANCMSG(MSGID=534,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1='SH3N',C2='SH3N',C3=TRIM(TITR),
     .                    I1=ID)
              IF(N2D/=0)
     .        CALL ANCMSG(MSGID=534,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1='TRIA',C2='TRIA',C3=TRIM(TITR),
     .                    I1=ID)
            ENDIF 
            IF (IPRI > 0) THEN
              WRITE(IOUT,'(A)') ' '
              WRITE(IOUT,'(A)') ' '
              IF(N2D==0)WRITE(IOUT,'(A,I10,3A,I6,A)')' SH3N. GROUP:', ID,',',TRIM(TITR),',',NEL,' ELEMENTS: '
              IF(N2D/=0)WRITE(IOUT,'(A,I10,3A,I6,A)')' TRIA. GROUP:', ID,',',TRIM(TITR),',',NEL,' ELEMENTS: '
              WRITE(IOUT,'(A)')' --------------------'
              WRITE(IOUT,FMT=FMW_10I)(IXTG(NIXTG,IGRSH3N(I)%ENTITY(J)),J=1,NEL)
            END IF
210      CONTINUE
          ENDDO
        ENDIF
C-----------------------------------------------
        IF (NGRPART > 0) THEN
c          MESS ='PART GROUP'
c          WRITE(ISTDO,'(2A)')' .. ',MESS
          DO I = 1,NGRPART
            ID = IGRPART(I)%ID
            TITR = IGRPART(I)%TITLE
            NEL =IGRPART(I)%NENTITY
              IF (NSUBDOM>0) THEN
C-----------> Multidomaines -> on prend le nb de parts avant split-----      
                 IF ((NEL==0).AND.(IGRPART(I)%R2R_ALL/=0)) GOTO 220
              ENDIF   
C----------------------------------------------------------------------         
            IF (NEL == 0 .AND. IGRPART(I)%SET_GROUP == 0) THEN
              CALL ANCMSG(MSGID=534,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    C1='PART',C2='PART',C3=TRIM(TITR),
     .                    I1=ID)
            ENDIF  
            IF (IPRI > 0) THEN
              WRITE(IOUT,'(//)')
              WRITE(IOUT,'(A,I10,3A,I6,A)')' PART GROUP:',ID,',',TRIM(TITR),',',NEL,' PARTS: '
              WRITE(IOUT,'(A)')' ---------------------'
              WRITE(IOUT,FMT=FMW_10I)(IPART(4,IGRPART(I)%ENTITY(J)),J=1,NEL)
            END IF
220      CONTINUE      
          ENDDO
        ENDIF
C-----------------------------------------------
        IF (NSURF > 0)THEN
          DO I=1,NSURF
            SRFTYP=IGRSURF(I)%TYPE
            IF (SRFTYP == 100) THEN
              IF (IGRSURF(I)%ID_MADYMO == 0) THEN
                CALL ANCMSG(MSGID=692,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=IGRSURF(I)%ID)
              END IF
            ELSEIF (SRFTYP == 101) THEN
              CONTINUE
            ELSEIF (SRFTYP == 200) THEN
              CONTINUE
            ELSE
              NSEG=IGRSURF(I)%NSEG
              NSEGIGE=IGRSURF(I)%NSEG_IGE
C-----------> Multidomaines -> on prend le nb de segments avant split----      
                IF (NSUBDOM>0) NSEG=ISURF_R2R(2,I)
C----------------------------------------------------------------------          
              IF (NSEG == 0 .AND. NSEGIGE == 0 .AND. IGRSURF(I)%SET_GROUP == 0) THEN
                CALL ANCMSG(MSGID=692,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=IGRSURF(I)%ID)
              ENDIF
            ENDIF
          ENDDO
C---
            IF (IPRI > 0) THEN
              DO I=1,NSURF
C-----------> Multidomaines --------------------------------------------      
                 IF (NSUBDOM>0) THEN
                   IF((IGRSURF(I)%NSEG==0).AND.(ISURF_R2R(2,I)/=0)) GOTO 300
                 ENDIF
C----------------------------------------------------------------------          
                SRFTYP=IGRSURF(I)%TYPE
                IF (SRFTYP == 100) THEN
                  WRITE(IOUT,'(A)') ' '
                  WRITE(IOUT,'(A)') ' '
                  TITR = IGRSURF(I)%TITLE
                  WRITE(IOUT,'(A,I10,1X,A)')' EXTERNAL COUPLING TO SURFACE:',IGRSURF(I)%ID,TRIM(TITR)
                  WRITE(IOUT,'(A)') ' ----------------------------'
                  WRITE(IOUT,1000) IGRSURF(I)%ID_MADYMO
                  WRITE(IOUT,'(10X,A)')'CORRESPONDING DATA WILL BE READ IN RADIOSS ENGINE.'
                ELSEIF (SRFTYP == 101) THEN
                  WRITE(IOUT,'(A)') ' '
                  WRITE(IOUT,'(A)') ' '
                  TITR = IGRSURF(I)%TITLE
                  WRITE(IOUT,'(A,I10,1X,A)')' ELLIPSOIDAL SURFACE:',IGRSURF(I)%ID,TRIM(TITR)
                  WRITE(IOUT,'(A)') ' --------------------'
                    DGR=BUFSF(IGRSURF(I)%IAD_BUFR+36)
                  WRITE(IOUT,1010) BUFSF(IGRSURF(I)%IAD_BUFR+4),BUFSF(IGRSURF(I)%IAD_BUFR+5),
     .                       BUFSF(IGRSURF(I)%IAD_BUFR+6),BUFSF(IGRSURF(I)%IAD_BUFR+1),
     .                       BUFSF(IGRSURF(I)%IAD_BUFR+2),BUFSF(IGRSURF(I)%IAD_BUFR+3),
     .                       DGR,IGRSURF(I)%ID_MADYMO,
     .                       (BUFSF(IGRSURF(I)%IAD_BUFR+7+J-1),J=1,3),
     .                       (BUFSF(IGRSURF(I)%IAD_BUFR+7+J-1),J=4,6),
     .                       (BUFSF(IGRSURF(I)%IAD_BUFR+7+J-1),J=7,9)
                IGRSURF(I)%ID_MADYMO=0
                ELSEIF (SRFTYP == 200) THEN
                  WRITE(IOUT,'(A)') ' '
                  WRITE(IOUT,'(A)') ' '
                  TITR = IGRSURF(I)%TITLE
                  WRITE(IOUT,'(A,I10,A,A)')' INFINITE PLANE SURFACE:',IGRSURF(I)%ID,',',TRIM(TITR)
                  WRITE(IOUT,'(A)') ' --------------------'
                  WRITE(IOUT,'(A)') ' NORMAL PLANE VECTOR :'
                  WRITE(IOUT,1020)(BUFSF(IGRSURF(I)%IAD_BUFR+J),J=1,6)
                ELSE
                  NSEG=IGRSURF(I)%NSEG
!!                  IF (NSEG/=0) THEN
                    WRITE(IOUT,'(A)') ' '
                    WRITE(IOUT,'(A)') ' '
                    TITR = IGRSURF(I)%TITLE
                    WRITE(IOUT,'(A,I10,3A,I10,A)')' SURFACE:',IGRSURF(I)%ID,',',TRIM(TITR),',',NSEG,' SEGMENTS: '
                    WRITE(IOUT,'(A)')' -------------------'
                    WRITE(IOUT,'(A)')' NODE 1  NODE 2  NODE 3  NODE 4  ELTYP  ELEMENT'
                    DO J=1,NSEG
                      ITYP=IGRSURF(I)%ELTYP(J)
                      IF (ITYP == 1) THEN
                        NE=IXS(NIXS,IGRSURF(I)%ELEM(J))
                      ELSEIF (ITYP == 2) THEN
                       NE=IXQ(NIXQ,IGRSURF(I)%ELEM(J))
                      ELSEIF (ITYP == 3) THEN
                        NE=IXC(NIXC,IGRSURF(I)%ELEM(J))
                      ELSEIF (ITYP == 7) THEN
                        NE=IXTG(NIXTG,IGRSURF(I)%ELEM(J))
                      ELSE
                        NE=0
                      ENDIF
                      IF(N2D == 0)THEN
                        WRITE(IOUT,'(6I10)')(ITAB(IGRSURF(I)%NODES(J,L)),L=1,4),ITYP,NE
                      ELSE
                        WRITE(IOUT,'(2I10,16X,2I10)')(ITAB(IGRSURF(I)%NODES(J,L)),L=1,2),ITYP,NE
                      ENDIF
                    ENDDO
!!                  ENDIF ! IF (NSEG/=0)
                  IF (NSEGIGE/=0) THEN
                    NSEGIGE=IGRSURF(I)%NSEG_IGE
                    WRITE(IOUT,'(A)') ' '
                    WRITE(IOUT,'(A)') ' '
                    TITR = IGRSURF(I)%TITLE
                    WRITE(IOUT,'(A,I10,3A,I10,A)')' SURFACE:',IGRSURF(I)%ID,',',TRIM(TITR),',',NSEGIGE,' SEGMENTS ISOGEOMETRIQUES: '
                    WRITE(IOUT,'(A)')' -------------------'
                    WRITE(IOUT,'(A)')'      CP 1      CP 2      CP 3      CP 4     ELTYP   ELEMENT'
                    DO J=1,NSEGIGE
                      ITYP=IGRSURF(I)%ELTYP_IGE(J)
                      IF (ITYP == 101) THEN
                        NE=KXIG3D(5,IGRSURF(I)%ELEM_IGE(J))
                      ELSE
                        NE=0
                      ENDIF
                      IF(N2D == 0)THEN
                        WRITE(IOUT,FMT=FMW_6I)(IGRSURF(I)%NODES_IGE(J,L),L=1,4),ITYP,NE
                      ELSE
                        WRITE(IOUT,'(2I10,16X,2I10)')(IGRSURF(I)%NODES_IGE(J,L),L=1,2),ITYP,NE
                      ENDIF
                    ENDDO
                  ENDIF ! IF (NSEGIGE/=0)
                ENDIF ! IF (SRFTYP == 100)
300    CONTINUE
              ENDDO ! DO I=1,NSURF
            ENDIF ! IF (IPRI > 0)
          ENDIF ! IF (NSURF > 0)
C-----------------------------------------------
        IF (NSLIN > 0) THEN
          DO I=1,NSLIN
            NSEG=IGRSLIN(I)%NSEG
C-----------> Multidomaines -> on prend le nb de segments avant split----
              IF (NSUBDOM>0) NSEG=IGRSLIN(I)%NSEG_R2R_ALL
C-----------> Type19 interface -> warning deactivated for lines generated in translator
            TITR = IGRSLIN(I)%TITLE
            IF (TITR(1:19)=='Type19 interface nb') CYCLE      
C----------------------------------------------------------------------        
            IF (NSEG == 0 .AND. IGRSLIN(I)%SET_GROUP == 0) THEN
              CALL ANCMSG(MSGID=691,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=IGRSLIN(I)%ID)
            END IF
          END DO
C---
          DO I=1,NSLIN
            NSEG=IGRSLIN(I)%NSEG
              IF (NSUBDOM>0) THEN
C-----------> Multidomaines --------------------------------------------      
                 IF ((NSEG==0).AND.(IGRSLIN(I)%NSEG_R2R_ALL/=0)) GOTO 310
              ENDIF
C----------------------------------------------------------------------      
            IF (IPRI > 0) THEN
              WRITE(IOUT,'(A)') ' '
              WRITE(IOUT,'(A)') ' '
              TITR = IGRSLIN(I)%TITLE
              WRITE(IOUT,'(A,I10,3A,I10,A)')' LINE:   ',IGRSLIN(I)%ID,',',TRIM(TITR),',',NSEG,' SEGMENTS: '
              WRITE(IOUT,'(A)')' ----------------------'
              WRITE(IOUT,'(A)')' NODE 1  NODE 2  ELTYP  ELEMENT'
              DO J=1,NSEG
                ITYP=IGRSLIN(I)%ELTYP(J)
                IF(ITYP == 1)THEN
                 NE=IXS(NIXS,IGRSLIN(I)%ELEM(J))
                ELSEIF(ITYP == 2)THEN
                  NE=IXQ(NIXQ,IGRSLIN(I)%ELEM(J))
                ELSEIF(ITYP == 3)THEN
                  NE=IXC(NIXC,IGRSLIN(I)%ELEM(J))
                ELSEIF(ITYP == 7)THEN
                  NE=IXTG(NIXTG,IGRSLIN(I)%ELEM(J))
                ELSEIF(ITYP == 4)THEN
                  NE=IXT(NIXT,IGRSLIN(I)%ELEM(J))
                ELSEIF(ITYP == 5)THEN
                  NE=IXP(NIXP,IGRSLIN(I)%ELEM(J))
                ELSEIF(ITYP == 6)THEN
                  NE=IXR(NIXR,IGRSLIN(I)%ELEM(J))
                ELSEIF(ITYP == 8)THEN
                  NE=KXX(NIXX,IGRSLIN(I)%ELEM(J))
                ELSE
                  NE=0
                ENDIF
                WRITE(IOUT,FMT=FMW_4I)(ITAB(IGRSLIN(I)%NODES(J,L)),L=1,2),ITYP,NE
              ENDDO
            END IF
310      CONTINUE
          ENDDO
        ENDIF
C-----------------------------------------------
        IF (NINICRACK > 0) THEN
          DO I=1,NINICRACK
              NSEG=INICRACK(I)%NSEG
              IF (NSEG==0) THEN
                CALL ANCMSG(MSGID=693,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=INICRACK(I)%ID)
              END IF
          END DO
C
          DO I=1,NINICRACK
            NSEG=INICRACK(I)%NSEG
            IF (IPRI > 0) THEN
              WRITE(IOUT,'(A)') ' '
              WRITE(IOUT,'(A)') ' '
              TITR = INICRACK(I)%TITLE
              WRITE(IOUT,'(A,I10,3A)')' INICRACK:  ',INICRACK(I)%ID,',',TRIM(TITR)
              WRITE(IOUT,'(A)')' ----------------------'
              WRITE(IOUT,'(A)')'    NODE 1    NODE 2                     RATIO'
              DO J=1,NSEG
                WRITE(IOUT,FMT=FMW_2I_X_F)(ITAB(INICRACK(I)%SEG(J)%NODES(L)),L=1,2),INICRACK(I)%SEG(J)%RATIO
              ENDDO
            END IF
          ENDDO
        ENDIF
C-----------------------------------------------
        IF (NGRNOD > 0)THEN
          DO I=1,NGRNOD
            ID = IGRNOD(I)%ID
            NNOD=IGRNOD(I)%NENTITY
              IF (NSUBDOM>0) NNOD=IGRNOD(I)%R2R_ALL
C-----------> Multidomaines -> on prend le nb de noeuds avant split----      
C----------------------------------------------------------------------      
            IF (NNOD==0) THEN
              CALL ANCMSG(MSGID=690,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID)
            END IF
          ENDDO
C
          DO I=1,NGRNOD
            ID = IGRNOD(I)%ID
            NNOD=IGRNOD(I)%NENTITY
              IF (NSUBDOM>0) THEN
C-----------> Multidomaines --------------------------------------------      
                 IF ((NNOD==0).AND.(IGRNOD(I)%R2R_ALL/=0)) GOTO 350
              ENDIF
C----------------------------------------------------------------------                     
            IF (IPRI > 0) THEN
              WRITE(IOUT,'(A)') ' '
              WRITE(IOUT,'(A)') ' '
              TITR = IGRNOD(I)%TITLE
              WRITE(IOUT,'(A,I10,3A,I10,A)')' NODE GROUP:',ID,',',TRIM(TITR),',',NNOD,' NODES: '
              WRITE(IOUT,'(A)')' -------------------'
            WRITE(IOUT,FMT=FMW_10I)(ITAB(IGRNOD(I)%ENTITY(J)),J=1,NNOD)
            END IF
350      CONTINUE
          ENDDO
        ENDIF
C-----------
      ENDIF
C-----------
1000         FORMAT(/10X,'SURFACE DEFINITION :    '
     .           /10X,'--------------------    '
     .           /10X,'REFERENCE TO MADYMO COUPLING SURFACE : ',I8)
1010         FORMAT(/10X,'ELLIPSOID DEFINITION :    '
     .           /10X,'----------------------    '
     .           /10X,'ELLIPSOID CENTER                 ',1P3G20.13
     .           /10X,'ELLIPSOID SEMI-AXES LENGTH       ',1P3G20.13
     .           /10X,'ELLIPSOID DEGREE                 ',I10
     .           /10X,'SKEW NUMBER FOR POSITIONING      ',I10
     .           /10X,'COMPUTED ELLIPSOID ROTATION      ',
     .           /10X,'       WITH RESPECT TO SKEW :    ',1P3G20.13
     .           /10X,'                                 ',1P3G20.13
     .           /10X,'                                 ',1P3G20.13)
1020         FORMAT(/10X,'PLANE DEFINITION :    '
     .           /10X,'----------------------    '
     .           /48X,'X-ORD               Y-ORD               Z-ORD'
     .           /10X,'TAIL COORDINATES               ',1P3G20.13
     .           /10X,'HEAD COORDINATES               ',1P3G20.13)
C-----------

      RETURN
      END
