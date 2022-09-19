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
Chd|  THICKVAR                      source/interfaces/interf1/inintr_thkvar.F
Chd|-- called by -----------
Chd|        ININTR_THKVAR                 source/interfaces/interf1/inintr_thkvar.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE THICKVAR(ELBUF_TAB,IPARG,THKSH4_VAR,THKSH3_VAR,THKNOD,
     .                    IXC  ,IXTG     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*), IXC(NIXC,*), IXTG(NIXTG,*)
C     REAL
      my_real
     .   THKSH4_VAR(*), THKSH3_VAR(*), THKNOD(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, I, J, N, NEL
C-----------------------------------------------
      DO NG=1,NGROUP
        MTN=IPARG(1,NG)
        IF (MTN==0 .OR. MTN==13) CYCLE
        NEL=IPARG(2,NG)
        NFT=IPARG(3,NG)
        ITY=IPARG(5,NG)
        IF (ITY == 3) THEN
          DO I=1,NEL
            N=NFT+I
            THKSH4_VAR(N)=ELBUF_TAB(NG)%GBUF%THK(I)
            DO J=2,5
              THKNOD(IXC(J,N))=MAX(THKNOD(IXC(J,N)),THKSH4_VAR(N))
            END DO
          END DO
        ELSEIF(ITY == 7)THEN
          DO I=1,NEL
            N=NFT+I
            THKSH3_VAR(N)=ELBUF_TAB(NG)%GBUF%THK(I)
            DO J=2,4
              THKNOD(IXTG(J,N))=MAX(THKNOD(IXTG(J,N)),THKSH3_VAR(N))
            END DO
          END DO
        END IF
      END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  ININTR_THKVAR                 source/interfaces/interf1/inintr_thkvar.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        ININT3_THKVAR                 source/interfaces/inter3d1/inint3_thkvar.F
Chd|        THICKVAR                      source/interfaces/interf1/inintr_thkvar.F
Chd|        UPGRADE_MULTIMP               ../common_source/interf/upgrade_multimp.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFSCRATCH_MOD             source/interfaces/interf1/intbufscratch_mod.F
Chd|        INTSTAMP_MOD                  share/modules1/intstamp_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ININTR_THKVAR(ELBUF_TAB,
     1                  IPARI   ,INTBUF_TAB,INSCR   ,X       ,
     2                  IXS     ,IXC     ,PM      ,GEO     ,ITAB    ,
     3                  MWA     ,RWA     ,IXTG    ,IKINE   ,
     4                  IPARG   ,KNOD2ELS,
     5                  KNOD2ELC,KNOD2ELTG,NOD2ELS,NOD2ELC ,NOD2ELTG,
     6                  INTSTAMP,SKEW     ,MS     ,IN      ,V       ,
     7                  VR       ,RBY     ,NPBY   ,LPBY    ,IPARTS  ,
     8                  IPARTC,IPARTG,THK_PART,NOM_OPT,PTR_NOPT_INTER)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_MOD
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD
      USE INTBUFDEF_MOD
      USE INTBUFSCRATCH_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),  IXS(*),
     .   IXC(*), ITAB(*), MWA(*), IXTG(*), IKINE(*),
     .   IPARG(NPARG,*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*),
     .   NOD2ELS(*), NOD2ELC(*), NOD2ELTG(*),
     .   NPBY(NNPBY,*), LPBY(*), IPARTS(*), IPARTC(*), IPARTG(*)
      TYPE(INTSTAMP_DATA), TARGET :: INTSTAMP(*)
      TYPE(INTSTAMP_DATA),POINTER :: pINTSTAMP
      my_real
     .   X(3,*), PM(*), GEO(*), RWA(6,*),
     .   MS(*), IN(*),  V(3,*), VR(3,*), RBY(NRBY,*), SKEW(LSKEW,*),
     .   THK_PART(*)
      INTEGER NOM_OPT(LNOPT1,*),PTR_NOPT_INTER
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(SCRATCH_STRUCT_) INSCR(*)


C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, JINSCR, NINTI, IWRN, I, I_MEM,
     .   RESORT
      INTEGER NTY, STAT, ISTAMP, MULTIMP,LEN_FILNAM
      my_real,
     .   DIMENSION(:),ALLOCATABLE:: THKSH4_VAR,THKSH3_VAR,THKNOD
      CHARACTER*2148 FILNAM
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
      I_MEM = 0
      RESORT = 0
C----
      ALLOCATE (THKSH4_VAR(NUMELC) ,STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='THKSH4_VAR')
      ALLOCATE (THKSH3_VAR(NUMELTG) ,STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='THKSH3_VAR')
      ALLOCATE (THKNOD(NUMNOD) ,STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='THKNOD')
      THKSH4_VAR=ZERO
      THKSH3_VAR=ZERO
      THKNOD    =ZERO
C
      CALL THICKVAR(ELBUF_TAB,IPARG,THKSH4_VAR,THKSH3_VAR,THKNOD,
     .              IXC  ,IXTG     )
C
      IWRN = 0
      ISTAMP=0
      DO 100 N=1,NINTER
      RESORT = 0
      NTY=IPARI(7,N)
      IF (NTY /= 21 .AND. NTY /=23) GOTO 100
C
      IF(NTY==21) ISTAMP=ISTAMP+1
C
 200  CONTINUE
C

       IF (I_MEM == 2)THEN
          MULTIMP = MAX(IPARI(23,N)+8,NINT(IPARI(23,N)*1.5))
          CALL UPGRADE_MULTIMP(N,MULTIMP,INTBUF_TAB(N))
         I_MEM = 0
       ENDIF



      JINSCR=IPARI(10,N)
      NINTI=N
      ID=NOM_OPT(1,PTR_NOPT_INTER+NINTI)
      CALL FRETITL2(TITR,
     .              NOM_OPT(LNOPT1-LTITR+1,PTR_NOPT_INTER+NINTI),LTITR)

      IF(ISTAMP > 0)THEN
        pINTSTAMP => INTSTAMP(ISTAMP)
      ELSE
        NULLIFY(pINTSTAMP)
      ENDIF

      CALL ININT3_THKVAR(
     1  INTBUF_TAB(N),INSCR(NINTI)%WA           ,X        ,IXS   ,
     2  IXC          ,IXTG         ,PM        ,GEO      ,IPARI(1,N),
     3  NINTI         ,ITAB         ,MWA       ,RWA      ,IWRN      ,
     4  IKINE        ,KNOD2ELS     ,KNOD2ELC  ,KNOD2ELTG ,NOD2ELS  ,
     5  NOD2ELC   ,NOD2ELTG        ,
     6  THKSH4_VAR,THKSH3_VAR   ,THKNOD  ,pINTSTAMP  ,SKEW  ,
     7  MS        ,IN           ,V       ,VR             ,RBY      ,
     8  NPBY      ,LPBY         ,I_MEM   ,RESORT       ,IPARTS     ,
     9  IPARTC    ,IPARTG       ,THK_PART     ,ID      ,TITR,
     A  NOM_OPT)
      IF (I_MEM /= 0) GOTO 200
  100 CONTINUE
C
      IF(IWRN/=0) THEN
        LEN_FILNAM = OUTFILE_NAME_LEN + ROOTLEN + 6
        FILNAM = OUTFILE_NAME(1:OUTFILE_NAME_LEN)//ROOTNAM(1:ROOTLEN)//'.coord'
        OPEN(UNIT=IOU2,FILE=FILNAM(1:LEN_FILNAM),STATUS='UNKNOWN',
     .       FORM='FORMATTED')
        WRITE(IOU2,'(2A)')'#--1---|---2---|---3---|---4---|',
     .                    '---5---|---6---|---7---|---8---|'
        WRITE(IOU2,'(A)')'# NEW NODES COORDINATES'
        WRITE(IOU2,'(2A)')'#--1---|---2---|---3---|---4---|',
     .                    '---5---|---6---|---7---|---8---|'
        WRITE(IOU2,'(I10,1P3G20.13)')
     .               (ITAB(I),X(1,I),X(2,I),X(3,I),I=1,NUMNOD)
        WRITE(IOU2,'(2A)')'#--1---|---2---|---3---|---4---|',
     .                    '---5---|---6---|---7---|---8---|'
        WRITE(IOU2,'(A)')'# END OF NEW NODES COORDINATES'
        WRITE(IOU2,'(2A)')'#--1---|---2---|---3---|---4---|',
     .                    '---5---|---6---|---7---|---8---|'
        CLOSE(UNIT=IOU2)
      ENDIF
C
      DEALLOCATE (THKSH4_VAR,THKSH3_VAR)
C-----------
      RETURN
      END
