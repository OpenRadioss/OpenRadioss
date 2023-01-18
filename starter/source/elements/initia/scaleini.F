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
Chd|  SCALEINI                      source/elements/initia/scaleini.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SCALEINI(
     .   ELBUF_STR, IXS      , SIGSP   ,SIGI    ,NSIGI   ,
     .   NEL ,LFT , LLT      ,NFT    ,NSIGS ,
     .   PT  ,IGEO )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD      
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include       "param_c.inc"
#include       "com01_c.inc" 
#include       "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NSIGS, NSIGI,NEL,LFT,LLT,NFT ,PT(*), IXS(NIXS,*),
     . IGEO(NPROPGI,*)
      my_real
     .   SIGSP(NSIGI,NEL),SIGI(NSIGS,NEL)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*nchartitle,
     .   TITR
      INTEGER I,J,IIP,JPT, II, JJ, IPT, IPP,IUS,IPSU,N,IPID1,
     .        IFLAGINI,NVAR_TMP,IR,IS,IT,NPTT,NPTS,NPTR,PID1,IGTYP,
     .        IIS,IPT_ALL,ILAY,NLAY,KK,NPT,NPS,NPR,NLAYI,JHBET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MA
      my_real,
     .   DIMENSION(:), POINTER :: FACYLD 
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C-----------------------------------------------
      DO I=LFT,LLT                               
C    
        II=NFT+I
        N =IXS(NIXS,II)
        JJ=PT(II)
        IFLAGINI = 1
        IF(JJ == 0)IFLAGINI = 0   
c                                                        
        IF (IFLAGINI == 1)THEN                            
            IIS =NVSOLID1 + NVSOLID2 + NVSOLID3 + NUSOLID + 4 + NVSOLID4 

 
            NPT  =  NINT(SIGSP(IIS +3 , JJ))
            NPS  =  NINT(SIGSP(IIS +2 , JJ) )
            NPR  =  NINT(SIGSP(IIS +1 , JJ) )
            NLAYI=  NINT(SIGSP(IIS +4 , JJ) )

            NPTT  = ELBUF_STR%NPTT
            NPTS  = ELBUF_STR%NPTS
            NPTR  = ELBUF_STR%NPTR
            NLAY  = ELBUF_STR%NLAY
            IGTYP = ELBUF_STR%IGTYP

            IF ( NPT /= NPTT .OR.  NPS /= NPTS .OR. NPR /= NPTR .OR. NLAYI /= NLAY )THEN
             IF(IGTYP /= 20 .AND. IGTYP /= 21 .AND. IGTYP /= 22)THEN
             IPID1=IXS(NIXS-1,II)
             PID1  = IGEO(1,IPID1)
             JHBET = IGEO(10,IPID1)
             CALL FRETITL2(TITR,PID1,LTITR)
             CALL ANCMSG(MSGID=1214,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=PID1,
     .                I2=JHBET,
     .                I3=N)
            ENDIF

            ENDIF

            IIS = IIS + 7 


            DO ILAY = 1,NLAY
              IF(ELBUF_STR%BUFLY(ILAY)%L_FAC_YLD > 0 ) THEN
                DO IT=1,NPTT
                  DO IS=1,NPTS
                   DO IR=1,NPTR
                    KK = NPTR*NPTS*NPTT*(ILAY-1)+ NPTR*NPTS*(IT-1)+NPTR*(IS-1)+IR
                    LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT) 
                    LBUF%FAC_YLD(I) =   SIGSP(IIS+ KK ,JJ) 
                   ENDDO !R
                  ENDDO !S
                ENDDO !T
              ENDIF
           ENDDO
        ENDIF                                                              
      ENDDO !  I=LFT,LLT                                                           
c-----------
      RETURN
      END
