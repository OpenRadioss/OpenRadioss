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
Chd|  DOMDEC2                       source/spmd/domdec2.F         
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CHECK_SKEW                    source/spmd/domain_decomposition/check_skew.F
Chd|        C_DOMS10                      source/spmd/domdec2.F         
Chd|        FRONTPLUS_RM                  source/spmd/node/frontplus.F  
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        SPLIT_JOINT                   source/constraints/general/cyl_joint/split_joint.F
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SKEW_MOD                      share/modules1/skew_mod.F     
Chd|====================================================================
      SUBROUTINE DOMDEC2(
     1   DD_IAD ,IPARI,IB    ,NPBY  ,
     2   LPBY   ,IXRI ,IBVEL ,LBVEL ,
     3   IPARG  ,CEL    ,IXS  ,IXS10 ,IXS20 ,
     4   IXS16  ,IXQ    ,IXC  ,IXT   ,IXP   ,
     5   IXR    ,IXUR   ,IXTG ,IXTG6 ,T_MONVOL,
     6   IGRSURF,ADSKY ,LCNE  ,GEO   ,
     7   NPRW   ,LPRW   ,LCNI2 ,ADSKYI2,CEPI2,
     8   CELI2  ,I2NSNT ,ISKN  ,ISKWP,NSKWP  ,
     9   LSENSOR,ISENSP ,NSENSP,IACCP,NACCP  ,
     A   LACCELM, IBCV  ,IRBE3 ,LRBE3 ,FRONT_RM,
     B   IRBYM  ,LCRBYM ,CEP   ,IBCR  ,IRBE2 ,
     C   LRBE2  ,CEPSP  ,CELSPH,ILOADP,LLOADP,
     D   LGAUGE ,IGAUP  ,NGAUP ,INTBUF_TAB,IBFFLUX,
     E   ICNDS10,ITAGND ,IGEO  ,TAG_SKN,MULTIPLE_SKEW,
     F   IBFV   ,IBCSCYC,LBCSCYC,R_SKEW,IPM,
     G   SENSOR_TAB)
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD    
      USE FRONT_MOD  
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
      USE SKEW_MOD
      USE MONVOL_STRUCT_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "warn_c.inc"
#include      "fluid_scr_c.inc"
#include      "sphcom.inc"
#include      "submod_c.inc"
#include      "thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER),
     .        DD_IAD(NSPMD+1,NSPGROUP), NPRW(*), LPRW(*),
     .        NPBY(NNPBY,*), LPBY(*), IXRI(4,*),
     .        IBVEL(NBVELP,*), LBVEL(*), IPARG(NPARG,*), CEL(*),
     .        IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .        IXS10(6,*),IXS20(12,*),IXS16(8,*),IXTG6(4,*),IB(NIBCLD,*),
     .        IXUR(NIXUR,*),I2NSNT,
     .        ADSKY(0:*), LCNE, LCNI2, ADSKYI2(0:*),CEPI2(*),CELI2(*),
     .        ISKN(LISKN,*),ISKWP(*),NSKWP(*), LSENSOR(NSENSI,*),
     .        ISENSP(2,*), NSENSP(*), IACCP(*), NACCP(*),
     .        LACCELM(3,*),IBCV(NICONV, *),IRBE3(NRBE3L,*), LRBE3(*),
     .        FRONT_RM(NRBYM,NSPMD), IRBYM(NIRBYM,*) ,LCRBYM(*), CEP(*),
     .        IBCR(NIRADIA,*),IRBE2(NRBE2L,*), LRBE2(*),
     .        CEPSP(NUMSPH), CELSPH(NUMSPH),ILOADP(SIZLOADP,*),LLOADP(*),
     .        LGAUGE(3,*), IGAUP(*), NGAUP(*), IBFFLUX(NITFLUX,*),
     .        ICNDS10(3,*),ITAGND(*),IBFV(NIFV,*),IBCSCYC(4,*),LBCSCYC(2,*),
     .        R_SKEW(*),IPM(NPROPMI,*)
        INTEGER, DIMENSION(NPROPGI,*), INTENT(IN) :: IGEO
        INTEGER, DIMENSION(NUMSKW+NSUBMOD+1), INTENT(INOUT) :: TAG_SKN
        TYPE(SKEW_TYPE), DIMENSION(NUMSKW+1), INTENT(INOUT) :: MULTIPLE_SKEW
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       TAG_SKN : integer ; dimension=NUMSKW+NSUBMOD+1
!                 tag array --> tag the i SKEW if a SPRING uses it
!                 tag array=0 --> the SKEW is not used by a SPRING
!                 tag array=1 --> the SKEW is used by one SPRING      
!                 tag array>1 --> the SKEW is used by several SPRING
!       MULTIPLE_SKEW : SKEW_TYPE ; dimension=NUMSKW+1
!                       MULTIPLE_SKEW(I)%PLIST(:) is a list of processor
!                       where the SKEW is stuck
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
      my_real
     .        GEO(NPROPG,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN) :: T_MONVOL
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       SENSOR_TAB : structure, dimension = NSENSOR
!                    new structure for SENSOR
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IP,IPROC,IF1, IF2, TYP,
     .        INSNMAX, IPMAX, SUM, INSNP, LASTM,
     .        K, I, IS, NN_S, IAD, J, K1, K3, NN, NL,NL_L,
     .        NSN,NMN,P,N,N1,N2,N3,N4,NTY,NGROU,NEL,NG,L,M,NIR,
     .        IMASTER,KK,NRTS,NRTM,PM,
     .        OFF, PROC, NIN, ITY_OLD, ITY, MSR, NSL_L, NSL,
     .        IELS(NSPMD), P_SPH, INT2FLAG,INT2FRPLUS,
     .        ILEV,CNT,OFFSET,FINGEO,IAD1,IAD2,NUMLOADP,ITE2FRPLUS
      INTEGER :: ISENS
      my_real
     .        ECT_AK,FR_AK
C-----------------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------------
C
C-----------------------------------------------------
      INT2FLAG=0
      CNT = 0
 5000 CONTINUE
c     count flag, how many times we redo this task
c      CNT = CNT+1
c      print*,'Count:',CNT
      INT2FRPLUS=0
C-----------------------------------------------------
      ! ------------------------
      DO ISENS=1,NSENSOR
            ! ------------------------   
            ! dist-surf sensor : for plane defined by 3 nodes, 
            !                    add the 3 nodes and the reference node
            !                    on all spmd processors
            IF (SENSOR_TAB(ISENS)%TYPE==16) THEN
                ! -------------
                ! reference node : %IPARAM(1)
                N1 = SENSOR_TAB(ISENS)%IPARAM(1)
                DO P=1,NSPMD
                    CALL IFRONTPLUS(N1,P)
                ENDDO
                ! -------------
                IF(SENSOR_TAB(ISENS)%IPARAM(2)==0) THEN
                    ! plane nodes : %IPARAM(3:5)
                    DO I=1,3
                        N1 = SENSOR_TAB(ISENS)%IPARAM(3+I-1)
                        DO P=1,NSPMD
                            CALL IFRONTPLUS(N1,P)
                        ENDDO
                    ENDDO
                    ! -------------
                ENDIF
            ENDIF
            ! ------------------------
      ENDDO
      ! ------------------------
      ! check if a user sensor is used
      IF(SENSOR_USER_STRUCT%IS_USED) THEN
            ! ------------------------
            ! add all the nodes on the NSPMD domains
            IF(SENSOR_USER_STRUCT%POINTER_NODE>0) THEN
                DO I=1,SENSOR_USER_STRUCT%NUMBER_NODE
                    N1 = SENSOR_USER_STRUCT%NODE_LIST(I)
                    DO P=1,NSPMD
                        CALL IFRONTPLUS(N1,P)
                    ENDDO
                ENDDO
            ENDIF     
            ! ------------------------ 
      ENDIF
      ! ------------------------
      IF(NUMSKW.GT.0)THEN
C skew global fixe
       ISKWP(1)=1
       DO P = 1, NSPMD
         NSKWP(P) = 0
       END DO        
       IF(N2D.EQ.0)THEN
        OFFSET = NUMELS + NUMELQ + NUMELC + NUMELT + NUMELP
!       check if a SPRING is linked with a SKEW
        CALL CHECK_SKEW(IXR,IGEO,ISKN,CEP(OFFSET+1),ISKWP,NSKWP,TAG_SKN,MULTIPLE_SKEW,
     .                  R_SKEW,IPM)


        DO I=1,NUMSKW
          IF(TAG_SKN(I+1)/=0) CYCLE     !       tag/=0 --> already done in CHECK_SKEW
          N1=ISKN(1,I+1)
          N2=ISKN(2,I+1)
          N3=ISKN(3,I+1)
          INSNMAX = 0
          IMASTER = 1
          IF(N1+N2+N3.NE.0) THEN
            DO P = 1, NSPMD
              NN = NLOCAL(N1,P)+
     +             NLOCAL(N2,P)+
     +             NLOCAL(N3,P)       
              IF(NN.GT.INSNMAX)THEN
                INSNMAX=NN
                IMASTER=P
              END IF
            END DO
            IF(INSNMAX.NE.3)THEN
              CALL IFRONTPLUS(N1,IMASTER)
              CALL IFRONTPLUS(N2,IMASTER)
              CALL IFRONTPLUS(N3,IMASTER)
            END IF
          END IF
          ISKWP(I+1) = IMASTER
          NSKWP(IMASTER) = NSKWP(IMASTER)+1
        END DO
       ELSE
        DO I=1,NUMSKW
          N1=ISKN(1,I+1)
          N2=ISKN(2,I+1) 
          INSNMAX = 0
          IMASTER = 1
          IF(N1+N2.NE.0) THEN
            DO P = 1, NSPMD
              NN = NLOCAL(N1,P)+
     .               NLOCAL(N2,P)
              IF(NN.GT.INSNMAX)THEN
                INSNMAX=NN
                IMASTER=P
              END IF
            END DO
            IF(INSNMAX.NE.2)THEN
              CALL IFRONTPLUS(N1,IMASTER)
              CALL IFRONTPLUS(N2,IMASTER)             
            END IF
          END IF
          ISKWP(I+1) = IMASTER
          NSKWP(IMASTER) = NSKWP(IMASTER)+1
        END DO
       END IF
      END IF                
C-----------------------------------------------------
C Traitement special rigid wall moving
C-----------------------------------------------------
      K = 0
      DO N = 1, NRWALL
        N3 = 2*NRWALL+N
        NSL=NPRW(N)
        MSR = NPRW(N3)
        IF(MSR.NE.0) THEN
          DO P = 1, NSPMD
            NSL_L = 0
            DO KK = 1, NSL
              NN = LPRW(K+KK)
              IF(NLOCAL(NN,P)==1)THEN
                NSL_L = NSL_L + 1
              ENDIF
            ENDDO
            IF(NSL_L.GT.0) CALL IFRONTPLUS(MSR,P) 
          ENDDO
        ENDIF
        K = K + NSL
      ENDDO
C
C-----------------------------------------------------
C Traitement special pressure loads + forces concentrees
C-----------------------------------------------------
      DO N = 1, NCONLD
       N1 = IB(1,N)
       N2 = IB(2,N)
       N3 = IB(3,N)
       N4 = IB(4,N)
       IF(N4.NE.-1.AND.N2D.EQ.0.AND.N4.NE.0)THEN
         DO P = 1, NSPMD
           IF(NLOCAL(N1,P)==1.AND.
     +        NLOCAL(N2,P)==1.AND.
     +        NLOCAL(N3,P)==1.AND.
     +        NLOCAL(N4,P)==1)THEN     
             GOTO 9999
           ENDIF
         ENDDO
       ENDIF
       IF(N4.NE.-1.AND.N2D.EQ.0)THEN
         DO P = 1, NSPMD
            IF(NLOCAL(N1,P)==1.AND.
     +        NLOCAL(N2,P)==1.AND.
     +        NLOCAL(N3,P)==1)THEN    
             IF(N4.NE.0) THEN
               CALL IFRONTPLUS(N4,P)
             ENDIF
             GOTO 9999
           ENDIF
         ENDDO
       ENDIF
       IF(N4.NE.-1)THEN
         DO P = 1, NSPMD
            IF(NLOCAL(N1,P)==1.AND.
     +        NLOCAL(N2,P)==1)THEN    
             IF(N2D.EQ.0.AND.N4.NE.0) THEN
               CALL IFRONTPLUS(N4,P)
             ENDIF
             IF(N2D.EQ.0) THEN
               CALL IFRONTPLUS(N3,P) 
             ENDIF
             GOTO 9999
           ENDIF
         ENDDO
       ENDIF
       DO P = 1, NSPMD
         IF(NLOCAL(N1,P)==1) THEN
           IF(N2D.EQ.0.AND.N4.NE.0.AND.N4.NE.-1) THEN
             CALL IFRONTPLUS(N4,P)
           ENDIF
           IF(N2D.EQ.0.AND.N4.NE.-1) THEN
             CALL IFRONTPLUS(N3,P)
           ENDIF
           IF(N4.NE.-1) THEN
             CALL IFRONTPLUS(N2,P)
           ENDIF
           GOTO 9999
         ENDIF
       ENDDO
       IF(N4.NE.0.AND.N4.NE.0.AND.N4.NE.-1) THEN
         CALL IFRONTPLUS(N4,1)
       ENDIF
       IF(N2D.EQ.0.AND.N4.NE.-1) THEN
         CALL IFRONTPLUS(N3,1) 
       ENDIF
       IF(N4.NE.-1) THEN
         CALL IFRONTPLUS(N2,1)
       ENDIF
       CALL IFRONTPLUS(N1,1)
 9999  CONTINUE
      ENDDO
C-----------------------------------------------------
C Traitement special flux conv  for heat transfert
C-----------------------------------------------------
      DO N = 1, NUMCONV
       N1 = IBCV(1,N)
       N2 = IBCV(2,N)
       N3 = IBCV(3,N)
       N4 = IBCV(4,N)
       IF(N2D.EQ.0.AND.N4.NE.0)THEN
         DO P = 1, NSPMD
           IF(NLOCAL(N1,P)==1.AND.
     +        NLOCAL(N2,P)==1.AND.
     +        NLOCAL(N3,P)==1.AND.
     +        NLOCAL(N4,P)==1)THEN     
             GOTO 9191
           ENDIF
         ENDDO
       ENDIF
       IF(N2D.EQ.0)THEN
         DO P = 1, NSPMD
           IF(NLOCAL(N1,P)==1.AND.
     +        NLOCAL(N2,P)==1.AND.
     +        NLOCAL(N3,P)==1)THEN
             IF(N4.NE.0) THEN
               CALL IFRONTPLUS(N4,P)
             ENDIF
             GOTO 9191
           ENDIF
         ENDDO
       ENDIF
       DO P = 1, NSPMD
         IF(NLOCAL(N1,P)==1.AND.
     +      NLOCAL(N2,P)==1)THEN
           IF(N2D.EQ.0.AND.N4.NE.0) THEN
             CALL IFRONTPLUS(N4,P)
           ENDIF
           IF(N2D.EQ.0) THEN
             CALL IFRONTPLUS(N3,P)
           ENDIF
           GOTO 9191
         ENDIF
       ENDDO
       DO P = 1, NSPMD
         IF(NLOCAL(N1,P)==1) THEN        
           IF(N2D.EQ.0.AND.N4.NE.0) THEN
             CALL IFRONTPLUS(N4,P)
           ENDIF
           IF(N2D.EQ.0) THEN
             CALL IFRONTPLUS(N3,P)
           ENDIF
             CALL IFRONTPLUS(N2,P)
           GOTO 9191
         ENDIF
       ENDDO
       IF(N4.NE.0) THEN
         CALL IFRONTPLUS(N4,1)
       ENDIF
       IF(N2D.EQ.0) THEN
         CALL IFRONTPLUS(N3,1)
       ENDIF
       CALL IFRONTPLUS(N2,1)
       CALL IFRONTPLUS(N1,1)       
 9191  CONTINUE
      ENDDO
C
C-----------------------------------------------------
C Traitement special radiative flux for heat transfert
C-----------------------------------------------------
      DO N = 1, NUMRADIA
       N1 = IBCR(1,N)
       N2 = IBCR(2,N)
       N3 = IBCR(3,N)
       N4 = IBCR(4,N)
       IF(N2D.EQ.0.AND.N4.NE.0)THEN
         DO P = 1, NSPMD
            IF(NLOCAL(N1,P)==1.AND.
     +        NLOCAL(N2,P)==1.AND.
     +        NLOCAL(N3,P)==1.AND.
     +        NLOCAL(N4,P)==1)THEN    
             GOTO 9192
           ENDIF
         ENDDO
       ENDIF
       IF(N2D.EQ.0)THEN
         DO P = 1, NSPMD
             IF(NLOCAL(N1,P)==1.AND.
     +        NLOCAL(N2,P)==1.AND.
     +        NLOCAL(N3,P)==1)THEN   
             IF(N4.NE.0) THEN
               CALL IFRONTPLUS(N4,P)
             ENDIF
             GOTO 9192
           ENDIF
         ENDDO
       ENDIF
       DO P = 1, NSPMD
         IF(NLOCAL(N1,P)==1.AND.
     +      NLOCAL(N2,P)==1)THEN
           IF(N2D.EQ.0.AND.N4.NE.0) THEN
             CALL IFRONTPLUS(N4,P)
           ENDIF
           IF(N2D.EQ.0) THEN
             CALL IFRONTPLUS(N3,P)
           ENDIF
           GOTO 9192
         ENDIF
       ENDDO
       DO P = 1, NSPMD
         IF(NLOCAL(N1,P)==1) THEN
           IF(N2D.EQ.0.AND.N4.NE.0) THEN
             CALL IFRONTPLUS(N4,P)           
           ENDIF
           IF(N2D.EQ.0) THEN
             CALL IFRONTPLUS(N3,P) 
           ENDIF
             CALL IFRONTPLUS(N2,P)
           GOTO 9192
         ENDIF
       ENDDO
       IF(N4.NE.0) THEN
         CALL IFRONTPLUS(N4,1)
       ENDIF
       IF(N2D.EQ.0) THEN
         CALL IFRONTPLUS(N3,1)
       ENDIF
       CALL IFRONTPLUS(N2,1)
       CALL IFRONTPLUS(N1,1)       
 9192  CONTINUE
      ENDDO
C---------------------------------------------------------
C Traitement special imposed heat flux  for heat transfert
C---------------------------------------------------------
      DO N = 1, NFXFLUX
       IF(IBFFLUX(10,N) == 1) CYCLE
       N1 = IBFFLUX(1,N)
       N2 = IBFFLUX(2,N)
       N3 = IBFFLUX(3,N)
       N4 = IBFFLUX(4,N)
       IF(N2D.EQ.0.AND.N4.NE.0)THEN
         DO P = 1, NSPMD
           IF(NLOCAL(N1,P)==1.AND.
     +        NLOCAL(N2,P)==1.AND.
     +        NLOCAL(N3,P)==1.AND.
     +        NLOCAL(N4,P)==1) GOTO 9193
         ENDDO
       ENDIF
       IF(N2D.EQ.0)THEN
         DO P = 1, NSPMD
           IF(NLOCAL(N1,P)==1.AND.
     +        NLOCAL(N2,P)==1.AND.
     +        NLOCAL(N3,P)==1)THEN
             IF(N4.NE.0) CALL IFRONTPLUS(N4,P)
             GOTO 9193
           ENDIF
         ENDDO
       ENDIF
       DO P = 1, NSPMD
         IF(NLOCAL(N1,P)==1.AND.NLOCAL(N2,P)==1)THEN
           IF(N2D.EQ.0.AND.N4.NE.0) CALL IFRONTPLUS(N4,P)
           IF(N2D.EQ.0) CALL IFRONTPLUS(N3,P)
           GOTO 9193
         ENDIF
       ENDDO
       DO P = 1, NSPMD
         IF(NLOCAL(N1,P)==1) THEN        
           IF(N2D.EQ.0.AND.N4.NE.0) CALL IFRONTPLUS(N4,P)
           IF(N2D.EQ.0) CALL IFRONTPLUS(N3,P)
               CALL IFRONTPLUS(N2,P)
           GOTO 9193
         ENDIF
       ENDDO
       IF(N4.NE.0)  CALL IFRONTPLUS(N4,1)
       IF(N2D.EQ.0) CALL IFRONTPLUS(N3,1)
       CALL IFRONTPLUS(N2,1)
       CALL IFRONTPLUS(N1,1)       
 9193  CONTINUE
      ENDDO
C-----------------------------------------------------
C Traitement special load/Pfluid
C-----------------------------------------------------
      DO N = 1, NLOADP
       DO I = 1,ILOADP(1,N)/4
        N1=LLOADP(ILOADP(4,N)+4*(I-1))
        N2=LLOADP(ILOADP(4,N)+4*(I-1)+1)
        N3=LLOADP(ILOADP(4,N)+4*(I-1)+2)
        N4=LLOADP(ILOADP(4,N)+4*(I-1)+3)
       IF(N4.NE.-1.AND.N2D.EQ.0.AND.N4.NE.0)THEN
         DO P = 1, NSPMD
           IF(NLOCAL(N1,P)==1.AND.
     +        NLOCAL(N2,P)==1.AND.
     +        NLOCAL(N3,P)==1.AND.
     +        NLOCAL(N4,P)==1)THEN           
             GOTO 8888
           ENDIF
         ENDDO
       ENDIF
       IF(N4.NE.-1.AND.N2D.EQ.0)THEN
         DO P = 1, NSPMD
           IF(NLOCAL(N1,P)==1.AND.
     +        NLOCAL(N2,P)==1.AND.
     +        NLOCAL(N3,P)==1)THEN           
             IF(N4.NE.0) THEN
               CALL IFRONTPLUS(N4,P)
             ENDIF
             GOTO 8888
           ENDIF
         ENDDO
       ENDIF
       IF(N4.NE.-1)THEN
         DO P = 1, NSPMD
           IF(NLOCAL(N1,P)==1.AND.
     +        NLOCAL(N2,P)==1)THEN           
             IF(N2D.EQ.0.AND.N4.NE.0) THEN
               CALL IFRONTPLUS(N4,P)
             ENDIF
             IF(N2D.EQ.0) THEN
               CALL IFRONTPLUS(N3,P)
             ENDIF
             GOTO 8888
           ENDIF
         ENDDO
       ENDIF
       DO P = 1, NSPMD
         IF(NLOCAL(N1,P)==1) THEN
           IF(N2D.EQ.0.AND.N4.NE.0.AND.N4.NE.-1) THEN
             CALL IFRONTPLUS(N4,P)
           ENDIF
           IF(N2D.EQ.0.AND.N4.NE.-1) THEN
             CALL IFRONTPLUS(N3,P)
           ENDIF
           IF(N4.NE.-1) THEN
             CALL IFRONTPLUS(N2,P)
           ENDIF
           GOTO 8888
         ENDIF
       ENDDO
       IF(N4.NE.0.AND.N4.NE.0.AND.N4.NE.-1) THEN
         CALL IFRONTPLUS(N4,1)
       ENDIF
       IF(N2D.EQ.0.AND.N4.NE.-1) THEN
         CALL IFRONTPLUS(N3,1)
       ENDIF
       IF(N4.NE.-1) THEN
         CALL IFRONTPLUS(N2,1)
       ENDIF
       CALL IFRONTPLUS(N1,1)
 8888  CONTINUE
       ENDDO
      ENDDO
C
C-----------------------------------------------------
C Traitement supplementaire rivets
C-----------------------------------------------------
      DO P = 1, NSPMD
        DO J=1,NRIVET
          IF1 = NLOCAL(IXRI(2,J),P)
          IF2 = NLOCAL(IXRI(3,J),P)
          IF (IF1.EQ.1.OR.IF2.EQ.1) THEN
            CALL IFRONTPLUS(IXRI(2,J),P)
            CALL IFRONTPLUS(IXRI(3,J),P)            
          ENDIF
        ENDDO
      ENDDO

C
C-----------------------------------------------------
C Traitement supplementaire RBE2
C-----------------------------------------------------
      IF (NRBE2.GT.0.AND.NSPMD.GT.1) THEN
        DO N = 1, NRBE2
          NSN = IRBE2(5,N)
          M   = IRBE2(3,N)
          IAD = IRBE2(1,N)
          DO P = 1, NSPMD
            IF (NLOCAL(M,P).EQ.0) THEN
              IMASTER = 0
              DO J = 1, NSN
                L = LRBE2(IAD+J)
                IF(NLOCAL(L,P).NE.0)THEN
                  IMASTER = 1
                  GO TO 186
                ENDIF
              ENDDO
 186          CONTINUE
              IF(IMASTER.EQ.1)THEN
                CALL IFRONTPLUS(M,P)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
Cel traitement noeuds non connectes
        DO N = 1, NRBE2
          NSN = IRBE2(5,N)
          M   = IRBE2(3,N)
          IAD = IRBE2(1,N)
          SUM = 0
          INSNMAX = 0
          IPMAX = 1
          LASTM = 0
          DO P=1,NSPMD
            IF(NLOCAL(M,P).NE.0) THEN
              SUM = SUM + 1
              LASTM = P
            ENDIF
            INSNP = 0
            DO J = 1, NSN
              L = LRBE2(IAD+J)
              IF(NLOCAL(L,P).NE.0)THEN
                INSNP = INSNP + 1
              ENDIF
            ENDDO
            IF (INSNP.GT.INSNMAX) THEN
              IPMAX = P
              INSNMAX = INSNP
            ENDIF
          END DO
          IF(SUM.EQ.0) THEN
            IF(INSNMAX.EQ.0) THEN
              CALL IFRONTPLUS(M,1)
            ELSE
              CALL IFRONTPLUS(M,IPMAX)
            ENDIF
          ELSEIF(INSNMAX.EQ.0) THEN
            IPMAX = LASTM
          ENDIF
C
          DO J = 1, NSN
            L = LRBE2(IAD+J)
            SUM = 0
            DO P=1,NSPMD
              IF(NLOCAL(L,P).NE.0)THEN
                SUM = SUM + 1
              ENDIF
            ENDDO
            IF(SUM.EQ.0) THEN
              CALL IFRONTPLUS(L,IPMAX)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
C-----------------------------------------------------
C Traitement RBE3 : Idem int2
C-----------------------------------------------------
      IF (NRBE3.GT.0.AND.NSPMD.GT.1) THEN
        DO N = 1, NRBE3
          NIR = IRBE3(5,N)
          K   = IRBE3(3,N)
          IAD = IRBE3(1,N)
              IMASTER = 0
              DO P = 1, NSPMD
                IF (NLOCAL(K,P).NE.0) THEN
                  IMASTER = 1
                ENDIF
              ENDDO
              IF (IMASTER.EQ.0) THEN
                IMASTER = 1
                DO J=1,NIR
                  KK = LRBE3(IAD+J)
                  DO P = 1, NSPMD
                    IF (NLOCAL(KK,P).NE.0) THEN
                      IMASTER = P
                      GOTO 51
                    ENDIF
                  ENDDO
                ENDDO
 51             CONTINUE
                CALL IFRONTPLUS(K,IMASTER)
              ENDIF
              DO P = 1, NSPMD
                IF (NLOCAL(K,P).EQ.0) THEN
                  GO TO 201
                ENDIF
C pas d'optimisation possible
                DO J=1,NIR
                  KK = LRBE3(IAD+J)
                  IF (NLOCAL(KK,P).EQ.0) THEN
                    CALL IFRONTPLUS(KK,P)
                  ENDIF
                ENDDO
C optimisation possible
 201            CONTINUE
              ENDDO
        ENDDO
      ENDIF


C
C-----------------------------------------------------
C Traitement supplementaire rigid bodies
C-----------------------------------------------------
      IF (NRBYKIN.GT.0.AND.NSPMD.GT.1) THEN
C--------------------------------------------------------------
C Dans le cas de noeuds SECONDARYs de Rigid bodies non connectees
C on les affecte sur le PMASTER
C--------------------------------------------------------------
        K=0
        DO N = 1, NRBYKIN
          NSN = NPBY(2,N)
          M   = NPBY(1,N)
C Recherche du 1er  proc qui a le noeud master
          DO P=1,NSPMD
            IF (NLOCAL(M,P).NE.0) GOTO 86
          ENDDO
 86       CONTINUE
          PM = P

          DO J = 1, NSN
            L = LPBY(K+J)
            DO P=1,NSPMD
              IF(NLOCAL(L,P).NE.0) GOTO 87
            ENDDO
            CALL IFRONTPLUS(L,PM)
 87         CONTINUE
          ENDDO
          K = K + NSN
        ENDDO
C--------------------------------------------------------------
        K=0
        DO N = 1, NRBYKIN
          NSN = NPBY(2,N)
          M   = NPBY(1,N)
          DO P = 1, NSPMD
            IF(NLOCAL(M,P)==0)THEN
              IMASTER = 0
              DO J = 1, NSN
                L = LPBY(K+J)
                IF(NLOCAL(L,P).NE.0)THEN
                  IMASTER = 1
                  GO TO 85
                ENDIF
              ENDDO
 85           CONTINUE
              IF(IMASTER.EQ.1)THEN
                CALL IFRONTPLUS(M,P)
              ENDIF
            ENDIF
          ENDDO
          K = K + NSN
        ENDDO
Cel traitement noeuds non connectes
        K=0
        DO N = 1, NRBYKIN
          NSN = NPBY(2,N)
          M   = NPBY(1,N)
          SUM = 0
          INSNMAX = 0
          IPMAX = 1
          LASTM = 0
          DO P=1,NSPMD
            IF(NLOCAL(M,P).NE.0) THEN
              SUM = SUM + 1
              LASTM = P
            ENDIF
            INSNP = 0
            DO J = 1, NSN
              L = LPBY(K+J)
              IF(NLOCAL(L,P).NE.0)THEN
                INSNP = INSNP + 1
              ENDIF
            ENDDO
            IF (INSNP.GT.INSNMAX) THEN
              IPMAX = P
              INSNMAX = INSNP
            ENDIF
          END DO
C
          IF(SUM.EQ.0) THEN
            IF(INSNMAX.EQ.0) THEN
              CALL IFRONTPLUS(M,1)
            ELSE
              CALL IFRONTPLUS(M,IPMAX)
            ENDIF
C cas au moins un master connecte mais aucun noeud SECONDARY de connecte
          ELSEIF(INSNMAX.EQ.0) THEN
            IPMAX = LASTM
          ENDIF
C
          DO J = 1, NSN
            L = LPBY(K+J)
            SUM = 0
            DO P=1,NSPMD
              IF(NLOCAL(L,P).NE.0)THEN
                SUM = SUM + 1
              ENDIF
            ENDDO
            IF(SUM.EQ.0) THEN
              CALL IFRONTPLUS(L,IPMAX)
            ENDIF
          ENDDO
          K = K + NSN
        ENDDO

      ENDIF
C

C-----------------------------------------------------
C Traitement supplementaire RBM : Idem RB
C-----------------------------------------------------
      IF(NFXVEL > 0 .AND. NSPMD > 1)THEN

        DO N=1,NFXVEL
           FINGEO = IBFV(13,N)
           IF (FINGEO == 2)THEN      ! FINGEO=2 option /IMPVEL/FGEO
              N1 = IBFV(1,N)         ! This impvel option has 2 nodes, 
              N2 = IBFV(14,N)        ! ensure that the Nodes are on same domain.

              IAD1 = IFRONT%IENTRY(N1)
              IAD2 = IFRONT%IENTRY(N2)

              IF (IAD1 == -1 .AND. IAD2 == -1)THEN   ! Case Both nodes are free
                 CALL  IFRONTPLUS(N1,1)              ! Stick them on processor 1 to avoid them
                 CALL  IFRONTPLUS(N2,1)              ! to be sticked on different DOMAINS
              ELSE
                 IF(IAD1 /= -1 ) THEN
                   DO WHILE (IAD1 /= 0)              ! IAD1 is the pointer of Node1
                     P = IFRONT%P(1,IAD1)
                     CALL IFRONTPLUS(N2,P)           ! Stick Node2 where Node1 is
                     IAD1=IFRONT%P(2,IAD1)           ! NEXT
                   ENDDO
                 ENDIF

                 IF(IAD2 /= -1 ) THEN
                   DO WHILE (IAD2 /= 0)              ! IAD2 is the pointer of Node2
                     P = IFRONT%P(1,IAD2)
                     CALL IFRONTPLUS(N1,P)           ! Stick Node1 where Node2 is
                     IAD2=IFRONT%P(2,IAD2)           ! NEXT
                   ENDDO
                 ENDIF

              ENDIF
           ENDIF
        ENDDO
      ENDIF                                          ! IF(NFXVEL > 0 .AND. NSPMD > 1)THEN

      IF (NIBVEL.GT.0.AND.NSPMD.GT.1) THEN
        K=0
        DO N = 1, NIBVEL
          NSN = IBVEL(3,N)
          M   = IBVEL(4,N)
          DO P = 1, NSPMD
            IF (NLOCAL(M,P).EQ.0) THEN
              IMASTER = 0
              DO J = 1, NSN
                L = LBVEL(K+J)
                IF(NLOCAL(L,P).NE.0)THEN
                  IMASTER = 1
                  GO TO 185
                ENDIF
              ENDDO
 185          CONTINUE
              IF(IMASTER.EQ.1)THEN
                CALL IFRONTPLUS(M,P)
              ENDIF
            ENDIF
          ENDDO
          K = K + NSN
        ENDDO
Cel traitement noeuds non connectes
        K=0
        DO N = 1, NIBVEL
          NSN = IBVEL(3,N)
          M   = IBVEL(4,N)
          SUM = 0
          INSNMAX = 0
          IPMAX = 1
          LASTM = 0
          DO P=1,NSPMD
            IF(NLOCAL(M,P).NE.0) THEN
              SUM = SUM + 1
              LASTM = P
            ENDIF
            INSNP = 0
            DO J = 1, NSN
              L = LBVEL(K+J)
              IF(NLOCAL(L,P).NE.0)THEN
                INSNP = INSNP + 1
              ENDIF
            ENDDO
            IF (INSNP.GT.INSNMAX) THEN
              IPMAX = P
              INSNMAX = INSNP
            ENDIF
          END DO
          IF(SUM.EQ.0) THEN
            IF(INSNMAX.EQ.0) THEN
              CALL IFRONTPLUS(M,1)
            ELSE
              CALL IFRONTPLUS(M,IPMAX)
            ENDIF
          ELSEIF(INSNMAX.EQ.0) THEN
            IPMAX = LASTM
          ENDIF
C
          DO J = 1, NSN
            L = LBVEL(K+J)
            SUM = 0
            DO P=1,NSPMD
              IF(NLOCAL(L,P).NE.0)THEN
                SUM = SUM + 1
              ENDIF
            ENDDO
            IF(SUM.EQ.0) THEN
              CALL IFRONTPLUS(L,IPMAX)
            ENDIF
          ENDDO
          K = K + NSN
        ENDDO
      ENDIF
C
C-----------------------------------------------------
C Traitement  rigid materials
C-----------------------------------------------------
      IF (IRIGID_MAT.GT.0.AND.NSPMD.GT.1) THEN
        K=0
        DO N = 1, NRBYM
          NSN = IRBYM(2,N)
          DO P = 1, NSPMD
              IMASTER = 0
              DO J = 1, NSN
                L = LCRBYM(K+J)
                IF(NLOCAL(L,P).NE.0)THEN
                  IMASTER = 1
                  GO TO 195
                ENDIF
              ENDDO
 195           CONTINUE
              IF(IMASTER.EQ.1)THEN
                CALL FRONTPLUS_RM(FRONT_RM(N,P),1)
              ENDIF
          ENDDO
          K = K + NSN
        ENDDO
C traitement cdg  non connectes
        K=0
        DO N = 1, NRBYM
          NSN = IRBYM(2,N)
          M   = IRBYM(1,N)
          SUM = 0
          INSNMAX = 0
          IPMAX = 1
          LASTM = 0
          DO P=1,NSPMD
            IF(FRONT_RM(M,P).NE.0.AND.FRONT_RM(M,P).NE.100) THEN
              SUM = SUM + 1
              LASTM = P
            ENDIF
            INSNP = 0
            DO J = 1, NSN
              L = LCRBYM(K+J)
              IF(NLOCAL(L,P).NE.0)THEN
                INSNP = INSNP + 1
              ENDIF
            ENDDO
            IF (INSNP.GT.INSNMAX) THEN
              IPMAX = P
              INSNMAX = INSNP
            ENDIF
          END DO
C
          IF(SUM.EQ.0) THEN
            IF(INSNMAX.EQ.0) THEN
              CALL FRONTPLUS_RM(FRONT_RM(M,1),1)
            ELSE
              CALL FRONTPLUS_RM(FRONT_RM(M,IPMAX),1)
            ENDIF
C cas au moins un master connecte mais aucun noeud SECONDARY de connecte
          ELSEIF(INSNMAX.EQ.0) THEN
            IPMAX = LASTM
          ENDIF
C
          DO J = 1, NSN
            L = LCRBYM(K+J)
            SUM = 0
            DO P=1,NSPMD
              IF(NLOCAL(L,P).NE.0)THEN
                SUM = SUM + 1
              ENDIF
            ENDDO
            IF(SUM.EQ.0) THEN
              CALL IFRONTPLUS(L,IPMAX)
            ENDIF
          ENDDO
          K = K + NSN
        ENDDO
      ELSEIF(IRIGID_MAT > 0) THEN
          DO N = 1, NRBYM
            CALL FRONTPLUS_RM(FRONT_RM(N,1),1)
          ENDDO
      ENDIF
C-----------------------------------------------------
C Traitement special /BSC/CYCLIC
C-----------------------------------------------------
      DO N = 1, NBCSCYC
        K = IBCSCYC(1,N)
        NSN=IBCSCYC(3,N)
        DO J = 1, NSN
          N1 = LBCSCYC(1,K+J)
          N2 = LBCSCYC(2,K+J)
          DO P = 1, NSPMD
            IF(NLOCAL(N1,P)==1 .AND. NLOCAL(N2,P)==0)CALL IFRONTPLUS(N2,P) 
            IF(NLOCAL(N2,P)==1 .AND. NLOCAL(N1,P)==0)CALL IFRONTPLUS(N1,P) 
          ENDDO
        ENDDO
      ENDDO
C-----------------------------------------------------
C Traitement Itet=2 of S10
C-----------------------------------------------------
      IF (NS10E>0.AND.NSPMD>1) THEN
        CALL C_DOMS10(ICNDS10,ITAGND,ITE2FRPLUS)
        IF (ITE2FRPLUS > 0 ) GOTO 5000
      ENDIF
C-----------------------------------------------------
C Traitement supplementaire interface type 2
C-----------------------------------------------------
      IF (NINTER.GT.0.AND.NSPMD.GT.1) THEN
        IF (N2D.EQ.0) THEN
          NIR = 4
        ELSE
          NIR = 2
        ENDIF
        DO N = 1, NINTER
          NTY = IPARI(7,N)
          IF (NTY.EQ.2) THEN
            NRTS  = IPARI(3,N)
            NRTM  = IPARI(4,N)
            NSN   = IPARI(5,N)
            NMN   = IPARI(6,N)
            ILEV  = IPARI(20,N)
            IF (ILEV == 25 .or. ILEV == 26 .or. ILEV == 27 .or. ILEV == 28) INT2FLAG=1
            DO I=1,NSN
              L = INTBUF_TAB(N)%IRTLM(I)
              K = INTBUF_TAB(N)%NSV(I)
C
              IMASTER = 0
              DO P = 1, NSPMD
                IF (NLOCAL(K,P).NE.0) THEN
                  IMASTER = 1
                ENDIF
              ENDDO
              IF (IMASTER.EQ.0) THEN
                IMASTER = 1
                DO J=1,NIR
                  KK = INTBUF_TAB(N)%IRECTM((L-1)*4+J)
                  DO P = 1, NSPMD
                    IF (NLOCAL(KK,P).NE.0) THEN
                      IMASTER = P
                      GOTO 50
                    ENDIF
                  ENDDO
                ENDDO
 50             CONTINUE
                CALL IFRONTPLUS(K,IMASTER)
                INT2FRPLUS=1

              ENDIF
              DO P = 1, NSPMD
                IF (NLOCAL(K,P).EQ.0) THEN
                  GO TO 200
                ENDIF
C pas d'optimisation possible
                DO J=1,NIR
                  KK = INTBUF_TAB(N)%IRECTM((L-1)*4+J)
                  IF (NLOCAL(KK,P).EQ.0) THEN
                    CALL IFRONTPLUS(KK,P)
                    INT2FRPLUS=1
                  ENDIF
                ENDDO
C optimisation possible
 200            CONTINUE
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDIF
      IF (INT2FRPLUS /= 0 .AND. INT2FLAG/=0)GOTO 5000
C-----------------------------------------------------
C Traitement special noeuds non encore affectes
C-----------------------------------------------------
C mise sur Pi des noeuds non affectes (round robbin)    
        IPROC = 1
        DO I = 1,NUMNOD
          SUM = 0
          IF(IFRONT%IENTRY(I)==-1) THEN
            IFRONT%IENTRY(I)=I
            IFRONT%P(1,I) = IPROC
            IFRONT%P(2,I) = 0
            IPROC = MOD(IPROC,NSPMD)+1
          ENDIF
        END DO  
C-----------------------------------------------------
C Traitement special sensor type2
C-----------------------------------------------------
      IF(NSENSOR.GT.0) THEN
       DO P = 1, NSPMD
         NSENSP(P) = 0
       END DO
       DO I=1,NSENSOR
        TYP=LSENSOR(2,I)
        ISENSP(1,I) = 0
        ISENSP(2,I) = 0
C
        IF(TYP.EQ.0)THEN
        ELSEIF(TYP.EQ.1)THEN
        ELSEIF(TYP.EQ.2)THEN
C--------------------------------
C         CAPTEUR - DEPLACEMENT
C--------------------------------
          N1 = LSENSOR(4,I)
          DO P = 1, NSPMD
            IF(NLOCAL(N1,P)==1)THEN
              ISENSP(1,I) = P
              NSENSP(P) = NSENSP(P)+1
              GOTO 500
            END IF
          END DO
 500      CONTINUE
          N2 = LSENSOR(5,I)
          DO P = 1, NSPMD
            IF(NLOCAL(N2,P)==1)THEN
              ISENSP(2,I) = P
              NSENSP(P) = NSENSP(P)+1
              GOTO 600
            END IF
          END DO
 600      CONTINUE
        ELSEIF(TYP.EQ.3)THEN
        ELSEIF(TYP.EQ.4)THEN
        ELSEIF(TYP.EQ.5)THEN
        ELSEIF(TYP.EQ.6)THEN
        ELSEIF(TYP.EQ.7)THEN
        ELSEIF(TYP.EQ.8)THEN
c
        ELSEIF(TYP.EQ.13)THEN  ! SENSOR WORK
          N1 = LSENSOR(4,I)
          DO P = 1, NSPMD
            IF (NLOCAL(N1,P)==1) THEN
              ISENSP(1,I) = P
              NSENSP(P) = NSENSP(P)+1
              EXIT
            END IF
          END DO
          N2 = LSENSOR(5,I)
          IF (N2 > 0) THEN
            DO P = 1, NSPMD
              IF (NLOCAL(N2,P)==1) THEN
                ISENSP(2,I) = P
                NSENSP(P) = NSENSP(P)+1
                EXIT
              END IF
            END DO
          ENDIF
c
        ELSEIF(TYP.EQ.14)THEN
        ELSEIF(TYP.GE.29.AND.TYP.LE.31) THEN
        ELSE
        ENDIF
       ENDDO
      END IF
C
C-----------------------------------------------------
C Traitement special accelerometres 
C-----------------------------------------------------
      IF(NACCELM.GT.0) THEN
        DO P = 1, NSPMD
          NACCP(P) = 0
        END DO
C
        DO I=1,NACCELM
          N1 = LACCELM(1,I)
          DO P = 1, NSPMD
            IF(NLOCAL(N1,P)==1)THEN
              IACCP(I) = P
              NACCP(P) = NACCP(P)+1
              EXIT
            END IF
          END DO
        END DO
      END IF
C
C-----------------------------------------------------
C Traitement special gauges
C-----------------------------------------------------
      IF(NBGAUGE.GT.0) THEN
        DO P = 1, NSPMD
          NGAUP(P) = 0
        END DO
C
        DO I=1,NBGAUGE
          N1=LGAUGE(3,I)
          IF(N1>0)THEN
            DO P = 1, NSPMD
              IF(NLOCAL(N1,P)==1)THEN
                IGAUP(I) = P
                NGAUP(P) = NGAUP(P)+1
                EXIT
              END IF
            END DO
          ELSE
            N1 = -N1 + NUMELS
            P  = CEP(N1  ) + 1
            IGAUP(I) = P
            NGAUP(P) = NGAUP(P) + 1
          ENDIF
        END DO
      END IF

      IF(NJOINT>0) CALL SPLIT_JOINT( )

C-----------------------------------------------------
C-----------------------------------------------------
C dd_iad => dd_grp : nb de groupes par sous domaine
      NGROU = 0
      DO I = 1, NSPGROUP
        DO P = 1, NSPMD
c          IF (DD_IAD(P+1,I).GT.0) THEN
c            NEL = DD_IAD(P+1,I) - DD_IAD(P,I)
c            IF (NEL.GT.0) THEN
c              NG = (NEL-1)/NVSIZ + 1
c              NGROU = NGROU + NG
c            ELSE
c              NG = 0
c            ENDIF
c            DD_IAD(P,I) = NG
c          ELSE
c            DD_IAD(P,I) = 0
c          ENDIF
C seule la verification est conservee, le remplacement de dd_iad est fait directement dans les routines xtails
          NGROU = NGROU + DD_IAD(P,I)
        ENDDO
      ENDDO
      IF (NGROU.NE.NGROUP) THEN
C        WRITE(IOUT,*)'** ERROR : DOMAIN DEC AND NGROUP DIFFER'
C        WRITE(ISTDO,*)'** ERROR : DOMAIN DEC AND NGROUP DIFFER'
C        IERR = IERR + 1
         CALL ANCMSG(MSGID=363,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=NGROU,
     .               I2=NGROUP)
      ENDIF
C
C-----------------------------------------------------
C Preparation de ADDCNE : Adresse matrice CNE
C-----------------------------------------------------
      DO N=0,NUMNOD+1
        ADSKY(N) = 0
      ENDDO
C
      DO K=2,9
        DO I=1,NUMELS
          N = IXS(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C
      IF(NUMELS10.GT.0) THEN
        DO K=1,6
          DO I=1,NUMELS10
            N = IXS10(K,I) + 1
            ADSKY(N) = ADSKY(N) + 1
          ENDDO
        ENDDO
      ENDIF
      IF(NUMELS20.GT.0)THEN
        DO K=1,12
          DO I=1,NUMELS20
            N = IXS20(K,I) + 1
            ADSKY(N) = ADSKY(N) + 1
          ENDDO
        ENDDO
      ENDIF
C
      IF(NUMELS16.GT.0)THEN
        DO K=1,8
          DO I=1,NUMELS16
            N = IXS16(K,I) + 1
            ADSKY(N) = ADSKY(N) + 1
          ENDDO
        ENDDO
      ENDIF
C
      DO K=2,5
        DO I=1,NUMELQ
          N = IXQ(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C

      DO K=2,5
        DO I=1,NUMELC
          N = IXC(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C
      DO K=2,3
        DO I=1,NUMELT
          N = IXT(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C
      DO K=2,3
        DO I=1,NUMELP
          N = IXP(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C
Cel traitement a part du 3eme noeud optionnel sauf type 12
      DO K=2,3
        DO I=1,NUMELR
          N = IXR(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
      DO I=1,NUMELR
        N = IXR(4,I) + 1
        IF(IGEO(11,IXR(1,I)).NE.12) N = 0
        ADSKY(N) = ADSKY(N) + 1
      ENDDO
C
      DO K=2,4
        DO I=1,NUMELTG
          N = IXTG(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C elem penta6
      IF(NUMELTG6.GT.0)THEN
        DO K=1,3
          DO I=1,NUMELTG6
            N = IXTG6(K,I)+1
            ADSKY(N) = ADSKY(N) + 1
          END DO
        END DO
      END IF
C
      DO K=2,5
        DO I=1,NUMELUR
          N = IXUR(K,I) + 1
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C--------------------------------------
Cel prise en compte des forces des mv
C--------------------------------------
      IF (NVOLU.GT.0) THEN
        K3 = 1 + NIMV * NVOLU + NICBAG * NVOLU * NVOLU
        K1 = 1
        DO N = 1, NVOLU
          IS = T_MONVOL(N)%EXT_SURFID
          NN_S = IGRSURF(IS)%NSEG
          DO J = 1, NN_S
            ITY=IGRSURF(IS)%ELTYP(J)
            I = IGRSURF(IS)%ELEM(J)
            IF (ITY.EQ.3) THEN
              DO K = 2,5
               NN = IXC(K,I) + 1
               ADSKY(NN) = ADSKY(NN) + 1
              ENDDO
            ELSE
              DO K=2,4
               NN = IXTG(K,I) + 1
               ADSKY(NN) = ADSKY(NN) + 1
              END DO
            ENDIF
          ENDDO
          K1 = K1 + NIMV
        ENDDO
      ENDIF
C--------------------------------------
C prise en compte des forces concentrees + pressure loads
C--------------------------------------
      IF(NCONLD.GT.0) THEN
        DO NL = 1, NCONLD
          N1=IB(1,NL)
          N2=IB(2,NL)
          N3=IB(3,NL)
          N4=IB(4,NL)
          NN = N1 + 1
          ADSKY(NN) = ADSKY(NN) + 1
          IF(N4.NE.-1)THEN
            NN = N2 + 1
            ADSKY(NN) = ADSKY(NN) + 1
            IF(N2D.EQ.0)THEN
              NN = N3 + 1
              ADSKY(NN) = ADSKY(NN) + 1
              IF(N4.NE.0) THEN
                NN = N4 + 1
                ADSKY(NN) = ADSKY(NN) + 1
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
C pseudo element BC for heat transfert
C-----------------------------------------------
      IF(NUMCONV.GT.0) THEN
        DO NL = 1, NUMCONV
          N1=IBCV(1,NL)
          N2=IBCV(2,NL)
          N3=IBCV(3,NL)
          N4=IBCV(4,NL)
          NN = N1 + 1
          ADSKY(NN) = ADSKY(NN) + 1
          IF(N4.NE.-1)THEN
            NN = N2 + 1
            ADSKY(NN) = ADSKY(NN) + 1
            IF(N2D.EQ.0)THEN
              NN = N3 + 1
              ADSKY(NN) = ADSKY(NN) + 1
              IF(N4.NE.0) THEN
                NN = N4 + 1
                ADSKY(NN) = ADSKY(NN) + 1
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
C pseudo element BR for radiative heat transfert
C-----------------------------------------------
      IF(NUMRADIA.GT.0) THEN
        DO NL = 1, NUMRADIA
          N1=IBCR(1,NL)
          N2=IBCR(2,NL)
          N3=IBCR(3,NL)
          N4=IBCR(4,NL)
          NN = N1 + 1
          ADSKY(NN) = ADSKY(NN) + 1
          NN = N2 + 1
          ADSKY(NN) = ADSKY(NN) + 1
          IF(N2D.EQ.0)THEN
            NN = N3 + 1
            ADSKY(NN) = ADSKY(NN) + 1
            IF(N4.NE.0) THEN
              NN = N4 + 1
              ADSKY(NN) = ADSKY(NN) + 1
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
C pseudo element  for imposed heat flux
C-----------------------------------------------
      IF(NFXFLUX.GT.0) THEN
        DO NL = 1, NFXFLUX
          IF(IBFFLUX(10,NL) == 1) CYCLE
          N1=IBFFLUX(1,NL)
          N2=IBFFLUX(2,NL)
          N3=IBFFLUX(3,NL)
          N4=IBFFLUX(4,NL)
          NN = N1 + 1
          ADSKY(NN) = ADSKY(NN) + 1
          IF(N4.NE.-1)THEN
            NN = N2 + 1
            ADSKY(NN) = ADSKY(NN) + 1
            IF(N2D.EQ.0)THEN
              NN = N3 + 1
              ADSKY(NN) = ADSKY(NN) + 1
              IF(N4.NE.0) THEN
                NN = N4 + 1
                ADSKY(NN) = ADSKY(NN) + 1
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C--------------------------------------
C prise en compte des load/pfluid
C--------------------------------------
      IF(NLOADP.GT.0) THEN
       DO NL = 1, NLOADP
       DO I = 1,ILOADP(1,NL)/4
         N1=LLOADP(ILOADP(4,NL)+4*(I-1))
         N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
         N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
         N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)
          NN = N1 + 1
          ADSKY(NN) = ADSKY(NN) + 1
          IF(N4.NE.-1)THEN
            NN = N2 + 1
            ADSKY(NN) = ADSKY(NN) + 1
            IF(N2D.EQ.0)THEN
              NN = N3 + 1
              ADSKY(NN) = ADSKY(NN) + 1
              IF(N4.NE.0) THEN
                NN = N4 + 1
                ADSKY(NN) = ADSKY(NN) + 1
              ENDIF             
            ENDIF
          ENDIF
        ENDDO
        ENDDO
      ENDIF
C-----------------------------------------------
C   CALCUL DES ADRESSES DU VECTEUR SKYLINE
C-----------------------------------------------
      ADSKY(1) = 1
      DO I=2,NUMNOD+1
        ADSKY(I)=ADSKY(I)+ADSKY(I-1)
      ENDDO
C
      LCNE = ADSKY(NUMNOD+1)-1
C
C-----------------------------------------------
C Remplissage de CEL : connection Element/Local
C-----------------------------------------------
      DO PROC = 1, NSPMD
        OFF = 0
        NIN = 0
        ITY_OLD = 0
        DO NG = 1, NGROUP
          NEL = IPARG(2,NG)
          P   = IPARG(32,NG)+1
          ITY = IPARG(5,NG)
          IF (ITY.NE.ITY_OLD) THEN
            NIN = 0
            ITY_OLD = ITY
          ENDIF
Cel SPH non pris en comp
          IF(ITY.NE.51) THEN
           IF (P.EQ.PROC) THEN
            DO I = 1, NEL
              CEL(I+OFF) = NIN+I
            ENDDO
            NIN = NIN + NEL
           ENDIF
           OFF = OFF + NEL
          ENDIF
        ENDDO
      ENDDO

c tableau IELS initialise a 0 (pour traitement SPH)
      DO PROC = 1,NSPMD
        IELS(PROC) = 0
      ENDDO

c on remplit le tableau CELSPH pour traitement SPH
      DO J = 1, NUMSPH
       P_SPH = CEPSP(J) + 1
       IELS(P_SPH) = IELS(P_SPH) + 1
       CELSPH(J) = IELS(P_SPH)
      ENDDO
C-----------------------------------------------
C Ajout pseudo element BCL
C-----------------------------------------------
      IF(NCONLD.GT.0) THEN
        DO NL = 1, NCONLD
          CEL(OFF+NL) = 0
        ENDDO
C
        DO PROC = 1, NSPMD
          NL_L = 0
          DO NL = 1, NCONLD
            IF(CEL(OFF+NL).EQ.0) THEN
              N1=IB(1,NL)
              N2=IB(2,NL)
              N3=IB(3,NL)
              N4=IB(4,NL)
              IF(N4.NE.-1)THEN
                IF(N2D.EQ.0)THEN
                  IF(N4.NE.0) THEN
                    IF(NLOCAL(N1,PROC)==1.AND.
     +                 NLOCAL(N2,PROC)==1.AND.
     +                 NLOCAL(N3,PROC)==1.AND.
     +                 NLOCAL(N4,PROC)==1)THEN
                      NL_L = NL_L + 1
                      CEL(NL+OFF) = NL_L
                    ENDIF
                  ELSE
                    IF(NLOCAL(N1,PROC)==1.AND.
     +                 NLOCAL(N2,PROC)==1.AND.
     +                 NLOCAL(N3,PROC)==1)THEN
                      NL_L = NL_L + 1
                      CEL(NL+OFF) = NL_L
                    ENDIF
                  ENDIF
                ELSE
                  IF(NLOCAL(N1,PROC)==1.AND.
     +               NLOCAL(N2,PROC)==1)THEN
                    NL_L = NL_L + 1
                    CEL(NL+OFF) = NL_L
                  ENDIF
                ENDIF
              ELSE
                IF(NLOCAL(N1,PROC)==1) THEN
                  NL_L = NL_L + 1
                  CEL(NL+OFF) = NL_L
                ENDIF
              ENDIF
            ENDIF
          ENDDO
C
        ENDDO
        OFF = OFF + NCONLD
      ENDIF
C-----------------------------------------------
C Ajout pseudo element bc for heat tranfert
C-----------------------------------------------
      IF(NUMCONV.GT.0) THEN
        DO NL = 1, NUMCONV
          CEL(OFF+NL) = 0
        ENDDO
C
        DO PROC = 1, NSPMD
          NL_L = 0
          DO NL = 1, NUMCONV
            IF(CEL(OFF+NL).EQ.0) THEN
              N1=IBCV(1,NL)
              N2=IBCV(2,NL)
              N3=IBCV(3,NL)
              N4=IBCV(4,NL)
              IF(IBCV(7,NL) == 1) THEN
               IF(PROC-1 == CEP(IBCV(8,NL))) THEN 
                NL_L = NL_L + 1
                CEL(NL+OFF) = NL_L               
               ENDIF
              ELSE
                IF(N2D.EQ.0)THEN
                  IF(N4.NE.0) THEN
                    IF(NLOCAL(N1,PROC)==1.AND.
     +                 NLOCAL(N2,PROC)==1.AND.
     +                 NLOCAL(N3,PROC)==1.AND.
     +                 NLOCAL(N4,PROC)==1)THEN
                      NL_L = NL_L + 1
                      CEL(NL+OFF) = NL_L
                    ENDIF
                  ELSE
                    IF(NLOCAL(N1,PROC)==1.AND.
     +                 NLOCAL(N2,PROC)==1.AND.
     +                 NLOCAL(N3,PROC)==1)THEN
                      NL_L = NL_L + 1
                      CEL(NL+OFF) = NL_L
                    ENDIF
                  ENDIF
                ELSE
                  IF(NLOCAL(N1,PROC)==1.AND.
     +               NLOCAL(N2,PROC)==1)THEN
                    NL_L = NL_L + 1
                    CEL(NL+OFF) = NL_L
                  ENDIF
                ENDIF
            ENDIF
            ENDIF
          ENDDO
        ENDDO
        OFF = OFF + NUMCONV
      ENDIF
C-----------------------------------------------
C Ajout pseudo element br for radiative heat tranfert
C-----------------------------------------------
      IF(NUMRADIA.GT.0) THEN
        DO NL = 1, NUMRADIA
          CEL(OFF+NL) = 0
        ENDDO
C
        DO PROC = 1, NSPMD
          NL_L = 0
          DO NL = 1, NUMRADIA
            IF(CEL(OFF+NL).EQ.0) THEN
              N1=IBCR(1,NL)
              N2=IBCR(2,NL)
              N3=IBCR(3,NL)
              N4=IBCR(4,NL)
              IF(IBCR(7,NL) == 1) THEN
               IF(PROC-1== CEP(IBCR(8,NL))) THEN
                 NL_L = NL_L + 1
                 CEL(NL+OFF) = NL_L
                ENDIF
              ELSE
                IF(N2D.EQ.0)THEN
                  IF(N4.NE.0) THEN
                    IF(NLOCAL(N1,PROC)==1.AND.
     +                 NLOCAL(N2,PROC)==1.AND.
     +                 NLOCAL(N3,PROC)==1.AND.
     +                 NLOCAL(N4,PROC)==1)THEN
                      NL_L = NL_L + 1
                      CEL(NL+OFF) = NL_L
                    ENDIF
                  ELSE
                    IF(NLOCAL(N1,PROC)==1.AND.
     +                 NLOCAL(N2,PROC)==1.AND.
     +                 NLOCAL(N3,PROC)==1)THEN
                      NL_L = NL_L + 1
                      CEL(NL+OFF) = NL_L
                    ENDIF
                  ENDIF
                ELSE
                  IF(NLOCAL(N1,PROC)==1.AND.
     +               NLOCAL(N2,PROC)==1)THEN
                    NL_L = NL_L + 1
                    CEL(NL+OFF) = NL_L
                  ENDIF
                ENDIF
            ENDIF
            ENDIF
          ENDDO
        ENDDO
        OFF = OFF + NUMRADIA
      ENDIF 
C-----------------------------------------------
C Ajout pseudo element for imposed heat flux
C-----------------------------------------------
      IF(NFXFLUX.GT.0) THEN
        DO NL = 1, NFXFLUX
           CEL(OFF+NL) = 0
        ENDDO
C
        DO PROC = 1, NSPMD
          NL_L = 0
          DO NL = 1, NFXFLUX
            IF(IBFFLUX(10,NL) == 1) CYCLE
            IF(CEL(OFF+NL).EQ.0) THEN
              N1=IBFFLUX(1,NL)
              N2=IBFFLUX(2,NL)
              N3=IBFFLUX(3,NL)
              N4=IBFFLUX(4,NL)
              IF(N2D.EQ.0)THEN
                IF(N4.NE.0) THEN
                  IF(NLOCAL(N1,PROC)==1.AND.
     +               NLOCAL(N2,PROC)==1.AND.
     +               NLOCAL(N3,PROC)==1.AND.
     +               NLOCAL(N4,PROC)==1)THEN
                     NL_L = NL_L + 1
                     CEL(NL+OFF) = NL_L
                  ENDIF
                ELSE
                  IF(NLOCAL(N1,PROC)==1.AND.
     +               NLOCAL(N2,PROC)==1.AND.
     +               NLOCAL(N3,PROC)==1)THEN
                     NL_L = NL_L + 1
                     CEL(NL+OFF) = NL_L
                  ENDIF
                ENDIF
              ELSE
                IF(NLOCAL(N1,PROC)==1.AND.NLOCAL(N2,PROC)==1)THEN
                   NL_L = NL_L + 1
                   CEL(NL+OFF) = NL_L
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        OFF = OFF + NFXFLUX
      ENDIF
C-----------------------------------------------
C Ajout pseudo element pfluid 
C-----------------------------------------------
      IF(NLOADP.GT.0) THEN
        NUMLOADP=0
        DO NL = 1, NLOADP
          DO I = 1,ILOADP(1,NL)/4
            CEL(OFF+NUMLOADP+I) = 0
          ENDDO
          NUMLOADP=NUMLOADP+ILOADP(1,NL)/4
        ENDDO
C
        DO PROC = 1, NSPMD
          NL_L = 0
          NUMLOADP=0
          DO NL = 1, NLOADP
           DO I = 1,ILOADP(1,NL)/4
            IF(CEL(OFF+NUMLOADP+I).EQ.0) THEN
              N1=LLOADP(ILOADP(4,NL)+4*(I-1))
              N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
              N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
              N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)
              IF(N4.NE.-1)THEN
                IF(N2D.EQ.0)THEN
                  IF(N4.NE.0) THEN
                    IF(NLOCAL(N1,PROC)==1.AND.
     +                 NLOCAL(N2,PROC)==1.AND.
     +                 NLOCAL(N3,PROC)==1.AND.
     +                 NLOCAL(N4,PROC)==1)THEN
                      NL_L = NL_L + 1          
                      CEL(OFF+NUMLOADP+I) = NL_L
                    ENDIF
                  ELSE
                    IF(NLOCAL(N1,PROC)==1.AND.
     +                 NLOCAL(N2,PROC)==1.AND.
     +                 NLOCAL(N3,PROC)==1)THEN 
                      NL_L = NL_L + 1          
                      CEL(OFF+NUMLOADP+I) = NL_L
                    ENDIF
                  ENDIF
                ELSE
                  IF(NLOCAL(N1,PROC)==1.AND.
     +               NLOCAL(N2,PROC)==1)THEN      
                    NL_L = NL_L + 1          
                    CEL(OFF+NUMLOADP+I) = NL_L
                  ENDIF
                ENDIF
              ELSE
                IF(NLOCAL(N1,PROC)==1) THEN
                  NL_L = NL_L + 1          
                  CEL(OFF+NUMLOADP+I) = NL_L
                ENDIF
              ENDIF
            ENDIF
           ENDDO
           NUMLOADP=NUMLOADP+ILOADP(1,NL)/4
          ENDDO
        ENDDO
        OFF = OFF + NUMLOADP
      ENDIF     
C
C Traitement assemblage // int2
C
      IF(I2NSNT.GT.0) THEN
C
C-----------------------------------------------------
C Preparation de ADDCNI2 : Adresse matrice CNI2 (connectivite interface type 2)
C-----------------------------------------------------
        DO N=0,NUMNOD+1
          ADSKYI2(N) = 0
        ENDDO
C
        IF (N2D.EQ.0) THEN
          NIR = 4
        ELSE
          NIR = 2
        ENDIF
        DO N = 1, NINTER
          NTY = IPARI(7,N)
          IF (NTY.EQ.2) THEN
            NRTS  = IPARI(3,N)
            NRTM  = IPARI(4,N)
            NSN   = IPARI(5,N)
            NMN   = IPARI(6,N)
            DO I=1,NSN
              L = INTBUF_TAB(N)%IRTLM(I)
              K = INTBUF_TAB(N)%NSV(I)
              DO J=1,NIR
                KK = INTBUF_TAB(N)%IRECTM((L-1)*4+J) + 1
                ADSKYI2(KK) = ADSKYI2(KK) + 1
              END DO
            END DO
          END IF
        END DO
C-----------------------------------------------
C   CALCUL DES ADRESSES DU VECTEUR SKYLINE
C-----------------------------------------------
        ADSKYI2(1) = 1
        DO I=2,NUMNOD+1
          ADSKYI2(I)=ADSKYI2(I)+ADSKYI2(I-1)
        ENDDO
        LCNI2 = ADSKYI2(NUMNOD+1)-1
C-----------------------------------------------
C Remplissage de CEPI2 : connection Element/Local
C-----------------------------------------------
        OFF = 0
        DO N = 1, NINTER
          NTY = IPARI(7,N)
          IF (NTY.EQ.2) THEN
            NRTS  = IPARI(3,N)
            NRTM  = IPARI(4,N)
            NSN   = IPARI(5,N)
            NMN   = IPARI(6,N)
            DO I=1,NSN
              L = INTBUF_TAB(N)%IRTLM(I)
              K = INTBUF_TAB(N)%NSV(I)
              CELI2(OFF+I) = 0
              DO P = 1, NSPMD
                IF(NLOCAL(K,P)==1)THEN
                  CEPI2(OFF+I) = P-1
                  GO TO 102
                ENDIF
              ENDDO
 102          CONTINUE
            ENDDO
            OFF = OFF + NSN
          END IF
        END DO
C-----------------------------------------------
C Remplissage de CEL : connection Element/Local
C-----------------------------------------------
        DO P = 1, NSPMD
          OFF = 0
          NL_L = 0
          DO N = 1, NINTER
            NTY = IPARI(7,N)
            IF (NTY.EQ.2) THEN
              NRTS  = IPARI(3,N)
              NRTM  = IPARI(4,N)
              NSN   = IPARI(5,N)
              NMN   = IPARI(6,N)
              DO I=1,NSN
                L = INTBUF_TAB(N)%IRTLM(I)
                K = INTBUF_TAB(N)%NSV(I)
                IF(CELI2(OFF+I).EQ.0) THEN
                  IF(NLOCAL(K,P)==1)THEN
                    NL_L = NL_L + 1
                    CELI2(OFF+I) = NL_L
                  END IF
                END IF
              END DO
              OFF = OFF + NSN
            END IF
          END DO
        END DO
      END IF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  FILLCNE                       source/spmd/domdec2.F         
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE FILLCNE(
     1   CNE    ,LCNE   ,IXS  ,IXS10  ,IXS20  ,
     2   IXS16  ,IXQ    ,IXC  ,IXT    ,IXP    ,
     3   IXR    ,IXUR   ,IXTG ,IXTG6  ,T_MONVOL ,
     4   IGRSURF,IB   ,ADDCNE ,CEP    ,
     5   ILEN   ,GEO    ,IBCV ,IBCR   ,IBFFLUX,
     6   ILOADP ,LLOADP ,CEL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------    
      USE GROUPDEF_MOD
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr17_c.inc"
#include      "thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),CEP(*),
     .        IXS10(6,*),IXS20(12,*),IXS16(8,*),IXTG6(4,*),
     .        IXUR(NIXUR,*),IB(NIBCLD,*),
     .        ADDCNE(0:*), CNE(*), LCNE, ILEN,
     .        IBCV(NICONV,*), IBCR(NIRADIA,*),IBFFLUX(NITFLUX,*),
     .        ILOADP(SIZLOADP,*),LLOADP(*)
      INTEGER CEL(*)
      my_real
     .        GEO(NPROPG,*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN) :: T_MONVOL
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N, IDEB, OFF, OFFC, OFFTG, K1, K3, NL, NUMLOADP,
     .        N1, N2, N3, N4, NN, P, NL_L, IS, NN_S, IAD, ITY,
     .        ADSKY(0:NUMNOD+1), ITRI(ILEN), INDEX(2*ILEN),
     .        WORK(70000)
c     .        ADSKY(0:1000000), ITRI(1000000), INDEX(2000000),
      INTEGER :: NELEM
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C-----------------------------------------------
C   CALCUL DE CNE ADDCNE
C-----------------------------------------------
      DO I = 0, NUMNOD+1
        ADSKY(I) = ADDCNE(I)
      ENDDO
C
C tri des elements locaux suivants num user
C
      DO I = 1, NUMELS
        ITRI(I) = IXS(11,I)
      ENDDO
C rajout condition type element solide
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELS8,1)
      IDEB = NUMELS8+1
      IF(NUMELS10.GT.0)
     .  CALL MY_ORDERS(0,WORK,ITRI(IDEB),INDEX(IDEB),NUMELS10,1)
C
      DO J=1, NUMELS10
        INDEX(IDEB+J-1) = INDEX(IDEB+J-1)+NUMELS8
      ENDDO
C
      IDEB = IDEB + NUMELS10
      IF(NUMELS20.GT.0)
     .  CALL MY_ORDERS(0,WORK,ITRI(IDEB),INDEX(IDEB),NUMELS20,1)
C
      DO J=1, NUMELS20
        INDEX(IDEB+J-1) = INDEX(IDEB+J-1)+NUMELS8+NUMELS10
      ENDDO
C
      IDEB = IDEB + NUMELS20
      IF(NUMELS16.GT.0)
     .  CALL MY_ORDERS(0,WORK,ITRI(IDEB),INDEX(IDEB),NUMELS16,1)
C
      DO J=1, NUMELS16
        INDEX(IDEB+J-1) = INDEX(IDEB+J-1)+NUMELS8+NUMELS10+NUMELS20
      ENDDO
C
      DO J=1,NUMELS
        I = INDEX(J)
        DO K=1,8
          N = IXS(K+1,I)
            IF(N.NE.0) THEN
          CNE(ADSKY(N)) = I
          ADSKY(N) = ADSKY(N) + 1
            ENDIF
        ENDDO
      ENDDO
C
      IF(NUMELS10.GT.0) THEN
        DO J=1,NUMELS10
          I = INDEX(NUMELS8+J)
          DO K=1,6
            N = IXS10(K,I-NUMELS8)
            IF(N.NE.0) THEN
              CNE(ADSKY(N)) = I
              ADSKY(N) = ADSKY(N) + 1
            ENDIF
          ENDDO
        ENDDO
      ENDIF
      IF(NUMELS20.GT.0)THEN
        DO J=1,NUMELS20
          I = INDEX(NUMELS8+NUMELS10+J)
          DO K=1,12
            N = IXS20(K,I-NUMELS8-NUMELS10)
            IF(N.NE.0) THEN
              CNE(ADSKY(N)) = I
              ADSKY(N) = ADSKY(N) + 1
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
      IF(NUMELS16.GT.0)THEN
        DO J=1,NUMELS16
          I = INDEX(NUMELS8+NUMELS10+NUMELS20+J)
          DO K=1,8
            N = IXS16(K,I-NUMELS8-NUMELS10-NUMELS20)
            IF(N.NE.0) THEN
              CNE(ADSKY(N)) = I
              ADSKY(N) = ADSKY(N) + 1
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
      OFF = NUMELS
C
      DO I = 1, NUMELQ
        ITRI(I) = IXQ(7,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELQ,1)
      DO J=1,NUMELQ
        I = INDEX(J)
        DO K=1,4
          N = IXQ(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
      OFF = OFF + NUMELQ
C
C tri des elements locaux suivants num user
C
      DO I = 1, NUMELC
        ITRI(I) = IXC(7,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELC,1)
      DO J=1,NUMELC
        I = INDEX(J)
        DO K=1,4
          N = IXC(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
      OFFC = OFF
      OFF = OFF + NUMELC
C
      DO I = 1, NUMELT
        ITRI(I) = IXT(5,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELT,1)
      DO J=1,NUMELT
        I = INDEX(J)
        DO K=1,2
          N = IXT(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
      OFF = OFF + NUMELT
C
      DO I = 1, NUMELP
        ITRI(I) = IXP(6,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELP,1)
      DO J=1,NUMELP
        I = INDEX(J)
        DO K=1,2
          N = IXP(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
      OFF = OFF + NUMELP
C
      DO I = 1, NUMELR
        ITRI(I) = IXR(6,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELR,1)
      DO J=1,NUMELR
        I = INDEX(J)
        DO K=1,2
          N = IXR(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
        IF(NINT(GEO(12,IXR(1,I))).EQ.12) THEN
          N = IXR(4,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDIF
      ENDDO
      OFF = OFF + NUMELR
C
      DO I = 1, NUMELTG
        ITRI(I) = IXTG(6,I)
      ENDDO
C rajout condition type element triangle
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELTG-NUMELTG6,1)
      IDEB = NUMELTG-NUMELTG6+1     
      IF (NUMELTG6.NE.0)
     .  CALL MY_ORDERS(0,WORK,ITRI(IDEB),INDEX(IDEB),NUMELTG6,1)
      DO J=1, NUMELTG6
        INDEX(IDEB+J-1) = INDEX(IDEB+J-1)+NUMELTG-NUMELTG6
      ENDDO
C
      DO J=1,NUMELTG
        I = INDEX(J)
        DO K=1,3
          N = IXTG(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
C
      IF(NUMELTG6.GT.0)THEN
        DO J=1,NUMELTG6
          I = INDEX(NUMELTG-NUMELTG6+J)
          DO K=1,3
            N = IXTG6(K,I-NUMELTG+NUMELTG6)
            IF(N.NE.0) THEN
              CNE(ADSKY(N)) = I
              ADSKY(N) = ADSKY(N) + 1
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
      OFFTG = OFF
      OFF = OFF + NUMELTG
C
      DO I = 1, NUMELUR
        ITRI(I) = IXUR(6,I)
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELUR,1)
      DO J=1,NUMELUR
        I = INDEX(J)
        DO K=1,4
          N = IXUR(K+1,I)
          CNE(ADSKY(N)) = I+OFF
          ADSKY(N) = ADSKY(N) + 1
        ENDDO
      ENDDO
      OFF = OFF + NUMELUR
      OFF = OFF + NUMELX
C
C---------------------------------------------
Cel mv
      IF (NVOLU.GT.0) THEN
        K1 = 1
        DO N = 1, NVOLU
          IS  = T_MONVOL(N)%EXT_SURFID
          NN_S= IGRSURF(IS)%NSEG
          DO J = 1, NN_S
            ITY= IGRSURF(IS)%ELTYP(J)
            I  = IGRSURF(IS)%ELEM(J)
            IF (ITY.EQ.3) THEN
              DO K = 2,5
               NN = IXC(K,I)
               CNE(ADSKY(NN)) = I+OFFC
               ADSKY(NN) = ADSKY(NN) + 1
              ENDDO
            ELSE
              DO K=2,4
               NN = IXTG(K,I)
               CNE(ADSKY(NN)) = I+OFFTG
               ADSKY(NN) = ADSKY(NN) + 1
              END DO
            ENDIF
          ENDDO
          K1 = K1 + NIMV
        ENDDO
      ENDIF
C-----------------------------------------------
C pseudo element BCL
C-----------------------------------------------
      IF(NCONLD.GT.0) THEN
        DO NL = 1, NCONLD
          N1=IB(1,NL)
          N2=IB(2,NL)
          N3=IB(3,NL)
          N4=IB(4,NL)
          NN = N1
          CNE(ADSKY(NN)) = NL+OFF
          ADSKY(NN) = ADSKY(NN) + 1
          IF(N4.NE.-1)THEN
            NN = N2
            CNE(ADSKY(NN)) = NL+OFF
            ADSKY(NN) = ADSKY(NN) + 1
            IF(N2D.EQ.0)THEN
              NN = N3
              CNE(ADSKY(NN)) = NL+OFF
              ADSKY(NN) = ADSKY(NN) + 1
              IF(N4.NE.0) THEN
                NN = N4
                CNE(ADSKY(NN)) = NL+OFF
                ADSKY(NN) = ADSKY(NN) + 1
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
C pseudo element BCL : affectation a un proc
C-----------------------------------------------
      IF(NCONLD.GT.0) THEN
        DO NL = 1, NCONLD
          N1=IB(1,NL)
          N2=IB(2,NL)
          N3=IB(3,NL)
          N4=IB(4,NL)
          IF(N4.NE.-1)THEN
            IF(N2D.EQ.0)THEN
              IF(N4.NE.0) THEN
                DO P = 1, NSPMD
                  IF(NLOCAL(N1,P)==1.AND.
     +               NLOCAL(N2,P)==1.AND.
     +               NLOCAL(N3,P)==1.AND.
     +               NLOCAL(N4,P)==1)THEN
                    CEP(NL+OFF) = P-1
                    GOTO 9
                  ENDIF
                ENDDO
 9              CONTINUE
              ELSE
                DO P = 1, NSPMD
                  IF(NLOCAL(N1,P)==1.AND.
     +               NLOCAL(N2,P)==1.AND.
     +               NLOCAL(N3,P)==1)THEN
                    CEP(NL+OFF) = P-1
                    GOTO 99
                  ENDIF
                ENDDO
 99          CONTINUE
              ENDIF
            ELSE
              DO P = 1, NSPMD
                IF(NLOCAL(N1,P)==1.AND.
     +             NLOCAL(N2,P)==1)THEN
                  CEP(NL+OFF) = P-1
                  GOTO 999
                ENDIF
              ENDDO
 999          CONTINUE
            ENDIF
          ELSE
            DO P = 1, NSPMD
              IF(NLOCAL(N1,P)==1) THEN
                CEP(NL+OFF) = P-1
                GOTO 9999
              ENDIF
            ENDDO
 9999        CONTINUE
          ENDIF
        ENDDO
        OFF = OFF + NCONLD
      ENDIF
C
C-----------------------------------------------
C pseudo element BC for heat transfert
C-----------------------------------------------
      IF(NUMCONV.GT.0) THEN
        DO NL = 1, NUMCONV
          N1=IBCV(1,NL)
          N2=IBCV(2,NL)
          N3=IBCV(3,NL)
          N4=IBCV(4,NL)
          NN = N1
          CNE(ADSKY(NN)) = NL+OFF
          ADSKY(NN) = ADSKY(NN) + 1
          IF(N4.NE.-1)THEN
            NN = N2
            CNE(ADSKY(NN)) = NL+OFF
            ADSKY(NN) = ADSKY(NN) + 1
            IF(N2D.EQ.0)THEN
              NN = N3
              CNE(ADSKY(NN)) = NL+OFF
              ADSKY(NN) = ADSKY(NN) + 1
              IF(N4.NE.0) THEN
                NN = N4
                CNE(ADSKY(NN)) = NL+OFF
                ADSKY(NN) = ADSKY(NN) + 1
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF

C-----------------------------------------------
C pseudo element BC for heat transfert : affectation a un proc
C-----------------------------------------------
      IF(NUMCONV.GT.0) THEN
        DO NL = 1, NUMCONV
          N1=IBCV(1,NL)
          N2=IBCV(2,NL)
          N3=IBCV(3,NL)
          N4=IBCV(4,NL)
          IF(IBCV(7,NL) == 1) THEN
            P = CEP(IBCV(8,NL))
            CEP(NL+OFF) = P
          ELSE
          IF(N2D.EQ.0)THEN
            IF(N4.NE.0) THEN
              DO P = 1, NSPMD
                IF(NLOCAL(N1,P)==1.AND.
     +             NLOCAL(N2,P)==1.AND.
     +             NLOCAL(N3,P)==1.AND.
     +             NLOCAL(N4,P)==1)THEN
                  CEP(NL+OFF) = P-1
                  GOTO 1
                ENDIF
              ENDDO
 1            CONTINUE
            ELSE
              DO P = 1, NSPMD
                IF(NLOCAL(N1,P)==1.AND.
     +             NLOCAL(N2,P)==1.AND.
     +             NLOCAL(N3,P)==1)THEN
                  CEP(NL+OFF) = P-1
                  GOTO 11
                ENDIF
              ENDDO
 11        CONTINUE
            ENDIF
          ELSE
            DO P = 1, NSPMD
              IF(NLOCAL(N1,P)==1.AND.
     +           NLOCAL(N2,P)==1)THEN
                CEP(NL+OFF) = P-1
                GOTO 111
              ENDIF
            ENDDO
 111       CONTINUE
          ENDIF
          ENDIF
        ENDDO
        OFF = OFF + NUMCONV
      ENDIF
C
C-----------------------------------------------
C pseudo element BC for radiative heat transfert
C-----------------------------------------------
      IF(NUMRADIA.GT.0) THEN
        DO NL = 1, NUMRADIA
          N1=IBCR(1,NL)
          N2=IBCR(2,NL)
          N3=IBCR(3,NL)
          N4=IBCR(4,NL)
          NN = N1
          CNE(ADSKY(NN)) = NL+OFF
          ADSKY(NN) = ADSKY(NN) + 1
          NN = N2
          CNE(ADSKY(NN)) = NL+OFF
          ADSKY(NN) = ADSKY(NN) + 1
          IF(N2D.EQ.0)THEN
            NN = N3
            CNE(ADSKY(NN)) = NL+OFF
            ADSKY(NN) = ADSKY(NN) + 1
            IF(N4.NE.0) THEN
              NN = N4
              CNE(ADSKY(NN)) = NL+OFF
              ADSKY(NN) = ADSKY(NN) + 1
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
C pseudo element BC for heat transfert : affectation a un proc
C-----------------------------------------------
      IF(NUMRADIA.GT.0) THEN
        DO NL = 1, NUMRADIA
          N1=IBCR(1,NL)
          N2=IBCR(2,NL)
          N3=IBCR(3,NL)
          N4=IBCR(4,NL)
          IF(IBCR(7,NL) == 1) THEN
             P = CEP(IBCR(8,NL)) 
             CEP(NL+OFF) = P
          ELSE
          IF(N2D.EQ.0)THEN
            IF(N4.NE.0) THEN
              DO P = 1, NSPMD
                IF(NLOCAL(N1,P)==1.AND.
     +             NLOCAL(N2,P)==1.AND.
     +             NLOCAL(N3,P)==1.AND.
     +             NLOCAL(N4,P)==1)THEN
                  CEP(NL+OFF) = P-1
                  GOTO 2
                ENDIF
              ENDDO
 2            CONTINUE
            ELSE
              DO P = 1, NSPMD
                IF(NLOCAL(N1,P)==1.AND.
     +             NLOCAL(N2,P)==1.AND.
     +             NLOCAL(N3,P)==1)THEN
                  CEP(NL+OFF) = P-1
                  GOTO 22
                ENDIF
              ENDDO
 22        CONTINUE
            ENDIF
          ELSE
            DO P = 1, NSPMD
              IF(NLOCAL(N1,P)==1.AND.
     +           NLOCAL(N2,P)==1)THEN
                CEP(NL+OFF) = P-1
                GOTO 222
              ENDIF
            ENDDO
 222       CONTINUE
          ENDIF
         ENDIF
        ENDDO
        OFF = OFF + NUMRADIA
      ENDIF
C-----------------------------------------------
C pseudo element BC for imposed heat flux
C-----------------------------------------------
      IF(NFXFLUX.GT.0) THEN
        DO NL = 1, NFXFLUX
          IF(IBFFLUX(10,NL) == 1) CYCLE
          N1=IBFFLUX(1,NL)
          N2=IBFFLUX(2,NL)
          N3=IBFFLUX(3,NL)
          N4=IBFFLUX(4,NL)
          NN = N1
          CNE(ADSKY(NN)) = NL+OFF
          ADSKY(NN) = ADSKY(NN) + 1
          IF(N4.NE.-1)THEN
            NN = N2
            CNE(ADSKY(NN)) = NL+OFF
            ADSKY(NN) = ADSKY(NN) + 1
            IF(N2D.EQ.0)THEN
              NN = N3
              CNE(ADSKY(NN)) = NL+OFF
              ADSKY(NN) = ADSKY(NN) + 1
              IF(N4.NE.0) THEN
                NN = N4
                CNE(ADSKY(NN)) = NL+OFF
                ADSKY(NN) = ADSKY(NN) + 1
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF

C-----------------------------------------------
C pseudo element BC for heat transfert : affectation a un proc
C-----------------------------------------------
      IF(NFXFLUX.GT.0) THEN
        DO NL = 1, NFXFLUX
         IF(IBFFLUX(10,NL) == 0) THEN
C SURFACIC FLUX
          N1=IBFFLUX(1,NL)
          N2=IBFFLUX(2,NL)
          N3=IBFFLUX(3,NL)
          N4=IBFFLUX(4,NL)
          IF(N2D.EQ.0)THEN
            IF(N4.NE.0) THEN
              DO P = 1, NSPMD
                IF(NLOCAL(N1,P)==1.AND.
     +             NLOCAL(N2,P)==1.AND.
     +             NLOCAL(N3,P)==1.AND.
     +             NLOCAL(N4,P)==1)THEN
                  CEP(NL+OFF) = P-1
                  GOTO 3
                ENDIF
              ENDDO
 3            CONTINUE
            ELSE
              DO P = 1, NSPMD
                IF(NLOCAL(N1,P)==1.AND.
     +             NLOCAL(N2,P)==1.AND.
     +             NLOCAL(N3,P)==1)THEN
                  CEP(NL+OFF) = P-1
                  GOTO 33
                ENDIF
              ENDDO
 33        CONTINUE
            ENDIF
          ELSE
            DO P = 1, NSPMD
              IF(NLOCAL(N1,P)==1.AND.NLOCAL(N2,P)==1)THEN
                CEP(NL+OFF) = P-1
                GOTO 333
              ENDIF
            ENDDO
 333       CONTINUE
          ENDIF
C VOLUMIC FLUX
         ELSEIF(IBFFLUX(10,NL) == 1) THEN
c          N1 = IBFFLUX(1,NL)
           N1 = IBFFLUX(8,NL)
           IBFFLUX(2,NL) = 0 
           IF(NSPMD > 1) THEN
             IBFFLUX(1,NL) = CEL(N1)
             IBFFLUX(2,NL) = CEP(N1)
           ENDIF
           CEP(NL+OFF) = CEP(N1)
         ENDIF
        ENDDO
        OFF = OFF + NFXFLUX
      ENDIF
C-----------------------------------------------
C pseudo element BCL
C-----------------------------------------------
      IF(NLOADP.GT.0) THEN
        NUMLOADP=0
        DO NL = 1, NLOADP
         DO I = 1,ILOADP(1,NL)/4
          N1=LLOADP(ILOADP(4,NL)+4*(I-1))
          N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
          N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
          N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)
          NN = N1
          CNE(ADSKY(NN)) = OFF+NUMLOADP+I
          ADSKY(NN) = ADSKY(NN) + 1
          IF(N4.NE.-1)THEN
            NN = N2
            CNE(ADSKY(NN)) = OFF+NUMLOADP+I
            ADSKY(NN) = ADSKY(NN) + 1
            IF(N2D.EQ.0)THEN
              NN = N3
              CNE(ADSKY(NN)) = OFF+NUMLOADP+I
              ADSKY(NN) = ADSKY(NN) + 1
              IF(N4.NE.0) THEN
                NN = N4
                CNE(ADSKY(NN)) = OFF+NUMLOADP+I
                ADSKY(NN) = ADSKY(NN) + 1
              ENDIF             
            ENDIF
          ENDIF
         ENDDO
         NUMLOADP=NUMLOADP+ILOADP(1,NL)/4
        ENDDO
      ENDIF
C-----------------------------------------------
C pseudo element LLOADP : affectation a un proc 
C-----------------------------------------------
      IF(NLOADP.GT.0) THEN
        DO NL = 1, NLOADP
         DO I = 1,ILOADP(1,NL)/4
          N1=LLOADP(ILOADP(4,NL)+4*(I-1))
          N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
          N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
          N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)
          IF(N4.NE.-1)THEN
            IF(N2D.EQ.0)THEN
              IF(N4.NE.0) THEN
                DO P = 1, NSPMD
                  IF(NLOCAL(N1,P)==1.AND.
     +               NLOCAL(N2,P)==1.AND.
     +               NLOCAL(N3,P)==1.AND.
     +               NLOCAL(N4,P)==1)THEN           
                    CEP(I+OFF) = P-1
                    GOTO 4
                  ENDIF
                ENDDO
 4              CONTINUE
              ELSE
                DO P = 1, NSPMD
                  IF(NLOCAL(N1,P)==1.AND.
     +               NLOCAL(N2,P)==1.AND.
     +               NLOCAL(N3,P)==1)THEN           
                    CEP(I+OFF) = P-1
                    GOTO 44
                  ENDIF
                ENDDO
 44          CONTINUE
              ENDIF
            ELSE
              DO P = 1, NSPMD
                IF(NLOCAL(N1,P)==1.AND.
     +             NLOCAL(N2,P)==1)THEN           
                  CEP(I+OFF) = P-1
                  GOTO 444
                ENDIF
              ENDDO
 444          CONTINUE
            ENDIF
          ELSE
            DO P = 1, NSPMD
              IF(NLOCAL(N1,P)==1) THEN
                CEP(I+OFF) = P-1
                GOTO 4444
              ENDIF
            ENDDO
 4444        CONTINUE
          ENDIF
         ENDDO
         OFF = OFF + ILOADP(1,NL)/4
        ENDDO
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  FILLCNI2                      source/spmd/domdec2.F         
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE FILLCNI2(
     1   CNI2  ,LCNI2 ,ADDCNI2, IPARI, INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ADDCNI2(0:*), CNI2(*), 
     .        LCNI2, IPARI(NPARI,NINTER)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, K, N, OFF, NTY, NRTS, NRTM, NSN, NMN,
     .        KK, NIR,
     .        ADSKYI2(0:NUMNOD+1)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C-----------------------------------------------
C   CALCUL DE CNE ADDCNE
C-----------------------------------------------
      DO I = 0, NUMNOD+1
        ADSKYI2(I) = ADDCNI2(I)
      ENDDO
C
C ordre => ordre des elements dans l'interface type 2
C
C
      OFF = 0
      IF (N2D.EQ.0) THEN
        NIR = 4
      ELSE
        NIR = 2
      ENDIF
      DO N = 1, NINTER
        NTY = IPARI(7,N)
        IF (NTY.EQ.2) THEN
          NRTS  = IPARI(3,N)
          NRTM  = IPARI(4,N)
          NSN   = IPARI(5,N)
          NMN   = IPARI(6,N)
          DO I=1,NSN
            L = INTBUF_TAB(N)%IRTLM(I)
            K = INTBUF_TAB(N)%NSV(I)
            DO J=1,NIR
              KK = INTBUF_TAB(N)%IRECTM((L-1)*4+J)
              CNI2(ADSKYI2(KK)) = OFF+I
              ADSKYI2(KK) = ADSKYI2(KK) + 1
            END DO
          END DO
          OFF = OFF + NSN
        END IF
      END DO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  DDPRINT                       source/spmd/domdec2.F         
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE  DDPRINT(DDSTAT, DSMEMORY, MEMFLOW)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "commandline.inc"
#include      "units_c.inc"
#include      "rnur_c.inc"
#include      "sphcom.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER DDSTAT(50,*)
      INTEGER(KIND=8) :: DSMEMORY(7,*), MEMFLOW(2,*)
C DDSTAT
C 1 : NUMNOD Local
C 2 : NELEM Local
C 3 : NUMELS_L
C 4 : NUMELQ_L
C 5 : NUMELC_L
C 6 : NUMELP_L
C 7 : NUMELT_L
C 8 : NUMELR_L
C 9 : NUMELUR_L
C 10: NUMELTG_L
C 11: NUMELX_L
C 12: NBDDPROC :  nb de proc frontiere
C 13: NBDDBOUN :  nb de noeud frontiere
C 14: NBDDNOD :   taille des comm en nb de noeuds
C 15: NBDDNRB :   taille des comm en nb de noeuds master de rby
C 16: NRBYKIN_L : nombre de rigid body MAIN locaux
C 17: NUMSPH_L : nombre de particules SPH locales
C 18: MEMI : taille memoire locale entier MA
C 19: MEMR : taille memoire locale reel AM
C 20: NSNT_L : nombre de noeuds SECONDARYs d'interface de contact (7,10,11)
C 21: NMNT_L : nombre de noeuds MAIN d'interface de contact (7,10,11)
C 22: NSNT2_L : nombre de noeuds SECONDARYs d'interface type2
C 23: NMNT2_L : nombre de noeuds MAIN d'interface type2
C 24: RESTSIZE : Taille du restart en MB
C 24: NSLARB_L : nombre de noeuds SECONDARY rigid body 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER P, NACTIVE, J
      INTEGER (KIND=8) MEMTOTAL,RTOBYTES,ITOBYTES,MBYTE
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::AVERAGE,DEVIATION   
C-----------------------------------------------
      NACTIVE=50
C Double of float to Bytes conversion
C In double precision : one double = 8 bytes
C In single precision : one float  = 4 bytes
      ALLOCATE( AVERAGE(NACTIVE) )
      ALLOCATE( DEVIATION(NACTIVE) ) 
      MBYTE=1024*1024
      IF (IRESP==1) THEN
         RTOBYTES = 4
      ELSE
         RTOBYTES = 8
      ENDIF
      ITOBYTES = 4

C
      IF(NSPMD.GT.1) THEN
       DO J=1, NACTIVE
         AVERAGE(J)=ZERO
         DEVIATION(J)=ZERO
       END DO
       DO P=1,NSPMD
         DO J=1, NACTIVE
           AVERAGE(J) = AVERAGE(J) + DDSTAT(J,P)
         END DO
       ENDDO
       DO J=1, NACTIVE
         AVERAGE(J) = AVERAGE(J) / NSPMD
       END DO
C
       DO P=1,NSPMD
         DO J=1, NACTIVE
           DEVIATION(J) = DEVIATION(J) + (DDSTAT(J,P)-AVERAGE(J))**2
         END DO
       ENDDO
       DO J=1, NACTIVE
         DEVIATION(J) = SQRT(DEVIATION(J)/NSPMD)
       END DO
       
       WRITE(IOUT,*)
       WRITE(IOUT,*)
       WRITE(IOUT,*)'STATISTICS ON DOMAIN DECOMPOSITION '
       WRITE(IOUT,*)'---------------------------------- '
       WRITE(IOUT,*)
       WRITE(IOUT,'(A,I6)')
     .  'AVERAGE NB. OF BOUNDARY NODES :',NINT(AVERAGE(13))
           WRITE(IOUT,'(A,I6)')
     .  'STANDARD DEVIATION            :',NINT(DEVIATION(13))
       WRITE(IOUT,*)
       WRITE(IOUT,'(A,I8)')
     .  'AVERAGE NUMBER OF LOCAL NODES :',NINT(AVERAGE(1))
           WRITE(IOUT,'(A,I8)')
     .  'STANDARD DEVIATION            :',NINT(DEVIATION(1))
       WRITE(IOUT,*)
       IF(AVERAGE(20)+AVERAGE(21) >= UN)THEN
         WRITE(IOUT,'(A,I8,A3,I8)')
     .     'AVERAGE NB. OF CONTACT NODES(SECONDARY/MAIN) :',
     .     NINT(AVERAGE(20)),' / ',NINT(AVERAGE(21))
         WRITE(IOUT,'(A,I8,A3,I8)')
     .     'STANDARD DEVIATION                         :',
     .    NINT(DEVIATION(20)),' / ',NINT(DEVIATION(21))
         WRITE(IOUT,*)
       END IF
       
       IF(AVERAGE(22)+AVERAGE(23) >= UN)THEN
         WRITE(IOUT,'(A,I8,A3,I8)')
     .     'AVERAGE NB. OF INT2 NODES(SECONDARY/MAIN):',
     .     NINT(AVERAGE(22)),' / ',NINT(AVERAGE(23))
         WRITE(IOUT,'(A,I8,A3,I8)')
     .     'STANDARD DEVIATION                     :',
     .     NINT(DEVIATION(22)),' / ',NINT(DEVIATION(23))
         WRITE(IOUT,*)
       END IF
       IF(NUMSPH.GT.0.AND.AVERAGE(17) >= UN) THEN
         WRITE(IOUT,'(A,I8,A3,I8)')
     .     'AVERAGE NB. OF SPH PARTICLES  :',
     .     NINT(AVERAGE(17))
         WRITE(IOUT,'(A,I8,A3,I8)')
     .     'STANDARD DEVIATION            :',
     .     NINT(DEVIATION(17))
         WRITE(IOUT,*)
       END IF
       
       DO P=1,NSPMD
        WRITE(IOUT,*)
     .  'PROC    NB OF ELTS    NB OF BOUND. NODES    NB OF BOUND. PROCS'
        WRITE(IOUT,1000) P,DDSTAT(2,P),DDSTAT(13,P),DDSTAT(12,P)
       ENDDO
C
       DO P=1,NSPMD
        WRITE(IOUT,*)
        WRITE(IOUT,'(1X,A,I4)')
     .  'DOMAIN DECOMPOSITION SUMMARY FOR SPMD PROCESSOR',P
        WRITE(IOUT,*)
     .  '----------------------------------------------------'
        WRITE(IOUT,*)  'NUMBER OF NODES................. :',DDSTAT(1,P)
        IF(NUMELS.GT.0)
     .    WRITE(IOUT,*)'NUMBER OF SOLID ELEMENTS........ :',DDSTAT(3,P)
        IF(NUMELQ.GT.0)
     .    WRITE(IOUT,*)'NUMBER OF QUAD ELEMENTS......... :',DDSTAT(4,P)
        IF(NUMELC.GT.0)
     .    WRITE(IOUT,*)'NUMBER OF 4-N SHELL ELEMENTS.... :',DDSTAT(5,P)
        IF(NUMELP.GT.0)
     .    WRITE(IOUT,*)'NUMBER OF BEAM ELEMENTS......... :',DDSTAT(6,P)
        IF(NUMELT.GT.0)
     .    WRITE(IOUT,*)'NUMBER OF TRUSS ELEMENTS........ :',DDSTAT(7,P)
        IF(NUMELR.GT.0)
     .    WRITE(IOUT,*)'NUMBER OF SPRING ELEMENTS....... :',DDSTAT(8,P)
        IF(NUMELUR.GT.0)
     .    WRITE(IOUT,*)'NUMBER OF USER ELEMENTS......... :',DDSTAT(9,P)
        IF(NUMELTG.GT.0)
     .    WRITE(IOUT,*)'NUMBER OF 3-N SHELL ELEMENTS.... :',DDSTAT(10,P)
        IF(NUMELX.GT.0)
     .    WRITE(IOUT,*)'NUMBER OF MULTIPURPOSE ELEMENTS. :',DDSTAT(11,P)
        WRITE(IOUT,*)  'TOTAL NUMBER OF NODES FOR COMM.. :',DDSTAT(14,P)
        IF(NRBYKIN.GT.0)THEN
          WRITE(IOUT,*)'NUMBER OF RIGID BODY COMPONENTS. :',DDSTAT(16,P)
          WRITE(IOUT,*)'NUMBER OF R.B.M. NODES FOR COMM. :',DDSTAT(15,P)
          WRITE(IOUT,*)'NUMBER OF SECONDARY RIGID BODY NODES :',DDSTAT(24,P)
        ENDIF
        IF(NINTER.GT.0)THEN
          WRITE(IOUT,*)'NUMBER OF INT2 SECONDARY NODES...... :',DDSTAT(22,P)
          WRITE(IOUT,*)'NUMBER OF INT2 MAIN NODES..... :',DDSTAT(23,P)
          WRITE(IOUT,*)'NUMBER OF CONTACT SECONDARY NODES... :',DDSTAT(20,P)
          WRITE(IOUT,*)'NUMBER OF CONTACT MAIN NODES.. :',DDSTAT(21,P)
        END IF
        IF(NUMSPH.GT.0)
     .    WRITE(IOUT,*)'NUMBER OF SMOOTH PARTICLES...... :',DDSTAT(17,P)
        WRITE(IOUT,*)
        DDSTAT(18,P)=MAX(DDSTAT(18,P),1310720)              ! 5 Mo en entiers / 4 bytes
        DDSTAT(19,P)=MAX(DDSTAT(19,P),5242880/RTOBYTES)     ! 5 Mo en flottants
        MEMTOTAL=DDSTAT(19,P)*RTOBYTES + DDSTAT(18,P) * ITOBYTES
        IF( GOT_INSPIRE_ALM == 1)THEN
          WRITE(IOUT,1201)P,
     .          DDSTAT(19,P)*RTOBYTES/MBYTE,
     .          DDSTAT(18,P)*ITOBYTES/MBYTE,
     .          MEMTOTAL/MBYTE,
     .          DDSTAT(25,P)/1024
        ELSE
          WRITE(IOUT,1200)P,
     .          DDSTAT(19,P)*RTOBYTES/MBYTE,
     .          DDSTAT(18,P)*ITOBYTES/MBYTE,
     .          MEMTOTAL/MBYTE,
     .          DDSTAT(25,P)/1024
        ENDIF

        IF (NDSOLV.GT.0) THEN
           WRITE(IOUT,*)
           IF( GOT_INSPIRE_ALM == 1)THEN
             WRITE(IOUT,'(A)')
     .  '     ADDITIONAL SOLVER STORAGE FOR MULTI-LEVEL CONDENSATION'
           ELSE
             WRITE(IOUT,'(A)')
     .  '     ADDITIONAL ENGINE STORAGE FOR MULTI-LEVEL CONDENSATION'
           ENDIF
           WRITE(IOUT,'(A)')
     .  '     ------------------------------------------------------'
           WRITE(IOUT,1300) DSMEMORY(1,P)*RTOBYTES,
     *                      DSMEMORY(2,P)*RTOBYTES
           IF (P.EQ.1.AND.DSMEMORY(5,P).EQ.0) THEN
              WRITE(IOUT,1310) DSMEMORY(3,P)*RTOBYTES
           ELSE
              WRITE(IOUT,1320) DSMEMORY(3,P)*RTOBYTES,DSMEMORY(6,P)
           ENDIF
           WRITE(IOUT,'(A)')
     .  '                                 ---------'
           MEMTOTAL=MEMTOTAL+
     .       (DSMEMORY(1,P)+DSMEMORY(2,P)+DSMEMORY(3,P))*RTOBYTES
           WRITE(IOUT,1340) MEMTOTAL/1048576
           WRITE(IOUT,*)
           WRITE(IOUT,'(A)')
     .'     DISK SPACE REQUIREMENTS IN TMPDIR FOR GRAPH STORAGE'
           WRITE(IOUT,'(A)')
     .'     ---------------------------------------------------'
           WRITE(IOUT,1350) DSMEMORY(7,P)/(1024*1024),
     .                      DSMEMORY(4,P)/(1024*1024),
     .                      (DSMEMORY(7,P)+DSMEMORY(4,P))/(1024*1024)
        ENDIF
        IF (NFLOW.GT.0) THEN
           WRITE(IOUT,*)
           IF( GOT_INSPIRE_ALM == 1)THEN
             WRITE(IOUT,'(A)')
     .'       ADDITIONAL SOLVER STORAGE FOR BEM SOLUTIONS'
           ELSE
             WRITE(IOUT,'(A)')
     .'       ADDITIONAL ENGINE STORAGE FOR BEM SOLUTIONS'
           ENDIF
           WRITE(IOUT,'(A)')
     .'       -------------------------------------------'
           MEMTOTAL=MEMTOTAL+MEMFLOW(1,P)*4+MEMFLOW(2,P)*RTOBYTES
           WRITE(IOUT,1400) MEMFLOW(2,P)*RTOBYTES/1048576,
     *                      MEMFLOW(1,P)*4/1048576,
     *                      MEMTOTAL/1048576
        ENDIF
       ENDDO
      ELSE
        P=1
        DDSTAT(18,P)=MAX(DDSTAT(18,P),1310720)              ! 5 Mo en entiers / 4 bytes
        DDSTAT(19,P)=MAX(DDSTAT(19,P),5242880/RTOBYTES)     ! 5 Mo en flottants
        MEMTOTAL=DDSTAT(19,P)*RTOBYTES+DDSTAT(18,P)*ITOBYTES

        IF( GOT_INSPIRE_ALM == 1)THEN
          WRITE(IOUT,1201)P,DDSTAT(19,P)*RTOBYTES/1048576,
     .                    DDSTAT(18,P)*4/1048576,
     .                    MEMTOTAL/1048576,
     .                    DDSTAT(25,P)/1024
        ELSE
          WRITE(IOUT,1200)P,DDSTAT(19,P)*RTOBYTES/1048576,
     .                    DDSTAT(18,P)*4/1048576,
     .                    MEMTOTAL/1048576,
     .                    DDSTAT(25,P)/1024
        ENDIF

        IF (NDSOLV.GT.0) THEN
           WRITE(IOUT,*)
           IF( GOT_INSPIRE_ALM == 1)THEN
             WRITE(IOUT,'(A)')
     .  '     ADDITIONAL SOLVER STORAGE FOR MULTI-LEVEL CONDENSATION' 
           ELSE
             WRITE(IOUT,'(A)')
     .  '     ADDITIONAL ENGINE STORAGE FOR MULTI-LEVEL CONDENSATION'
           ENDIF
           WRITE(IOUT,'(A)')
     .  '     ------------------------------------------------------'
           WRITE(IOUT,1300) DSMEMORY(1,P)*RTOBYTES,
     .                      DSMEMORY(2,P)*RTOBYTES
           IF (DSMEMORY(5,P).EQ.0) THEN
              WRITE(IOUT,1310) DSMEMORY(3,P)*RTOBYTES
           ELSE
              WRITE(IOUT,1320) DSMEMORY(3,P)*RTOBYTES,DSMEMORY(6,P)
           ENDIF
           WRITE(IOUT,'(A)')
     .  '                                 ---------'
           MEMTOTAL=MEMTOTAL+
     .       DSMEMORY(1,P)*RTOBYTES +
     .       DSMEMORY(2,P)*RTOBYTES +
     .       DSMEMORY(3,P)*RTOBYTES
           WRITE(IOUT,1340) MEMTOTAL/1048576
           WRITE(IOUT,*)
           WRITE(IOUT,'(A)')
     .'     DISK SPACE REQUIREMENTS IN TMPDIR FOR GRAPH STORAGE'
           WRITE(IOUT,'(A)')
     .'     ---------------------------------------------------'
           WRITE(IOUT,1350) DSMEMORY(7,P)/(1024*1024),
     .                      DSMEMORY(4,P)/(1024*1024),
     .                      (DSMEMORY(7,P)+DSMEMORY(4,P))/(1024*1024)
        ENDIF
        IF (NFLOW.GT.0) THEN
           WRITE(IOUT,*)
           IF( GOT_INSPIRE_ALM == 1)THEN
             WRITE(IOUT,'(A)')
     .'       ADDITIONAL SOLVER STORAGE FOR BEM SOLUTIONS'
           ELSE
             WRITE(IOUT,'(A)')
     .'       ADDITIONAL ENGINE STORAGE FOR BEM SOLUTIONS'
           ENDIF
           WRITE(IOUT,'(A)')
     .'       -------------------------------------------'
           MEMTOTAL=MEMTOTAL+MEMFLOW(1,P)*4+MEMFLOW(2,P)*RTOBYTES
           WRITE(IOUT,1400) MEMFLOW(2,P)*RTOBYTES/1048576,
     .                      MEMFLOW(1,P)*4/1048576,
     .                      MEMTOTAL/1048576
        ENDIF
      END IF
      WRITE(IOUT,*)
C
 1000 FORMAT(I5,8X,I6,16X,I6,16X,I6)
 1200 FORMAT(/,
     . '     LOCAL ENGINE STORAGE EVALUATION FOR SPMD PROCESSOR',I6,/
     . '     --------------------------------------------------------'/
     . '        MEMORY USED FOR REALS   ',I10,' MB ',/
     . '        MEMORY USED FOR INTEGERS',I10,' MB ',/
     . '        TOTAL MEMORY EVALUATION ',I10,' MB ',/
     . '                                            '/,
     . '        RESTART FILE SIZE',I10,' MB')
 1201 FORMAT(/,
     . '     LOCAL SOLVER STORAGE EVALUATION FOR SPMD PROCESSOR',I6,/
     . '     --------------------------------------------------------'/
     . '        MEMORY USED FOR REALS   ',I10,' MB ',/
     . '        MEMORY USED FOR INTEGERS',I10,' MB ',/
     . '        TOTAL MEMORY EVALUATION ',I10,' MB ',/
     . '                                            '/,
     . '        RESTART FILE SIZE',I10,' MB')

 1300 FORMAT(
     . '        NODAL FIELDS. . . . . . ',I10,/
     . '        CONDENSED DATA. . . . . ',I10)
 1310 FORMAT(
     . '        TEMPORARY STORAGE . . . ',I10,' -> CONDENSED PROBLEM')
 1320 FORMAT(
     . '        TEMPORARY STORAGE . . . ',I10,' ->  SUP. ELEM: ',I10)
 1340 FORMAT(
     . '        NEW TOTAL . . . . . . . ',I10,' MB'/)
 1350 FORMAT(
     . '        SUP. ELEM MODES . . . . ',I10,' MB',/
     . '        CONDENSED MATRICES. . . ',I10,' MB',/
     . '                                 ---------'/
     . '        TOTAL . . . . . . . . . ',I10,' MB')
 1400 FORMAT(
     . '        ADD. MEMORY FOR REALS . ',I10,' MB',/
     . '        ADD. MEMORY FOR INTEGERS',I10,' MB',/
     . '                                 ---------'/
     . '        NEW TOTAL . . . . . . . ',I10,' MB'/)
C
      DEALLOCATE( AVERAGE )
      DEALLOCATE( DEVIATION ) 
      RETURN
      END
Chd|====================================================================
Chd|  C_DOMS10                      source/spmd/domdec2.F         
Chd|-- called by -----------
Chd|        DOMDEC2                       source/spmd/domdec2.F         
Chd|-- calls ---------------
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE C_DOMS10(ICNDS10,ITAGND,IPLUS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  ICNDS10(3,*),ITAGND(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN,N1,N2,P,NF,NS,NF0,NFMAX,IPLUS
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGI,NNF
C-----------------------------------------------
!     TAGI->N: NNF(N)=MAX_NF
      ALLOCATE( TAGI(NUMNOD),NNF(NS10E) )
      TAGI(1:NUMNOD) = 0 
      NNF(1:NS10E) = 1 
      IPLUS = 0
C-------------only one of the mid-node is choosen (max_nf) for 1er pass---------
      DO N = 1, NS10E
        NN = ICNDS10(1,N)
        IF(ITAGND(NN)>NS10E) CYCLE
        N1 = ICNDS10(2,N)
        N2 = ICNDS10(3,N)
C----- normally N1,N2 are local as  NN-----     
         NF = 0
         DO P = 1, NSPMD
          NF = NF +NLOCAL(NN,P)           
         ENDDO
         NNF(N) = NF
c         IF(NF <=1 ) CYCLE
         IF (TAGI(N1)==0) THEN
          TAGI(N1) = N
         ELSE
          NS = ICNDS10(1,TAGI(N1))
          NF0 = 0
          DO P = 1, NSPMD
           NF0 = NF0 +NLOCAL(NS,P)                
          ENDDO
          IF (NF>NF0) TAGI(N1)=N 
         END IF
         IF (TAGI(N2)==0) THEN
          TAGI(N2) = N
         ELSE
          NS = ICNDS10(1,TAGI(N2))
          NF0 = 0
          DO P = 1, NSPMD
           NF0 = NF0 +NLOCAL(NS,P)                
          ENDDO
          IF (NF>NF0) TAGI(N2)=N 
         END IF
      END DO
C-------------avoid non symmetry M/S S/M--------
      DO N = 1, NS10E
        NN = ICNDS10(1,N)
        IF(ITAGND(NN)>NS10E) CYCLE
        N1 = ICNDS10(2,N)
        N2 = ICNDS10(3,N)
        IF (TAGI(N1)==N) THEN
         DO P = 1, NSPMD
          IF(NLOCAL(N1,P)==1.AND.NLOCAL(NN,P)/=1) CALL IFRONTPLUS(NN,P)
         ENDDO
        END IF 
        IF (TAGI(N2)==N) THEN
         DO P = 1, NSPMD
          IF(NLOCAL(N2,P)==1.AND.NLOCAL(NN,P)/=1) CALL IFRONTPLUS(NN,P)
         ENDDO
        END IF 
      END DO
C-------------3nd pass for the case- in certain proc--(not necessary)-----
C      DO N = 1, NS10E
C        NN = ICNDS10(1,N)
C        IF(ITAGND(NN)>NS10E) CYCLE
C        N1 = ICNDS10(2,N)
C        N2 = ICNDS10(3,N)
C        NFMAX = TAGI(N1)
C        IF (NFMAX>0.AND.NFMAX/=N) THEN
C         NS = ICNDS10(1,NFMAX)
C         NF = 0
C         DO P = 1, NSPMD
C          IF(NLOCAL(NN,P)==1.OR.NLOCAL(NS,P)==1) NF = NF + 1                     
C         ENDDO
C         IF (NF > NNF(NFMAX)) THEN
C          NF0 = 0
C          DO P = 1, NSPMD
C           IF(NLOCAL(N1,P)==1) NF0 = NF0 + 1                     
C          ENDDO
C          IF (NF0 > NF) THEN
C           DO P = 1, NSPMD
C            IF(NLOCAL(N1,P)==1.AND.NLOCAL(NN,P)/=1) CALL IFRONTPLUS(NN,P)
C           ENDDO
C          END IF !(NF0 > NF) THEN
C         END IF 
C        END IF 
C        NFMAX = TAGI(N2)
C        IF (NFMAX>0.AND.NFMAX/=N) THEN
C         NS = ICNDS10(1,NFMAX)
C         NF = 0
C         DO P = 1, NSPMD
C          IF(NLOCAL(NN,P)==1.OR.NLOCAL(NS,P)==1) NF = NF + 1                     
C         ENDDO
C         IF (NF > NNF(NFMAX)) THEN
C          NF0 = 0
C          DO P = 1, NSPMD
C           IF(NLOCAL(N2,P)==1) NF0 = NF0 + 1                     
C          ENDDO
C          IF (NF0 > NF) THEN
C           DO P = 1, NSPMD
C            IF(NLOCAL(N2,P)==1.AND.NLOCAL(NN,P)/=1) CALL IFRONTPLUS(NN,P)
C           ENDDO
C          END IF !(NF0 > NF) THEN
C         END IF 
C        END IF 
C      END DO
      
      DO N = 1, NS10E
        NN = ICNDS10(1,N)
        IF(ITAGND(NN)>NS10E) CYCLE
        N1 = ICNDS10(2,N)
        N2 = ICNDS10(3,N)
          DO P = 1, NSPMD
            IF(NLOCAL(NN,P)==1)THEN
              IF(NLOCAL(N1,P)/=1) THEN
               CALL IFRONTPLUS(N1,P)
               IPLUS =1
              END IF
              IF(NLOCAL(N2,P)/=1) THEN
               CALL IFRONTPLUS(N2,P)
               IPLUS =1
              END IF
            END IF
          END DO
      END DO
C ----------------------------
      DEALLOCATE( TAGI,NNF )
C ----------------------------
C
      RETURN
      END
