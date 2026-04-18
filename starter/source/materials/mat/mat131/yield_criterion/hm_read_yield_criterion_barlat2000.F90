!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    hm_read_yield_criterion_barlat2000_mod   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_barlat2000.F90
!||--- called by ------------------------------------------------------
!||    hm_read_yield_criterion                  ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion.F90
!||====================================================================
      module hm_read_yield_criterion_barlat2000_mod
        implicit none
! \brief Read Barlat 2000 yield criterion input data for /MAT/LAW131
! \details Read the Barlat 2000 (Yld2000-2d) anisotropic yield criterion
!          parameters for /MAT/LAW131.
      contains
!||====================================================================
!||    hm_read_yield_criterion_barlat2000   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_barlat2000.F90
!||--- called by ------------------------------------------------------
!||    hm_read_yield_criterion              ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                               ../starter/source/output/message/message.F
!||    crityld2000                          ../starter/source/materials/mat/mat087/law87_upd.F90
!||    hm_get_float_array_index             ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    invert                               ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
!||    prodmatvect                          ../starter/source/materials/mat/mat087/law87_upd.F90
!||    r_yld2000                            ../starter/source/materials/mat/mat087/law87_upd.F90
!||--- uses       -----------------------------------------------------
!||    calculp2_mod                         ../starter/source/materials/mat/mat057/calculp2.F90
!||    hm_option_read_mod                   ../starter/share/modules1/hm_option_read_mod.F
!||    law87_upd_mod                        ../starter/source/materials/mat/mat087/law87_upd.F90
!||    message_mod                          ../starter/share/message_module/message_mod.F
!||    submodel_mod                         ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_yield_criterion_barlat2000(                         &
          ikey     ,icrit    ,nupar_crit,upar_crit,is_available,unitab   ,     &
          lsubmodel,iout     ,is_encrypted,mat_id ,titr        ,ifit     )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use calculp2_mod
          use precision_mod, only : WP
          use message_mod
          use law87_upd_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< Material key
          integer,                 intent(inout) :: icrit                 !< Yield criterion type
          integer,                 intent(inout) :: nupar_crit            !< Number of yield criterion parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_crit         !< Yield criterion parameters
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          integer,                 intent(in)    :: mat_id                !< Material ID
          character(len=nchartitle),intent(in)   :: titr                  !< Material law user title
          integer,                 intent(in)    :: ifit                  !< Fitting flag
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          integer :: info,k,error,niter,iok
          real(kind=WP) ::                                                     &
            gamma,delta,fct,yield,dx,dy,g1,g2,g3,g13,g23,g33,g15,g25,g35,      &
            df1,df2,df3,df13,df23,df33,df15,df25,df35,f1,f2,                   &
            dv1a7,dv11a8,dw11a8,r00,r45,r90,rb,s00,s45,s90,sb,                 &
            x1,x2,x11,x22 ,v1,v11,w11,tal7,tal8,aswift,epso,qvoce,beta,ko,     &
            alpha,nexp,conjtf4a8,tf4a8,dal78(2),expv,kswift,kvoce,puis,pla,    &
            residu,g(2),dal(8),dg(2,2),al(8),f(8),df(6,6),                     &
            dfinv(6,6),dginv(2,2),dyld_dp(1),                                  &
            xvec(1,2),yld(1),expa,expam2,k1,k2,ahs,bhs,mhs,eps0hs,nhs,hmart,   &
            temp0,expo,aexp,atemp,vm0
          double precision :: lp(3,3),lpp(3,3),wr(3),wi(3),work(102),vl(3,3),  &
            vr(3,3)
!===============================================================================
!  
          !===================================================================
          !< Barlat (2000) yield criterion
          !===================================================================
          !< Yield criterion type
          icrit = 5
          !< Classical Barlat 2000 coefficients
          if (ifit == 1) then
            call hm_get_float_array_index("CRIT_BARL00_A1" ,al(1),ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_A2" ,al(2),ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_A3" ,al(3),ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_A4" ,al(4),ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_A5" ,al(5),ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_A6" ,al(6),ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_A7" ,al(7),ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_A8" ,al(8),ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_AXP",expa ,ikey,is_available,lsubmodel,unitab)
            do k = 1,8
              if (al(k) == zero) al(k) = one
            end do
            if (expa == zero) expa = two
          !< Alternative fitting procedure based on yield stresses in specific loading directions
          elseif (ifit == 2) then
            call hm_get_float_array_index("CRIT_BARL00_SIG00",s00  ,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_SIG45",s45  ,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_SIG90",s90  ,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_SIGB" ,sb   ,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_YLD0" ,yield,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_R00"  ,r00  ,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_R45"  ,r45  ,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_R90"  ,r90  ,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_RB"   ,rb   ,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_BARL00_AXP"  ,expa ,ikey,is_available,lsubmodel,unitab)
            if (expa == zero) expa = two
            expam2 = expa - 2
!
            !< Start newton loops to find al parameters from 1 to 6
            residu  = ep30
            niter   = 0
            iok     = 0
            al(1:8) = one
            gamma   = two_third
            delta   = -third
            do while (abs(residu) > em06 .and. niter < 100 .and. iok == 0)
!
              call crityld2000(fct,gamma, delta, expa, al)
              f(1) = fct - two * (yield /s00)**expa
!
              call crityld2000(fct,delta,gamma,  expa, al)
              f(2) = fct - two * (yield /s90)**expa
!
              call crityld2000(fct,delta,delta,  expa, al)
              f(3) = fct - two * (yield /sb)**expa
!
              call r_yld2000(dx,dy,gamma, delta, expa, al)
              f(4) = (one-r00)*dx - (two + r00)*dy
!
              call r_yld2000(dx,dy, delta,gamma, expa, al)
              f(5) = (two+r90)*dx + (r90 -one)*dy
!
              call r_yld2000(dx,dy,delta, delta, expa, al)
              f(6) = (one+two *rb)*dx - (two + rb)*dy
!
              !< Jacobian matrix
              df1 = (al(1)*two_third + al(2)*third)*abs(al(1)*two_third + al(2)*third)**expam2
              df(1,1) = two_third*expa*df1
              df(1,2) = third*expa *df1
              df13 = (al(3)*two_third - two*al(4)*third)*abs(al(3)*two_third - two*al(4)*third)**expam2
              df(1,3) = two_third*expa*df13
              df(1,4) = -two_third*expa*df13
              df15 = (two*al(5)*two_third - al(6)*third)*abs(two*al(5)*two_third - al(6)*third)**expam2
              df(1,5) = two*two_third*expa*df15
              df(1,6) = -third*expa*df15
!
              df2 = (-al(1)*third - al(2)*two_third)*abs(-al(1)*third - al(2)*two_third)**expam2
              df(2,1) = -third *expa *df2
              df(2,2) = -two_third*expa *df2
              df23 = (-al(3)*third + two*al(4)*two_third)*abs(-al(3)*third + two*al(4)*two_third)**expam2
              df(2,3) = -third      *expa*df23
              df(2,4) = two*two_third *expa*df23
              df25 = (-two*al(5)*third + al(6)*two_third)*abs(-two*al(5)*third + al(6)*two_third)**expam2
              df(2,5) = -two_third*expa*df25
              df(2,6) =  two_third*expa*df25
!
              df3 = (-al(1)*third + al(2)*third)*abs(-al(1)*third + al(2)*third)**expam2
              df(3,1) = -third*expa*df3
              df(3,2) = third*expa *df3
              df33 = (-al(3)*third - two*al(4)*third)*abs(-al(3)*third - two*al(4)*third)**expam2
              df(3,3) = -third  *expa*df33
              df(3,4) = -two_third *expa*df33
              df35 = (-two*al(5)*third - al(6)*third)*abs(-two*al(5)*third - al(6)*third)**expam2
              df(3,5) = -two*third*expa*df35
              df(3,6) = -third     *expa*df35
!
              g1=al(1)*two_third+al(2)*third
              df(4,1)=((one-r00)*g1+two_third*(expa-one)*(al(1)*(one-r00)+al(2)*(two+r00)))*abs(g1)**expam2
              df(4,2)=((two+r00)*g1+third*(expa-one)*(al(1)*(one-r00)+al(2)*(two+r00)))*abs(g1)**expam2
              g13=al(3)*two_third-two*al(4)*third
              df(4,3)=((one-r00)*g13+two_third*(expa-one)*(al(3)*(one-r00)-two*al(4)*(two+r00)))*abs(g13)**expam2
              df(4,4)=(-two*(two+r00)*g13-third*(expa-one)*(al(3)*(one-r00)-two*al(4)*(two+r00)))*abs(g13)**expam2
              g15=two*al(5)*two_third-al(6)*third
              df(4,5)=(two*(one-r00)*g15+two*two_third*(expa-one)*(two*al(5)*(one-r00)-al(6)*(two+r00)))*abs(g15)**expam2
              df(4,6)=(-(two+r00)*g15-third*(expa-one)*(two*al(5)*(one-r00)-al(6)*(two+r00)))*abs(g15)**expam2
!
              g2=-al(1)*third-al(2)*two_third
              df(5,1)=((two+r90)*g2-third*(expa-one)*(al(1)*(two+r90)+al(2)*(one-r90)))*abs(g2)**expam2
              df(5,2)=((one-r90)*g2-two_third*(expa-one)*(al(1)*(two+r90)+al(2)*(one-r90)))*abs(g2)**expam2
              g23=-al(3)*third+two*al(4)*two_third
              df(5,3)=((two+r90)*g23-third*(expa-one)*(al(3)*(two+r90)-two*al(4)*(one-r90)))*abs(g23)**expam2
              df(5,4)=(-two*(one-r90)*g23+two_third*(expa-one)*(al(3)*(two+r90)-two*al(4)*(one-r90)))*abs(g23)**expam2
              g25=-two*al(5)*third+al(6)*two_third
              df(5,5)=(two*(two+r90)*g25-two*third*(expa-one)*(two*al(5)*(two+r90)-al(6)*(one-r90)))*abs(g25)**expam2
              df(5,6)=(-(one-r90)*g25+two_third*(expa-one)*(two*al(5)*(two+r90)-al(6)*(one-r90)))*abs(g25)**expam2
!
              g3=-al(1)*third+al(2)*third
              df(6,1)=((one+two*rb)*g3-third*(expa-one)*(al(1)*(one+two*rb)+al(2)*(two+rb)))*abs(g3)**expam2
              df(6,2)=((two+rb)*g3+third*(expa-one)*(al(1)*(one+two*rb)+al(2)*(two+rb)))*abs(g3)**expam2
              g33=-al(3)*third-two*al(4)*third
              df(6,3)=((one+two*rb)*g33-third*(expa-one)*(al(3)*(one+two*rb)-two*al(4)*(two+rb)))*abs(g33)**expam2
              df(6,4)=(-two*(two+rb)*g33-third*(expa-one)*(al(3)*(one+two*rb)-two*al(4)*(two+rb)))*abs(g33)**expam2
              g35=-two*al(5)*third-al(6)*third
              df(6,5)=(two*(one+two*rb)*g35-two*third*(expa-one)*(two*al(5)*(one+two*rb)-al(6)*(two+rb)))*abs(g35)**expam2
              df(6,6)=(-(two+rb)*g35-third*(expa-one)*(two*al(5)*(one+two*rb)-al(6)*(two+rb)))*abs(g35)**expam2
!
              call invert(df,dfinv,6,error)
              call prodmatvect(dfinv, f, dal, 6)
              do k = 1, 6
                al(k) = al(k) - dal(k)
                if (al(k) > ep05)then
                  iok = 1
                  call ancmsg(msgid=3131,                                      &
                    msgtype=msgerror,                                          &
                    anmode=aninfo_blind_2,                                     &
                    i1=mat_id,                                                 &
                    c1="ERROR",                                                &
                    c2=titr,                                                   &
                    c3="CRIT_BARLAT2000_2",                                    &
                    c4="NO CONVERGENCE IN IDENTIFICATION ALGORITHM PLEASE" //  &
                       " CHECK YOUR INPUT")  
                  exit
                endif
              enddo
              residu = sqrt(f(1)**2+f(2)**2+f(3)**2+f(4)**2+f(5)**2+f(6)**2)/6
              niter = niter + 1
            enddo
!
            !< Compute al(7) and al(8)
            x1  = (al(1)+al(2))/3
            x2  = (al(1)-al(2))/3
            x11 = (al(3)     + two*al(4) + two*al(5)+al(6)      ) /nine
            x22 = (two*al(5)+      al(6) - al(3)     -two *al(4))/three
!
            !< Loop
            residu = ep30
            niter  = 0
            do while (abs(residu) > em06 .and. niter < 100 .and. iok == 0)
!
              tal7 = sqrt(max(zero,x2**2  + four * al(7)**2) )/two
              tal8 = sqrt(max(zero,x22**2 + four * al(8)**2) )
              tf4a8=      (three*x11 + tal8 ) / four
              conjtf4a8 = (three*x11 - tal8 ) / four
!
              v1  = expa*(tal7)**(expa-1)
              v11 = expa*(conjtf4a8)*(abs(conjtf4a8))**(expa-2)
              w11 = expa*(tf4a8)*(abs(tf4a8))**(expa-2)
!
              f1= tal7**expa + abs(conjtf4a8)**expa + abs(tf4a8)**expa
              f2 =v1* (x2**2/(two*tal7) )   +  three_half*x11*(v11+w11) + half*(x22**2/tal8) * (w11-v11)
!
              g(1) = f1 - two*(yield/s45)**expa
              g(2) = f2 - (two*expa/(one+r45))*(yield/s45)**expa
!
              dv1a7  = expa*(expa-one)*al(7) * tal7**(expa-3)
              dv11a8 = -(expa*(expa-one)*al(8) /tal8 ) *  abs(conjtf4a8)**(expa-2)
              dw11a8 =  (expa*(expa-one)*al(8) /tal8 ) *  abs(tf4a8 ) **(expa-2)
!
              dg(1,1)=(expa*abs(tal7)**(expa-1))*al(7)/tal7
              dg(1,2)=expa*(al(8)/tal8)*(tf4a8*abs(tf4a8)**(expa-2)-conjtf4a8*abs(conjtf4a8)**(expa-2))
              dg(2,1)=dv1a7*x2**2/(two*tal7)-four*v1*al(7)*x2**2/sqrt(max(zero,x2**2+four*al(7)**2))**3
              dg(2,2)=three_half*x11*(dv11a8+dw11a8)+half*(x22**2/tal8)*(dw11a8-dv11a8)-(w11-v11)*al(8)*x22**2/tal8**3
!
              call invert(dg,dginv,2,error)
              call prodmatvect(dginv, g, dal78, 2)
              do k = 1, 2
                al(6+k) = al(6+k) - dal78(k)
                if (al(6+k) > ep05) then
                  iok = 1
                  call ancmsg(msgid=3131,                                      &
                    msgtype=msgerror,                                          &
                    anmode=aninfo_blind_2,                                     &
                    i1=mat_id,                                                 &
                    c1="ERROR",                                                &
                    c2=titr,                                                   &
                    c3="CRIT_BARLAT2000_2",                                    &
                    c4="NO CONVERGENCE IN IDENTIFICATION ALGORITHM PLEASE" //  &
                       " CHECK YOUR INPUT")  
                  exit
                end if
              end do
              residu = sqrt(g(1)**2+g(2)**2)/2
              niter = niter + 1
            enddo ! while iterations
!
            if (iok ==0) then
              do k=1,8
                if (al(k) <= zero .or. al(k) > ten) then
                  iok = 1
                  call ancmsg(msgid=3131,                                      &
                    msgtype=msgerror,                                          &
                    anmode=aninfo_blind_2,                                     &
                    i1=mat_id,                                                 &
                    c1="ERROR",                                                &
                    c2=titr,                                                   &
                    c3="CRIT_BARLAT2000_2",                                    &
                    c4="NO CONVERGENCE IN IDENTIFICATION ALGORITHM PLEASE" //  &
                       " CHECK YOUR INPUT")  
                  exit
                endif
              enddo
            endif
          endif
!
          !------------------------------------------------------------------
          ! -> Check yield surface convexity
          !------------------------------------------------------------------
          !< Check first linear transformation matrix principal values
          ! (must be positive to ensure convexity)
          lp = zero
          lp(1,1) = two*al(1)/three
          lp(1,2) = -al(1)/three
          lp(2,1) = -al(2)/three
          lp(2,2) = two*al(2)/three
          lp(3,3) = al(7)
          call dgeev("N","N",3,lp,3,wr,wi,vl,3,vr,3,work,102,info)
          if (minval(wr) <= zero) then
            call ancmsg(msgid=3131,                                            &
              msgtype=msgwarning,                                              &
              anmode=aninfo_blind_1,                                           &
              i1=mat_id,                                                       &
              c1="WARNING",                                                    &
              c2=titr,                                                         &
              c3="CRIT_BARLAT2000",                                            &
              c4="INPUT OR IDENTIFIED COMBINATION OF ALPHA_i PARAMETERS" //    &
               " MIGHT LEAD TO NON-CONVEX YIELD SURFACE. EIGENVALUES OF FIRST"//&
               " LINEAR TRANSFORMATION LP MUST BE STRICTLY POSITIVE. ")    
          endif
          !< Check second linear transformation matrix principal values
          ! (must be positive to ensure convexity)
          lpp = zero
          lpp(1,1) = (-two*al(3) +   two*al(4) + eight*al(5) -  two*al(6))/nine
          lpp(1,2) = (     al(3) -  four*al(4) -  four*al(5) + four*al(6))/nine
          lpp(2,1) = (four*al(3) -  four*al(4) -  four*al(5) +      al(6))/nine
          lpp(2,2) = (-two*al(3) + eight*al(4) +   two*al(5) -  two*al(6))/nine
          lpp(3,3) = al(8)
          wr = zero
          wi = zero
          vl = zero
          vr = zero
          work = zero
          call dgeev("N","N",3,lpp,3,wr,wi,vl,3,vr,3,work,102,info)
          !< Check second linear transformation matrix principal values
          ! (must be positive to ensure convexity)
          if (minval(wr) <= zero) then
            call ancmsg(msgid=3131,                                            &
              msgtype=msgwarning,                                              &
              anmode=aninfo_blind_1,                                           &
              i1=mat_id,                                                       &
              c1="WARNING",                                                    &
              c2=titr,                                                         &
              c3="CRIT_BARLAT2000",                                            &
              c4="INPUT OR IDENTIFIED COMBINATION OF ALPHA_i PARAMETERS" //    &
              " MIGHT LEAD TO NON-CONVEX YIELD SURFACE. EIGENVALUES OF SECOND"//&
              " LINEAR TRANSFORMATION LPP MUST BE STRICTLY POSITIVE. ")
          endif
          !< Number of parameters
          nupar_crit = 11
          !< Save yield criterion parameters
          upar_crit(1)  = two*al(1)/three
          upar_crit(2)  = -al(1)/three
          upar_crit(3)  = -al(2)/three
          upar_crit(4)  = two*al(2)/three
          upar_crit(5)  = al(7)
          upar_crit(6)  = (-two*al(3) +   two*al(4) + eight*al(5) -  two*al(6))/nine
          upar_crit(7)  = (     al(3) -  four*al(4) -  four*al(5) + four*al(6))/nine
          upar_crit(8)  = (four*al(3) -  four*al(4) -  four*al(5) +      al(6))/nine
          upar_crit(9)  = (-two*al(3) + eight*al(4) +   two*al(5) -  two*al(6))/nine
          upar_crit(10) = al(8)
          upar_crit(11) = expa
          !< Printing yield criterion parameters
          if (is_encrypted) then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000)
            if (ifit == 1) then
              write(iout,1007) expa,al(1),al(2),al(3),al(4),al(5),al(6),al(7),al(8)
            else
              write(iout,1006) expa,s00,s45,s90,sb,yield,r00,r45,r90,rb,al(1),al(2),al(3),al(4),al(5),al(6),al(7),al(8)
            endif  
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"BARLAT (2000) YIELD CRITERION                          ",/,      &
          5X,"-------------------------------------------------------")
1006 format(/                                                                  &
          5X,"EXPERIMENTAL DATA USED TO FIT BARLAT 2000:             ",/,      &
          5X,"------------------------------------------             ",/,      &
          5X,"EXPONENT OF YIELD CRITERION A. . . . . . . . . . . . .=",1PG20.13/&
          5X,"YIELD STRESS IN DIRECTION 00 . . . . . . . . . . . . .=",1PG20.13/&
          5X,"YIELD STRESS IN DIRECTION 45 . . . . . . . . . . . . .=",1PG20.13/&
          5X,"YIELD STRESS IN DIRECTION 90 . . . . . . . . . . . . .=",1PG20.13/&
          5X,"YIELD STRESS FOR BIAXIAL . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"GLOBAL YIELD STRESS AT 0.2% PLASTIC STRAIN . . . . . .=",1PG20.13/&
          5X,"R-VALUE IN DIRECTION 00. . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"R-VALUE IN DIRECTION 45. . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"R-VALUE IN DIRECTION 90. . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"R-VALUE FOR BIAXIAL. . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"                                                       ",/,      &
          5X,"FITTED BARLAT 2000 ANISOTROPY COEFFICIENTS:            ",/,      &
          5X,"-------------------------------------------            ",/,      &
          5X,"ANISOTROPY COEFFICIENT ALPHA1. . . . . . . . . . . . .=",1PG20.13/&
          5X,"ANISOTROPY COEFFICIENT ALPHA2. . . . . . . . . . . . .=",1PG20.13/&
          5X,"ANISOTROPY COEFFICIENT ALPHA3. . . . . . . . . . . . .=",1PG20.13/&
          5X,"ANISOTROPY COEFFICIENT ALPHA4. . . . . . . . . . . . .=",1PG20.13/&
          5X,"ANISOTROPY COEFFICIENT ALPHA5. . . . . . . . . . . . .=",1PG20.13/&
          5X,"ANISOTROPY COEFFICIENT ALPHA6. . . . . . . . . . . . .=",1PG20.13/&
          5X,"ANISOTROPY COEFFICIENT ALPHA7. . . . . . . . . . . . .=",1PG20.13/&
          5X,"ANISOTROPY COEFFICIENT ALPHA8. . . . . . . . . . . . .=",1PG20.13/)
1007 format(                                                                   &
          5X,"EXPONENT OF YIELD CRITERION A. . . . . . . . . . . . .=",1PG20.13/&
          5X,"ANISOTROPY COEFFICIENT ALPHA1. . . . . . . . . . . . .=",1PG20.13/&
          5X,"ANISOTROPY COEFFICIENT ALPHA2. . . . . . . . . . . . .=",1PG20.13/&
          5X,"ANISOTROPY COEFFICIENT ALPHA3. . . . . . . . . . . . .=",1PG20.13/&
          5X,"ANISOTROPY COEFFICIENT ALPHA4. . . . . . . . . . . . .=",1PG20.13/&
          5X,"ANISOTROPY COEFFICIENT ALPHA5. . . . . . . . . . . . .=",1PG20.13/&
          5X,"ANISOTROPY COEFFICIENT ALPHA6. . . . . . . . . . . . .=",1PG20.13/&
          5X,"ANISOTROPY COEFFICIENT ALPHA7. . . . . . . . . . . . .=",1PG20.13/&
          5X,"ANISOTROPY COEFFICIENT ALPHA8. . . . . . . . . . . . .=",1PG20.13/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_yield_criterion_barlat2000
      end module hm_read_yield_criterion_barlat2000_mod