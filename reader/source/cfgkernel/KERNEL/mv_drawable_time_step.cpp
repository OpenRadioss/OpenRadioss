/*Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/

#include <UTILS/win32_utils.h>  


#include <UTILS/mv_cmath.h> 


#include "mv_drawable_time_step.h"


/* --------- Evaluation --------- */

//******************************************************************************
// MvDrawableTimeStep_t::init
//******************************************************************************

//******************************************************************************
// MvDrawableTimeStep_t::close
//******************************************************************************

//******************************************************************************
// MvDrawableTimeStep_t::evaluate
//******************************************************************************
double MvDrawableTimeStep_t::evaluate(const hwCFGDrawableInf *hwcfg_draw_inf) const {
  double a_result=0.;
  /*   temporary code to avoid compilation errors
  if(object.isElement()) {
    MvElement_t a_elem = object;
    MvObject_t  a_part = a_elem.getObjectAttribute(ELT_SUBMODEL);
    if(!a_part.isEmpty()) {
      switch(a_elem.getObjectType()) {
      case SHELL3N:
      case SHELL4N:
	{
	  a_result=(myCelerityArray==NULL ? 
		    a_part.evaluateDrawable("SOUND_SPEED") : 
		    myCelerityArray[a_part.getGlobalIndex()]);
	  if(a_result!=0.) a_result=a_elem.getCaracteristicLength()/a_result;
	}
	break;
      case SOLID:
      case TETRA4:
      //case TETRA10:
	{
	  int    a_part_ind     = a_part.getGlobalIndex();
	  double a_celerity     = (myCelerityArray==NULL ?
				   a_part.evaluateDrawable("SOUND_SPEED") :
				   myCelerityArray[a_part_ind]);
	  double a_knu          = (myKinematicViscosityArray==NULL ?
				   a_part.evaluateDrawable("KINEMATIC_VISCOSITY") :
				   myKinematicViscosityArray[a_part_ind]);
	  double a_qb           = (myLinearBulkViscosityArray==NULL ?
				   a_part.evaluateDrawable("LINEAR_BULK_VISCOSITY") :
				   myLinearBulkViscosityArray[a_part_ind]);
	  double a_carac_length = a_elem.getCaracteristicLength();
	  double a_qx           = a_qb*a_celerity+2*a_knu/a_carac_length;
	  //
	  a_result=a_qx+sqrt(a_qx*a_qx+a_celerity*a_celerity);
	  if(a_result<1.E-20) a_result=1.E-20;
	  a_result=a_carac_length/a_result;
	}
	break;
      case BEAM:
	{
	  int    a_part_ind     = a_part.getGlobalIndex();
	  double a_carac_length = a_elem.getCaracteristicLength();
	  double a_celerity     = (myCelerityArray==NULL ?
				   a_part.evaluateDrawable("SOUND_SPEED") :
				   myCelerityArray[a_part_ind]);
	  double a_beam_coeff   = (myBeamCoeffArray==NULL ?
				   a_part.evaluateDrawable("BEAM_COEFF") :
				   myBeamCoeffArray[a_part_ind]);
	  double a_b_coeff      = a_carac_length*a_carac_length*a_beam_coeff;
	  double a_a_coeff      = 1.+a_b_coeff/12.;
	  a_b_coeff/=3;
	  if(a_a_coeff>a_b_coeff) a_a_coeff=a_b_coeff;
	  if(a_a_coeff>4.)        a_a_coeff=4.;
	  a_a_coeff=sqrt(a_a_coeff)/2;
	  if(a_celerity!=0.) a_result=a_a_coeff*a_carac_length/a_celerity;
	}
	break;
      case TRUSS:
	{
	  a_result=(myCelerityArray==NULL ? 
		    a_part.evaluateDrawable("SOUND_SPEED") : 
		    myCelerityArray[a_part.getGlobalIndex()]);
	  if(a_result!=0.) a_result=a_elem.getCaracteristicLength()/a_result;
	}
	break;
      default:
	break;
      }
    }
  }
  */
  return a_result;
}


