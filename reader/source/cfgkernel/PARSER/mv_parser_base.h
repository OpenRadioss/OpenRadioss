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


#ifndef MV_PARSER_BASE_H
#define MV_PARSER_BASE_H

#include <PARSER/mv_read_base.h>


class HC_DATA_DLL_API MvParserBase_t : public MvReadBase_t {

public:    // Constructor and destructor
// @@ MPOST R.A 18/10/2004 MP_DEV_2004_83: Zipp Param File
  MvParserBase_t(const string &fullname,bool is_gzipped=false ,bool use_local_msg=false);
  virtual ~MvParserBase_t();

protected: // MvReadBase_t redefined functions
  
  bool seof(bool do_strict); 
  
  virtual char readChar();  // modified for taking comments into account


protected: 
  virtual inline bool isSeparator(char c) const { return c==';' || c==',' || c==':'; }
  
  
  virtual inline bool isBlank(char c)     const { return c==' ' || c=='\t' || c=='\0' || c=='\n' || c=='\r'; }
  
  virtual inline bool isOperator(char c)  const { return c=='='; }
  virtual inline bool isBoundary(char c)  const { 
    return c=='{' || c=='}' || c=='(' || c==')' || c=='[' || c==']';
  }
  virtual inline bool isComparatorBegin(char c) const {
    return c=='=' || c=='!' || c=='>' || c=='<';
  }
  virtual inline bool isComparator(const string &s) const {
    return s=="==" || s=="!=" || s==">" || s==">=" || s=="<" || s=="<=";
  }
  
  virtual inline bool isLogicalOperatorBegin(char c) const {
    return c=='!' || c=='|' || c=='&';
  }
  virtual inline bool isLogicalOperator(const string &s) const {
    return s=="!" || s=="||" || s=="&&";
  }
  

protected: // Research
  // Next not blank
  char   getNextChar();
  // Next before a separator, a blank, an operator, a boundary, a comparator, or a logical operator
  string getNextString(bool withspace = false);
  // Next before a separator, a blank, an operator, a boundary, a comparator, or a logical operator
  string getNextQuotedString(char comma='\"');
  // Next before a separator, a blank, an operator, a boundary, a comparator, or a logical operator
  int getNextInt();

  bool getNextBool();
  unsigned int getNextUInt(); 
  // Next before a separator, a blank, an operator, a boundary, a comparator, or a logical operator
  double getNextFloat();
  string getNextComparator();
  string getNextLogicalOperator(); 

protected: // Error management
  void throwError(const char *format,...) const;
  void throwError(const string &msg)      const;

};


#endif //MV_PARSER_BASE_H




