/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_PLA_TAB_H_INCLUDED
# define YY_YY_PLA_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    INTNUMBER = 2561,
    REALNUMBER = 2562,
    IDENT = 257,
    CONST = 258,
    VAR = 259,
    PROCEDURE = 260,
    CALL = 261,
    _BEGIN = 262,
    END = 263,
    IF = 264,
    THEN = 2651,
    ELSE = 2652,
    WHILE = 266,
    DO = 267,
    EQ = 268,
    NE = 269,
    LT = 270,
    LE = 271,
    GT = 272,
    GE = 273,
    ASS = 274,
    KOMMA = 275,
    SEMICOLON = 276,
    PLUS = 277,
    MINUS = 278,
    MULT = 279,
    DIV = 280,
    KLAUF = 281,
    KLZU = 282,
    PUNKT = 283,
    COLON = 284,
    INT = 285,
    REAL = 286,
    BOOL = 287,
    WAHR = 288,
    FALSCH = 289,
    PROGEND = 290,
    FI = 291
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 44 "pla.y" /* yacc.c:1909  */

	int num;
	double realnum;
	char idname[300];

#line 101 "pla.tab.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PLA_TAB_H_INCLUDED  */
