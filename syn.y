%{
   #include<stdio.h>
   #include<stdlib.h>
   #include<string.h>

   extern nb_ligne,semligne; 
   int Col=1,semcol,bloccol;
   



  
%}
%union{
   int Integer ;
   float float;
   char* str;
   char* car;
   double val_tmps;
}

%token 
tabulation
sautdligne
mc_if
mc_else
mc_while
mc_for
mc_range
mc_and
mc_or
mc_not
mc_in
<str>mc_int
<str>mc_float 
<str>mc_bool
<str>mc_char
<str>idf 
<entier>cst_int
<reel>cst_reel 
<car>cst_car 
cst_bool
virgule
point_Virgule
point
deux_Point
addition
substraction
division
multiplication
affectation
parenthese_ouvr
parenthese_ferm
crocht_ouvr
crocht_ferm
accolade_ouvr
accolade_ferm
modulo
address
ampersand
diese
inf
sup
inf_equal
sup_equal
equal
not_equal


%left equal not_equal inf sup inf_equal sup_equal
%left addition substraction
%left multiplication division
%right idf sautdligne

%right tabulation

%start S
%%
S:  List_Dec S | INST S | SAUT S| {printf("\n\n syntaxe correcte"); YYACCEPT;} ;

/* Declaration */

SAUT : sautdligne SAUT | sautdligne;

List_Dec : DEC SAUT ;
            

// declaration du genre : type = idf , si idf est deja declare alors error double_declare , sinon affecter type a idf 

DEC: TYPE idf LIST_VAR { 
                        
                        if (doubleDeclaration($2)==0){ Inserer_type($2,sauvidf); }
                        else{{printf("    >>>>>>> Errreur symantique a la ligne %d colonne %d , DOUBLE DECLARATION de idf %s \n",semligne,semcol,$2);}}
                        }
                        
                       
                         | TAB_DEC ;

// un tableau est reconnue comme type table , pas d'allocation d'espace 

TAB_DEC: TYPE idf crocht_ouvr EXP_ARRITH crocht_ferm {
                  tmp=Depiler();
                  if(tmp<=0){
                  printf("    >>>>>>> Erreur semantique ligne %d colonne %d La taille du tableau doit etre superieure a 0  \n",semligne,semcol);}
                  else{
                  if(doubleDeclaration($2)==0){
                  Inserer_type($2,"TABLE");
                  }else {printf("    >>>>>>> Erreur semantique ligne %d colonne %d ,DOUBLE DECLARATION de idf %s  \n",semligne,semcol,$2);}   
                  
                  }};
   

LIST_VAR: virgule idf LIST_VAR { 
                        if (doubleDeclaration($2)==0){ Inserer_type($2,sauvidf); }
                        else{{printf("\n=======> Errreur symantique a la ligne %d , DOUBLE DECLARATION  de idf %s\n",semligne,semcol,$2);}}}

                        
                        | ;

TYPE:       mc_int {strcpy(sauvidf,"INTEGER");}
            | mc_float {strcpy(sauvidf,"FLOAT");}
            | mc_bool {strcpy(sauvidf,"BOOLEAN");}
            | mc_char {strcpy(sauvidf,"CHAR");}



/* instruction */




INST : AFFECT | IF_ELSE | WHILE | FOR_RANGE ;
//| COND |	WHILE  | FOR | FOR_RANGE;



// idf1 = idf2 , le minipy autorise la declaration avec affectation donc on test pas si idf1 existe 
// tester existance de idf2 + compatibilité de type 
// le type int et float sont compatible

AFFECT :    idf affectation idf SAUT
          {
            if(doubleDeclaration($3)==0){
               printf("    >>>>>>> Erreur semantique ligne %d colonne %d   idf %s n'existe pas \n",semligne,semcol,$3);
            }else{
               get_type_of_idf($3,sauvidf);
               get_type_of_idf($1,sauvval);
               if(type_compatible(sauvval,sauvidf)==0){
                  printf("    >>>>>>> Erreur semantique ligne %d colonne %d INCOMPATIBILITE DE TYPE , %s est %s , ne peut pas recevoir %s \n",semligne,semcol,$1,sauvval,sauvidf);
               }else{
                  if(strcmp(sauvidf,"FLOAT")==0 || strcmp(sauvidf,"INTEGER")==0){
                     Inserer_type($1,sauvidf);
                     get_value_of_idf($3,sauvval);
                     set_value_of_idf($1,sauvval);
                  }else{
                     Inserer_type($1,sauvidf);
                     get_valstring_of_idf($3,sauvval);
                     set_valstring_of_idf($1,sauvval);
                  }
               }

               
            }

          }

            | AFFECT_ARITHM




// idf = valeur , valeur : boolean , char , numerique 
// si idf est deja declare , tester compatibilite de type , si idf est nouveau on affect


		   | idf affectation cst_car SAUT { 

            if (doubleDeclaration($1)==0){              // new idf 
                Inserer_type($1,"CHAR"); 
                set_valstring_of_idf($1,$3);
                
                quadruplet("=",$3,"",$1);
                
                   }
            else{
               get_type_of_idf($1,sauvidf);
               if(strcmp(sauvidf,"CHAR")==0){    // idf de type char deja existant
                  set_valstring_of_idf($1,$3);

                  quadruplet("=",$3,"",$1);

               }else{
                  printf("    >>>>>>> Erreur semantique ligne %d colonne %d INCOMPATIBILITE DE TYPE , idf %s est un %s , ne peut pas recevoir un CHAR\n",semligne,semcol,$1,sauvidf);
               }

            }}
            
		   | idf affectation cst_bool SAUT{ 
            if (doubleDeclaration($1)==0){     
                Inserer_type($1,"BOOLEAN");
                set_valstring_of_idf($1,$3);

                quadruplet("=",$3,"",$1);
            }
            else{
               get_type_of_idf($1,sauvidf);
               if(strcmp(sauvidf,"BOOLEAN")==0){
                  set_valstring_of_idf($1,$3);

                  quadruplet("=",$3,"",$1);
               }else{
                  printf("    >>>>>>> Erreur semantique ligne %d colonne %d INCOMPATIBILITE DE TYPE , idf %s est pas un %s , ne peut pas recevoir un BOOLEAN \n",semligne,semcol,$1,sauvidf);
               
                 }}}

		   ;


// fonction Is_int test si la valeur retourne par ex_arth est int ou float 

AFFECT_ARITHM : idf affectation EXP_ARRITH  SAUT {   

            tmp=Depiler();
            get_type_of_idf($1,sauvidf);

            // si idf est deja declare comme un boolean or char  or table ,il ne peut pas recevoir une valeur numerique 

            if(strcmp(sauvidf,"CHAR")==0){
               printf("    >>>>>>> Erreur semantique ligne %d colonne %d INCOMPATIBILITE DE TYPE , idf %s est un caractere , ne peut pas recevoir une valeure numerique \n",semligne,semcol,$1);}
            else{
               if(strcmp(sauvidf,"BOOLEAN")==0){
               printf("    >>>>>>> Erreur semantique ligne %d colonne %d INCOMPATIBILITE DE TYPE , idf %s est un BOOLEAN , ne peut pas recevoir une valeure numerique \n",semligne,semcol,$1);}
               else{
                  if(strcmp(sauvidf,"TABLE")==0){
                   printf("    >>>>>>> Erreur semantique ligne %d colonne %d INCOMPATIBILITE DE TYPE , idf %s est un tableau , ne peut pas recevoir une valeure numerique \n ",semligne,semcol,$1);}

               // idf est soit de type int , float ou c sa premiere declaration donc type = ""
               // tester si la valeur retourne dans tmp est int ou float pour donner le type exact 

               else{ 

               set_value_of_idf($1,&tmp);

               ajout_quad_affect_val($1,&tmp);
               
               
               if(Is_int(&tmp)==1)
               {
                  printf("\n its an int ");
                  Inserer_type($1,"INTEGER");
                  
               }else{
                  printf("its a float");
                  Inserer_type($1,"FLOAT");
                  }}}}
     
}


// pour les expressions arrithmetique on utilise la pile dans "pile.h"


EXP_ARRITH:

          

			EXP_ARRITH addition EXP_ARRITH     
         {
            op2=Depiler();op1=Depiler(); tmp = op1+op2; Empiler(tmp);

            ajout_quad_opbinaire('+',&op1,&op2);
            
         }
			|EXP_ARRITH substraction EXP_ARRITH   
          {
            op2=Depiler();op1=Depiler(); tmp = op1-op2; Empiler(tmp);
            ajout_quad_opbinaire('-',&op1,&op2);

            }
			|EXP_ARRITH multiplication EXP_ARRITH 
          {
            op2=Depiler();op1=Depiler(); tmp = op1*op2; Empiler(tmp);
            ajout_quad_opbinaire('*',&op1,&op2);
  
          }

			|EXP_ARRITH division EXP_ARRITH 
           {
              
              op2=Depiler();op1=Depiler();
             if(op2==0){
				    printf ("    >>>>>>> Erreur semantique ligne %d colonne %d DIVISION PAR 0 \n",semligne,semcol);
			     }else{

                tmp = op1/op2; Empiler(tmp);
                ajout_quad_opbinaire('/',&op1,&op2);

      
           }}

         |substraction EXP_ARRITH
          {
            op1=Depiler();tmp= -op1; Empiler(tmp);
            ajout_quad_opunaire(&op1);
            }

		 |parenthese_ouvr EXP_ARRITH parenthese_ferm

         |idf      

         // idf1 = idf2 , on doit tester si idf2 existeait deja ! 
         
          { if (doubleDeclaration($1)==0){ 
            {printf("\n=======> Errreur symantique a la ligne %d colonne %d , operand %s non declare\n",semligne,semcol,$1);}}
            else{
            get_value_of_idf($1,&tmp);
             Empiler(tmp);}}


			|cst_int  {Empiler($1);}
			|cst_reel    { Empiler($1);}
			;


BLOC_INST : tabulation INST BLOC_INST | tabulation INST ;
//IF_ELSE : mc_if parenthese_ouvr COND parenthese_ferm deux_Point SAUT tabulation <A> BLOC_INST mc_else deux_Point SAUT tabulation <C>BLOC_INST <D>;


// maj de fin 

IF_ELSE :  B BLOC_INST {
		sprintf(sauvindex,"%d",qc);
		maj_quad(quadindex2,1,sauvindex);


} ; 

// brancher de inst1 vers fin et  maj debut inst 2 
B : A BLOC_INST mc_else deux_Point SAUT  {
	quadindex2=qc;
	quadruplet("BR","","","");
	sprintf(sauvindex,"%d",qc);
	maj_quad(quadindex1,1,sauvindex);
  



}; 

//test condition brancher vers inst2 

A : mc_if parenthese_ouvr COND parenthese_ferm deux_Point SAUT  {
	tmp=Depiler();
	ajout_quad_affect_val("tmp_cond",&tmp);
	quadindex1=qc;
	quadruplet(quad1 ,"","","tmp_cond");
   

};
 

COND : cst_bool OP_LOG1 cst_bool
		{
			tmp= test_on_string(quad1,$1,$3);Empiler(tmp);
		}

	   |cst_car OP_LOG1 cst_car
	   {
			tmp= test_on_string(quad1,$1,$3); Empiler(tmp);

	   }
	   |idf OP_LOG1 cst_bool
	   {
		if(doubleDeclaration($1)==0){
			printf("\n=======> Errreur symantique a la ligne %d colonne %d , idf %s non declare\n",semligne,semcol,$1);
		}else{
		if(type_compatible($1,"BOOLEAN")==0){
			printf("\n=======> Errreur symantique a la ligne %d colonne %d , idf %s n'est pas BOOLEAN \n",semligne,semcol,$1);

		}else{	
			get_valstring_of_idf($1,sauvval);
			tmp= test_on_string(quad1,sauvval,$3); Empiler(tmp);

		}}
	   }
	   |idf OP_LOG1 cst_car 
	   {
		if(doubleDeclaration($1)==0){
			printf("\n=======> Errreur symantique a la ligne %d colonne %d , idf %s non declare\n",semligne,semcol,$1);
		}else{
		if(type_compatible($1,"CHAR")==0){
			printf("\n=======> Errreur symantique a la ligne %d colonne %d , idf %s n'est pas CHAR \n",semligne,semcol,$1);

		}else{	
			get_valstring_of_idf($1,sauvval);
			tmp= test_on_string(quad1,sauvval,$3); Empiler(tmp);

		
	   }}}
	   |EXP_ARRITH equal EXP_ARRITH
	   {
		strcpy(quad1,"BNZ");
		op2=Depiler();op1=Depiler(); tmp=(op1 == op2); Empiler(tmp);
	   }
	   |EXP_ARRITH not_equal EXP_ARRITH
	   {
	   strcpy(quad1,"BZ");
		op2=Depiler();op1=Depiler(); tmp=(op1 != op2); Empiler(tmp);
	   }
	   |EXP_ARRITH inf EXP_ARRITH
	   {
		{
		strcpy(quad1,"BGE");
		op2=Depiler();op1=Depiler(); tmp=(op1 < op2); Empiler(tmp);
	   }
	   }
	   |EXP_ARRITH inf_equal EXP_ARRITH
	   {strcpy(quad1,"BG");
		op2=Depiler();op1=Depiler(); tmp=(op1 <= op2); Empiler(tmp);
		}
	   |EXP_ARRITH sup EXP_ARRITH
	   {
		strcpy(quad1,"BLE");
		op2=Depiler();op1=Depiler(); tmp=(op1 > op2); Empiler(tmp);
	   }
	   |EXP_ARRITH sup_equal EXP_ARRITH {
		strcpy(quad1,"BL");
		op2=Depiler();op1=Depiler(); tmp=(op1 >= op2); Empiler(tmp);
		
	   };

OP_LOG1 : equal {strcpy(quad1,"BNZ");}
		| not_equal {strcpy(quad1,"BZ");};





//WHILE : mc_while parenthese_ouvr <AA> COND parenthese_ferm deux_Point tabulation <BB> INST <CC>
// <AA> -> sauvgarder debut cond 
// <BB> -> test + brancher vers fin 
// <CC> -> brancher vers debut cond +  maj de fin 


WHILE : BB BLOC_INST  {
	sprintf(sauvindex,"%d",quadindex1);
	quadruplet("BR",sauvindex,"","");
	sprintf(sauvindex,"%d",qc);
	maj_quad(quadindex2,1,sauvindex);
   

};
BB: AA COND parenthese_ferm deux_Point SAUT {
	tmp=Depiler();
	ajout_quad_affect_val("tmp_cond",&tmp);
	quadindex2=qc;
	quadruplet(quad1 ,"","","tmp_cond");
};
 AA : mc_while parenthese_ouvr {
	quadindex1 = qc;
	
 }


/*
FOR_RANGE : mc_for idf <AAA> mc_in mc_range parenthese_ouvr E <BBB> virgule E <CCC> parenthese_ferm deux_Point SAUT INST <DDD>;
<AAA> : si idf existe alors tester s' il est bien de type int sinon le creer avec type int 
<BBB> : tester si Exparth retourn un int , si oui affecter valeur a idf sinon erreur 
<CCC> : tester si Expath2 retourn un int , si oui tester si e1 < e2 et brancher fin 
<DDD> : incrementer valeur de idf , brancher vers condition , maj de fin 
*/
FOR_RANGE :CCC parenthese_ferm deux_Point SAUT BLOC_INST{
	inc_val_idf(sauvidf);
   quadruplet("+",sauvidf,"1",sauvidf);
	sprintf(quad2,"%d",quadindex1);
	quadruplet("BR",quad2,"","");
	sprintf(quad2,"%d",qc);

	maj_quad(quadindex1,1,quad2);
	
	
};

CCC: BBB virgule EXP_ARRITH {

	op2=Depiler();
	if(Is_int(&op2)==0){
		printf("\n=======> Errreur symantique a la ligne %d colonne %d ,  %f n'est pas INT \n",semligne,semcol,tmp); return 1;

	}else{

		quadindex1=qc;
		quadruplet("BG","","","tmp_cond");
	}
};
BBB : AAA mc_in mc_range parenthese_ouvr EXP_ARRITH{
	op1=Depiler();
	if(Is_int(&op1)==0){
		printf("\n=======> Errreur symantique a la ligne %d colonne %d ,  %f n'est pas INT \n",semligne,semcol,tmp);return 1;

	}else{
		set_value_of_idf(sauvidf,&tmp);
	}
};

AAA : mc_for idf
{
	if(doubleDeclaration($2)==0){
		Inserer_type($2,"INTEGER");
		strcpy(sauvidf,$2);

		}else{
		get_type_of_idf($2,sauvidf);
		if(strcmp(sauvidf,"INTEGER")==0){
			strcpy(sauvidf,$2);

		}else{
			printf("\n=======> Errreur symantique a la ligne %d colonne %d , idf %s n'est pas INT \n",semligne,semcol,$2);return 1;

		
	}}
};  





//COND_MULT : AND_OR COND| ;

//AND_OR : mc_or | mc_and ;

//FOR: mc_for idf mc_in idf deux_Point SAUT BLOC_INST;

%%

main()
{ 
   initialisation();
   yyparse();
   afficher();
   afficher_qdr();
}
yywrap()
{
   return 1;
}
int yyerror(char *msg)
{
  printf("\n   =====> Erreur Syntaxique  \n au niveau la ligne %d et a la colonne %d \n", semligne,semcol);
   return 1;
}


