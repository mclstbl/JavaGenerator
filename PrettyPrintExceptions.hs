module PrettyPrintExceptions where
{-	
	JavaMatrix types are processed and produce Strings which are part
	of the final Java code
	All functions here produce Strings because they directly go into the template
	This module makes Strings that make up the Exception files
-}
import AST (
	Arrangement(..),
	ClassName(..),
	Constructors(..),
	DeterminantMethod(..),
	Errors(..),
	Format(..),
	JavaMatrix(..),
	makeJavaData,
	Matrix(..),
	Variables
	) 
import Data.List

makeExceptions :: Matrix -> JavaMatrix -> [String]
makeExceptions m (MS _ _ e _ _) =
	[
	"package assignment3;\n",

	"public class " ++ (show e) ++ " extends Exception{\n",

    "\tprivate static final long serialVersionUID = 1L;\n",

    "\tpublic " ++ (show e) ++ " () { }",
    "\tpublic " ++ (show e) ++ " (String message) { super (message); }",
    "\tpublic " ++ (show e) ++ " (Throwable cause) { super (cause); }",
    "\tpublic " ++ (show e) ++ " (String message, Throwable cause) {",
    "\t\tsuper (message, cause);",
   	"\t}",
	"}"
	]

