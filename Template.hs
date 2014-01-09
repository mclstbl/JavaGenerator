module Template where
{-	
	The "fails if length is not 9" restriction can be removed
	by removing the 3x3 constructor for nxn records in
	PrettyPrintConstructors.hs
-}
import AST (
	Arrangement(..),
	ClassName(..),
	Constructors(..),
	DeterminantMethod(..),
	Errors(..),
	Format(..),
	getClassName,
	JavaMatrix(..),
	makeJavaData,
	Matrix(..),
	Variables
	)
import PrettyPrintConstructors
import PrettyPrintDeterminant
import PrettyPrintVariablesAndErrors

followTemplate :: Matrix -> [String]
followTemplate m =
	["package assignment3;\n"] ++
	["public class " ++ (getClassName ms) ++ "{"] ++
	(makeVariables m ms) ++ 
	[""] ++
	(makeConstructors m ms) ++ 
	[""] ++
	(makeDeterminantMethod m ms) ++ 
	["}"] ++ 
	(makeHelperClass m ms)
	where ms = makeJavaData m

showErrors :: Errors -> IO()
showErrors e = putStrLn $ show e
