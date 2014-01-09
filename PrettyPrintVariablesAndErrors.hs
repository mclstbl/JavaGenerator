module PrettyPrintVariablesAndErrors where
{-	
	JavaMatrix types are processed and produce Strings which are part of the final Java code
	all functions here produce Strings because they directly go into the template
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

makeVariables :: Matrix -> JavaMatrix -> [String]
{-	
	This pattern creates initial variable declarations for all Record types
-}

makeVariables (M Record _ _) (MS _ [m,a,b1,b2,cl,v,v1,j2,v2] e c det) = 
    [
    "\tprivate " ++ cl ++ " Matrix = new " ++ cl ++ "();",
    "\tprivate int n = 0;",
    "\tprivate int n2 = 0;",
    "\tprivate NotSquareMatrixException Error1;"
    ]
{- 
	These patterns create variable declarations for all Array type matrices
-}
makeVariables (M Array Flat _) (MS _ [m,a,b1,b2,cl,v,v1,j2,v2] e c det) = 
	[
    "\tprivate long[] " ++ m ++ v2 ++ ";",
    "\tprivate int n = 0;",
    "\tprivate int n2 = 0;",
    "\tprivate NotSquareMatrixException Error1;"
    ]

makeVariables (M Array _ _) (MS _ [m,a,b1,b2,cl,v,v1,j2,v2] e c det) = 
	[
    "\tprivate long[][] " ++ m ++ ";",
    "\tprivate long[] " ++ a ++ ";",
    "\tprivate int n = 0;",
    "\tprivate int n2 = 0;",
    "\tprivate NotSquareMatrixException Error1;"
    ]

makeHelperClass :: Matrix -> JavaMatrix -> [String]
{-
	Only applicable to RECORD types
	These are extra classes that hold their records
-}
makeHelperClass (M Record Flat d) (MS _ [m,a,b1,b2,cl,v,v1,j2,v2] e c det) = 
	[
	"class " ++ cl ++ " {"
	] ++
	(zipWith (++)
		(map (("\tlong " ++ v) ++ ) 
			(zipWith (++)
				(foldl (++) ([]) $ map (replicate d) (map show [1..d]))
				((cycle $ map show [1..d]))
			)
		)
		(replicate (d * d) ";")
	) ++
	[
	"}"
	]

makeHelperClass (M Record _ d) (MS _ [m,a,b1,b2,cl,v,v1,j2,v2] e c det) = 
	[
	"class " ++ cl ++ " {"
	] ++
	(zipWith (++)
		(map (("\t" ++ v ++ " " ++ v1) ++) (map show [1..d]))
		(replicate d (" = new " ++ v ++ "();"))
	) ++
	[
	"}\n"
	] ++

	[
	"class " ++ v ++ " {"
	] ++
    (map (("\tlong " ++ v2) ++) 
    	(zipWith (++)
    		(map show [1..d])
    		(replicate d ";")
    	)
    ) ++
    [
    "}"
    ]

makeHelperClass _ _ = []












   