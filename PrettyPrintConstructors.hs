module PrettyPrintConstructors where
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

makeConstructors :: Matrix -> JavaMatrix -> [String]
makeConstructors (M Record _ d) (MS className [m,a,b1,b2,cl,v,v1,j2,v2] e c det) = 
	[
	"\tpublic " ++ className ++ "(long[] array, int general) throws NotSquareMatrixException {",
    "\t\tn2 = array.length;",
    "\t\tn = (int)Math.sqrt(n2);",
    "\t\tif((n2 == Math.pow((double)n, 2)) && (general == 1)) {"
    ] ++
    (zipWith (++)
		(zipWith (++)	
			(foldr (++) [] $ map (replicate d) $ map (("\t\t\t" ++ m) ++) $ map show [1..d])
			(foldr (++) [] $ replicate d $ map ((b1 ++ a) ++) $ map show [1..d])
		)
		(zipWith (++)
			(map (" = array[" ++) (map show [0..((d * d) -1)]))
			(replicate (d * d) "];")
		)
	) ++
	[
	"\t\t} else {",
    "\t\t\tthrow new NotSquareMatrixException(Error1);",
    "\t\t}",
    "\t}"
    ]

makeConstructors (M Array Flat d) (MS className [m,a,b1,b2,i,j,k,v2,j2] e constructor det) = 
	[
	"\tpublic " ++ className ++ "(long[] array, int general) throws NotSquareMatrixException {",
    "\t\tn2 = array.length;",
    "\t\tn = (int)Math.sqrt(n2);\n",
        
    "\t\tif((n2 == Math.pow((double)n, 2)) && (general == 1)) {",
    "\t\t\t" ++ m ++ j2 ++ " = new long[n2];",
    "\t\t\tSystem.arraycopy(array, 0, " ++ m ++ j2 ++ ", 0, n2);",
    "\t\t} else {",
    "\t\t\tthrow new NotSquareMatrixException(Error1);",
    "\t\t}",
    "\t}"
    ]

makeConstructors (M Array _ d) (MS className [r,c,b1,b2,i,j,k,j2,v2] e constructor det) = 
	[
	"\tpublic " ++ className ++ "(long[] array, int general) throws NotSquareMatrixException {",
    "\t\tn2 = array.length;",
    "\t\tn = (int)Math.sqrt(n2);\n",
        
    "\t\tif(n2 == Math.pow((double)n, 2) && (general == 1)) {",
    "\t\t\t" ++ r ++ " = new long[n][n];",
    "\t\t\t" ++ c ++ " = new long[n];\n",
            
    "\t\t\tfor(int " ++ i ++ " = 0; " ++ i ++ " < n; " ++ i ++ "++) {",
    "\t\t\t\tfor(int " ++ j ++ " = 0; " ++ j ++ " < n; " ++ j ++ "++) {",
    "\t\t\t\t\t" ++ c ++ "[" ++ j ++ "]" ++ " = array[n * i + j];",
    "\t\t\t\t}",
    "\t\t\t\tSystem.arraycopy(" ++ c ++ ", 0, " ++ r ++ "[" ++ i ++ "]" ++ ", 0, n);",
    "\t\t\t}",
    "\t\t} else {",
    "\t\t\t\tthrow new NotSquareMatrixException(Error1);",
    "\t\t\t}",
    "\t\t}"
    ]
