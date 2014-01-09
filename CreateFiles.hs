module CreateFiles where

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
import Data.List
import System.IO
import System.Directory
import System.FilePath.Posix
import Template (followTemplate)
import PrettyPrintExceptions (makeExceptions)

main = do
	createDirectoryIfMissing True "assignment3"
	file 	<- openFile "testingTest.java" ReadMode
	content <- readFile "testingTest.java"
	let xs = lines content
	setCurrentDirectory "assignment3"
	writeFile "testingTest.java" (unlines xs)
	writeAllFiles [a,b,c,d,e,f,g,h,i,j]
	writeFile "NotSquareMatrixException.java" (unlines $ makeExceptions (a) (makeJavaData a)) --InvalidArrayLengthException)
	hClose file
	where
		a = M Record Flat 1
		b = M Record Flat 6
		c = M Record RC 3
		d = M Record RC 6
		e = M Record CR 2
		f = M Record CR 6
		g = M Record Flat 8
		h = M Array Flat 6
		i = M Array RC 6
		j = M Array CR 6

writeAllFiles :: [Matrix] -> IO()
writeAllFiles [] = putStrLn "All Files Have been printed and are in the assignment3 directory"
writeAllFiles (f:fs) = do
	writeFile ((getClassName (makeJavaData f)) ++ ".java") (unlines $ followTemplate f)
	writeAllFiles fs










