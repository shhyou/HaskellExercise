module Main

(>>) : Monad m => m a -> m b -> m b
(>>) m1 m2 = m1 >>= \_ => m2

data Fmt : Type where
  FmtInt : Fmt
  FmtDbl : Fmt
  FmtStr : Fmt
  FmtIgn : (x : Char) -> Fmt

mkFmt : (fmt : List Char) -> List Fmt
mkFmt [] = []
mkFmt ('%'::'d'::xs) = FmtInt::mkFmt xs
mkFmt ('%'::'f'::xs) = FmtDbl::mkFmt xs
mkFmt ('%'::'s'::xs) = FmtStr::mkFmt xs
mkFmt (x::xs) = FmtIgn x::mkFmt xs

mkPrintfTy : List Fmt -> Type
mkPrintfTy [] = IO ()
mkPrintfTy (FmtInt::fs) = Int -> mkPrintfTy fs
mkPrintfTy (FmtDbl::fs) = Float -> mkPrintfTy fs
mkPrintfTy (FmtStr::fs) = String -> mkPrintfTy fs
mkPrintfTy (FmtIgn _::fs) = mkPrintfTy fs

mkPrintf : (fmt : List Fmt) -> (acc : IO ()) -> mkPrintfTy fmt
mkPrintf [] acc = acc
mkPrintf (FmtInt::fs) acc = \n => mkPrintf fs (acc >> putStr (show n))
mkPrintf (FmtDbl::fs) acc = \f => mkPrintf fs (acc >> putStr (show f))
mkPrintf (FmtStr::fs) acc = \s => mkPrintf fs (acc >> putStr s)
mkPrintf (FmtIgn x::fs) acc = mkPrintf fs (acc >> putChar x)

printf : (fmt : String) -> mkPrintfTy (mkFmt (unpack fmt))
printf fmt = mkPrintf (mkFmt (unpack fmt)) (return ())

main : IO ()
main = do
  printf "Hello, %s! It's %d/%d/%d.\nWelcome to %s!" "John" 2014 1 9 "Idris"
