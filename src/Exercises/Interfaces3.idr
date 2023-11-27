module Exercises.Interfaces3

-- 1
record Complex where
  constructor MkComplex
  real : Double
  imaginary : Double

someComplex : Complex
someComplex = MkComplex 2.0 3.0

implementation Eq Complex where
  (==) complex_a complex_b = 
    complex_a.real == complex_b.real && complex_a.imaginary == complex_b.imaginary

implementation Num Complex where
  (+) complex_a complex_b =
    MkComplex (complex_a.real + complex_b.real) (complex_a.imaginary + complex_b.imaginary)
  
  (*) (MkComplex r1 i1) (MkComplex r2 i2) =
    MkComplex (r1 * r2 - i1 * i2) (r1 * i2 + r2 * i1)

  fromInteger num = MkComplex (cast num) 0.0

implementation Neg Complex where
  negate (MkComplex r i) = MkComplex (-r) (-i)

  (-) complex_a complex_b =
    MkComplex (complex_a.real - complex_b.real) (complex_a.imaginary - complex_b.imaginary)

implementation Fractional Complex where
  (/) (MkComplex r1 i1) (MkComplex r2 i2) = MkComplex (r1 / r2) (i1 / i2)

  (/) (MkComplex r1 i1) (MkComplex r2 i2) = 
    let
      denominator = (r2 * r2 + i2 * i2)
    in
      MkComplex ((r1 * r2 + i1 * i2) / denominator)
                ((r1 * i2 - r1 * i2) / denominator)
                
  recip (MkComplex r i) = MkComplex r (-i)


-- 2
-- implementation Show Complex where   
