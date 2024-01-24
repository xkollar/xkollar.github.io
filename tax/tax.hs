import Data.Fixed (Centi, resolution)

type Money = Centi

trunc a = a / fr * fr
  where
    fr = fromInteger $ resolution a

go m ((b, r1):(e, r2):s) = m'*r1 + go (m-m') ((e,r2):s)
  where
    m' = min (e-b) m
go m [(_, r)] = m*r

income_tax :: Money -> Money
income_tax income = go income l
  where
    band_00 = max 0 (12570 - trunc (max 0 (income - 100000) / 2))
    band_20 = band_00 + 37700
    band_40 = 125140
    l =
        [ (0,       0.00)
        , (band_00, 0.20)
        , (band_20, 0.40)
        , (band_40, 0.45)
        ]

national_insurance :: Money -> Money
national_insurance income = go income l
  where
    l =
        [(0,     0.00)
        ,(12570, 0.12)
        ,(50270, 0.02)
        ]

main = writeFile "tax-effective-and-perceived-2023-2024.dat"
    . unlines
    . map (\ i -> unwords $ map show [i, national_insurance i, income_tax i])
    . takeWhile (<=200000)
    $ iterate (+100) 0
