```sh 
stack clean && stack build && stack exec projeto


porcelana/editar/muda/#EstoqueporcelanaId                 EditEstoqueporcelanaR  GET POST
porcelana/apagar/#EstoqueporcelanaId                      DelEstoqueporcelanaR   POST
porcelana/editar/alterar/#EstoqueporcelanaId              AlterarestporcelanaR   GET


perimetro :: Orcmoldura -> Orcmoldura -> Double
perimetro orcmolduraLado1 orcmolduraLado2 = (orcmolduraLado1 + orcmolduraLado2) * 2

area :: Orcmoldura -> Orcmoldura -> Double
area orcmolduraLado1 orcmolduraLado2 = orcmolduraLado1 * orcmolduraLado2

somoldura :: Orcmoldura -> Double -> Double
somoldura orcmolduraModelo perimetro = orcmolduraModelo * perimetro

molduravar :: Double -> Orcmoldura -> Double -> Double
molduravar perimetro orcmolduraModelo area = (perimetro * orcmolduraModelo) + (area * 250)

molduravc :: Double -> Orcmoldura -> Double -> Double
molduravc perimetro orcmolduraModelo area = (perimetro * orcmolduraModelo) + (area * 200)

molduraespelho :: Double -> Orcmoldura -> Double -> Double
molduraespelho perimetro orcmolduraModelo area = (perimetro * orcmolduraModelo) + (area * 400)


```# porcelanas
