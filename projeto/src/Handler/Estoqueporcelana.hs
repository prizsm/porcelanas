{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Estoqueporcelana where

import Foundation
import Yesod
import Yesod.Static
import Data.Text
import Control.Applicative
import Database.Persist (replace)
import Database.Persist.Postgresql
import Data.Text

formEstoqueporcelana :: Form Estoqueporcelana
formEstoqueporcelana = renderDivs $ Estoqueporcelana
    <$> areq intField  "Código:  "       Nothing
    <*> areq textField "Tipo:  "         Nothing
    <*> areq textField "Sub-Tipo:  "     Nothing
    <*> areq textField "Tamanho:  "      Nothing
    <*> areq intField  "Quantidade:  "   Nothing
    <*> areq textField "Observação:  "   Nothing

getEstoqueporcelanaR :: Handler Html
getEstoqueporcelanaR = do
            (widget, enctype) <- generateFormPost formEstoqueporcelana
            defaultLayout $ do
                setTitle "Cidinha Porcelanas"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
                addStylesheet $ StaticR css_estilo_css
                toWidgetHead [hamlet|
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1.0">
                |]
                $(whamletFile "templates/menu3.hamlet")
                [whamlet|
                     <h2 align="center"> Cadastrar Estoque Porcelanas
                     <form method=post action=@{EstoqueporcelanaR} enctype=#{enctype}>
                         ^{widget}
                         <input type="submit" value="Cadastrar">
                |]
                

postEstoqueporcelanaR :: Handler Html
postEstoqueporcelanaR = do
            ((result, _), _) <- runFormPost formEstoqueporcelana
            case result of
                FormSuccess estoqueporcelana -> do
                    porcid <- runDB $ insert estoqueporcelana
                    defaultLayout [whamlet|
                        <h2 align="center">Louça Cadastrada!!
                    |]
                _ -> redirect HomeR

getListEstoqueporcelanaR :: Handler Html
getListEstoqueporcelanaR = do
            porcelanas <- runDB $ selectList [] [Asc EstoqueporcelanaCodigo]
            defaultLayout $(whamletFile "templates/tabela3.hamlet")

postDelEstoqueporcelanaR :: EstoqueporcelanaId -> Handler Html
postDelEstoqueporcelanaR porcid = do 
                runDB $ delete porcid
                redirect ListEstoqueporcelanaR

getEditarQuantidadeEstoqueR :: EstoqueporcelanaId -> Handler Html
getEditarQuantidadeEstoqueR porcid = do
    porcelanas <- runDB $ selectList [EstoqueporcelanaId ==. porcid] []
    defaultLayout [whamlet|
        ^{menu3}
        $forall Entity porcid estoqueporcelana <- porcelanas
            <h2 align="center">Editar Estoque de Porcelanas</h2>
                <tr>
                    <form action=@{EditarQuantidadeEstoqueR porcid} method=post>
                        <td>Código: #{show (estoqueporcelanaCodigo estoqueporcelana)}<br>
                        <td>Tipo: #{unpack (estoqueporcelanaTipo estoqueporcelana)}<br>
                        <td>Sub Tipo #{unpack (estoqueporcelanaSubtipo estoqueporcelana)}<br>
                        <td>Tamanho: #{unpack (estoqueporcelanaTamanho estoqueporcelana)}<br>
                        <td>Quantidade: <input type="number" name="estoqueporcelanaQuantidade" value=#{show (estoqueporcelanaQuantidade estoqueporcelana)}><br>
                        <td>Observação: <input type="text" name="estoqueporcelanaObservacao" value=#{unpack (estoqueporcelanaObservacao estoqueporcelana)}><br>
                        <td><input class="botao2" type="submit" value="Alterar">
    |]
    
postEditarQuantidadeEstoqueR :: EstoqueporcelanaId -> Handler Html
postEditarQuantidadeEstoqueR porcid = do
            (estoqueporcelanaQuantidade, estoqueporcelanaObservacao) <- runInputPost $ (,)
                <$> ireq intField "estoqueporcelanaQuantidade"
                <*> ireq textField "estoqueporcelanaObservacao"
            porcelanas <- runDB $ update porcid [EstoqueporcelanaQuantidade =. estoqueporcelanaQuantidade, EstoqueporcelanaObservacao =. estoqueporcelanaObservacao]
            redirect ListEstoqueporcelanaR

menu3 :: Widget
menu3 = do
    addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
    addStylesheet $ StaticR css_estilo_css
    toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
    |]
    $(whamletFile "templates/menu3.hamlet")