{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Orcmoldura where

import Foundation
import Yesod
import Yesod.Static
import Data.Text
import Control.Applicative
import Database.Persist
import Database.Persist.Postgresql

formOrcmoldura :: Form Orcmoldura
formOrcmoldura = renderDivs $ Orcmoldura
    <$> areq doubleField  "Tamanho do lado 1:  "       Nothing
    <*> areq doubleField  "Tamanho do lado 2:  "       Nothing
    <*> areq (selectFieldList modelo) "Modelo escolhido:  " Nothing
    <*> areq (selectFieldList paspatour) "Paspatour:  " Nothing

modelo :: [(Text, Double)]
modelo = [("004", 0), ("024", 18.95), ("028", 18.95), ("032", 18.95), ("043", 18.95), ("094", 18.95), ("095", 18.95), ("275", 18.95), ("320", 18.95), ("401", 18.95), ("416", 18.95), ("418", 18.95), ("419", 18.95), ("426", 18.95), ("428", 18.95), ("432", 18.95), ("438", 18.95), ("440", 18.95), ("511", 18.95), ("523", 18.95), ("712", 18.95), ("724", 18.95), ("765", 18.95), ("766", 18.95), ("767", 18.95), ("769", 18.95), ("775", 18.95), ("794", 18.95), ("802", 18.95), ("810", 18.95)]

paspatour :: [(Text, Double)]
paspatour = [("Não", 0), ("Sim", 18.95)]

getOrcmolduraR :: Handler Html
getOrcmolduraR = do
            (widget, enctype) <- generateFormPost formOrcmoldura
            defaultLayout $ do
                setTitle "Cidinha Porcelanas"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
                addStylesheet $ StaticR css_estilo_css
                toWidgetHead [hamlet|
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1.0">
                <base href="https://cidinha-prizsorensen.c9users.io/">
                |]
                $(whamletFile "templates/menu2.hamlet")
                [whamlet|
                    <div class="orcamento1"><h2>Orçamento de Molduras</h2><form method=post action=@{OrcmolduraR} enctype=#{enctype}>
                        ^{widget}
                        <input class="botao2" type="submit" value="Fazer Orçamento"></div>
                |]
                $(whamletFile "templates/footer.hamlet")

postOrcmolduraR :: Handler Html
postOrcmolduraR = do
            ((result, _), _) <- runFormPost formOrcmoldura
            case result of
                FormSuccess orcmoldura -> do
                    orcmolid <- runDB $ insert orcmoldura
                    defaultLayout $ do
                        redirect HomeR
                _ -> redirect HomeR

getOrcmoldurafinalR :: OrcmolduraId -> Handler Html
getOrcmoldurafinalR orcmolid = do
            orcmoldura <- runDB $ get404 orcmolid
            defaultLayout $(whamletFile "templates/orcmoldurafinal.hamlet")

getListOrcmolduraR :: Handler Html
getListOrcmolduraR = do
            orcmolduras <- runDB $ selectList [] [Desc OrcmolduraId]
            defaultLayout $(whamletFile "templates/tabela5.hamlet")

postDelOrcmolduraR :: OrcmolduraId -> Handler Html
postDelOrcmolduraR orcmolid = do 
                runDB $ delete orcmolid
                redirect ListOrcmolduraR

menu2 :: Widget
menu2 = do
    setTitle "Cidinha Porcelanas"
    addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
    addStylesheet $ StaticR css_estilo_css
    toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <base href="https://cidinha-prizsorensen.c9users.io/">
    |]
    $(whamletFile "templates/menu2.hamlet")
    
menu3 :: Widget
menu3 = do
    addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
    addStylesheet $ StaticR css_estilo_css
    toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
    |]
    $(whamletFile "templates/menu3.hamlet")
    
rodape :: Widget
rodape = do
    $(whamletFile "templates/footer.hamlet")
