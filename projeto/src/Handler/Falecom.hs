{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Falecom where

import Foundation
import Yesod
import Yesod.Static
import Data.Text
import Control.Applicative
import Database.Persist
import Database.Persist.Postgresql

formFalecom :: Form Falecom
formFalecom = renderDivs $ Falecom
    <$> areq textField "Nome:  "           Nothing
    <*> areq intField  "Telefone:  "       Nothing
    <*> areq textField "Email:  "          Nothing
    <*> areq textField "Mensagem:  "       Nothing

getFalecomR :: Handler Html
getFalecomR = do
            (widget, enctype) <- generateFormPost formFalecom
            defaultLayout $ do
                setTitle "Cidinha Porcelanas"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
                addStylesheet $ StaticR css_estilo_css
                toWidgetHead [hamlet|
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1.0">
                |]
                $(whamletFile "templates/menu2.hamlet")
                [whamlet|
                    <div id="contato1"><h1>Contato</h1><form method=post action=@{FalecomR} enctype=#{enctype}>
                        ^{widget}
                        <input class="botao2" type="submit" value="Enviar">
                |]
                $(whamletFile "templates/footer.hamlet")

postFalecomR :: Handler Html
postFalecomR = do
            ((result, _), _) <- runFormPost formFalecom
            case result of
                FormSuccess falecom -> do
                    fcid <- runDB $ insert falecom
                    defaultLayout $ do
                        setTitle "Cidinha Porcelanas"
                        addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
                        addStylesheet $ StaticR css_estilo_css
                        toWidgetHead [hamlet|
                        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
                        <meta name="viewport" content="width=device-width, initial-scale=1.0">
                        |]
                        $(whamletFile "templates/menu2.hamlet")
                        [whamlet|
                            <h2 align="center"> Contato enviado com sucesso!!
                        |]
                        $(whamletFile "templates/footer.hamlet")
                _ -> redirect HomeR

getListFalecomR :: Handler Html
getListFalecomR = do
            contatos <- runDB $ selectList [] [Asc FalecomNome]
            defaultLayout $(whamletFile "templates/tabela1.hamlet")
                
postDelFalecomR :: FalecomId -> Handler Html
postDelFalecomR fcid = do 
                runDB $ delete fcid
                redirect ListFalecomR

menu2 :: Widget
menu2 = do
    addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
    addStylesheet $ StaticR css_estilo_css
    toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
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