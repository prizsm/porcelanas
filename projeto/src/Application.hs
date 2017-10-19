{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes       #-}

module Application where

import Yesod
import Foundation
import Home
import Handler.Usr
import Handler.Estoqueporcelana
import Handler.Estoquemoldura
import Handler.Falecom
import Handler.Orcporcelana
import Handler.Orcmoldura
import Handler.Pedidomoldura
import Handler.Pedidoporcelana
------------------
mkYesodDispatch "App" resourcesApp

getAdmR :: Handler Html
getAdmR = do
    sess <- lookupSession "_ID"
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
            <h2 align="center"> Área Administrativa
                $maybe _ <- sess
                    <li> 
                        <form action=@{LogoutR} method=post>
                            <input type="submit" value="Logout">
                $nothing
                    <li> <a href=@{LoginR}>Login
        |]        
        $(whamletFile "templates/footer.hamlet")
        
getEnderecoR :: Handler Html
getEnderecoR = do
    defaultLayout $ do
        setTitle "Cidinha Porcelanas"
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
        addStylesheet $ StaticR css_estilo_css
        toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        |]
        $(whamletFile "templates/menu2.hamlet")
        $(whamletFile "templates/endereco.hamlet")
        $(whamletFile "templates/footer.hamlet")
        
getSobreR :: Handler Html
getSobreR = do
    defaultLayout $ do
        setTitle "Cidinha Porcelanas"
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
        addStylesheet $ StaticR css_estilo_css
        toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        |]
        $(whamletFile "templates/menu2.hamlet")
        $(whamletFile "templates/footer.hamlet")
        
getPortfolioR :: Handler Html
getPortfolioR = do
    defaultLayout $ do
        setTitle "Cidinha Porcelanas"
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
        addStylesheet $ StaticR css_estilo_css
        toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        |]
        $(whamletFile "templates/menu2.hamlet")
        $(whamletFile "templates/footer.hamlet")