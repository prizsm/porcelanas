{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Orcporcelana where

import Foundation
import Yesod
import Yesod.Static
import Data.Text
import Control.Applicative
import Database.Persist
import Database.Persist.Postgresql

formOrcporcelana :: Form Orcporcelana
formOrcporcelana = renderDivs $ Orcporcelana
    <$> areq (selectFieldList comer) "Louças para solidos:  " Nothing
    <*> areq intField  "Quantidade:  "       Nothing
    <*> areq (selectFieldList beber) "Louças para líquidos:  " Nothing
    <*> areq intField  "Quantidade:  "       Nothing
    <*> areq (selectFieldList outros) "Outros Utensílios:  " Nothing
    <*> areq intField  "Quantidade:  "       Nothing
    <*> areq (selectFieldList arte) "Arte a ser desenvolvida:  " Nothing

comer :: [(Text, Double)]
comer = [("Nenhum", 0), ("Boll - Pequeno", 18.95), ("Boll - Médio", 18.95), ("Boll - Grande", 18.95), ("Bule (com tampa) - Médio", 18.95), ("Bule (com tampa) - Grande", 18.95), ("Prato de Bolo", 18.95), ("Prato Fundo", 18.95), ("Prato de Pão", 18.95), ("Prato Raso", 18.95), ("Prato de Sobremesa", 18.95)]

beber :: [(Text, Double)]
beber = [("Nenhum", 0), ("Caneca - Pequena", 18.95), ("Caneca - Média", 18.95), ("Caneca - Grande", 18.95), ("Consumê (com pires)", 18.95), ("Copo de Vidro - Pequeno (shot)", 18.95), ("Copo de Vidro - Médio", 18.95), ("Copo de Vidro - Grande", 18.95), ("Leiteira", 18.95), ("Moringa - Média", 18.95), ("Moringa - Grande", 18.95), ("Taça", 18.95), ("Xícara (com pires) - Pequena", 18.95), ("Xícara (com pires) - Média", 18.95), ("Xícara (com pires) - Grande", 18.95)]

outros :: [(Text, Double)]
outros = [("Nenhum", 0), ("Cinzeiro", 18.95), ("Porta Guardanapo", 18.95), ("Porta Temperos", 18.95), ("Potes (com tampa) - Pequeno", 18.95), ("Potes (com tampa) - Médio", 18.95), ("Potes (com tampa) - Grande", 18.95)]

arte :: [(Text, Double)]
arte = [("Nenhuma", 0), ("Pintura a mão", 18.95), ("Decalque Personalizado", 18.95), ("Decalque do acervo", 18.95)]

getOrcporcelanaR :: Handler Html
getOrcporcelanaR = do
            (widget, enctype) <- generateFormPost formOrcporcelana
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
                    <div class="orcamento1"><h2>Orçamento de Porcelanas</h2><form method=post action=@{OrcporcelanaR} enctype=#{enctype}>
                        ^{widget}
                        <input class="botao2" type="submit" value="Fazer orçamento"></div>
                |]
                $(whamletFile "templates/footer.hamlet")

postOrcporcelanaR :: Handler Html
postOrcporcelanaR = do
            ((result, _), _) <- runFormPost formOrcporcelana
            case result of
                FormSuccess orcporcelana -> do
                    orcporcid <- runDB $ insert orcporcelana
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
                                <div class="orcamento1"><h2>COLOCAR DADOS DO ORÇAMENTO!!
                                    <p>Teste
                        |]
                        $(whamletFile "templates/footer.hamlet")
                _ -> redirect HomeR

getOrcporcelanafinalR :: OrcporcelanaId -> Handler Html
getOrcporcelanafinalR orcporcid = do
             orcporcelana <- runDB $ get404 orcporcid 
             defaultLayout $(whamletFile "templates/orcporcelanafinal.hamlet")
             
getListOrcporcelanaR :: Handler Html
getListOrcporcelanaR = do
            orcporcelanas <- runDB $ selectList [] [Desc OrcporcelanaId]
            defaultLayout $(whamletFile "templates/tabela4.hamlet")
                
postDelOrcporcelanaR :: OrcporcelanaId -> Handler Html
postDelOrcporcelanaR orcporcid = do 
                runDB $ delete orcporcid
                redirect ListOrcporcelanaR

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
    
rodape :: Widget
rodape = do
    $(whamletFile "templates/footer.hamlet")
