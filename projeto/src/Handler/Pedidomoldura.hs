{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Pedidomoldura where

import Foundation
import Yesod
import Yesod.Static
import Data.Text
import Control.Applicative
import Database.Persist
import Database.Persist.Postgresql
import Data.Text

formPedidomoldura :: Form Pedidomoldura
formPedidomoldura = renderDivs $ Pedidomoldura
    <$> areq (selectFieldList status)  "Status: "            Nothing
    <*> areq textField  "Nome do Cliente: "                  Nothing
    <*> areq textField  "Telefone: "                         Nothing
    <*> areq doubleField  "Lado 1: "                         Nothing
    <*> areq doubleField  "Lado 2: "                         Nothing
    <*> areq intField  "Quantidade: "                        Nothing
    <*> areq (selectFieldList modelo)  "Modelo: "            Nothing
    <*> areq textField  "Paspatur: "                         Nothing
    <*> areq (selectFieldList vidro)  "Vidro: "              Nothing
    <*> areq textField  "Data de Entrada: "                  Nothing
    <*> areq textField  "Data de Entrega: "                  Nothing
    <*> areq textField  "Observação: "                       Nothing
    <*> areq textField  "Forma de Pagamento: "               Nothing
    <*> areq doubleField  "Valor Final: "                    Nothing

status :: [(Text, Text)]
status = [("Em Andamento", "Em Andamento"), ("Finalizado", "Finalizado")]

modelo :: [(Text, Text)]
modelo = [("004", "004"), ("024", "024"), ("028", "028"), ("032", "032"), ("043", "043"), ("094", "094"), ("095", "095"), ("275", "275"), ("320", "320"), ("401", "401"), ("416", "416"), ("418", "418"), ("419", "419"), ("426", "426"), ("428", "428"), ("432", "432"), ("438", "438"), ("440", "440"), ("511", "511"), ("523","523"), ("712", "712"), ("724", "724"), ("765", "765"), ("766", "766"), ("767","767"), ("769", "765"), ("775", "775"), ("794", "795"), ("802", "802"), ("810", "810")]

vidro :: [(Text, Text)]
vidro = [("Vidro Comum", "Vidro Comum"), ("Vidro Antireflexo", "Vidro Antireflexo"), ("Sem Vidro", "Sem Vidro"), ("Espelho", "Espelho")]

getPedidomolduraR :: Handler Html
getPedidomolduraR = do
            (widget, enctype) <- generateFormPost formPedidomoldura
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
                     <h2 align="center"> Cadastrar Pedido molduras
                     <form method=post action=@{PedidomolduraR} enctype=#{enctype}>
                         ^{widget}
                         <input type="submit" value="Cadastrar">
                |]
                

postPedidomolduraR :: Handler Html
postPedidomolduraR = do
            ((result, _), _) <- runFormPost formPedidomoldura
            case result of
                FormSuccess pedidomoldura -> do
                    pedmolid <- runDB $ insert pedidomoldura
                    redirect AdmR
                _ -> redirect HomeR

getListPedidomolduraR :: Handler Html
getListPedidomolduraR = do
            molduras <- runDB $ selectList [] [Asc PedidomolduraStatus]
            defaultLayout $(whamletFile "templates/tabela7.hamlet")

postDelPedidomolduraR :: PedidomolduraId -> Handler Html
postDelPedidomolduraR pedmolid = do 
                runDB $ delete pedmolid
                redirect ListPedidomolduraR

getEditarQtdePedidoR :: PedidomolduraId -> Handler Html
getEditarQtdePedidoR pedmolid = do
    molduras <- runDB $ selectList [PedidomolduraId ==. pedmolid] []
    defaultLayout [whamlet|
        ^{menu3}
        $forall Entity pedmolid pedidomoldura <- molduras
            <h2 align="center">Editar Pedido de molduras</h2>
                <tr>
                    <form action=@{EditarQtdePedidoR pedmolid} method=post>
                        <td>Status: <input type="text" name="pedidomolduraStatus" value=#{unpack (pedidomolduraStatus pedidomoldura)}><br>
                        <td>Nome Cliente:  #{unpack (pedidomolduraClientenome     pedidomoldura)}<br>
                        <td>Telefone: #{unpack (pedidomolduraClientetelefone pedidomoldura)}<br>
                        <td>Lado 1: #{show (pedidomolduraLado1  pedidomoldura)}<br>
                        <td>Lado 2: #{show (pedidomolduraLado2  pedidomoldura)}<br>
                        <td>Quantidade: #{show (pedidomolduraQuantidade  pedidomoldura)}<br>
                        <td>Modelo: #{unpack (pedidomolduraModelo  pedidomoldura)}<br>
                        <td>Paspatur: #{unpack (pedidomolduraPaspatour  pedidomoldura)}<br>
                        <td>Vidro: #{unpack (pedidomolduraVidro  pedidomoldura)}<br>
                        <td>Data de Entrada: #{unpack (pedidomolduraEntrada  pedidomoldura)}<br>
                        <td>Data de Entrega: #{unpack (pedidomolduraSaida  pedidomoldura)}<br>
                        <td>Observação: #{unpack (pedidomolduraObservacao  pedidomoldura)}<br>
                        <td>Forma de Pagamento: <input type="text" name="pedidomolduraPagamento" value=#{unpack (pedidomolduraPagamento pedidomoldura)}><br>
                        <td>Valor Final: #{show (pedidomolduraValorfinal  pedidomoldura)}<br>
                        <td><input class="botao2" type="submit" value="Alterar">
    |]
    
postEditarQtdePedidoR :: PedidomolduraId -> Handler Html
postEditarQtdePedidoR pedmolid = do
            (pedidomolduraStatus, pedidomolduraPagamento, pedidomolduraSaida) <- runInputPost $ (,,)
                <$> ireq (selectFieldList status)  "pedidomolduraStatus"
                <*> ireq textField  "pedidomolduraPagamento"
                <*> ireq textField  "pedidomolduraSaida"
            molduras <- runDB $ update pedmolid [PedidomolduraStatus =. pedidomolduraStatus, PedidomolduraPagamento =. pedidomolduraPagamento, PedidomolduraSaida =. pedidomolduraSaida]
            redirect ListPedidomolduraR

menu3 :: Widget
menu3 = do
    addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
    addStylesheet $ StaticR css_estilo_css
    toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
    |]
    $(whamletFile "templates/menu3.hamlet")