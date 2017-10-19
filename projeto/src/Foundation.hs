{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable, EmptyDataDecls,
             GeneralizedNewtypeDeriving, ViewPatterns, FlexibleInstances #-}
module Foundation where

import Yesod
import Data.Text
import Database.Persist
import Database.Persist.Postgresql
    (ConnectionPool, SqlBackend, runSqlPool)
import Yesod.Static

data App = App {getStatic :: Static, connPool :: ConnectionPool}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Falecom
    nome     Text
    telefone Int
    email    Text
    mensagem Text
    deriving Show

Usr
    email       Text
    senha       Text
    UniqueEmail email

Estoqueporcelana
    codigo     Int
    tipo       Text
    subtipo    Text
    tamanho    Text
    quantidade Int
    observacao Text
    deriving   Show

Estoquemoldura
    codigo     Int
    quantidade Int
    observacao Text
    deriving   Show

Orcporcelana
    comer      Double
    qtdecomer  Int
    beber      Double
    qtdebeber  Int
    outros     Double
    qtdeoutros Int
    arte       Double
    deriving   Show

Orcmoldura
    lado1           Double
    lado2           Double
    modelo          Double
    paspatour       Double
--    somoldura       Double
--    molduravar      Double
--    molduravc       Double
--    molduraespelho  Double
    deriving        Show
    
Pedidoporcelana
    status          Text
    clientenome     Text
    clientetelefone Text
    tipo            Text
    subtipo         Text
    quantidade      Int
    arte            Text
    entrada         Text
    saida           Text
    observacao      Text
    pagamento       Text
    valorfinal      Double
    deriving        Show

Pedidomoldura
    status          Text
    clientenome     Text
    clientetelefone Text
    lado1           Double
    lado2           Double
    quantidade      Int
    modelo          Text
    paspatour       Text
    vidro           Text
    entrada         Text
    saida           Text
    observacao      Text
    pagamento       Text
    valorfinal      Double
    deriving        Show
|]

staticFiles "static"

mkYesodData "App" $(parseRoutesFile "routes")

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance Yesod App where
    authRoute _ = Just LoginR

    isAuthorized LoginR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized FalecomR _ = return Authorized
    isAuthorized OrcporcelanaR _ = return Authorized
    isAuthorized OrcmolduraR _ = return Authorized
    isAuthorized EnderecoR _ = return Authorized
    isAuthorized SobreR _ = return Authorized
    isAuthorized PortfolioR _ = return Authorized
    isAuthorized (OrcmoldurafinalR orcmolid) _ = return Authorized
    isAuthorized (EditarQuantidadeEstoqueR porcid) _ = return Authorized
    
    isAuthorized UsrR _ = return Authorized
    isAuthorized ListFalecomR _ = return Authorized
    isAuthorized ListUsrR _ = return Authorized
    isAuthorized EstoqueporcelanaR _ = return Authorized
--    isAuthorized (EstoqueporcelanasR porcid) _ = return Authorized

    isAuthorized _ _ = estaAutenticado

estaAutenticado :: Handler AuthResult
estaAutenticado = do
    msu <- lookupSession "_ID"
    case msu of
        Just _ -> return Authorized
        Nothing -> return AuthenticationRequired

instance YesodPersist App where
   type YesodPersistBackend App = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage