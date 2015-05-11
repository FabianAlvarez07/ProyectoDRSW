module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (bfs, BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
data Busqueda = Busqueda
     { busqueda :: Text 
     }
   deriving Show

busquedaForm :: AForm Handler Busqueda
busquedaForm = Busqueda <$> areq textField (bfs MsgFind) Nothing

getHomeR :: Handler Html
getHomeR = do
          mid <- maybeAuthId
          (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ busquedaForm 
          products <- runDB $ selectList [TiendaNombre !=. ""] [Asc TiendaId]
          defaultLayout $ do
              $(widgetFile "tienda/index")

postHomeR :: Handler Html
postHomeR = do
          mid <- maybeAuthId
          products <- runDB $ selectList [TiendaNombre !=. ""] [Asc TiendaId]
          ((result, widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ busquedaForm
          case result of
            FormSuccess busq -> do
                      products <- runDB $ selectList [TiendaNombre ==. (busqueda $ busq)] [Asc TiendaId]
                      defaultLayout $ do
                           $(widgetFile "tienda/index")
            _ -> defaultLayout $ do
                      $(widgetFile "tienda/index")

