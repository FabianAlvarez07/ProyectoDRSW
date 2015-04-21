module Handler.Tienda where

import Import
import Yesod.Form.Bootstrap3 (bfs, renderBootstrap3 , BootstrapFormLayout(..) )
import Yesod.Text.Markdown (markdownField)
 
tiendaForm :: AForm Handler Tienda
tiendaForm = Tienda
          <$> areq textField (bfs ("nombre"::Text)) Nothing 
          <*> areq markdownField (bfs ("descripcion"::Text)) Nothing
          <*> areq doubleField (bfs ("precio"::Text)) Nothing

getTiendaNewR :: Handler Html
getTiendaNewR = do
       (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm tiendaForm
       defaultLayout $ do
           $(widgetFile "tienda/new") 

postTiendaNewR :: Handler Html
postTiendaNewR = do
       ((result, widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm tiendaForm
       case result of
            FormSuccess tienda -> do
                                  tId <- runDB $ insert tienda
                                  redirect (TiendaR tId)
            _ -> defaultLayout $ do
                      $(widgetFile "tienda/new")

getTiendaR :: TiendaId -> Handler Html
getTiendaR tId = do
   tienda <- runDB $ get404 tId
   defaultLayout $ do
        $(widgetFile "tienda/details")           


deleteTiendaR :: TiendaId -> Handler Html
deleteTiendaR tId = do
              runDB $ delete tId
              defaultLayout $ do
                   [whamlet| |]          
