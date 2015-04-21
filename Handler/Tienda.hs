module Handler.Tienda where

import Import
import Yesod.Form.Bootstrap3 (bfs, renderBootstrap3 , BootstrapFormLayout(..) )
import Yesod.Text.Markdown (markdownField)
 
tiendaForm :: Maybe Tienda -> AForm Handler Tienda
tiendaForm mTienda = Tienda
          <$> areq textField (bfs ("nombre"::Text)) (tiendaNombre <$> mTienda)
          <*> areq markdownField (bfs ("descripcion"::Text)) (tiendaDescripcion <$> mTienda)
          <*> areq doubleField (bfs ("precio"::Text)) (tiendaPrecio <$> mTienda)

getTiendaNewR :: Handler Html
getTiendaNewR = do
       (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ tiendaForm Nothing
       defaultLayout $ do
           $(widgetFile "tienda/new") 

postTiendaNewR :: Handler Html
postTiendaNewR = do
       ((result, widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ tiendaForm Nothing
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

getTiendaUpdateR :: TiendaId -> Handler Html
getTiendaUpdateR tiendaId = do
             tienda <- runDB $ get404 tiendaId
             (widget, encoding) <- generateFormPost $
                      renderBootstrap3 BootstrapBasicForm $ tiendaForm (Just tienda)
             defaultLayout $ do
               $(widgetFile "tienda/edit")

postTiendaUpdateR :: TiendaId -> Handler Html
postTiendaUpdateR tiendaId = do
          tienda <- runDB $ get404 tiendaId
          ((result,widget), encoding) <- runFormPost $
                     renderBootstrap3 BootstrapBasicForm
                     $ tiendaForm (Just tienda)
          case result of
               FormSuccess tienda -> do
                        runDB $ replace tiendaId tienda
                        redirect (TiendaR tiendaId)
               _ -> defaultLayout $ do
                        $(widgetFile "tienda/edit")
