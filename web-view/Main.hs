{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif
import Control.Monad.IO.Class
import Miso
import Miso.String
import Miso.Html.Property
import Mehanics.Map.Map (getView) --TODO: временно, убрать, только для отладки

-- | Type synonym for an application model
data Model = Model
  { name :: String,
    number :: Int
  }
  deriving (Show, Eq)

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f =
  Warp.runSettings (Warp.setPort 8080 (Warp.setTimeout 3600 Warp.defaultSettings)) =<<
    JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model = Model {number = 0, name = "kek"} -- initial model
    update = updateModel -- update function
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff m2
  where
    n = number m
    m2 = m {number = n + 1}
updateModel SubtractOne m = noEff m2
  where
    n = number m
    m2 = m {number = n - 1}
updateModel NoOp m = noEff m
updateModel SayHelloWorld m = m <# do
  liftIO (putStrLn "Hello World") >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = 
  div_ [ class_ "sandbox" ]
      [ div_ [ class_ "resources-links" ]
          [ link_ 
              [ rel_ "stylesheet" 
              , href_ "https://gregoryghost.github.io/cyberpunk/web-view/styles.css" 
              ] 
          , link_ 
              [ href_ "https://unpkg.com/material-components-web@latest/dist/material-components-web.min.css" 
              , rel_ "stylesheet" 
              ] 
          , link_ 
              [ rel_ "stylesheet" 
              , href_ "https://fonts.googleapis.com/icon?family=Material+Icons" 
              ]
          ] 
      , div_ [ class_ "sandbox__gallery" ]
          [ aside_ [ class_ "mdc-drawer" ]
              [ div_ [ class_ "mdc-drawer__header" ]
                  [ div_ [ class_ "sandbox__gallery-search" ]
                      [ label_ [ class_ "mdc-text-field mdc-text-field--no-label" ]
                          [ span_ [ class_ "mdc-text-field__ripple" ][]
                          , input_ 
                              [ class_ "mdc-text-field__input" 
                              , type_ "text" 
                              , placeholder_ "Поиск" 
                              ] 
                          , span_ [ class_ "mdc-line-ripple" ][]
                          ] 
                      ]
                  ]
              , div_ [ class_ "mdc-drawer__content" ]
                  [ nav_ [ class_ "mdc-list" ]
                      [ a_ 
                          [ class_ "mdc-list-item mdc-list-item--activated" 
                          , href_ "#" 
                          , textProp "aria-current" "page" 
                          ] 
                          [ i_ 
                              [ class_ "material-icons mdc-list-item__graphic" 
                              , ariaHidden_ True 
                              ] [ "inbox" ]
                          , span_ [ class_ "mdc-list-item__text" ][ "Inbox" ]
                          ] 
                      , a_ 
                          [ class_ "mdc-list-item" 
                          , href_ "#" 
                          ] 
                          [ i_ 
                              [ class_ "material-icons mdc-list-item__graphic" 
                              , ariaHidden_ True 
                              ] [ "star" ]
                          , span_ [ class_ "mdc-list-item__text" ][ "Star" ]
                          ] 
                      , a_ 
                          [ class_ "mdc-list-item" 
                          , href_ "#" 
                          ] 
                          [ i_ 
                              [ class_ "material-icons mdc-list-item__graphic" 
                              , ariaHidden_ True 
                              ] [ "send" ]
                          , span_ [ class_ "mdc-list-item__text" ][ "Sent Mail" ]
                          ] 
                      , a_ 
                          [ class_ "mdc-list-item" 
                          , href_ "#" 
                          ] 
                          [ i_ 
                              [ class_ "material-icons mdc-list-item__graphic" 
                              , ariaHidden_ True 
                              ] [ "drafts" ]
                          , span_ [ class_ "mdc-list-item__text" ][ "Drafts" ]
                          ] 
                      , hr_ [ class_ "mdc-list-divider" ]
                      , h6_ [ class_ "mdc-list-group__subheader" ][ "Labels" ]
                      , a_ 
                          [ class_ "mdc-list-item" 
                          , href_ "#" 
                          ] 
                          [ i_ 
                              [ class_ "material-icons mdc-list-item__graphic" 
                              , ariaHidden_ True 
                              ] [ "bookmark" ]
                          , span_ [ class_ "mdc-list-item__text" ][ "Family" ]
                          ] 
                      , a_ 
                          [ class_ "mdc-list-item" 
                          , href_ "#" 
                          ] 
                          [ i_ 
                              [ class_ "material-icons mdc-list-item__graphic" 
                              , ariaHidden_ True
                              ] [ "bookmark" ]
                          , span_ [ class_ "mdc-list-item__text" ][ "Friends" ]
                          ] 
                      , a_ 
                          [ class_ "mdc-list-item" 
                          , href_ "#" 
                          ] 
                          [ i_ 
                              [ class_ "material-icons mdc-list-item__graphic" 
                              , ariaHidden_ True
                              ] [ "bookmark" ]
                          , span_ [ class_ "mdc-list-item__text" ][ "Work" ]
                          ] 
                      ] 
                  ]
              ] 
          ]
      , div_ [ class_ "sandbox__playground" ]
          [ div_ [ class_ "sandbox__toolbox" ]
              [ div_ [ class_ "sandbox__btn-start mdc-touch-target-wrapper" ]
                  [ button_ [ class_ "mdc-button mdc-button--touch" ]
                      [ div_ [ class_ "mdc-button__ripple" ][]
                      , span_ [ class_ "mdc-button__label" ][ "ЗАПУСТИТь" ]
                      , div_ [ class_ "mdc-button__touch" ][]
                      ] 
                  ]
              , div_ [ class_ "sandbox__btn-send-issue mdc-touch-target-wrapper" ]
                  [ button_ [ class_ "mdc-button mdc-button--touch" ]
                      [ div_ [ class_ "mdc-button__ripple" ][]
                      , span_ [ class_ "mdc-button__label" ][ "Сообщить о проблеме" ]
                      , div_ [ class_ "mdc-button__touch" ][]
                      ] 
                  ]
              ] 
          , div_ [ class_ "sandbox__view" ] [getView x]
          , div_ 
              [ class_ "mdc-tab-bar" 
              , role_ "tablist" 
              ] 
              [ div_ [ class_ "mdc-tab-scroller" ]
                  [ div_ [ class_ "sandbox__info mdc-tab-scroller__scroll-area" ]
                      [ div_ [ class_ "mdc-tab-scroller__scroll-content" ]
                          [ button_ 
                              [ class_ "sandbox__algo-description mdc-tab mdc-tab--active" 
                              , role_ "tab" 
                              , ariaSelected_ True 
                              , tabIndex_ 0
                              ] 
                              [ span_ [ class_ "mdc-tab__content" ]
                                  [ span_ [ class_ "mdc-tab__text-label" ][ "Описание механики" ] ]
                              , span_ [ class_ "mdc-tab-indicator mdc-tab-indicator--active" ]
                                  [ span_ [ class_ "mdc-tab-indicator__content mdc-tab-indicator__content--underline" ][] ]
                              , span_ [ class_ "mdc-tab__ripple" ][]
                              ] 
                          , button_ 
                              [ class_ "sandbox__algo-log mdc-tab" 
                              , role_ "tab" 
                              , ariaSelected_ True 
                              , tabIndex_ 1
                              ] 
                              [ span_ [ class_ "mdc-tab__content" ]
                                  [ span_ [ class_ "mdc-tab__text-label" ][ "Лог механики" ] ]
                              , span_ [ class_ "mdc-tab-indicator mdc-tab-indicator--active" ]
                                  [ span_ [ class_ "mdc-tab-indicator__content" ][] ]
                              , span_ [ class_ "mdc-tab__ripple" ][]
                              ] 
                          , button_ 
                              [ class_ "sandbox__common-log mdc-tab" 
                              , role_ "tab" 
                              , ariaSelected_ True 
                              , tabIndex_ 2
                              ] 
                              [ span_ [ class_ "mdc-tab__content" ]
                                  [ span_ [ class_ "mdc-tab__text-label" ][ "Лог песочницы" ] ]
                              , span_ [ class_ "mdc-tab-indicator mdc-tab-indicator--active" ]
                                  [ span_ [ class_ "mdc-tab-indicator__content" ][] ]
                              , span_ [ class_ "mdc-tab__ripple" ][]
                              ] 
                          ] 
                      ]
                  ]
              ]
          ] 
      ]
