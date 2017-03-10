module Login exposing (..)

import Html exposing (Html, div, text, program)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)

import Bootstrap.Grid as Grid

-- Form stuff
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Bootstrap.Grid.Col as Col

import Json.Encode as Encode
import Json.Decode exposing (list, string)

import Http exposing (Body)

-- MODEL

type alias Model =
    { username: String,
      password: String,
      badLogin: Bool }

init : ( Model, Cmd Msg )
init =
    ( Model "" "" False, Cmd.none )


-- MESSAGES

type Msg =
    Login 
    | Username String
    | Password String
    | Authenticate  (Result Http.Error Encode.Value)


-- VIEW

view : Model -> Html Msg
view model =
    Grid.container []                              
    [ div [ class "jumbotron"] [
        Html.h1 [] [ text "Chatterbeak Dashboard" ]
        ]            
        , Grid.row [] 
        [ Grid.col [ Col.md4 ]                                  
          [ Form.group []
            [ Form.label [for "username"] [ text "Username"]
            , Input.email [ Input.id "username", Input.onInput Username]
            ]
        , Form.group []
            [ Form.label [for "password"] [ text "Password"]
            , Input.password [ Input.id "password", Input.onInput Password]
            ]
        , Button.button [ Button.primary, Button.attrs[onClick Login]] [ text "Login"]
        ]
        ]
        , Html.footer [class "container-fluid", class "text-center" ] [ text "Copyright 2017 Chatterbeak, Inc."]
        ]

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login ->
            model ! [ doLogin model ]
        Password password ->
            ( { model | password = password}, Cmd.none)
        Username username->
            ( {model | username = username}, Cmd.none)
        Authenticate (Ok msg) ->
             model  ! []
        Authenticate (Err err) ->
             model  ! [] 

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- HTTP Handlers
type alias LoginResult =
    { result : String
    }

type alias LoginRequest =
    { username : String
    , password : String
    }



encodeRequest : LoginRequest -> Encode.Value
encodeRequest record =
    Encode.object
        [ ("username",  Encode.string <| record.username)
        , ("password",  Encode.string <| record.password)
        ]

doLogin :  Model -> Cmd Msg
doLogin model =
    let 
        body = 
            encodeRequest (LoginRequest model.username model.password)
    in  
        Http.post "http://localhost:8080/api/login" (Http.stringBody "application/json" (toString body)) Json.Decode.value
         |> Http.send Authenticate

-- MAIN

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }   