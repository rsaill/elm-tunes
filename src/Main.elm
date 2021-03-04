module Main exposing (..)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Regex
import Json.Decode exposing (Decoder, field, string, list)

-- MAIN

main =
  Browser.document
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- RYTHM

type Rythm = Reel | Jig | Hornpipe | Polka | OtherR

type alias Rythm_Filter =
  { reel: Bool
  , jig: Bool
  , polka: Bool
  , hornpipe: Bool
  , other: Bool
  }

all_rythms : Rythm_Filter
all_rythms = 
  { reel = True
  , jig = True
  , polka = True
  , hornpipe = True
  , other = True
  }

no_rythms : Rythm_Filter
no_rythms = 
  { reel = False
  , jig = False
  , polka = False
  , hornpipe = False
  , other = False
  }

set_rythm_filter : Rythm_Filter -> Rythm -> Bool -> Rythm_Filter
set_rythm_filter filter rythm bool =
  case rythm of
    Reel -> { filter | reel=bool }
    Jig -> { filter | jig=bool }
    Polka -> { filter | polka=bool }
    Hornpipe -> { filter | hornpipe=bool }
    OtherR -> { filter | other=bool }

all_rythms_checked : Rythm_Filter -> Bool
all_rythms_checked filter =
  filter.reel &&
  filter.jig &&
  filter.polka &&
  filter.hornpipe &&
  filter.other

rythmRegex : Regex.Regex
rythmRegex =
  Maybe.withDefault Regex.never
  <| Regex.fromString "R: ?([Rr]eel|[Jj]ig|[Pp]olka|[Hh]ornpipe)"

get_rythm : String -> Rythm
get_rythm tune =
  case Regex.findAtMost 1 rythmRegex tune of
    [] -> OtherR
    ma::_ ->
      case ma.submatches of
        [] -> OtherR
        opt::_ ->
          case opt of
            Nothing -> OtherR
            Just str ->
              let str2 = String.toLower str in
              if str2 == "reel" then Reel
              else if str2 == "jig" then Reel
              else if str2 == "polka" then Reel
              else if str2 == "hornpipe" then Reel
              else OtherR

match_rythm : Rythm_Filter -> String -> Bool
match_rythm filter tune =
  case (get_rythm tune) of
    Reel -> filter.reel
    Jig -> filter.jig
    Polka -> filter.polka
    Hornpipe -> filter.hornpipe
    OtherR -> filter.other

-- KEY

type Key =
   AMaj | AMin | ADor | AMix | BMin | CMaj | DMaj | DMin | DDor | DMix
 | EMin | EDor | FSharpMin | GMaj | GDor | OtherK

type alias Key_Filter =
  { amaj: Bool
  , amin: Bool
  , ador: Bool
  , amix: Bool
  , bmin: Bool
  , cmaj: Bool
  , dmaj: Bool
  , dmin: Bool
  , ddor: Bool
  , dmix: Bool
  , emin: Bool
  , edor: Bool
  , fmin: Bool
  , gmaj: Bool
  , gdor: Bool
  , other: Bool
  }

all_keys : Key_Filter
all_keys = 
  { amaj = True
  , amin = True
  , ador = True
  , amix = True
  , bmin = True
  , cmaj = True
  , dmaj = True
  , dmin = True
  , ddor = True
  , dmix = True
  , emin = True
  , edor = True
  , fmin = True
  , gmaj = True
  , gdor = True
  , other = True
  }

no_key : Key_Filter
no_key = 
  { amaj = False
  , amin = False
  , ador = False
  , amix = False
  , bmin = False
  , cmaj = False
  , dmaj = False
  , dmin = False
  , ddor = False
  , dmix = False
  , emin = False
  , edor = False
  , fmin = False
  , gmaj = False
  , gdor = False
  , other = False
  }

set_key_filter : Key_Filter -> Key -> Bool -> Key_Filter
set_key_filter filter key bool =
  case key of
    AMaj -> { filter | amaj=bool }
    AMin -> { filter | amin=bool }
    AMix -> { filter | amix=bool }
    ADor -> { filter | ador=bool }
    BMin -> { filter | bmin=bool }
    CMaj -> { filter | cmaj=bool }
    DMaj -> { filter | dmaj=bool }
    DMin -> { filter | dmin=bool }
    DDor -> { filter | ddor=bool }
    DMix -> { filter | dmix=bool }
    EMin -> { filter | emin=bool }
    EDor -> { filter | edor=bool }
    FSharpMin -> { filter | fmin=bool }
    GMaj -> { filter | gmaj=bool }
    GDor -> { filter | gdor=bool }
    OtherK -> { filter | other=bool }

all_keys_checked : Key_Filter -> Bool
all_keys_checked filter =
  filter.amaj &&
  filter.amin &&
  filter.ador &&
  filter.amix &&
  filter.bmin &&
  filter.cmaj &&
  filter.dmaj &&
  filter.dmin &&
  filter.ddor &&
  filter.dmix &&
  filter.emin &&
  filter.edor &&
  filter.fmin &&
  filter.gmaj &&
  filter.gdor &&
  filter.other

keyRegex : Regex.Regex
keyRegex =
  Maybe.withDefault Regex.never
  <| Regex.fromString "K: ?(A|A[Mm]aj|A[Mm]in|A[Mm]ix|A[Dd]or|B[Mm]in|C[Mm]aj|D|D[Mm]aj|D[Mm]in|D[Dd]or|D[Mm]ix|E[Mm]in|E[Dd]or|F#[Mm]in|G|G[Mm]aj|G[Dd]or)"

get_key : String -> Key
get_key tune =
  case Regex.findAtMost 1 keyRegex tune of
    [] -> OtherK
    ma::_ ->
      case ma.submatches of
        [] -> OtherK
        opt::_ ->
          case opt of
            Nothing -> OtherK
            Just str0 ->
              let str = String.toLower str0 in
              if str == "amaj" || str == "a" then AMaj
              else if str == "amin" then AMin
              else if str == "amix" then AMix
              else if str == "ador" then ADor
              else if str == "bmin" then BMin
              else if str == "cmaj" || str == "c" then CMaj
              else if str == "dmaj" || str == "d" then DMaj
              else if str == "dmin" then DMin
              else if str == "ddor" then DDor
              else if str == "dmix" then DMix
              else if str == "emin" then EMin
              else if str == "edor" then EDor
              else if str == "f#min" then FSharpMin
              else if str == "gmaj" || str == "g" then GMaj
              else if str == "gdor" then GDor
              else OtherK

match_key : Key_Filter -> String -> Bool
match_key filter tune =
  case (get_key tune) of
    AMaj -> filter.amaj
    AMin -> filter.amin
    ADor -> filter.ador
    AMix -> filter.amix
    BMin -> filter.bmin
    CMaj -> filter.cmaj
    DMaj -> filter.dmaj
    DMin -> filter.dmin
    DDor -> filter.ddor
    DMix -> filter.dmix
    EMin -> filter.emin
    EDor -> filter.edor
    FSharpMin -> filter.fmin
    GMaj -> filter.gmaj
    GDor -> filter.gdor
    OtherK -> filter.other

-- MODEL

type Status = Error String | Loading | AllGood

type alias Filter =
  { pattern: String
  , rythm: Rythm_Filter
  , key: Key_Filter
  }

noFilter : Filter
noFilter = { pattern="",rythm=all_rythms,key=all_keys }

type alias Model =
  { status : Status
  , tunes: List String
  , filter: Filter
  , limit: Int
  }

-- INIT

init : () -> (Model, Cmd Msg)
init _ = ({status=Loading,tunes=[],filter=noFilter, limit=0}, getTunes)

-- UPDATE

type Msg =
    GotTunes (Result Http.Error (List String))
  | Search String
  | CheckRythm Rythm Bool
  | CheckAllRythms Bool
  | CheckKey Key Bool
  | CheckAllKeys Bool
  | ChangeLimit Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotTunes result ->
      case result of
        Ok ts ->
          ({status=AllGood,tunes=ts,filter=noFilter,limit=20}, Cmd.none)
        Err err ->
          ({status=Error (error_to_string err),tunes=[],filter=noFilter,limit=0}, Cmd.none)

    Search pattern ->
      let filter = model.filter in
      ({ model | filter={filter|pattern=pattern} }, Cmd.none)

    CheckRythm rythm checked ->
      let filter = model.filter in
      ({ model | filter={filter|rythm=set_rythm_filter model.filter.rythm rythm checked} }, Cmd.none)

    CheckKey key checked ->
      let filter = model.filter in
      ({ model | filter={filter|key=set_key_filter model.filter.key key checked} }, Cmd.none)

    CheckAllRythms checked ->
      let filter = model.filter in
      if checked then
        ({ model | filter={filter|rythm=all_rythms} }, Cmd.none)
      else
        ({ model | filter={filter|rythm=no_rythms} }, Cmd.none)

    CheckAllKeys checked ->
      let filter = model.filter in
      if checked then
        ({ model | filter={filter|key=all_keys} }, Cmd.none)
      else
        ({ model | filter={filter|key=no_key} }, Cmd.none)

    ChangeLimit limit ->
      ({model|limit=(Basics.max 0 limit)},Cmd.none)

error_to_string : Http.Error -> String
error_to_string err =
  case err of
    Http.BadUrl _ -> "Bad url"
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "Network error"
    Http.BadStatus _ -> "Bad status"
    Http.BadBody _ -> "Bad body"

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

view : Model -> Document Msg
view model =
  case model.status of
    Error err -> { title="", body=[div [] [text ("Error: " ++ err)]] }
    Loading -> { title="", body=[div [] [text "Loading"]] }
    AllGood ->
      { title="Tunes"
      , body= [ menu model.limit model.filter
      , div [class "w3-cell w3-container w3-light-gray", style "width" "800px"]
        (view_tunes model.filter model.limit model.tunes) ] 
      }

menu : Int -> Filter -> Html Msg
menu limit filter =
  div [class "w3-cell w3-container w3-light-gray w3-monospace",style "width" "300px"]
  [
    p [] [input [class "w3-input w3-border", placeholder "🔍", onInput Search] []],
    h2 [class "w3-text-red w3-monospace"] [text "♯ Rythm"],
    div [class "w3-row"] [
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckRythm Reel),checked filter.rythm.reel] [], label [] [text " Reel"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckRythm Jig),checked filter.rythm.jig] [], label [] [text " Jig"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckRythm Hornpipe),checked filter.rythm.hornpipe] [], label [] [text " Hornpipe"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckRythm Polka),checked filter.rythm.polka] [], label [] [text " Polka"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckRythm OtherR),checked filter.rythm.other] [], label [] [text " Other"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck CheckAllRythms,checked (all_rythms_checked filter.rythm)] [], text " All"]
    ],
    h2 [class "w3-text-red w3-monospace"] [text "♯ Key"],
    div [class "w3-row"] [
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey DMaj),checked filter.key.dmaj] [], label [] [text " DMaj"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey DMin),checked filter.key.dmin] [], label [] [text " DMin"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey DDor),checked filter.key.ddor] [], label [] [text " DDor"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey DMix),checked filter.key.dmix] [], label [] [text " DMix"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey EMin),checked filter.key.emin] [], label [] [text " EMin"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey EDor),checked filter.key.edor] [], label [] [text " EDor"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey FSharpMin),checked filter.key.fmin] [], label [] [text " F#Min"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey GMaj),checked filter.key.gmaj] [], label [] [text " GMaj"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey GDor),checked filter.key.gdor] [], label [] [text " GDor"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey AMaj),checked filter.key.amaj] [], label [] [text " AMaj"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey AMin),checked filter.key.amin] [], label [] [text " AMin"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey ADor),checked filter.key.ador] [], label [] [text " ADor"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey AMix),checked filter.key.amix] [], label [] [text " AMix"]],
      p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey BMin),checked filter.key.bmin] [], label [] [text " BMin"]],
        p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey CMaj),checked filter.key.cmaj] [], label [] [text " CMaj"]],
        p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckKey OtherK),checked filter.key.other] [], label [] [text " Other"]],
        p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox",onCheck (CheckAllKeys),checked (all_keys_checked filter.key)] [], text " All"]
      ],
      --h2 [class "w3-text-red w3-monospace"] [text "♯ Mode"],
      --div [class "w3-row"] [
      --    p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox"] [], label [] [text " Maj"]],
      --    p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox"] [], label [] [text " Min"]],
      --    p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox"] [], label [] [text " Dor"]],
      --    p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox"] [], label [] [text " Mix"]],
      --    p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox"] [], label [] [text " Other"]],
      --    p [class "w3-col s6"] [input [class "w3-check",type_ "checkbox"] [], text " All"]
      --    ],
    h2 [class "w3-text-red w3-monospace"] [text "♯ Limit"],
    div [] [
      button [class "w3-button",onClick (ChangeLimit (limit-10))] [text "<<"],
      button [class "w3-button",onClick (ChangeLimit (limit-1))] [text "<"],
      text (String.fromInt limit),
      button [class "w3-button",onClick (ChangeLimit (limit+1))] [text ">"],
      button [class "w3-button",onClick (ChangeLimit (limit+10))] [text ">>"]
    ]
  ]

view_tune : String -> Html Msg
view_tune tune =
  div [class "w3-display-container"] [
    pre [class "w3-card w3-container w3-white"] [
      text tune,
      div [class "w3-display-topright"] [
        button [class "w3-button"] [text "🖉"],
        br [] [],
        button [class "w3-button"] [text "🔉"]
      ]
    ]
  ]

match : Filter -> String -> Bool
match filter tune =
  String.contains filter.pattern tune &&
  match_rythm filter.rythm tune &&
  match_key filter.key tune

view_tunes : Filter -> Int -> List String -> List (Html Msg)
view_tunes filter i lst =
  if i <= 0 then []
  else 
    case lst of
      [] -> []
      hd::tl ->
        if match filter hd then (view_tune hd)::(view_tunes filter (i-1) tl)
        else view_tunes filter i tl

-- HTTP request

getTunes : Cmd Msg
getTunes =
   Http.get
     { url = "http://tunes.slld.fr/json.php"
     , expect = Http.expectJson GotTunes tunesDecoder
     }

-- JSON decoder

tunesDecoder : Decoder (List String)
tunesDecoder =
  Json.Decode.list (field "tune" string)
