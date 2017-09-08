module Main exposing (..)

import Html
import Color
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Scale as Scale
import Element exposing (..)
import Element.Attributes exposing (..)
import Keyboard
import Array
import Window
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { currentSlide : Int
    , windowSize : Window.Size
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 { width = 0, height = 0 }, Task.perform ResizedWindow Window.size )


type Msg
    = HitKey Keyboard.KeyCode
    | ResizedWindow Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizedWindow size ->
            ( { model | windowSize = size }, Cmd.none )

        HitKey keyCode ->
            case keyCode of
                37 ->
                    ( { model | currentSlide = decrementSlide model.currentSlide }, Cmd.none )

                38 ->
                    ( { model | currentSlide = decrementSlide model.currentSlide }, Cmd.none )

                39 ->
                    ( { model | currentSlide = incrementSlide model.currentSlide }, Cmd.none )

                40 ->
                    ( { model | currentSlide = incrementSlide model.currentSlide }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


incrementSlide : Int -> Int
incrementSlide int =
    if (int + 1) >= (Array.length slides) then
        int
    else
        int + 1


decrementSlide : Int -> Int
decrementSlide int =
    if int <= 0 then
        0
    else
        int - 1


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs HitKey
        , Window.resizes ResizedWindow
        ]


phi : Float
phi =
    1.618


scaled : Int -> Float
scaled =
    Scale.modular 24 phi


type Styles
    = None
    | Main
    | Heading
    | Paragraph
    | InlinePre
    | Pre


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.stylesheet
        [ style None
            []
        , style Main
            [ paddingHint (scaled 1)
            , Font.lineHeight phi
            ]
        , style Heading
            [ Font.size (scaled 3)
            , Font.typeface [ "sans-serif" ]
            , Font.weight 700
            ]
        , style Paragraph
            [ Font.size (scaled 2) ]
        , style InlinePre
            [ Font.size (scaled 2)
            , Font.pre
            , Font.typeface [ "monospace" ]
            , Font.weight 700
            ]
        , style Pre
            [ paddingHint (scaled 1)
            , Border.all 1
            , Color.border Color.darkGrey
            , Font.pre
            , Font.typeface [ "monospace" ]
            , Font.size (scaled 1)
            ]
        ]


view : Model -> Html.Html Msg
view model =
    viewport stylesheet <|
        column
            Main
            []
            [ namedGrid None
                { columns = [ fill 1 ]
                , rows =
                    [ ( fill 4, [ span 1 "content" ] )
                    , ( fill 1, [ span 1 "footer" ] )
                    ]
                }
                [ -- subtract (scaled 3) from window height,
                  -- as (scaled 2) didn't cut it for some reason
                  height (px ((toFloat model.windowSize.height) - (scaled 3)))
                ]
                (case Array.get model.currentSlide slides of
                    Just ( title, list ) ->
                        [ named "content"
                            (column None
                                [ spacing (scaled 1) ]
                                ([ heading title
                                 , column None
                                    []
                                    (List.map
                                        (\item ->
                                            row None [] [ item ]
                                        )
                                        list
                                    )
                                 ]
                                )
                            )
                        , named "footer"
                            (row
                                None
                                [ alignRight, alignBottom ]
                                [ text (slideCount model) ]
                            )
                        ]

                    Nothing ->
                        []
                )
            ]


slideCount : Model -> String
slideCount model =
    (toString (model.currentSlide + 1))
        ++ "/"
        ++ (toString ((Array.length slides)))


slides : Array.Array ( String, List (Element Styles variation msg) )
slides =
    Array.fromList
        [ ( "Using and abusing ecto"
          , [ p "Adam Bowen"
            , p "ej4, LLC"
            , p "Twitter: @adamnbowen"
            ]
          )
        , ( "Warning"
          , [ p "The following may be a bad idea" ]
          )
        , ( "JSON API"
          , [ p "Imagine we have a 3rd party defined API specification for works of art."
            , p "So don't argue I should change the data model to solve my problems"
            ]
          )
        , ( "Credit where credit is due"
          , [ p "(I was inspired to change my example domain by Darin Wilson's talk today)" ]
          )
        , ( "JSON API structure"
          , [ p
                ("The data structures are well-defined, but highly variable. "
                    ++ "We will store the data in Postgres via jsonb."
                )
            , ul
                [ "A work of art has a type, which defines its possible keys"
                , "All works of art have an 'artist'"
                , "For example, an album may have a solo artist or it may have a band composed of artists under a nested 'artists' key."
                ]
            ]
          )
        , ( "Example (partial) JSON documents:"
          , [ row None
                [ spacing (scaled 1) ]
                [ code <| String.trim """
{
  "title": "Animal",
  "artist": {
    "type": "Artist",
    "name": "Kesha"
  }
}
"""
                , code <| String.trim """
{
  "title": "Love This Giant",
  "artist": {
    "type": "Band",
    "name": "David Byrne & St. Vincent"
    "artists": [{
      "type": "Artist",
      "name": "St. Vincent"
    }, {
      "type": "Artist",
      "name": "David Byrne"
    }]
  }
}
            """
                ]
            ]
          )
        , ( "Pattern Matching"
          , [ p "Great for:"
            , ul
                [ "breaking down validation into small parts"
                , "enforcing more global rules on the data structure"
                ]
            ]
          )
        , ( "Ecto Changesets"
          , [ ul
                [ "work well for individual data structures"
                , "help me organize validation rules into pipelines"
                , "provide great error messages"
                ]
            ]
          )
        , ( "Ecto Embedded Schemas"
          , [ paragraph None
                []
                [ p "using"
                , inlineCode " embedded_schema"
                , p ", I can validate all of my different data types while keeping the structure in a single jsonb column"
                ]
            ]
          )
        , ( "Nested Embedded Schemas"
          , [ paragraph None
                []
                [ p "using"
                , inlineCode " embeds_one "
                , p "and"
                , inlineCode " embeds_many"
                , p ", I can nest embedded structures within my data"
                ]
            ]
          )
        , ( "Basic schemas so far"
          , [ row None
                [ spacing (scaled 1) ]
                [ code <| String.trim """
defmodule WorkOfArt do
  use Ecto.Schema

  schema "works_of_art" do
    embeds_one :data, ArtData do
      field :title, :string
      embeds_one :artist, Creator
    end
  end
end
"""
                , code <| String.trim """
defmodule Creator do
  embedded_schema do
    # ... wait, what do we do here?
  end
end
            """
                ]
            ]
          )
        , ( "What I want"
          , [ p "Some way to embed either Artist or Band or ... and then:"
            , row None
                [ spacing (scaled 1) ]
                [ code <| String.trim """
defmodule Artist do
  embedded_schema do
    field :name, :string
  end
end
            """
                , code <| String.trim """
defmodule Band do
  embedded_schema do
    field : name, :string
    embeds_many :artists, Artist
  end
end
            """
                ]
            ]
          )
        , ( "The problem"
          , [ paragraph None
                []
                [ p "Unfortunately,"
                , inlineCode " embedded_schema "
                , p "does not support polymorphic embedding"
                ]
            ]
          )
        , ( "The problem"
          , [ p "Like most problems in Elixir, I thought pattern matching could get me where I needed to go."
            , p "Unfortunately, it wasn't possible to have an intermediate module that would use a different schema based on pattern matching."
            ]
          )
        , ( "To the rescue?"
          , [ paragraph None
                []
                [ p "I discovered"
                , inlineCode " Ecto.Type "
                , p "and saw a path through."
                ]
            ]
          )
        , ( "Ecto.Type"
          , [ paragraph None
                []
                [ p "To implement"
                , inlineCode " Ecto.Type"
                , p ", you just have to implement each of the 4 callbacks."
                ]
            ]
          )
        , ( "Ecto.Type"
          , [ code <| String.trim """
defmodule Creator do
  def type, do: :map

  def cast(map) when is_map(map), do {:ok, map}
  def cast(_), do: :error

  def load(map) when is_map(map), do: {:ok, map}

  def dump(map) when is_map(map), do: {:ok, map}
  def dump(_), do: :error
end
            """
            ]
          )
        , ( "Warning"
          , [ p "The solution is kinda gross" ]
          )
        , ( "Ecto.Type"
          , [ code <| String.trim """
defmodule Creator do
  def cast(%{"type" => "Artist"} = map), do: cast_changeset(Artist, map)
  def cast(%{"type" => "Band"} = map), do: cast_changeset(Band, map)
  def cast(_), do: :error

  # ... snip

  defp cast_changeset(module, map) do
    changeset = apply(module, :changeset, [struct(module), map])

    case changeset.valid? do
      true -> {:ok, map}
      false -> :error
    end
  end
end
            """
            ]
          )
        , ( "Solution?"
          , [ row None
                [ spacing (scaled 1) ]
                [ code <| String.trim """
defmodule WorkOfArt do
  use Ecto.Schema

  schema "works_of_art" do
    embeds_one :data, ArtData do
      field :title, :string
      field :artist, Creator
    end
  end
end
"""
                , code <| String.trim """
defmodule Artist do
  embedded_schema do
    field :name, :string
  end
end

defmodule Band do
  embedded_schema do
    field : name, :string
    embeds_many :artists, Artist
  end
end
            """
                ]
            ]
          )
        , ( "Issues"
          , [ ul
                [ "You cannot give specific error messages to the user" ]
            ]
          )
        , ( "Some possible alternatives"
          , [ ul
                [ "Use simple map fields and validate the maps as a separate embedded schema"
                , "Figure out how to replace Ecto.Type with something that can send error messages"
                , "Convince the Ecto maintainers that adding a new return type to Ecto.Type is a good idea"
                ]
            ]
          )
        , ( "Conclusion"
          , [ ul
                [ "Sometimes we need to build things our libraries don't support."
                , "Sometimes the situation calls for bad ideas and bad code."
                , "Sometimes that's okay."
                ]
            ]
          )
        , ( "Thanks!"
          , [ p "Let me know if I'm doing something truly terrible. I want to know!"
            , p "I'm new to this, and I know many of you are too, so let's learn together."
            , p "Twitter: @adamnbowen"
            ]
          )
        ]


heading : String -> Element Styles variation msg
heading string =
    el Heading [] (text string)


p : String -> Element Styles variation msg
p string =
    el Paragraph [] (text string)


ul : List String -> Element Styles variation msg
ul items =
    column None
        [ paddingLeft (scaled 1) ]
        (List.map
            (\item ->
                row None
                    [ spacing (scaled 1) ]
                    [ el Paragraph [] (text "•")
                    , p item
                    ]
            )
            items
        )


code : String -> Element Styles variation msg
code string =
    el Pre [] (text string)


inlineCode : String -> Element Styles variation msg
inlineCode string =
    el InlinePre [] (text string)
