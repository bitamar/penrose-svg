module Main exposing (main)

import Math.Vector2 exposing (add, scale, sub, fromTuple, getX, getY)
import Svg exposing (polygon, svg)
import Svg.Attributes as SvgA


type alias Point =
    Math.Vector2.Vec2


type TriangleType
    = T36
    | T108


type alias Triangle =
    { a : Point
    , b : Point
    , c : Point
    , triangleType : TriangleType
    }


createTrianglesCircle : List Triangle
createTrianglesCircle =
    let
        createTriangle i =
            let
                triangle =
                    { a = fromTuple ( 0, 0 )
                    , b = fromTuple <| fromPolar ( 100, (2 * toFloat i - 1) * pi / 10 )
                    , c = fromTuple <| fromPolar ( 100, (2 * toFloat i + 1) * pi / 10 )
                    , triangleType = T36
                    }
            in
                moveTriangleByPoint triangle <| fromTuple ( 100, 100 )
    in
        List.map createTriangle <| List.range 1 10


main : Svg.Svg msg
main =
    let
        triangles =
            createTrianglesCircle

        triangles_ =
            divideTriangles <| divideTriangles triangles
    in
        svg
            [ SvgA.version "1.1", SvgA.viewBox "0 0 300 300" ]
        <|
            List.map drawTriangle triangles_


divideTriangles : List Triangle -> List Triangle
divideTriangles triangles =
    List.concatMap divideTriangle triangles


divideTriangle : Triangle -> List Triangle
divideTriangle triangle =
    let
        phi =
            (1 + sqrt 5) / 2
    in
        case triangle.triangleType of
            T36 ->
                let
                    p =
                        scale (1 / phi) <| add triangle.a (sub triangle.b triangle.a)

                    triangle_ =
                        createTriangle triangle.c p triangle.b T36

                    triangle__ =
                        createTriangle p triangle.c triangle.a T108
                in
                    [ triangle, triangle_, triangle__ ]

            T108 ->
                let
                    p1 =
                        scale (1 / phi) <| add triangle.b (sub triangle.a triangle.b)

                    p2 =
                        scale (1 / phi) <| add triangle.b (sub triangle.c triangle.b)

                    triangle_ =
                        createTriangle p2 triangle.c triangle.a T108

                    triangle__ =
                        createTriangle p1 p2 triangle.b T108

                    triangle___ =
                        createTriangle p2 p1 triangle.a T36
                in
                    [ triangle, triangle_, triangle__, triangle___ ]


createTriangle : Point -> Point -> Point -> TriangleType -> Triangle
createTriangle a b c triangleType =
    { a = a
    , b = b
    , c = c
    , triangleType = triangleType
    }


drawTriangle : Triangle -> Svg.Svg msg
drawTriangle triangle =
    let
        color =
            case triangle.triangleType of
                T36 ->
                    "pink"

                T108 ->
                    "#f2a2d2"
    in
        polygon
            [ SvgA.fill color
            , SvgA.points <| toString (getX triangle.a) ++ "," ++ toString (getY triangle.a) ++ " " ++ toString (getX triangle.b) ++ "," ++ toString (getY triangle.b) ++ " " ++ toString (getX triangle.c) ++ "," ++ toString (getY triangle.c)
            , SvgA.fillOpacity "0.4"
            , SvgA.stroke "white"
            , SvgA.strokeWidth "0.1"
            ]
            []


moveTriangleByPoint : Triangle -> Point -> Triangle
moveTriangleByPoint triangle point =
    { a = add triangle.a point
    , b = add triangle.b point
    , c = add triangle.c point
    , triangleType = triangle.triangleType
    }
