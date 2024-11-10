type point = {
    x : int ;
    y : int ;
}

type t =
| Circle of point * float * int
| FilledCircle of point * float * int
| Line of point * point * int
| Point of point * int
| Polygon of point array * int
| FilledPolygon of point list * int
| Rect of point * point * int
| FilledRect of point * point * int
| Triangle of point * point * point * int
| FilledTriangle of point * point * point * int
