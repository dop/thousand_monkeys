open! Base

type url =
  | Local of string
  | Remote of string

let url_to_string = function
  | Local url -> url
  | Remote url -> url

let local_url url =
  Local url

let remote_url url =
  Remote url

type part =
  | Text of string
  | Image of string * url
  | Video of url

type doc =
  { title : string
  ; subtitle : string
  ; description : string
  ; keywords : string list
  ; author : string
  ; body : part list
  ; time : float
  }

let create ~title ~author ~time ?(subtitle="") ?(description="") ?(keywords=[]) body =
  { title
  ; author
  ; subtitle
  ; description
  ; keywords
  ; body
  ; time
  }

let text txt =
  Text txt

let image ?(alt="") src =
  Image (alt, src)

module type Theme_intf = sig
  val header
    :  title:string
    -> subtitle:string
    -> description:string
    -> author:string
    -> time:float
    -> [> Html_types.header] Tyxml.Html.elt

  val post
    :  part list
    -> [> Html_types.article] Tyxml.Html.elt
end

module BasicTheme : Theme_intf = struct
  open Tyxml.Html

  let header ~title:title_ ~subtitle ~description ~author ~time:time_ =
      header
        [ h1 [pcdata title_]
        ; h2 [pcdata subtitle]
        ; p [ pcdata "posted on "
            ; pcdata (Float.to_string time_)
            ; pcdata " by "
            ; span [pcdata author]
            ]
        ; p [pcdata description]
        ]

  let post parts =
    let to_p = function
      | Text text -> p [pcdata text]
      | Image (desc, url) -> p [img ~alt:desc ~src:(url_to_string url) ()]
      | Video _ -> p []
    in
    article (List.map ~f:to_p parts)
end

let basic_theme =
  (module BasicTheme : Theme_intf)

let to_html ~theme doc =
  let module Theme = (val theme : Theme_intf) in
  let open Tyxml.Html in
  html
    (head (title (pcdata doc.title))
       [ meta () ~a:[a_charset "utf-8"]
       ; meta () ~a:[a_name "author"; a_content doc.author]
       ; meta () ~a:[a_name "description"; a_content doc.description]
       ; meta () ~a:[ a_name "keywords"
                    ; a_content (String.concat ~sep:", " doc.keywords)
                    ]
       ])
    (body
       [ Theme.header
           ~title:doc.title
           ~subtitle:doc.subtitle
           ~author:doc.author
           ~description:doc.description
           ~time:doc.time
       ; Theme.post doc.body
       ])

let to_string ?(theme=basic_theme) doc : string =
  Caml.Format.asprintf "%a" (Tyxml.Html.pp ()) (to_html ~theme doc)
