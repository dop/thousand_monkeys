open! Base

let html_to_string html =
  Caml.Format.asprintf "%a" (Tyxml.Html.pp ()) html

let%test_unit "document has particular shape" =
  let title_ = "< Title >"
  and subtitle = "< Subtitle >"
  and author = "Donatas & Petrauskas"
  and keywords = ["bio"; "personal story"]
  and description = "Example article"
  and txt = "This is my story"
  and image_url = "http://localhost:8080/image.jpg"
  and time_ = Unix.time ()
  in
  [%test_result: string]
    Document.(
      to_string (
        create ~title:title_ ~subtitle ~author ~keywords ~description ~time:time_
          [ text txt
          ; image (remote_url image_url)
          ]
      ))
    ~expect:Tyxml.Html.(
        html_to_string (
          html
            (head (title (pcdata title_))
               [ meta () ~a:[a_charset "utf-8"]
               ; meta () ~a:[a_name "author"; a_content author]
               ; meta () ~a:[a_name "description"; a_content description]
               ; meta () ~a:[a_name "keywords"; a_content (String.concat ~sep:", " keywords)]
               ])
            (body [ header [ h1 [pcdata title_]
                           ; h2 [pcdata subtitle]
                           ; p [ pcdata "posted on "
                               ; pcdata (Float.to_string time_)
                               ; pcdata " by "
                               ; span [pcdata author]
                               ]
                           ; p [pcdata description]
                           ]
                  ; article [ p [pcdata txt]
                            ; p [img ~src:image_url ~alt:"" ()]
                            ]
                  ])
        ))
