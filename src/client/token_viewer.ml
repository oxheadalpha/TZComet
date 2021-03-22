open! Import

let go_action ctxt ~wip =
  let token_id = State.token_id ctxt |> Reactive.peek in
  let address = State.token_address ctxt |> Reactive.peek in
  let _logh msg = Async_work.log wip msg in
  let logm msg = Async_work.log wip (Message_html.render ctxt msg) in
  Async_work.wip wip ;
  Async_work.async_catch wip
    ~exn_to_html:(Errors_html.exception_html ctxt)
    Lwt.Infix.(
      fun ~mkexn () ->
        let id =
          try Int.of_string token_id
          with _ ->
            raise
              (mkexn
                 Meta_html.(
                   t "Hmmm, the token-id is not a nat anymore:" %% ct token_id))
        in
        logm
          Message.(
            t "Fetching metadata for" %% Fmt.kstr ct "%s/%s" address token_id) ;
        Contract_metadata.Token.fetch ctxt ~address ~id ~log:logm
        >>= fun token -> Async_work.ok wip token ; Lwt.return_unit) ;
  ()

let twitter_icon =
  {|data:image/x-icon;base64,
iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAAAXNSR0IArs4c6QAAA0pJREFUWAnt
Vk1oE1EQnnlJbFK3KUq9VJPYWgQVD/5QD0qpfweL1YJQoZAULBRPggp6kB78PQn14kHx0jRB0UO9
REVFb1YqVBEsbZW2SbVS0B6apEnbbMbZ6qbZdTempqCHPAjvzcw3P5mdmfcAiquYgX+cAVwu/+5A
dDMQnSPCHUhQA0hf+Rxy2OjicIvzm+qnKhito0qpb2wvJhWeJgCPP7oPELeHvdJ1VSGf3eOPnSWg
a0S0Qo9HxEkEusDBuNjbEca8G291nlBxmgDc/ukuIvAJxI6wr+yKCsq1ewLxQ2lZfpQLo8oQ4ZXd
CkfnACrGWpyDCl+oQmVn5xuVPU102e2P3qoJkFOhzVb9S7KSnL5jJs/mI+As01PJFPSlZeFSZZoA
GBRXBZyq9lk5NrC+e7pJ5en30c+JWk59pZ5vRDOuhAD381c/H/FKz1SMNgCE16rg505r5TT0uLqm
e93d0fbq+1SeLSeU83Ke0RHYFPGVPcjQfNDUwIa7M665+dQAEEjZoMwZMcEF9RxIDAgBQ2mCcqJ0
Z0b+h4MNbZ4RnyOSDbNmE2iRk5jCNgIIckFoZAs4IgfLGrlKGjkzS16iwj6pV9I4mUvCPf73JVyt
H9nRJj24QHrqU8NCIWrMaGqAC+Ut/3ZzAS63cx4v2K/x/IvQBOCwWzu5KmJGwEJ5PIgeG9nQBDDc
XPpFoDjJ7ThvBC6EZxXWkJG+JgAFwGM4KBAOcibeGCn8FQ/hyajXPmSk+1sACogn4hYk7OdiHDFS
WipPkPWSmY6mCzIghEEuxJvcEYUvxIdhX2mvmSHDDPBF9AJRnDZTyp+P40671JYLbxiAohDxSTfQ
Ig4oNxgPzCWPHaWQBViOf2jGqVwBaEaxGbAqOFMrp+SefC8eNhoFIY5lXzpmtnMGUB2IbU3JdIqV
W9m5zcxINn/hAYKiIexdaTh4srHKORMAP0b28PNgJyGt5gvHzQVYx91QpVcwpRFl/p63HSR1DLbi
d1OcTpAJQOG7u+KH+aI5Qwj13IsamU5vkUSIc8uGLDa8OtoivV8U5HcydFLtT7hlSDVy2nfxI2Ib
g9awuVU8IeJAOMF5m2B6jFs1tM5R9rS3GRP5uSuiihn4DzPwA7z7GDH+43gqAAAAAElFTkSuQmCC
|}

let instagram_icon =
  {|data:image/x-icon;base64,
AAABAAIAEBAAAAEAIABoBAAAJgAAACAgAAABACAAqBAAAI4EAAAoAAAAEAAAACAAAAABACAAAAAA
AAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB00vs5fd3/coXe/9eG4P//gNr//3jS/v9sxv7/
X7P9/1Wa/P9PePTYVFLtc2U54DoAAAAAAAAAAAD//wFVuPuBZMr/+m/W//9q0f//Z83+/2PI/v9c
v/3/U7H8/0ic+/9AgPf/QmL0/1dN9P9tN+X7eizYhP8A/wE7lvY4Q6T8/Uiw/v9SuP3/ktb+/7bk
//+75f//u+L+/7jc/v+y0f3/qMD7/4WQ9f9aRen/bS/i/3wu3f59Ldc5N4TydDaR//85l/n/w+L+
/83j/v+byf3/k8P9/5C8/P+Ps/z/kqz6/6Cr+P/U1Pv/zL/2/3Ix4P9/LuP/gC/Xjj9x6dgwdvL/
e6v4/8/f/f8rgPn/In/6/x50+f8lbvj/KWL3/ytS8/88SO//VD/p/9vS+f+kdun/eCfa/4Mv1/1N
ZuL/OmPn/6u/9/+Ztvn/J2v0/zBv9v+Iq/r/0N39/9Hb/f+Smvb/UkHr/1sp5P+2mfD/wqLv/3gm
2P+GMNb/X1XT/0xT2/+4vvP/mKj0/zJV7P+Rpfb/3d77/3V78f96dvD/4d37/6SI7/9iHN7/s5Dt
/8iq7/98Jtb/ii/U/3ZEv/9iQcv/wbnu/6Cd7v9LR+H/29r5/31z7P9OOOX/Vi7j/4xn6P/g0ff/
bCPb/7SP6//Lq+7/gSbT/5Au0f+ON6r/fTC0/8qy5f+tk+X/YzPS/9/W9v+LaOL/ZybY/2si2P+U
ZOP/4dL3/3Mk1v+3j+n/zqvs/4klzv+WLs3/mziZ/44tov/Rr93/uY/b/3YhwP+1iuD/5Nn2/5dj
3P+XZN7/5dv3/66B4v90G83/vJLm/9Gp6v+RJcn/nC/J/6Y7i/+YMJH/0anU/8SZ1/+GKbP/hzG/
/7qK2//j0fH/49Hx/7OC3f+0h+H/rnTb/8CT4f/Ro+X/mCfC/6Mxw/+sO37YpzWC/8OBtf/l0er/
kzSk/48rq/+IJLH/iSq3/4kpuv+EHbn/tnXR/7N1zv/hzO7/wHjU/6Iru/+pMrn9uztydL08ev+w
Pn7/5cHW/+XP5//LnNL/x5PS/8WR1P/GkNb/x5HX/8aT1//hyev/477m/6k2tP+0NLj/sDSwjsRA
ZDjFRGb9wEBp/7tEcP/RhKH/3qnA/+Cwyv/fsM//3q/S/92s1P/ao9L/ynrC/7A3qP+0M6f/ujan
/rc2oTn/AAABzlVTgdNbVvrUW1n/yVFX/8FKXf+9R2j/ukNy/7g/ev+2OoL/tTaJ/7k1kf/DOpr/
wjmY+704l4T/AP8BAAAAAAAAAADXZ0g52WlKctRnStfVZ0z/0WNR/85fWv/LW2P/ylZq/8pRc//J
SnjYykOFc8Y5iDoAAAAAAAAAAMADAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAMADAAAoAAAAIAAAAEAAAAABACAAAAAAAAAQAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGbM/wV92f81gd3/YYTf/36F3v/Th97/+4bf//+F3f//
g9z//3/Y//971f7/d9D+/3HK/v9rw/3/ZLr9/16w/P9Yo/v/VJb5/1CG9fxPdPPVUGDvgFZO6mJd
Qeg3VSvVBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABlx/9ObtH/yHPV//x73f//f+D//33c
//982f7/fNn+/3vY/v951v7/dtP+/3LP/v9tyv7/aMT9/2O8/f9ds/z/V6j8/1Kc+v9Ojvj/TH71
/0xu9P9RXff/WE7y/2BC5/1sN+PLcy/aUgAAAAAAAAAAAAAAAAAAAAAAAAAAVrn9dF/E/v1ly///
Zsr//2rN/v9tz/7/b9H+/3DR/v9w0f7/b9D+/23O/v9qy/7/Z8b+/2PB/f9eu/3/WbP8/1Sp/P9P
nvv/S5H5/0iD9v9Hc/T/R2Lx/0xS7f9VSOn/YD7l/2815v94L+D+ey7ZegAAAAAAAAAAAAAAAEqo
+ExPsv79Urj//1a8/f9awP3/XcT+/2DG/v9gx/7/Xcb+/1rE/v9Ywv7/Vr/+/1S8/v9Rt/3/TbH9
/0mq/P9Fofv/QJb7/zyK+v85fPj/OW/1/zxi8/9EV/D/Tk7t/1dF6f9iO+X/bTHg/3ct3v99L9/+
ey7XUwAAAABJktsHQJz2zESo//9Gq/z/SrH9/062/f9Puf3/S7f9/1q+/v99zP7/ltb+/6Ha/v+n
2/7/qNr+/6jY/v+m1f7/pdH+/6PN/f+gx/3/nL/8/5Sz+/+Gofn/a4D0/01Y7/9HQuv/WEDo/2Q4
5P9vL+D/di3d/34w4P99LtfScTnGCTqM9TU6kvj9Opj5/zyf+v8/pfz/P6j8/0Oq/P+X0f7/5fT/
////////////////////////////////////////////////////////////////////////////
4+T8/5mS8/9YOub/ZTHj/3Et3/92Ld3/ey7b/4Av2/6CMNg7N4PyYTmN/f81jfb/NZL4/zSW+v83
mfv/tdr+////////////4e/+/77d/v+v1f7/qNH+/6TN/f+jyv3/osf9/6HD/P+gv/z/obv7/6W6
+/+uvPr/wMf6/+Tl/f///////////7+v9P9mLeH/cSvf/3ct3f97Ltv/hDDh/4Ew12U4fO2AOYT6
/zSE8/8yiPb/J4T3/4y+/P//////8/j//3y1/P81kfv/K477/yuN+/8qivv/KYb6/yh/+v8nePn/
JnD5/yVo+P8nYPf/LFr1/zFT8/85S/D/Sk3u/5GK8v/29v7//////6iE7f9rJN3/dy3d/3wu2/+C
L93/gy/Wwzxz6dM6efD/NXvx/zB98/86hPX/5/D+//////94q/v/HHj5/ymE+v8shvr/LIX6/yyC
+v8sffr/LHf5/ytx+f8tbPj/MWf3/zVj9v86XfT/QFfy/0dP8P9LRez/Ri/n/5iE7///////6+L6
/3Y03/93K93/fC7b/4Av2P+GMNv/Q23m+z9w6v86dO3/L3Dw/2eX9f//////3uj+/y129/8pevj/
K335/yt8+f8revn/Knb5/yRs+P8oafj/M2v3/zZn9v8wXPT/M1bz/z9W8v9GUfD/TUru/1RC6/9Z
N+f/YjXj/+ni+///////kl3k/3Qn2/99Ltr/gS/Y/4Uv1v9KZuH/RWrl/0Bt6v8yZ+z/iaj1////
//+3y/v/J2r0/zB19v8wdvb/MHT3/yxu9/8wbff/eZ75/7/Q/P/e5v3/3uX9/8HL+/+Ajvb/RU7v
/0lG7f9UQ+v/Wzvp/2Iz5f9hJOD/zrv1//////+nfen/cyTZ/34u2f+CL9j/hjDW/1Jf2/9NYuD/
R2Xl/zlf5/+arvT//////6m9+P8tY/D/Nm3z/zZt9P8xaPT/Q3L1/8TS/P//////////////////
///////////////Hxvn/W0zr/1c46P9iNOb/aS3j/2Qh3v/DqvL//////7KL6/9zJNj/gC/Z/4Qv
1/+HL9X/W1jV/1Zb2v9QXt//QVbi/6Sv8v//////pbX2/zRb7P89Ze//O2Tw/z1i8P/I0/v/////
/+3v/v+Vn/b/ZXHx/2hv8f+cm/T/8PD9///////Mw/j/XzPl/2gs4/9uK+H/ZyLd/7+j8P//////
t5Lr/3Qk1v+CL9j/hjDW/4kw1P9mUM3/YFPT/1lW2P9KTtv/qq7w//////+mrvP/PVPn/0Vc6/89
Vev/h5Xz///////t7v3/Y2vv/zpC6/9ERuv/SELq/0Q15/9yYOv/8e79//////+ac+v/ZyPf/3Ar
3/9qItv/vqDv//////+6luv/diTV/4Qv1/+IMNX/jC7T/3FHxP9rSsr/ZE3R/1RF0/+wq+z/////
/6mq8P9GSeH/TVPl/0hN5v/Mzfj//////5qZ8f9BPeb/Ukfp/1ZE6P9ZQOf/XDvm/1In4f+pke7/
/////9G99f9qJd3/cizd/20i2v++n+7//////7uX6v95JdT/hjDW/4sv0/+QLdH/fj+4/3dBwP9w
RMf/YDzK/7Wo6P//////raXs/1BA2v9WSN//Wk3h/+fl+///////cWPn/1M74v9dP+T/YDzj/2Q3
4v9oMuH/ZCfe/4VW4///////5935/3Mv3P91K9v/cCPY/7+f7f//////vZfq/3wl0v+KL9T/jy3S
/5It0P+LN63/hDm1/307vf9tM8D/uqTk//////+zoej/XDfS/2I/2P9kRNr/6OT6//////96XOL/
XjLc/2g23v9sMt7/by/d/3Et3f9qJdr/iFfh///////o3fn/dzDa/3gs2f9yI9X/wJ/s//////+/
l+n/fyXQ/44t0v+SLdD/lS7O/5M2pP+PNav/ijSy/3ors//AoN7//////7md5P9pLcn/bzbP/2kx
0P/VxvL//////6mM5/9hIdL/ci7Y/3Qt2f91Ldn/dCzZ/2cf1P+xkOn//////9S+8v90J9X/fC3W
/3Yk0//CoOv//////8CX5/+EI83/kS3P/5Uuzv+XL8z/mDed/5Q2pP+QNar/gymq/8Se2f//////
v5vf/3Umv/98MMf/dCjI/6h73f//////8ev6/4ZQ1/9pIM7/cifS/3In0v9qH9D/ilTb//Pt+///
////pHTh/3cm0v+AL9T/eSXQ/8Sh6v//////w5Xl/4kjy/+ULs3/mC/L/5ovyv+dOZX/mTec/5U2
o/+JKqP/xZvV///////En93/fCe4/4Mxwf+BL8T/fTHH/9rF7///////8uz6/7CM4/+NVtb/jlfX
/7KO5f/z7vv//////9S/8P92Kc7/fSnP/4Mu0P98Jcz/yKTp///////DkuP/jSPI/5guyv+bL8n/
nS/H/6E6jf+fOZT/mjeb/48sm//Elc7//////8qn3f+CKLH/iTK7/4cywP+CLcH/iT/H/9rD7f//
///////////////////////////////Vvu7/fTTL/4tF0f+aVtb/hC3L/34jxv/Nquj//////8KM
4P+RJMX/my/H/54vxv+gMMT/pjqG/6M6i/+gOZL/li6U/8CIw///////1rjg/4cqqP+OM7T/jTO5
/4wzvP+GLr7/hjLA/6951v/Yv+z/6971/+vd9f/Xvu3/rHXZ/30qxf+PRs3/9Oz6//37/v+tb9f/
fBq9/9i76///////vX7a/5Ymwv+fMMT/oTHC/6QxwP+rO4L7qDqE/6U6if+dMoz/tGuu///////t
3+//kDaj/5Ezq/+SNLH/kTS1/480uf+NMrr/hiq6/4UsvP+KNcD/ijXC/4Urwf+FKcH/hyvC/6JZ
z////////////7iC1/+EJbn/7uL2//////+xX87/nCq//6MxwP+lMb7/pzG9/648fNOvPIH/qjuC
/6Y4hf+lQ5D/8+jy//////+5fr7/jSeZ/5Uzp/+VNa3/lDWx/5M0tP+SM7b/kDO4/44xuv+OMbv/
jzK9/5Exvv+RL77/kCy8/7Ry0P+7g9P/hSKx/7d90f//////8eP2/6I4vv+iL7z/pjK7/6gyuv+u
M7z/szx2gLo+ff+wPHz/rTt//6Qyff/Mkbv///////nz+P+8f7z/mDic/5Mtnf+SLKH/kSul/5Aq
qP+PKav/jyit/5Anrv+RJrD/kSWw/5ElsP+PI6//iByr/4smrf+5fM7/+vX7///////Ih9T/oCm0
/6gyuP+qM7b/sDS4/64zs8O7Om5hwT11/7c6dP+0O3f/sTl4/6w5e//ftc7////////////v4e7/
3bzd/9Wt1//RpdX/z6LV/8+g1v/Pn9b/z5/X/8+f2P/QoNn/0aPb/9Sq3f/bu+T/7+Hz////////
////2q3f/6Uwsf+pMrL/rDOy/64zsf+4Nrb/szWuZcA6ajXBPGz9vTxu/7s7cP+5O3L/tTdx/7E4
df/SjrL/8+Ls////////////////////////////////////////////////////////////////
////////////8eDx/8yIzf+oMav/rDGs/680rf+xNKv/szWr/7g2rP62NKk7tklJB8REYszJRmf/
wUNm/8BDaP++Qmr/vEBr/7Y4af+4R3j/xWuV/8+Hq//TlLf/1Zq+/9Wcwv/VnMX/1JvH/9Sbyf/T
msv/05nL/9KWy//Pj8n/yYDD/71itv+uO6b/qiyh/7A0pf+yNab/tDWl/7U1pf+8N6j/uTWj0qo5
qgkAAAAAzU1bTM9RW/3LUFz/x05d/8ZOX//FTWH/w01i/8BKZP+8RGb/uD9p/7Y9bv+0OnP/sjl3
/7A3fP+vNYH/rjOF/60xif+sL43/rC6Q/6wtk/+tLZb/ry+Z/7I0nf+1Np7/tjae/7c2nv+5N57/
vDee/8E4oP67N5pTAAAAAAAAAAAAAAAA0VhRdNVcU/3VXFX/zllV/8xYVv/LWFf/yldZ/8hWW//H
VGD/xVNl/8NRav/BT2//wE10/75LeP+9SXz/vEeA/7xFhP+7Qof/ukCL/7o+jv+6O5H/ujmT/7o4
lf+7N5b/vTiW/8M5mf/EOZj+wDiUegAAAAAAAAAAAAAAAAAAAAAAAAAA1WJLTtllTcjWZEz922dQ
/9pmUf/UY1H/0GBR/89fUv/NXlX/zFxa/8paXv/JWGP/x1Zo/8ZUbP/FUnD/xFBz/8RNd//DS3r/
wkh+/8JGgf/FRIX/ykOL/8pAjf/FO4v9xjqOy8Q4jFIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAzGYzBd1qSDXaaUdh1WhIf9dqSNLVaEn71WdL/9NmTP/TZE3/0WNR/9BhVv/OX1r/zV1f/8xb
Y//LWGb/y1Zq/8pUbf/KUXD/yU5z+8lLd9XJSHyAy0SAYsdBgjfVK4AGAAAAAAAAAAAAAAAAAAAA
APAAAA/gAAAHwAAAA4AAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACA
AAABwAAAA+AAAAfwAAAP
|}

let linkify_text s =
  let open Meta_html in
  let prefix_and_for_all ~prefix ~for_all s =
    String.is_prefix s ~prefix
    && String.for_all (String.chop_prefix_exn s ~prefix) ~f:for_all in
  let is_twitter_handle = function
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' | '.' -> true
    | _ -> false in
  let output = ref (empty ()) in
  let print c = output := !output % c in
  let is_word_sep = function
    | ' ' | '\'' | '(' | ')' | '!' | '[' | ']' -> true
    | _ -> false in
  let next_sep s ~pos =
    match String.lfindi ~pos s ~f:(fun _ -> is_word_sep) with
    | Some new_pos ->
        Some (new_pos + 1, s.[new_pos], String.sub s ~pos ~len:(new_pos - pos))
    | None -> None in
  let handle_tok = function
    | uri when String.is_prefix uri ~prefix:"https://" ->
        print (link ~target:uri (t uri))
    | handle
      when prefix_and_for_all ~prefix:"@" ~for_all:is_twitter_handle handle ->
        let heavy c =
          let a = [style "font-weight: bold"] in
          span ~a c in
        print
          ( heavy (t handle)
          % sup
              (let handle = String.chop_prefix_exn handle ~prefix:"@" in
               let img_a = [style "max-height: 0.9em"] in
               link
                 ~target:(Fmt.str "https://twitter.com/%s" handle)
                 (H5.img ~a:img_a ~src:(Lwd.pure twitter_icon)
                    ~alt:(Lwd.pure "Twitter") ())
               % t " "
               % link
                   ~target:(Fmt.str "https://instagram.com/%s" handle)
                   (H5.img ~a:img_a ~src:(Lwd.pure instagram_icon)
                      ~alt:(Lwd.pure "Instagram") ())) )
    | tz_address
      when prefix_and_for_all ~prefix:"tz" ~for_all:is_twitter_handle tz_address
      ->
        print
          (link
             ~target:(Fmt.str "https://tzkt.io/%s" tz_address)
             (bt (ellipsize_string ~ellipsis:"…" ~max_length:8 tz_address)))
    | tok -> print (t tok) in
  let rec go pos =
    match next_sep ~pos s with
    | Some (npos, sep, tok) ->
        handle_tok tok ;
        print (Fmt.kstr t "%c" sep) ;
        go npos
    | None ->
        let tok = String.sub s ~pos ~len:(String.length s - pos) in
        handle_tok tok ; () in
  go 0 ; !output

let show_token ctxt
    Contract_metadata.Token.
      { address
      ; id
      ; warnings
      ; network
      ; symbol
      ; name
      ; decimals
      ; main_multimedia
      ; metadata
      ; tzip21 } =
  let open Meta_html in
  let open Contract_metadata.Content.Tzip_021 in
  let warning = function
    | `Fetching_uri (uri, e) ->
        t "Fetching URI" %% ct uri %% Errors_html.exception_html ctxt e
    | `Parsing_uri (uri, err) ->
        t "Parsing URI" %% ct uri %% Tezos_html.error_trace ctxt err
    | `Getting_metadata_field m ->
        t "Parsing metadata" %% Message_html.render ctxt m in
  let warnings = List.map warnings ~f:(fun (k, v) -> (k, warning v)) in
  let metaname =
    match (name, symbol, tzip21.prefers_symbol) with
    | _, Some s, Some true -> bt s
    | Some n, _, _ -> bt n
    | None, _, _ -> Fmt.kstr ct "%s/%d" address id in
  let or_empty o f = match o with None -> empty () | Some o -> f o in
  let metadescription = or_empty tzip21.description linkify_text in
  let multimedia =
    match main_multimedia with
    | None -> Bootstrap.alert ~kind:`Warning (t "There is no multi-media.")
    | Some (Error exn) ->
        Bootstrap.alert ~kind:`Danger
          ( t "Error while getting multimedia content:"
          %% Errors_html.exception_html ctxt exn )
    | Some (Ok (title, mm)) ->
        let open Contract_metadata.Multimedia in
        let maybe_censor f =
          if mm.sfw || State.always_show_multimedia ctxt then f ()
          else
            Bootstrap.Collapse.(
              fixed_width_reactive_button_with_div_below (make ())
                ~kind:`Secondary ~width:"100%")
              ~button:(function
                | true ->
                    Fmt.kstr t "Show %s (potentially NSFW)"
                      ( match mm.format with
                      | `Image, "svg+xml" -> "Vector Graphics"
                      | `Image, _ -> "Image"
                      | `Video, _ -> "Video" )
                | false -> t "Hide Multimedia")
              f in
        let wrap_mm c =
          div c
            ~a:
              [ style
                  (* Can seem to limit the height and keeping the
                     images “inside”: *)
                  "max-width: 100%; /*max-height: 60vh;*/ padding: 2px; \
                   margin: auto; width: 100%; height: 100%" ] in
        let mm_style =
          let loadingable =
            "background: transparent url(./loading.gif) no-repeat scroll \
             center center;" in
          "height: 100%; width: 100%; object-fit: contain; " ^ loadingable
          (* "max-width: 100%; max-height: 100%" *) in
        maybe_censor (fun () ->
            match mm.format with
            | `Image, "svg+xml" ->
                wrap_mm
                  (link ~target:mm.converted_uri
                     (H5.object_
                        ~a:
                          [ H5.a_mime_type (Lwd.pure "image/svg+xml")
                          ; H5.a_data (Lwd.pure mm.converted_uri) ]
                        [ H5.img ~a:[style mm_style]
                            ~alt:
                              (Fmt.kstr Lwd.pure "%s at %s" title
                                 mm.converted_uri)
                            ~src:(Lwd.pure mm.converted_uri)
                            () ]))
            | `Image, _ ->
                wrap_mm
                  (link ~target:mm.converted_uri
                     (H5.img ~a:[style mm_style]
                        ~alt:
                          (Fmt.kstr Lwd.pure "%s at %s" title mm.converted_uri)
                        ~src:(Lwd.pure mm.converted_uri)
                        ()))
            | `Video, _ ->
                wrap_mm
                  (H5.video
                     ~a:[H5.a_controls (); style mm_style]
                     ~src:(Lwd.pure mm.converted_uri)
                     []))
    (* Tezos_html.multimedia_from_tzip16_uri ctxt ~title
       ~mime_types:(uri_mime_types tzip21) ~uri) *) in
  let creators =
    let each s = i (linkify_text s) in
    or_empty tzip21.creators (function
      | [] -> bt "Creators list is explicitly empty."
      | [one] -> bt "Creator:" %% each one
      | sl -> bt "Creators:" %% itemize (List.map sl ~f:each)) in
  let tags =
    or_empty tzip21.tags (fun sl ->
        bt "Tags:"
        %% list
             (oxfordize_list sl
                ~map:(fun t -> ct t)
                ~sep:(fun () -> t ", ")
                ~last_sep:(fun () -> t ", "))) in
  let contract_info =
    let open Tezos_contract_metadata.Metadata_contents in
    let sep () = t ", " in
    let item c = div (bt "→" %% c) in
    let itemo o = or_empty o item in
    bt "Contract:"
    %% item
         ( abbreviation address
             (ct (ellipsize_string ~ellipsis:"…" ~max_length:10 address))
         %% t "is on"
         %% it
              (Option.value_map ~f:Network.to_string
                 ~default:"an unknown network" network)
         %% parens
              ( t "Open in"
              %% State.link_to_explorer ctxt (t "Explorer") ~search:address
              % sep ()
              %% list
                   Tezos_html.Block_explorer.(
                     oxfordize_list all_vendors ~sep ~last_sep:sep
                       ~map:(fun vend ->
                         link
                           (t (vendor_show_name vend))
                           ~target:(kt1_url vend address))) ) )
    %% itemo
         (let open Option in
         let target =
           first_some metadata.homepage
             (bind metadata.source ~f:(fun s -> s.location)) in
         let name_part =
           first_some (map metadata.name ~f:(Fmt.str "“%s”")) target in
         let link_part =
           Option.map name_part ~f:(fun apart ->
               match target with
               | Some target -> link (bt apart) ~target
               | None -> bt apart) in
         let version_part =
           match metadata.version with
           | None -> empty ()
           | Some v -> t "version" %% bt v in
         let authors_part =
           match metadata.authors with
           | [] -> empty ()
           | al ->
               t " by"
               %% list
                    (oxfordize_list al
                       ~map:(Tezos_html.author ~namet:bt)
                       ~sep:(fun () -> t ", ")
                       ~last_sep:(fun () -> t ", and ")) in
         let description_part =
           or_empty metadata.description (fun s -> t ":" %% i (linkify_text s))
         in
         let tail = version_part % authors_part % description_part in
         match (link_part, metadata.authors) with
         | Some l, _ -> Some (l %% tail)
         | None, [] -> None
         | None, _ -> Some (it "NOT-NAMED" %% tail)) in
  let main_content =
    h3 ~a:[style "text-align: center"] metaname
    % multimedia
    (* % p
        (Fmt.kstr t "mm: %a"
           Fmt.(
             option
               (result
                  ~ok:(pair string Contract_metadata.Multimedia.pp)
                  ~error:Exn.pp))
           main_multimedia) *)
    % Bootstrap.p_lead metadescription
    % div creators % div tags % div contract_info in
  div
    ~a:[style "padding: 1em; border: solid 3px #aaa; max-width: 800px"]
    main_content
  % Bootstrap.Collapse.(
      fixed_width_reactive_button_with_div_below (make ()) ~kind:`Secondary
        ~width:(* Same as async_work *) "12em")
      ~button:(function
        | true -> t "Show token details" | false -> t "Hide details")
      (fun () ->
        Tezos_html.show_one_token ctxt ?symbol ?name ?decimals ~tzip_021:tzip21
          ~id ~warnings)

let render ctxt =
  let open Meta_html in
  let result = Async_work.empty () in
  let token_id = State.token_id ctxt in
  let token_id_bidi = Reactive.Bidirectional.of_var token_id in
  let token_address = State.token_address ctxt in
  let token_address_bidi = Reactive.Bidirectional.of_var token_address in
  let is_address_valid k =
    match B58_hashes.check_b58_kt1_hash k with
    | _ -> true
    | exception _ -> false in
  let address_valid ctxt token_address =
    Reactive.(map (get token_address) ~f:is_address_valid) in
  let is_token_id_valid i =
    match Int.of_string i with _ -> true | exception _ -> false in
  let token_id_valid ctxt token_id =
    Reactive.(map (get token_id) ~f:is_token_id_valid) in
  let input_valid ctxt =
    Reactive.(
      map
        (address_valid ctxt token_address ** token_id_valid ctxt token_id)
        ~f:(fun (add, tid) -> add && tid)) in
  let make_help ~validity ~input content =
    Reactive.(
      bind
        (validity ctxt input ** get input)
        ~f:(function
          | true, more -> content %% t "👍"
          | false, "" -> content
          | false, more ->
              content %% Bootstrap.color `Danger (ct more %% t "is wrong.")))
  in
  let enter_action () =
    if
      is_token_id_valid (Reactive.peek token_id)
      && is_address_valid (Reactive.peek token_address)
      && not (Async_work.peek_busy result)
    then go_action ctxt ~wip:result in
  let _once_in_tab = enter_action () in
  h2 (t "Token Viewer")
  % Bootstrap.Form.(
      State.if_explorer_should_go ctxt enter_action ;
      make ~enter_action
        [ row
            [ cell 2
                (submit_button (t "Pick A Random Token") (fun () ->
                     let addr, id = State.Examples.random_token ctxt in
                     Reactive.set token_address addr ;
                     Reactive.set token_id (Int.to_string id) ;
                     enter_action ()))
            ; cell 4
                (input
                   ~placeholder:(Reactive.pure "Contract address")
                   token_address_bidi
                   ~help:
                     (make_help ~validity:address_valid ~input:token_address
                        (t "A valid KT1 address on any known network.")))
            ; cell 2
                (input ~placeholder:(Reactive.pure "Token ID") token_id_bidi
                   ~help:
                     (make_help ~validity:token_id_valid ~input:token_id
                        (t "A natural number.")))
            ; cell 3
                (submit_button (t "Go!")
                   ~active:
                     Reactive.(
                       map
                         (input_valid ctxt ** Async_work.busy result)
                         ~f:(function
                           | false, _ -> false | _, true -> false | _ -> true))
                   enter_action)
              (* ; cell 1
                  (magic (t " – or – ")) *) ] ])
  % Async_work.render result ~f:(show_token ctxt)
