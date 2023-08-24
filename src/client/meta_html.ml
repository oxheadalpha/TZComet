open Import
include Lwd_bootstrap.Mono_html
module Bootstrap = Lwd_bootstrap.Bootstrap

module Example = struct
  let e0 () = t "Hello" %% it "World"

  let e1 () =
    let button_calls = Reactive.var 0 in
    p (e0 ())
    % Bootstrap.container_fluid
        (p (t "This is in a bootstrap" %% ct "container-fluid.")
        % p
            (Bootstrap.button ~kind:`Primary
               ~action:(fun () ->
                 Reactive.set button_calls (Reactive.peek button_calls + 1))
               (Reactive.bind_var button_calls ~f:(fun count ->
                    H5.span
                      [
                        Fmt.kstr
                          (if Stdlib.( mod ) count 2 = 0 then it else bt)
                          "Click %d" count;
                      ])))
        % p
            (Bootstrap.label `Danger
               (Reactive.bind_var button_calls ~f:(fun count ->
                    Fmt.kstr t "Button above clicked %d time%s." count
                      (if count = 1 then "" else "s"))))
        % p (t "A dropdown menu:")
        % Bootstrap.Dropdown_menu.(
            button
              (t "This is a" %% ct "Dropdown" %% t "menu")
              [
                item (t "The First") ~action:(fun () ->
                    dbgf "Hello from the first");
                header (t "This is a dropdown" %% it "header");
                item (t "The Second") ~action:(fun () ->
                    dbgf "Hellow from the second");
              ])
        % p (t "A Nav-bar …")
        % Bootstrap.Navigation_bar.(
            make
              ~brand:(it "Examples of Meta_html")
              [
                item (t "One")
                  ~action:(fun () -> dbgf "one from nav bar")
                  ~fragment:(Reactive.pure "page-one");
                item ~active:(Reactive.pure false) (t "One-inactive")
                  ~action:(fun () -> assert false);
              ])
        %
        let hello = Reactive.var "is it me …" in
        let checkboxed = Reactive.var false in
        let submissions = Reactive.var [] in
        p (t "And now some forms")
        % Bootstrap.Form.(
            make
              [
                input
                  (Reactive.Bidirectional.of_var hello)
                  ~label:(t "Say Hello");
                check_box ~label:(t "Check this box")
                  (Reactive.Bidirectional.of_var checkboxed);
                submit_button (t "Submit This!") (fun () ->
                    Reactive.set submissions
                      ((Reactive.peek hello, Reactive.peek checkboxed)
                      :: Reactive.peek submissions));
              ])
        % p
            (t "Form results:"
            %% Reactive.bind_var hello ~f:(fun v -> t "Hello:" %% ct v)
            % t ", checkbox is "
            %% Reactive.bind_var checkboxed ~f:(function
                 | false -> bt "not"
                 | true -> empty ())
            %% t "checked.")
        % itemize
            [
              t "Some item";
              t "Some other item";
              t "truc" %% it "bidule" %% bt "chouette";
              t "Form submissions:"
              %% Reactive.bind_var submissions ~f:(fun subs ->
                     itemize ~numbered:true
                       (List.rev_map subs ~f:(fun (h, c) ->
                            t "Submission:" %% ct h % t ","
                            %% if c then it "checked" else it "unchecked")));
            ]
        %
        let content = Reactive.var "content" in
        H5.div
          [
            (p (t "more input experiemnt" %% Reactive.bind_var content ~f:ct)
            %% H5.(
                 input
                   ~a:
                     [
                       a_input_type (Reactive.pure `Text);
                       a_value (Reactive.pure "hello");
                       a_oninput
                         (Tyxml_lwd.Lwdom.attr
                            Js_of_ocaml.(
                              fun ev ->
                                Js.Opt.iter ev##.target (fun input ->
                                    Js.Opt.iter (Dom_html.CoerceTo.input input)
                                      (fun input ->
                                        let v = input##.value |> Js.to_string in
                                        dbgf "TA inputs: %d bytes: %S"
                                          (String.length v) v;
                                        Reactive.set content v));
                                false));
                     ]
                   ()));
          ])
end
