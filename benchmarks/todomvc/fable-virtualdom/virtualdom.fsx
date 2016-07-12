#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-virtualdom/Fable.Helpers.Virtualdom.fs"

open Fable.Core
open Fable.Import
open Fable.Import.Browser

open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.App
open Fable.Helpers.Virtualdom.Html

// Todo model
type Filter =
    | All
    | Completed
    | Active

type Item =
    {
        Name: string
        Done: bool
        Id: int
        IsEditing: bool
    }

type TodoModel =
    {
        Items: Item list
        Input: string
        Filter: Filter
    }

// Todo update
type TodoAction =
    | Nop
    | AddItem
    | ChangeInput of string
    | MarkAsDone of Item
    | ToggleItem of Item
    | Destroy of Item
    | CheckAll
    | UnCheckAll
    | SetActiveFilter of Filter
    | ClearCompleted
    | EditItem of Item
    | SaveItem of Item*string

let todoUpdate model msg =
    let checkAllWith v =
        let items' =
            model.Items
            |> List.map (fun i -> { i with Done = v })
        {model with Items = items'}

    let updateItem i model =
        let items' =
            model.Items |> List.map (fun i' ->
                if i'.Id <> i.Id then i' else i)
        {model with Items = items'}

    let model' =
        match msg with
        | AddItem ->
            let maxId =
                if model.Items |> List.isEmpty then 1
                else
                    model.Items
                    |> List.map (fun x -> x.Id)
                    |> List.max
            let item' = {
                Id = maxId + 1
                Name = model.Input
                Done = false
                IsEditing = false}
            {model with Items = item'::model.Items; Input = ""}
        | ChangeInput v -> {model with Input = v}
        | MarkAsDone i ->
            let items' =
                model.Items |> List.map (fun i' ->
                    if i' <> i then i'
                    else {i with Done = true})
            {model with Items = items'}
        | CheckAll -> checkAllWith true
        | UnCheckAll -> checkAllWith false
        | Destroy i ->
            let items' =
                model.Items |> List.filter (fun i' -> i'.Id <> i.Id)
            {model with Items = items'}
        | ToggleItem i ->
            updateItem {i with Done = not i.Done} model
        | SetActiveFilter f ->
            { model with Filter = f }
        | ClearCompleted ->
            let items' =
                model.Items |> List.filter (fun i -> not i.Done)
            { model with Items = items'}
        | EditItem i ->
            updateItem { i with IsEditing = true} model
        | SaveItem (i,str) ->
            updateItem { i with Name = str; IsEditing = false} model
        | Nop -> model

    let jsCalls =
        match msg with
        | EditItem i -> [fun () ->
            document.getElementById("item-" + (i.Id.ToString())).focus()]
        | _ -> []
    model',jsCalls

// Todo view
let filterToTextAndUrl = function
    | All -> "All", ""
    | Completed -> "Completed", "completed"
    | Active -> "Active", "active"

let filter activeFilter f =
    let linkClass = if f = activeFilter then "selected" else ""
    let fText,url = f |> filterToTextAndUrl
    li
        [ onMouseClick (fun _ -> SetActiveFilter f)]
        [ a
            [ attribute "href" ("#/" + url); attribute "class" linkClass ]
            [ text fText] ]

let filters model =
    ul
        [ attribute "class" "filters" ]
        ([ All; Active; Completed ] |> List.map (filter model.Filter))

let todoFooter model =
    let clearVisibility =
        if model.Items |> List.exists (fun i -> i.Done)
        then ""
        else "none"
    let activeCount =
        model.Items
        |> List.filter (fun i -> not i.Done)
        |> List.length |> string
    footer
        [   attribute "class" "footer"; Style ["display","block"]]
        [   span
                [   attribute "class" "todo-count" ]
                [   strong [] [text activeCount]
                    text " items left" ]
            (filters model)
            button
                [   attribute "class" "clear-completed"
                    Style [ "display", clearVisibility ]
                    onMouseClick (fun _ -> ClearCompleted)]
                [ text "Clear completed" ] ]

let todoHeader model =
    header
        [attribute "class" "header"]
        [   h1 [] [text "todos"]
            input [ attribute "class" "new-todo"
                    attribute "id" "new-todo"
                    property "value" model
                    property "placeholder" "What needs to be done?"
                    onEvent "oninput" (fun x -> console.debug(sprintf "%A" x); ChangeInput (x?target?value :?> string))
                    onKeydown (fun x ->
                        if x.keyCode = 13
                        then AddItem
                        else Nop )]]
let listItem item =
    let itemChecked = if item.Done then "true" else ""
    let editClass = if item.IsEditing then "editing" else ""
    li [ attribute "class" ((if item.Done then "completed " else " ") + editClass)]
       [ div [  attribute "class" "view"
                onDblClick (fun x -> EditItem item) ]
             [ input [  property "className" "toggle"
                        property "type" "checkbox"
                        property "checked" itemChecked
                        onMouseClick (fun e -> ToggleItem item) ]
               label [] [ text item.Name ]
               button [ attribute "class" "destroy"
                        onMouseClick (fun e -> Destroy item) ] [] ]
         input [ attribute "class" "edit"
                 attribute "value" item.Name
                 property "id" ("item-"+item.Id.ToString())
                 onBlur (fun e -> SaveItem (item, (e?target?value :?> string))) ] ]

let itemList items activeFilter =
    let filterItems i =
        match activeFilter with
        | All -> true
        | Completed -> i.Done
        | Active -> not i.Done

    ul [attribute "class" "todo-list" ]
       (items |> List.filter filterItems |> List.map listItem)

let todoMain model =
    let items = model.Items
    let allChecked = items |> List.exists (fun i -> not i.Done)
    section [  attribute "class" "main"
               Style [ "style", "block" ] ]
            [   input [ property "id" "toggle-all"
                        attribute "class" "toggle-all"
                        property "type" "checkbox"
                        property "checked" (if not allChecked then "true" else "")
                        onMouseClick (fun e ->
                                    if allChecked
                                    then CheckAll
                                    else UnCheckAll) ]
                label [ attribute "for" "toggle-all" ]
                      [ text "Mark all as complete" ]
                (itemList items model.Filter) ]

let todoView model =
    section
        [property "class" "todoapp"]
        ((todoHeader model.Input)::(if model.Items |> List.isEmpty
                then []
                else [  (todoMain model)
                        (todoFooter model) ] ))

// Storage
module Storage =
    let private STORAGE_KEY = "vdom-storage"
    open Microsoft.FSharp.Core
    let fetch<'T> (): 'T [] =
        Browser.localStorage.getItem(STORAGE_KEY)
        |> function null -> "[]" | x -> unbox x
        |> JS.JSON.parse |> unbox

    let save<'T> (todos: 'T []) =
        Browser.localStorage.setItem(STORAGE_KEY, JS.JSON.stringify todos)

open Storage
let initList = fetch<Item>() |> List.ofArray
let initModel = {Filter = All; Items = initList; Input = ""}

let todoApp =
    createApp {Model = initModel; View = todoView; Update = todoUpdate}
//    |> (withSubscriber "storagesub" (function
//            | ModelChanged (newModel,old) ->
//                save (newModel.Items |> Array.ofList)
//            | _ -> ()))
//    |> (withSubscriber "modellogger" (printfn "%A"))
    |> withStartNode "#todo"

todoApp |> start renderer
