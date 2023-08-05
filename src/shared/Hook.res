type asyncArticles = AsyncResult.t<Shape.Articles.t, AppError.t>
type asyncTags = AsyncResult.t<Shape.Tags.t, AppError.t>
type asyncData = AsyncData.t<option<Shape.User.t>>
type asyncArticleEditor = AsyncResult.t<
  (Shape.Article.t, string, option<Shape.Editor.t>),
  AppError.t,
>
type asyncArticle = AsyncResult.t<Shape.Article.t, AppError.t>
type asyncComment = AsyncResult.t<array<Shape.Comment.t>, AppError.t>
type asyncAuthor = AsyncResult.t<Shape.Author.t, AppError.t>

let useArticles = (~feedType: Shape.FeedType.t): (
  asyncArticles,
  (asyncArticles => asyncArticles) => unit,
) => {
  let (data, setData) = React.useState(() => AsyncResult.init)

  React.useEffect2(() => {
    setData(prev => prev->AsyncResult.toBusy)

    switch feedType {
    | Tag(tag, limit, offset) => API.listArticles(~limit, ~offset, ~tag=?Some(tag), ())
    | Global(limit, offset) => API.listArticles(~limit, ~offset, ())
    | Personal(limit, offset) => API.feedArticles(~limit, ~offset, ())
    }
    ->Promise.then(data =>
      setData(
        _prev =>
          switch data {
          | Ok(ok) => AsyncResult.completeOk(ok)
          | Error(error) => AsyncResult.completeError(error)
          },
      )->Promise.resolve
    )
    ->ignore

    None
  }, (feedType, setData))

  (data, setData)
}

let useArticlesInProfile: (
  ~viewMode: Shape.Profile.viewMode,
) => (asyncArticles, (asyncArticles => asyncArticles) => unit) = (~viewMode) => {
  let (data, setData) = React.useState(() => AsyncResult.init)

  React.useEffect2(() => {
    setData(prev => prev->AsyncResult.toBusy)

    switch viewMode {
    | Author(author, limit, offset) => API.listArticles(~author, ~limit, ~offset, ())
    | Favorited(favorited, limit, offset) => API.listArticles(~favorited, ~limit, ~offset, ())
    }
    ->Promise.then(data =>
      setData(
        _prev =>
          switch data {
          | Ok(ok) => AsyncResult.completeOk(ok)
          | Error(error) => AsyncResult.completeError(error)
          },
      )->Promise.resolve
    )
    ->ignore

    None
  }, (viewMode, setData))

  (data, setData)
}

let useTags: unit => asyncTags = () => {
  let (data, setData) = React.useState(() => AsyncResult.init)

  React.useEffect0(() => {
    setData(prev => prev->AsyncResult.getOk->Option.getWithDefault([])->AsyncResult.reloadingOk)

    API.tags()
    ->Promise.then(data =>
      setData(
        _prev =>
          switch data {
          | Ok(ok) => ok->AsyncResult.completeOk
          | Error(error) => AsyncResult.completeError(error)
          },
      )->Promise.resolve
    )
    ->ignore

    None
  })

  data
}

let useCurrentUser: unit => (asyncData, (asyncData => asyncData) => unit) = () => {
  let (data, setData) = React.useState(() => AsyncData.init)

  React.useEffect0(() => {
    setData(prev => prev->AsyncData.toBusy)

    API.currentUser()
    ->Promise.then(data =>
      setData(
        _prev =>
          switch data {
          | Ok(data') => Some(data')->AsyncData.complete
          | Error(_error) => None->AsyncData.complete
          },
      )->Promise.resolve
    )
    ->Promise.catch(_error => setData(_prev => None->AsyncData.complete)->Promise.resolve)
    ->ignore

    None
  })

  (data, setData)
}

let useArticle = (~slug: string): (
  asyncArticleEditor,
  (asyncArticleEditor => asyncArticleEditor) => unit,
) => {
  let (data, setData) = React.useState(() => AsyncResult.init)

  React.useEffect1(() => {
    setData(AsyncResult.toBusy)

    API.article(~action=Read(slug), ())
    ->Promise.then(data =>
      setData(
        _prev =>
          switch data {
          | Ok(ok: Shape.Article.t) =>
            AsyncResult.completeOk((ok, ok.tagList->Array.joinWith(","), None))
          | Error(error) => AsyncResult.completeError(error)
          },
      )->Promise.resolve
    )
    ->ignore

    None
  }, [slug])

  (data, setData)
}

let useComments: (
  ~slug: string,
) => (
  asyncComment,
  Belt.Set.Int.t,
  (~slug: string, ~id: int) => unit,
  (asyncComment => asyncComment) => unit,
) = (~slug) => {
  let (data, setData) = React.useState(() => AsyncResult.init)
  let (busy, setBusy) = React.useState(() => Belt.Set.Int.empty)

  React.useEffect2(() => {
    setData(prev => prev->AsyncResult.toBusy)
    setBusy(_prev => Belt.Set.Int.empty)

    API.getComments(~slug, ())
    ->Promise.then(data =>
      setData(
        _prev =>
          switch data {
          | Ok(ok) => AsyncResult.completeOk(ok)
          | Error(error) => AsyncResult.completeError(error)
          },
      )->Promise.resolve
    )
    ->ignore

    None
  }, (slug, setData))

  let deleteComment = (~slug, ~id) => {
    setBusy(prev => prev->Belt.Set.Int.add(_, id))

    API.deleteComment(~slug, ~id, ())
    ->Promise.then(resp => {
      setBusy(prev => prev->Belt.Set.Int.remove(_, id))

      switch resp {
      | Ok((_slug, id)) =>
        setData(prev =>
          prev->AsyncResult.map(
            comments => comments->Array.filter((comment: Shape.Comment.t) => comment.id != id),
          )
        )
      | Error(_error) => ignore()
      }

      ignore()->Promise.resolve
    })
    ->ignore
  }

  (data, busy, deleteComment, setData)
}

let useFollow: (
  ~article: asyncArticle,
  ~user: option<Shape.User.t>,
) => (AsyncData.t<(string, bool)>, Link.onClickAction) = (~article, ~user) => {
  let (state, setState) = React.useState(() => AsyncData.init)

  let follow = switch state {
  | Init =>
    article
    ->AsyncResult.getOk
    ->Option.map((ok: Shape.Article.t) =>
      AsyncData.complete((ok.author.username, ok.author.following->Option.getWithDefault(false)))
    )
    ->Option.getWithDefault(AsyncData.complete(("", false)))
  | Loading as orig | Reloading(_) as orig | Complete(_) as orig => orig
  }

  let sendRequest = () => {
    let username =
      follow
      ->AsyncData.getValue
      ->Option.map(((username, _following)) => username)
      ->Option.getWithDefault("")

    let action =
      follow
      ->AsyncData.getValue
      ->Option.flatMap(((_username, following)) =>
        following ? Some(API.Action.Unfollow(username)) : None
      )
      ->Option.getWithDefault(API.Action.Follow(username))

    setState(_prev => follow->AsyncData.toBusy)

    API.followUser(~action, ())
    ->Promise.then(data =>
      setState(_prev =>
        switch data {
        | Ok(ok: Shape.Author.t) =>
          AsyncData.complete((ok.username, ok.following->Option.getWithDefault(false)))
        | Error(_error) => AsyncData.complete(("", false))
        }
      )->Promise.resolve
    )
    ->Promise.catch(_error => setState(_prev => AsyncData.complete(("", false)))->Promise.resolve)
    ->ignore
  }

  let onClick = switch user {
  | Some(_user) => Link.CustomFn(() => sendRequest())
  | None => Location(Link.register)
  }

  (follow, onClick)
}

let useFollowInProfile: (
  ~profile: asyncAuthor,
  ~user: option<Shape.User.t>,
) => (AsyncData.t<(string, bool)>, Link.onClickAction) = (~profile, ~user) => {
  let (state, setState) = React.useState(() => AsyncData.init)

  let follow = switch state {
  | Init =>
    profile
    ->AsyncResult.getOk
    ->Option.map((ok: Shape.Author.t) =>
      AsyncData.complete((ok.username, ok.following->Option.getWithDefault(false)))
    )
    ->Option.getWithDefault(AsyncData.complete(("", false)))
  | Loading as orig | Reloading(_) as orig | Complete(_) as orig => orig
  }

  let sendRequest = () => {
    let username =
      follow
      ->AsyncData.getValue
      ->Option.map(((username, _following)) => username)
      ->Option.getWithDefault("")

    let action =
      follow
      ->AsyncData.getValue
      ->Option.flatMap(((_username, following)) =>
        following ? Some(API.Action.Unfollow(username)) : None
      )
      ->Option.getWithDefault(API.Action.Follow(username))

    setState(_prev => follow->AsyncData.toBusy)

    API.followUser(~action, ())
    ->Promise.then(data =>
      setState(_prev =>
        switch data {
        | Ok(ok: Shape.Author.t) =>
          AsyncData.complete((ok.username, ok.following->Option.getWithDefault(false)))
        | Error(_error) => AsyncData.complete(("", false))
        }
      )->Promise.resolve
    )
    ->Promise.catch(_error => setState(_prev => AsyncData.complete(("", false)))->Promise.resolve)
    ->ignore
  }

  let onClick = switch user {
  | Some(_user) => Link.CustomFn(() => sendRequest())
  | None => Location(Link.register)
  }

  (follow, onClick)
}

let useFavorite = (~article: asyncArticle, ~user: option<Shape.User.t>): (
  AsyncData.t<(bool, int, string)>,
  Link.onClickAction,
) => {
  let (state, setState) = React.useState(() => AsyncData.init)

  let favorite = switch state {
  | Init =>
    article
    ->AsyncResult.getOk
    ->Option.map((ok: Shape.Article.t) =>
      AsyncData.complete((ok.favorited, ok.favoritesCount, ok.slug))
    )
    ->Option.getWithDefault(AsyncData.complete((false, 0, "")))
  | Loading as orig | Reloading(_) as orig | Complete(_) as orig => orig
  }

  let sendRequest = () => {
    let (favorited, _favoritesCount, slug) =
      favorite->AsyncData.getValue->Option.getWithDefault((false, 0, ""))

    let action = favorited ? API.Action.Unfavorite(slug) : API.Action.Favorite(slug)

    setState(_prev => favorite->AsyncData.toBusy)

    API.favoriteArticle(~action, ())
    ->Promise.then(data =>
      setState(_prev =>
        switch data {
        | Ok(ok: Shape.Article.t) => AsyncData.complete((ok.favorited, ok.favoritesCount, ok.slug))
        | Error(_error) => AsyncData.complete((false, 0, ""))
        }
      )->Promise.resolve
    )
    ->Promise.catch(_error =>
      setState(_prev => AsyncData.complete((false, 0, "")))->Promise.resolve
    )
    ->ignore
  }

  let onClick = switch user {
  | Some(_user) => Link.CustomFn(() => sendRequest())
  | None => Location(Link.register)
  }

  (favorite, onClick)
}

let useDeleteArticle: (
  ~article: asyncArticle,
  ~user: option<Shape.User.t>,
) => (bool, Link.onClickAction) = (~article, ~user) => {
  let (state, setState) = React.useState(() => false)

  let sendRequest = () => {
    let slug =
      article
      ->AsyncResult.getOk
      ->Option.map((ok: Shape.Article.t) => ok.slug)
      ->Option.getWithDefault("")

    setState(_prev => true)

    API.article(~action=Delete(slug), ())
    ->Promise.then(_data => {
      setState(_prev => false)
      Link.push(Link.home)
      ignore()->Promise.resolve
    })
    ->Promise.catch(_error => setState(_prev => false)->Promise.resolve)
    ->ignore
  }

  let onClick = switch (user, state) {
  | (Some(_user), false) =>
    Link.CustomFn(
      () =>
        if (
          Webapi.Dom.Window.confirm(
            Webapi.Dom.window,
            "Are you sure you want to delete this article?",
          )
        ) {
          sendRequest()
        } else {
          ignore()
        },
    )
  | (Some(_), true) | (None, true | false) => Link.CustomFn(ignore)
  }

  (state, onClick)
}

let useToggleFavorite: (
  ~setArticles: (asyncArticles => asyncArticles) => unit,
  ~user: option<Shape.User.t>,
) => (Belt.Set.String.t, (~action: API.Action.favorite) => unit) = (~setArticles, ~user) => {
  let (busy, setBusy) = React.useState(() => Belt.Set.String.empty)

  let sendRequest = (~action) => {
    let slug = switch (action: API.Action.favorite) {
    | Favorite(slug) | Unfavorite(slug) => slug
    }

    setBusy(prev => prev->Belt.Set.String.add(_, slug))

    API.favoriteArticle(~action, ())
    ->Promise.then(data => {
      setBusy(prev => prev->Belt.Set.String.remove(_, slug))

      switch data {
      | Ok(_) =>
        setArticles(prev =>
          prev->AsyncResult.map(
            (articles: Shape.Articles.t) => {
              ...articles,
              articles: articles.articles->Array.map(
                (article: Shape.Article.t) =>
                  if article.slug == slug {
                    {
                      ...article,
                      favorited: switch action {
                      | Favorite(_) => true
                      | Unfavorite(_) => false
                      },
                      favoritesCount: switch action {
                      | Favorite(_) => article.favoritesCount + 1
                      | Unfavorite(_) => article.favoritesCount - 1
                      },
                    }
                  } else {
                    article
                  },
              ),
            },
          )
        )
      | Error(_error) => ignore()
      }

      ignore()->Promise.resolve
    })
    ->Promise.catch(_error =>
      setBusy(prev => prev->Belt.Set.String.remove(_, slug))->Promise.resolve
    )
    ->ignore
  }

  let onToggle = (~action) =>
    switch user {
    | Some(_) => sendRequest(~action)
    | None => Link.push(Link.register)
    }

  (busy, onToggle)
}

let useProfile: (~username: string) => asyncAuthor = (~username) => {
  let (data, setData) = React.useState(() => AsyncResult.init)

  React.useEffect2(() => {
    setData(prev => prev->AsyncResult.toBusy)

    API.getProfile(~username, ())
    ->Promise.then(data =>
      setData(
        _prev =>
          switch data {
          | Ok(ok) => AsyncResult.completeOk(ok)
          | Error(error) => AsyncResult.completeError(error)
          },
      )->Promise.resolve
    )
    ->ignore

    None
  }, (username, setData))

  data
}

let useViewMode: (~route: Shape.Profile.viewMode) => (Shape.Profile.viewMode, int => unit) = (
  ~route,
) => {
  let (viewMode, setViewMode) = React.useState(() => None)
  let finalViewMode = viewMode->Option.getWithDefault(route)

  React.useEffect2(() => {
    setViewMode(_prev => None)
    None
  }, (route, setViewMode))

  let changeOffset = offset =>
    setViewMode(_prev => Some(
      switch finalViewMode {
      | Author(username, limit, _offset) => Author(username, limit, offset)
      | Favorited(username, limit, _offset) => Favorited(username, limit, offset)
      },
    ))

  (finalViewMode, changeOffset)
}
