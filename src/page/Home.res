module ArticlePreview = HomeArticlePreview
module PopularTags = HomePopularTags

let useFeedType = (~user: option<Shape.User.t>) =>
  React.useState(() =>
    switch user {
    | None => Shape.FeedType.Global(10, 0)
    | Some(_) => Shape.FeedType.Personal(10, 0)
    }
  )

@react.component
let make = (~user: option<Shape.User.t>) => {
  let (feedType, setFeedType) = useFeedType(~user)
  let (articles, setArticles) = Hook.useArticles(~feedType)
  let tags = Hook.useTags()
  let (toggleFavoriteBusy, onToggleFavorite) = Hook.useToggleFavorite(~setArticles, ~user)

  <div className="home-page">
    <div className="banner">
      <div className="container">
        <h1 className="logo-font"> {"conduit"->React.string} </h1>
        <p> {"A place to share your knowledge."->React.string} </p>
      </div>
    </div>
    <div className="container page">
      <div className="row">
        <div className="col-md-9">
          <WithTestId id="feed-toggle">
            <div className="feed-toggle">
              <ul className="nav nav-pills outline-active">
                <Security.AuthenticatedOnly user>
                  <li className="nav-item">
                    <a
                      className={switch feedType {
                      | Tag(_) | Global(_) => "nav-link"
                      | Personal(_) => "nav-link active"
                      }}
                      href="#your_feed"
                      onClick={event =>
                        if Utils.isMouseRightClick(event) {
                          event->ReactEvent.Mouse.preventDefault
                          setFeedType(_ => Personal(10, 0))
                        }}>
                      {"Your Feed"->React.string}
                    </a>
                  </li>
                </Security.AuthenticatedOnly>
                <li className="nav-item">
                  <a
                    className={switch feedType {
                    | Global(_) => "nav-link active"
                    | Tag(_) | Personal(_) => "nav-link"
                    }}
                    href="#global"
                    onClick={event =>
                      if Utils.isMouseRightClick(event) {
                        event->ReactEvent.Mouse.preventDefault
                        setFeedType(_ => Global(10, 0))
                      }}>
                    {"Global Feed"->React.string}
                  </a>
                </li>
                {switch feedType {
                | Tag(tag, _, _) =>
                  <li className="nav-item">
                    <a
                      className="nav-link active"
                      href="#"
                      onClick={event => event->ReactEvent.Mouse.preventDefault}>
                      <i className="ion-pound" />
                      {/* FIXME: Get rid of "space" string below */
                      " "->React.string}
                      {tag->React.string}
                    </a>
                  </li>
                | Global(_) | Personal(_) => React.null
                }}
                {if articles->AsyncResult.isBusy {
                  <li className="nav-item">
                    <Spinner />
                  </li>
                } else {
                  React.null
                }}
              </ul>
            </div>
          </WithTestId>
          {switch articles {
          | Init => React.null
          | Loading => React.null
          | Reloading(Ok({articles})) | Complete(Ok({articles})) =>
            articles
            ->Array.map(item =>
              <ArticlePreview
                key=item.slug
                data=item
                onToggleFavorite
                isFavoriteBusy={toggleFavoriteBusy->Belt.Set.String.has(_, item.slug)}
              />
            )
            ->React.array
          | Reloading(Error(_error)) | Complete(Error(_error)) => React.string("ERROR")
          }}
          {switch feedType {
          | Tag(_, limit, offset) | Global(limit, offset) | Personal(limit, offset) =>
            let total = switch articles {
            | Init | Loading | Reloading(Error(_)) | Complete(Error(_)) => 0
            | Reloading(Ok({articlesCount})) | Complete(Ok({articlesCount})) => articlesCount
            }

            <Pagination
              limit
              offset
              total
              onClick={offset =>
                setFeedType(x =>
                  switch x {
                  | Tag(tag, limit, _) => Tag(tag, limit, offset)
                  | Global(limit, _) => Global(limit, offset)
                  | Personal(limit, _) => Personal(limit, offset)
                  }
                )}
            />
          }}
        </div>
        <div className="col-md-3">
          <div className="sidebar">
            <PopularTags
              data=tags
              onClick={tag =>
                setFeedType(x =>
                  switch x {
                  | Tag(_, limit, offset) => Tag(tag, limit, offset)
                  | Global(_) | Personal(_) => Tag(tag, 10, 0)
                  }
                )}
            />
          </div>
        </div>
      </div>
    </div>
  </div>
}
