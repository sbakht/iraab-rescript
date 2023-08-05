let authenticated: (Shape.User.t => React.element, option<Shape.User.t>) => React.element = (
  getPage,
  user,
) =>
  switch user {
  | Some(s) => getPage(s)
  | None =>
    Link.home->Link.push
    React.null
  }

@react.component
let make = () => {
  let (currentUser, setCurrentUser) = Hook.useCurrentUser()
  let route = Route.useRoute()

  switch currentUser {
  | Init | Loading => React.null
  | Reloading(user) | Complete(user) =>
    <>
      <Header user />
      {switch route {
      | Settings => authenticated(user => <Settings user setUser=setCurrentUser />, user)
      | Login => <Login setUser=setCurrentUser />
      | Register => <Register setUser=setCurrentUser />
      | CreateArticle => authenticated(_user => <Editor />, user)
      | EditArticle(slug) => authenticated(_user => <Editor slug />, user)
      | Article(slug) => <Article slug user />
      | Profile(username) => <Profile viewMode={Shape.Profile.Author(username, 10, 0)} user />
      | Favorited(username) => <Profile viewMode={Shape.Profile.Favorited(username, 10, 0)} user />
      | Home => <Home user />
      }}
      <Footer />
    </>
  }
}
