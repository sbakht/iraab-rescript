type t = {
  username: string,
  email: string,
  password: string,
}

let empty = ({username: "", email: "", password: ""}, None)

@react.component
let make = (~setUser) => {
  let (data, setData) = React.useState(() => AsyncData.complete(empty))
  let isBusy = data->AsyncData.isBusy
  let (form, error) = data->AsyncData.getValue->Option.getWithDefault(empty)

  let handleSignupClick = async () => {
    if isBusy {
      ignore()
    } else {
      setData(AsyncData.toBusy)
      switch await API.register(
        ~username=form.username,
        ~email=form.email,
        ~password=form.password,
        (),
      ) {
      | Ok(user: Shape.User.t) =>
        setUser(_prev => Some(user)->AsyncData.complete)
        setData(AsyncData.toIdle)
        Utils.setCookie(Constant.Auth.tokenCookieName, Some(user.token))
        Link.home->Link.push
      | Error(AppError.Fetch((_code, _message, #json(json)))) =>
        try {
          let result =
            json
            ->Js.Json.decodeObject
            ->Option.getExn
            ->Js.Dict.get("errors")
            ->Option.getExn
            ->Shape.Register.decode
          switch result {
          | Ok(errors) =>
            setData(prev =>
              prev->AsyncData.toIdle->AsyncData.map(((form, _error)) => (form, Some(errors)))
            )
          | Error(_e) => ignore()
          }
        } catch {
        | _ => Js.log("Button.UpdateSettings: failed to decode json")
        }
      | Error(Fetch((_, _, #text(_)))) | Error(Decode(_)) => setData(AsyncData.toIdle)
      }
    }
  }

  <div className="auth-page">
    <div className="container page">
      <div className="row">
        <div className="col-md-6 offset-md-3 col-xs-12">
          <h1 className="text-xs-center"> {"Sign up"->React.string} </h1>
          <p className="text-xs-center">
            <Link onClick={Link.login->Link.location}> {"Have an account?"->React.string} </Link>
          </p>
          {switch error {
          | Some(detail: Shape.Register.t) =>
            <ul className="error-messages">
              <ErrorDetails label="email" error=detail.email />
              <ErrorDetails label="password" error=detail.password />
              <ErrorDetails label="username" error=detail.username />
            </ul>
          | None => React.null
          }}
          <form>
            <fieldset className="form-group">
              <input
                className="form-control form-control-lg"
                type_="text"
                placeholder="Your Name"
                disabled=isBusy
                value=form.username
                onChange={event => {
                  let username = ReactEvent.Form.target(event)["value"]
                  setData(prev =>
                    prev->AsyncData.map(((form, error)) => ({...form, username}, error))
                  )
                }}
              />
            </fieldset>
            <fieldset className="form-group">
              <input
                className="form-control form-control-lg"
                type_="text"
                placeholder="Email"
                disabled=isBusy
                value=form.email
                onChange={event => {
                  let email = ReactEvent.Form.target(event)["value"]
                  setData(prev => prev->AsyncData.map(((form, error)) => ({...form, email}, error)))
                }}
              />
            </fieldset>
            <fieldset className="form-group">
              <input
                className="form-control form-control-lg"
                type_="password"
                placeholder="Password"
                disabled=isBusy
                value=form.password
                onChange={event => {
                  let password = ReactEvent.Form.target(event)["value"]
                  setData(prev =>
                    prev->AsyncData.map(((form, error)) => ({...form, password}, error))
                  )
                }}
              />
            </fieldset>
            <button
              className="btn btn-lg btn-primary pull-xs-right"
              disabled=isBusy
              onClick={event => {
                event->ReactEvent.Mouse.preventDefault
                event->ReactEvent.Mouse.stopPropagation
                handleSignupClick()->ignore
              }}>
              {"Sign up"->React.string}
            </button>
          </form>
        </div>
      </div>
    </div>
  </div>
}
