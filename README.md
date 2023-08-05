# ![RealWorld Example App](logo.png)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/jihchi/rescript-react-realworld-example-app)
![GitHub last commit](https://img.shields.io/github/last-commit/jihchi/rescript-react-realworld-example-app)
![GitHub](https://img.shields.io/github/license/jihchi/rescript-react-realworld-example-app)

> ### ReScript + React codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.

### [Demo](https://rescript-react-realworld-example-app.vercel.app)&nbsp;&nbsp;&nbsp;&nbsp;[RealWorld](https://github.com/gothinkster/realworld)

This codebase was created to demonstrate a fully fledged fullstack application built with **[ReScript & React](https://rescript-lang.org/docs/react/latest/introduction)** including CRUD operations, authentication, routing, pagination, and more.

We've gone to great lengths to adhere to the **ReScript & React** community styleguides & best practices.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.

# How it works

Basically its just like React single-page-application but written in [ReScript](https://rescript-lang.org/) with [React](https://reactjs.org/).

- Using [Vite](https://vitejs.dev/) as the frontend build tool
- Seamlessly integrate with [ReScript](https://rescript-lang.org/) (previously known as BuckleScript/ReasonML) and [rescript-react](https://rescript-lang.org/docs/react/latest/introduction)
- Routing - ReScript React [Router](https://rescript-lang.org/docs/react/latest/router)

# Getting started

You can view a live demo over at https://rescript-react-realworld-example-app.vercel.app

To get the frontend running locally:

```bash
git clone https://github.com/jihchi/rescript-react-realworld-example-app.git
cd rescript-react-realworld-example-app
pnpm install
pnpm start
```

Then open http://localhost:5173 to see your app.

When you’re ready to deploy to production, create a production build with `pnpm run build` and you will find result in folder `/dist`, after you created a production build, you can execute `pnpm run serve` to serve the folder.

## Contributors

Many thanks for your help!

<a href="https://github.com/jihchi/rescript-react-realworld-example-app/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=jihchi/rescript-react-realworld-example-app" />
</a>

The image of contributors is made with [contrib.rocks](https://contrib.rocks).
