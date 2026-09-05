# bbitter

A static feed of posts from a hand-picked list of X accounts.
A build step fetches the latest 20 posts per account and writes plain HTML.
GitHub Actions rebuilds the site every hour and publishes it to GitHub Pages.

## Tasks

| Task        | What it does                                                      |
|-------------|-------------------------------------------------------------------|
| `bb fetch`  | Fetches every account in `accounts.edn` into `tmp/feed.edn`.      |
| `bb render` | Renders `tmp/feed.edn` into `public/`. Makes no API requests.     |
| `bb build`  | Runs `fetch` then `render`.                                       |
| `bb serve`  | Serves `public/` on http://localhost:1889 for a local preview.    |

## Secrets

Five environment variables are needed:

```
BBITTER_ACCOUNTS
TWITTER_CONSUMER_KEY
TWITTER_CONSUMER_SECRET
TWITTER_OAUTH_TOKEN
TWITTER_OAUTH_TOKEN_SECRET
```

Locally they live in `.secrets.toml`, which `mise` loads into the shell.
`BBITTER_ACCOUNTS` is the handle list (see below). The consumer key and
secret identify a first-party X app. The two OAuth values are a long-lived
access token pair for your own account. You set these values by hand.

## GitHub Pages

1. Push this repository to GitHub.
2. Settings > Pages > Source: **GitHub Actions**.
3. Settings > Secrets and variables > Actions: add the five secrets above.
4. Run the "Build and deploy" workflow once from the Actions tab.

The workflow in `.github/workflows/build.yml` then runs every hour.

## Cost

Each build makes 2 API requests per account. X allows 500 requests per
15 minutes per endpoint.

## Accounts

The handle list comes from the `BBITTER_ACCOUNTS` environment variable,
a list of handles separated by spaces or commas, for example:

```
BBITTER_ACCOUNTS="paulg foo bar"
```

Set it in `.secrets.toml` locally and as a GitHub Actions secret for CI.
If the variable is unset, `bb fetch` falls back to a local `accounts.edn`
file (`{:accounts ["paulg" ...]}`), which is gitignored.

