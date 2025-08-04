import {
  type AccessContext,
  OAuth2AuthCodePKCE,
} from "@bity/oauth2-auth-code-pkce";
import { useEffect, useState } from "react";
import toast, { Toaster } from "react-hot-toast";

function App() {
  const [email, setEmail] = useState("");
  const [accessContext, setAccessContext] = useState<AccessContext>();

  const lichessHost = "https://lichess.org";
  const scopes = ["email:read"];
  const clientId = "lichessauthexample.com";
  const clientUrl = (() => {
    const url = new URL(location.href);
    url.search = "";
    return url.href;
  })();

  const auth = new OAuth2AuthCodePKCE({
    authorizationUrl: `${lichessHost}/oauth`,
    tokenUrl: `${lichessHost}/api/token`,
    clientId,
    scopes,
    redirectUrl: clientUrl,
    onAccessTokenExpiry: (refreshAccessToken) => refreshAccessToken(),
    onInvalidGrant: (_retry) => {},
  });

  function authButtonOnClick() {
    auth.fetchAuthorizationCode();
  }

  async function getEmail(fetch: any) {
    const res = await fetch(`${lichessHost}/api/account/email`);
    return (await res.json()).email;
  }

  useEffect(() => {
    async function FetchEmail() {
      try {
        const hasAuthCode = await auth.isReturningFromAuthServer();
        if (hasAuthCode) {
          // Might want to persist accessContext.token until the user logs out.
          const ac = await auth.getAccessToken();
          setAccessContext(ac);

          // Can also use this convenience wrapper for fetch() instead of
          // using manually using getAccessToken() and setting the
          // "Authorization: Bearer ..." header.
          const fetch = auth.decorateFetchHTTPClient(window.fetch);
          const newEmail = await getEmail(fetch);
          setEmail(newEmail);
        }
      } catch (err) {
        alert(err);
      }
    }
    const params = new URLSearchParams(window.location.search);
    const logged = params.get("loggedin");
    if (logged === "true") {
      toast.success("Logged in");
    }
    FetchEmail();
  }, []);

  return (
    <>
      <Toaster />
      <button onClick={authButtonOnClick}>Authenticate</button>
      <span>your email on lichess is {email}</span>
    </>
  );
}

export default App;
