package main

import (
	"bytes"
	"crypto/rand"
	"crypto/sha256"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"net/http"
	"net/url"
	"time"

	"github.com/gorilla/sessions"
	"github.com/labstack/echo-contrib/session"
	"github.com/labstack/echo/v4"
	"github.com/labstack/echo/v4/middleware"
)

const (
	REQUEST_URL = "https://lichess.org/oauth"
	TOKEN_URL   = "https://lichess.org/api/token"
	EMAIL_URL   = "https://lichess.org/api/account/email"
	CLIENT_ID   = "mysuperuniqueclientidforlichess"
)

var (
	codeVerification = ""
	sessionStore     = make(map[string]*SessionStoreItem)
)

type TokenResponse struct {
	TokenType   string `json:"token_type"`
	AccessToken string `json:"access_token"`
	ExpiresIn   int64  `json:"expires_in"`
}

type SessionStoreItem struct {
	tr        *TokenResponse
	createdAt int64
}

// isExpired - returns whether the session token is expired or not if there is
// no tokenresponse stored it returns true because this should be revalidated
// in this case
func (ssi SessionStoreItem) isExpired() bool {
	if ssi.tr == nil {
		return true
	}
	if ssi.createdAt+ssi.tr.ExpiresIn >= time.Now().Unix() {
		return true
	}
	return false
}

func NewSessionStoreItem(tr TokenResponse, time int64) *SessionStoreItem {
	return &SessionStoreItem{tr: &tr, createdAt: time}
}

func randomString128() (string, error) {
	// 128 characters in base64 means 96 bytes (because base64 encodes 3 bytes into 4 chars)
	n := 96
	b := make([]byte, n)
	_, err := rand.Read(b)
	if err != nil {
		return "", err
	}

	// Encode to base64 URL-safe without padding
	s := base64.RawURLEncoding.EncodeToString(b)

	// s will be exactly 128 characters
	return s, nil
}

func main() {
	// Echo instance
	e := echo.New()

	// Middleware
	e.Use(middleware.Logger())
	e.Use(middleware.Recover())
	e.Use(session.Middleware(sessions.NewCookieStore([]byte("secret"))))

	// Routes
	e.GET("/login", request)
	e.GET("/redirect", callback)
	// TODO
	e.GET("/email", getEmail)
	e.GET("/", redirectHome)

	// Start server
	if err := e.Start(":8080"); err != nil && !errors.Is(err, http.ErrServerClosed) {
		slog.Error("failed to start server", "error", err)
	}
}

func getEmail(c echo.Context) error {
	sess, err := session.Get("session", c)
	if err != nil {
		fmt.Printf("Session error %v\n", sess)
		return c.Redirect(http.StatusFound, "/login")
	}
	val, ok := sess.Values["session_id"]
	if !ok {
		fmt.Printf("Session val not there %v\n", sess)
		return c.Redirect(http.StatusFound, "/login")
	}

	session_id := val.(string)
	auth := sessionStore[session_id].tr.AccessToken
	req, err := http.NewRequest("GET", EMAIL_URL, nil)
	if err != nil {
		return err
	}
	req.Header.Set("Authorization", fmt.Sprint("Bearer ", auth))

	fmt.Println("requesting...")
	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return fmt.Errorf("failed to send request")
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("not ok")
	}

	return c.HTML(200, fmt.Sprint("<b>Email:", string(body), "</b>"))
}

func redirectHome(c echo.Context) error {
	return c.Redirect(http.StatusFound, "http://localhost:5173/loggedin=true")
}

func callback(c echo.Context) error {
	sess, err := session.Get("session", c)
	if err != nil {
		fmt.Printf("Session error %v\n", sess)
		return c.Redirect(http.StatusFound, "/login")
	}
	val, ok := sess.Values["session_id"]
	if !ok {
		fmt.Printf("Session val not there %v\n", sess)
		return c.Redirect(http.StatusFound, "/login")
	}

	session_id := val.(string)

	r := c.Request()
	query := r.URL.Query()
	code := query.Get("code")
	state := query.Get("state")

	if code == "" || state == "" {
		return fmt.Errorf("Missing state or code")
	}

	// Exchange code for token
	form := url.Values{}
	form.Set("client_id", CLIENT_ID)
	form.Set("grant_type", "authorization_code")
	form.Set("code", code)
	form.Set("redirect_uri", "http://localhost:8080/redirect")
	form.Set("code_verifier", codeVerification)

	fmt.Printf("Body %s\n", form.Encode())
	req, err := http.NewRequest("POST", TOKEN_URL, bytes.NewBufferString(form.Encode()))
	if err != nil {
		return fmt.Errorf("Failed to create token")
	}
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")

	client := &http.Client{}
	fmt.Printf("Request %v", req)
	resp, err := client.Do(req)
	if err != nil {
		return fmt.Errorf("failed to send request")
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("not ok")
	}

	var tokenResp TokenResponse
	if err := json.Unmarshal(body, &tokenResp); err != nil {
		return fmt.Errorf("Failed to parse token response")
	}

	sessionStore[session_id] = NewSessionStoreItem(tokenResp, time.Now().Unix())

	return c.Redirect(http.StatusFound, "/")
}

func codeChallengeFromVerifier(verifier string) string {
	hash := sha256.Sum256([]byte(verifier))
	return base64.RawURLEncoding.EncodeToString(hash[:])
}

// Handler
func request(c echo.Context) error {
	sess, err := session.Get("session", c)
	if err != nil {
		return err
	}

	var session_id string
	val, ok := sess.Values["session_id"]
	if ok {
		session_id = val.(string)
		ssi, ok := sessionStore[session_id]
		if !ok {
			delete(sessionStore, session_id)
			delete(sess.Values, "session_id")
			if err := sess.Save(c.Request(), c.Response()); err != nil {
				return err
			}
			return c.Redirect(http.StatusFound, "/login")
		}
		// we have a auth token stored in the server
		if !ssi.isExpired() {
			return c.Redirect(http.StatusFound, "http://localhost:5173?loggedin=true")
		}
	} else {
		session_id, err = randomString128()
		if err != nil {
			return err
		}
		// create the session
		sess.Options = &sessions.Options{
			Path:     "/",
			MaxAge:   86400 * 7,
			HttpOnly: true,
		}
		// save the session id for the return of the flow
		sess.Values["session_id"] = session_id
		if err := sess.Save(c.Request(), c.Response()); err != nil {
			return err
		}
	}

	if codeVerification == "" {
		codeVerification, err = randomString128()
		if err != nil {
			return err
		}
	}

	challenge := codeChallengeFromVerifier(codeVerification)

	state, err := randomString128()
	if err != nil {
		return err
	}

	// Parse base URL
	u, err := url.Parse(REQUEST_URL)
	if err != nil {
		panic(err)
	}

	// Prepare query params
	q := u.Query() // get existing query params if any
	q.Set("response_type", "code")
	q.Set("client_id", CLIENT_ID)
	q.Set("redirect_uri", "http://localhost:8080/redirect")
	q.Set("code_challenge_method", "S256")
	q.Set("code_challenge", challenge)
	q.Set("scope", "email:read")
	q.Set("username", "")
	q.Set("state", state)

	u.RawQuery = q.Encode()

	fmt.Println(u.String())

	return c.Redirect(http.StatusFound, u.String())
}
