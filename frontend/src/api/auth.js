import auth0 from 'auth0-js';
import * as rxjs from 'rxjs';
import { tap } from 'rxjs/operators';

export class AuthService {
  constructor() {
    this._auth0 = new auth0.WebAuth({
      domain: process.env.AUTH0_DOMAIN,
      clientID: process.env.AUTH0_CLIENT_ID,
      redirectUri: process.env.AUTH0_REDIRECT_URI,
      audience: process.env.AUTH0_AUDIENCE,
      responseType: 'token id_token',
      scope: 'profile openid'
    });

    this.isAuthenticated$ = new rxjs.BehaviorSubject(this.isAuthenticated());
  }

  get accessToken() {
    return localStorage.getItem('access_token');
  }

  isAuthenticated() {
    const expiresAt = localStorage.getItem('expires_at');

    return expiresAt && Date.now() < expiresAt;
  }

  login() {
    this._auth0.authorize();
  }

  handleCallback() {
    return rxjs.bindNodeCallback(cb => this._auth0.parseHash(cb))()
      .pipe(tap(authResult => {
        console.log(authResult);
        localStorage.setItem('access_token', authResult.accessToken);
        localStorage.setItem('id_token', authResult.idToken);
        localStorage.setItem('expires_at', authResult.expiresIn * 1000 + Date.now());
        this.isAuthenticated$.next(true);
      }));
  }

  logout() {
    localStorage.removeItem('access_token');
    localStorage.removeItem('id_token');
    localStorage.removeItem('expires_at');
    this.isAuthenticated$.next(false);
    this._auth0.logout({
      returnTo: process.env.AUTH0_LOGOUT_URL
    });
  }
}

export default new AuthService();
