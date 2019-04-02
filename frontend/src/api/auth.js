import auth0 from 'auth0-js';
import * as api from './index';
import * as rxjs from 'rxjs';
import { tap, map, filter, switchMap, distinctUntilChanged, multicast, refCount } from 'rxjs/operators';

import jwtDecode from 'jwt-decode';

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

    this.userInfo$ = this.isAuthenticated$
      .pipe(
        distinctUntilChanged(),
        filter(isAuth => isAuth),
        switchMap(() =>
          api.getUserInfo()
        ),
        multicast(() => new rxjs.BehaviorSubject(null)),
        refCount()
      );

    this.idTokenInfo$ = this.isAuthenticated$
      .pipe(
        map(() => this.idTokenInfo),
        multicast(() => new rxjs.BehaviorSubject(null)),
        refCount()
      );
  }

  get accessToken() {
    return localStorage.getItem('access_token');
  }

  get accessTokenInfo() {
    return jwtDecode(this.accessToken);
  }

  get userId() {
    return this.accessTokenInfo.sub;
  }

  get expiresAt() {
    const expiresAtStr = localStorage.getItem('expires_at');

    return expiresAtStr ? new Date(Number(expiresAtStr)) : null;
  }

  get idToken() {
    return localStorage.getItem('id_token');
  }

  get idTokenInfo() {
    if(!this.isAuthenticated()) {
      return null;
    }

    return jwtDecode(localStorage.getItem('id_token'));
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
